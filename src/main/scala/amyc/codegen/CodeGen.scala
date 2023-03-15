package amyc
package codegen

import analyzer._
import amyc.ast.Identifier
import amyc.ast.SymbolicTreeModule.{
  Call => AmyCall,
  Div => AmyDiv,
  And => AmyAnd,
  Or => AmyOr,
  _
}
import amyc.utils.{Context, Pipeline, Character}
import wasm._
import Instructions._
import Utils._

// Generates WebAssembly code for an Amy program
object CodeGen extends Pipeline[(Program, SymbolTable), Module] {
  def run(ctx: Context)(v: (Program, SymbolTable)): Module = {
    val (program, table) = v

    val seq = (a: Code, b: Code) => a <:> b

    // Generate code for an Amy module
    def cgModule(moduleDef: ModuleDef): List[Function] = {
      val ModuleDef(name, defs, optExpr) = moduleDef
      // Generate code for all functions
      defs.collect {
        case fd: FunDef if !builtInFunctions(fullName(name, fd.name)) =>
          cgFunction(fd, name, false)
      } ++
        // Generate code for the "main" function, which contains the module expression
        optExpr.toList.map { expr =>
          val mainFd =
            FunDef(Identifier.fresh("main"), Nil, TypeTree(IntType), expr)
          cgFunction(mainFd, name, true)
        }
    }

    // Generate code for a function in module 'owner'
    def cgFunction(fd: FunDef, owner: Identifier, isMain: Boolean): Function = {
      // Note: We create the wasm function name from a combination of
      // module and function name, since we put everything in the same wasm module.
      val name = fullName(owner, fd.name)
      Function(name, fd.params.size, isMain) { lh =>
        val locals = fd.paramNames.zipWithIndex.toMap
        val body = cgExpr(fd.body)(locals, lh)
        val comment = Comment(fd.toString)
        if (isMain) {
          body <:> Drop // Main functions do not return a value,
          // so we need to drop the value generated by their body
        } else {
          comment <:> body
        }
      }
    }

    // Generate code for an expression expr.
    // Additional arguments are a mapping from identifiers (parameters and variables) to
    // their index in the wasm local variables, and a LocalsHandler which will generate
    // fresh local slots as required.
    def cgExpr(
        expr: Expr
    )(implicit locals: Map[Identifier, Int], lh: LocalsHandler): Code = {
      expr match {
        case IntLiteral(i) =>
          // Push i to the stack.
          // The comments are optional but can help you debug.
          Comment(expr.toString) <:> Const(i)

        case BooleanLiteral(value) =>
          if (value) then Const(1) else Const(0)

        case c: CharLiteral =>
          val chr = Character.getValue(c)
          Const(chr.toInt)

        case StringLiteral(value) =>
          mkString(value)

        case UnitLiteral() =>
          Const(0)

        case Plus(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Add

        case Minus(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Sub

        case Times(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Mul

        case AmyDiv(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Div

        case Mod(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Rem

        case LessThan(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Lt_s

        case LessEquals(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Le_s

        case AmyAnd(lhs, rhs) =>
          // if lhs is false then we should not continue
          cgExpr(lhs) <:>
            If_i32 <:>
            cgExpr(rhs) <:>
            Else <:>
            Const(0) <:>
            End

        case AmyOr(lhs, rhs) =>
          // if lhs is true then no need to continue
          cgExpr(lhs) <:>
            If_i32 <:>
            Const(1) <:>
            Else <:>
            cgExpr(rhs) <:>
            End

        case Equals(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Eq

        case Concat(lhs, rhs) =>
          cgExpr(lhs) <:> cgExpr(rhs) <:> Call("String_concat")

        case Not(e) =>
          cgExpr(e) <:> If_i32 <:> Const(0) <:> Else <:> Const(1) <:> End

        case Neg(e) =>
          Const(0) <:> cgExpr(e) <:> Sub

        case Ite(i, t, e) =>
          cgExpr(i) <:> If_i32 <:> cgExpr(t) <:> Else <:> cgExpr(e) <:> End

        case AmyCall(qname, args) =>
          table.getConstructor(qname) match
            // simple function
            case None =>
              val sig = table.getFunction(qname).get
              args.map(cgExpr) <:> Call(
                fullName(sig.owner, qname)
              )
            // constructor
            case Some(constrSig) =>
              val pointer = lh.getFreshLocal()
              val saveBoundary = GetGlobal(memoryBoundary) <:> SetLocal(pointer)
              val incrBoundary =
                GetGlobal(memoryBoundary) <:> adtField(args.size) <:> SetGlobal(
                  memoryBoundary
                )
              val storeConstr =
                GetLocal(pointer) <:> Const(constrSig.index) <:> Store

              val storeFields = args.zipWithIndex.map { case (field, index) =>
                val offset = index + 1
                GetLocal(pointer) <:> Const(4 * offset) <:> Add <:> cgExpr(
                  field
                ) <:> Store

              }

              saveBoundary <:> incrBoundary <:> storeConstr <:> cs2c(
                storeFields
              ) <:> GetLocal(pointer)

        case Let(ParamDef(name, tt), value, body) =>
          val newLocal = lh.getFreshLocal()
          cgExpr(value) <:> SetLocal(newLocal) <:> cgExpr(body)(
            locals + (name -> newLocal),
            lh
          )

        case Sequence(e1, e2) =>
          cgExpr(e1) <:> Drop <:> cgExpr(e2)

        case Match(scrut, cases) =>
          // Checks if a value matches a pattern.
          // Assumes value is on top of stack (and CONSUMES it)
          // Returns the code to check the value, and a map of bindings.
          def matchAndBind(pat: Pattern): (Code, Map[Identifier, Int]) =
            pat match {
              case IdPattern(id) =>
                val idLocal = lh.getFreshLocal()
                (
                  Comment(pat.toString) <:>
                    // Assign val to id.
                    SetLocal(idLocal) <:>
                    // Return true (IdPattern always matches).
                    Const(1),
                  // Let the code generation of the expression which corresponds to this pattern
                  // know that the bound id is at local idLocal.
                  Map(id -> idLocal)
                )

              case WildcardPattern()       => (Drop <:> Const(1), Map())
              case LiteralPattern(literal) => (cgExpr(literal) <:> Eq, Map())
              case CaseClassPattern(constr, args) =>
                val pointer = lh.getFreshLocal()
                val constrSig = table.getConstructor(constr).get
                val (code, spltBindings) =
                  (args.zipWithIndex.map { case (pattern, index) =>
                    val offset = index + 1
                    val (code, bindings) = matchAndBind(pattern)
                    // load *(pointer + offset)
                    // check that the index-th pattern/field matches the corresponding value
                    // AND the accumalated matchAndBinds with the last one
                    (
                      GetLocal(pointer) <:> Const(
                        4 * offset
                      ) <:> Add <:> Load <:> code <:> And,
                      bindings
                    )
                  }).unzip

                (
                  SetLocal(
                    pointer
                  ) <:> // save the address pointing to the properties of CaseClassPattern (currently on the stack) in a local
                    GetLocal(pointer) <:>
                    Load <:> // load *(pointer)
                    Const(
                      constrSig.index
                    ) <:> // put the index of the signature of the constructor on the stack in order to compare it with the one in the value (v)
                    Eq <:>
                    If_i32 <:>
                    Const(1) <:> // start value of the AND accumulator (= true)
                    cs2c(code) <:>
                    Else <:>
                    Const(0) <:>
                    End,
                  spltBindings.flatten.toMap
                )
              case _ => (Unreachable, Map())
            }

          val pointer = lh.getFreshLocal()
          val matchedCasePointer = lh.getFreshLocal()
          val break = getFreshLabel("break")

          def cases2c(cases: List[MatchCase]): Code = cases match
            case MatchCase(pat, expr) :: cs =>
              val (code, bindings) = matchAndBind(pat)
              GetLocal(pointer) <:>
                code <:>
                If_void <:>
                cgExpr(expr)(locals ++ bindings, lh) <:>
                SetLocal(matchedCasePointer) <:>
                Br(break) <:> // Amy spec: break in case of a match
                Else <:>
                cases2c(cs) <:>
                End
            case Nil =>
              mkString("Match error!") <:>
                Call("Std_printString") <:>
                Unreachable

          // Why doesn't this work???
          // def cases2c(cases: List[MatchCase]): Code =
          //   (cases.foldLeft(Code(Nil)) { (acc: Code, matchCase: MatchCase) =>
          //     acc <:> (matchCase match
          //       case MatchCase(pat, expr) =>
          //         val (code, bindings) = matchAndBind(pat)
          //         GetLocal(pointer) <:>
          //           code <:>
          //           If_void <:>
          //           cgExpr(expr)(locals ++ bindings, lh) <:>
          //           SetLocal(matchedCasePointer) <:>
          //           Br(break) <:> // Amy spec: break in case of a match
          //           Else
          //     )
          //   }) <:> mkString("Match error!") <:>
          //     Call("Std_printString") <:>
          //     Unreachable <:>
          //     End

          Block(break) <:>
            cgExpr(scrut) <:>
            SetLocal(pointer) <:>
            cases2c(cases) <:>
            End <:>
            GetLocal(matchedCasePointer)

        case Variable(name) =>
          GetLocal(locals(name))

        case Error(msg) =>
          cgExpr(Concat(StringLiteral("Error: "), msg)) <:> Call(
            "Std_printString"
          ) <:> Unreachable

        case _ =>
          Unreachable
      }
    }

    Module(
      program.modules.last.name.name,
      defaultImports,
      globalsNo,
      wasmFunctions ++ (program.modules flatMap cgModule)
    )

  }
}