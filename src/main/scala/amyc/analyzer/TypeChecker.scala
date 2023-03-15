package amyc
package analyzer

import amyc.utils._
import amyc.ast.SymbolicTreeModule._
import amyc.ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker
    extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit
        env: Map[Identifier, Type]
    ): List[Constraint] = {

      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))

      e match {
        case IntLiteral(_) =>
          topLevelConstraint(IntType)
        case BooleanLiteral(v) =>
          topLevelConstraint(BooleanType)
        case CharLiteral(v, _) =>
          topLevelConstraint(CharType)
        case StringLiteral(v) =>
          topLevelConstraint(StringType)
        case UnitLiteral() =>
          topLevelConstraint(UnitType)

        case Equals(lhs, rhs) =>
          // HINT: Take care to implement the specified Amy semantics
          val tv1 = TypeVariable.fresh()
          val c1 = genConstraints(lhs, tv1)
          val c2 = genConstraints(rhs, tv1)

          topLevelConstraint(BooleanType) ++ c1 ++ c2

        case And(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++ genConstraints(
            lhs,
            BooleanType
          ) ++ genConstraints(rhs, BooleanType)

        case Or(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++ genConstraints(
            lhs,
            BooleanType
          ) ++ genConstraints(rhs, BooleanType)

        case LessEquals(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++ genConstraints(
            lhs,
            IntType
          ) ++ genConstraints(rhs, IntType)

        case LessThan(lhs, rhs) =>
          topLevelConstraint(BooleanType) ++ genConstraints(
            lhs,
            IntType
          ) ++ genConstraints(rhs, IntType)

        case Not(e2) =>
          topLevelConstraint(BooleanType) ++ genConstraints(e2, BooleanType)
        case Neg(e2) =>
          topLevelConstraint(IntType) ++ genConstraints(e2, IntType)

        case Plus(lhs, rhs) =>
          topLevelConstraint(IntType) ++ genConstraints(
            lhs,
            IntType
          ) ++ genConstraints(rhs, IntType)
        case Minus(lhs, rhs) =>
          topLevelConstraint(IntType) ++ genConstraints(
            lhs,
            IntType
          ) ++ genConstraints(rhs, IntType)
        case Times(lhs, rhs) =>
          topLevelConstraint(IntType) ++ genConstraints(
            lhs,
            IntType
          ) ++ genConstraints(rhs, IntType)
        case Div(lhs, rhs) =>
          topLevelConstraint(IntType) ++ genConstraints(
            lhs,
            IntType
          ) ++ genConstraints(rhs, IntType)
        case Mod(lhs, rhs) =>
          topLevelConstraint(IntType) ++ genConstraints(
            lhs,
            IntType
          ) ++ genConstraints(rhs, IntType)

        case Ite(cond, thenn, elze) =>
          val tv = TypeVariable.fresh()
          topLevelConstraint(tv) ++ genConstraints(thenn, tv) ++ genConstraints(
            elze,
            tv
          ) ++ genConstraints(cond, BooleanType)

        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)

          val endType = TypeVariable.fresh()
          def handlePattern(
              pat: Pattern,
              scrutExpected: Type
          ): (List[Constraint], Map[Identifier, Type]) = {
            val tv = TypeVariable.fresh()
            val mc = Constraint(tv, scrutExpected, pat.position)

            pat match {
              case LiteralPattern(lit) =>
                (genConstraints(lit, scrutExpected), Map())
              case WildcardPattern() => (Nil, Map())
              case IdPattern(id)     => (Nil, Map(id -> scrutExpected))
              case CaseClassPattern(constr, args) =>
                table.getConstructor(constr) match
                  case None =>
                    ctx.reporter.fatal(
                      "Fatal error",
                      e.position
                    ) // should never happen
                  case Some(sig) =>
                    val argConstraintList =
                      args.zip(sig.argTypes).map(handlePattern(_, _))
                    val argConstraint = argConstraintList.flatMap(_._1)
                    val argMap = argConstraintList
                      .flatMap(_._2)
                      .toMap + (constr -> scrutExpected)

                    (
                      argConstraint :+ Constraint(
                        sig.retType,
                        scrutExpected,
                        pat.position
                      ),
                      argMap
                    )
            }
          }

          def handleCase(
              cse: MatchCase,
              scrutExpected: Type
          ): List[Constraint] = {
            val handlep = handlePattern(cse.pat, scrutExpected)

            val bodyConstraints =
              genConstraints(cse.expr, endType)(env ++ handlep._2)
            handlep._1 ++ bodyConstraints
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))

        case Error(emsg) =>
          topLevelConstraint(expected) ++ genConstraints(emsg, StringType)

        case Sequence(e1, e2) => {
          val tv = TypeVariable.fresh()
          val tv1 = TypeVariable.fresh()
          topLevelConstraint(tv) ++ genConstraints(e1, tv1) ++ genConstraints(
            e2,
            tv
          )
        }

        case Let(ParamDef(name, TypeTree(tt)), value, body) => {
          val bodyType = TypeVariable.fresh()
          topLevelConstraint(bodyType) ++ genConstraints(
            value,
            tt
          ) ++ genConstraints(body, bodyType)(env + (name -> tt))
        }

        case Call(qname, args) =>
          val fn = table.getFunction(qname)
          fn match {
            case Some(FunSig(argTypes, retType, owner)) =>
              args
                .zip(argTypes)
                .flatMap(genConstraints(_, _)) ++ topLevelConstraint(retType)

            case None =>
              table.getConstructor(qname) match {
                case Some(cs @ ConstrSig(argTypes, parent, index)) =>
                  args
                    .zip(argTypes)
                    .flatMap((arg1, argType1) =>
                      genConstraints(arg1, argType1)
                    ) ++ topLevelConstraint(cs.retType)

                case None =>
                  ctx.reporter.fatal(
                    "Fatal error",
                    e.position
                  ) // should never happen
              }
          }

        case Concat(lhs, rhs) =>
          topLevelConstraint(StringType) ++ genConstraints(
            lhs,
            StringType
          ) ++ genConstraints(rhs, StringType)

        case Variable(name) =>
          topLevelConstraint(env(name))

      }
    }

    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(
        constraints: List[Constraint],
        from: Int,
        to: Type
    ): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other                => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and report errors
    //  using `ctx.reporter.error` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil                                      => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          expected match {
            case TypeVariable(id) => solveConstraints(subst_*(more, id, found))
            case _ => {
              found match {
                case TypeVariable(id2) =>
                  solveConstraints(Constraint(expected, found, pos) :: more)
                case _ =>
                  if (!expected.toString().equals(found.toString())) then
                    ctx.reporter.fatal(
                      s"Type mismatch at pos: ${pos.toString()} ${expected
                          .toString()} != ${found.toString()} "
                    )
                  else solveConstraints(more)
              }
            }
          }

      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map { case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}
