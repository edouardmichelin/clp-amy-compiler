package amyc
package interpreter

import amyc.utils._
import amyc.ast.SymbolicTreeModule._
import amyc.ast.Identifier
import amyc.analyzer.SymbolTable

// An interpreter for Amy programs, implemented in Scala
object Interpreter extends Pipeline[(Program, SymbolTable), Unit] {

  // A class that represents a value computed by interpreting an expression
  abstract class Value {
    def asInt: Int = this.asInstanceOf[IntValue].i
    def asBoolean: Boolean = this.asInstanceOf[BooleanValue].b
    def asString: String = this.asInstanceOf[StringValue].s

    override def toString: String = this match {
      case IntValue(i) => i.toString
      case BooleanValue(b) => b.toString
      case StringValue(s) => s
      case UnitValue => "()"
      case CaseClassValue(constructor, args) =>
        constructor.name + "(" + args.map(_.toString).mkString(", ") + ")"
    }
  }
  case class IntValue(i: Int) extends Value
  case class BooleanValue(b: Boolean) extends Value
  case class StringValue(s: String) extends Value
  case object UnitValue extends Value
  case class CaseClassValue(constructor: Identifier, args: List[Value]) extends Value

  def run(ctx: Context)(v: (Program, SymbolTable)): Unit = {
    val (program, table) = v

    // These built-in functions do not have an Amy implementation in the program,
    // instead their implementation is encoded in this map
    val builtIns: Map[(String, String), (List[Value]) => Value] = Map(
      ("Std", "printInt")    -> { args => println(args.head.asInt); UnitValue },
      ("Std", "T_fun")    -> { args => println(args.head.asInt); UnitValue },
      ("Std", "printString") -> { args => println(args.head.asString); UnitValue },
      ("Std", "readString")  -> { args => StringValue(scala.io.StdIn.readLine()) },
      ("Std", "readInt")     -> { args =>
        val input = scala.io.StdIn.readLine()
        try {
          IntValue(input.toInt)
        } catch {
          case ne: NumberFormatException =>
            ctx.reporter.fatal(s"""Could not parse "$input" to Int""")
        }
      },
      ("Std", "intToString")   -> { args => StringValue(args.head.asInt.toString) },
      ("Std", "digitToString") -> { args => StringValue(args.head.asInt.toString) }
    )

    // Utility functions to interface with the symbol table.
    def isConstructor(name: Identifier) = table.getConstructor(name).isDefined
    def findFunctionOwner(functionName: Identifier) = table.getFunction(functionName).get.owner.name
    def findFunction(owner: String, name: String) = {
      program.modules.find(_.name.name == owner).get.defs.collectFirst {
        case fd@FunDef(fn, _, _, _) if fn.name == name => fd
      }.get
    }

    // Interprets a function, using evaluations for local variables contained in 'locals'
    // TODO: Complete all missing cases. Look at the given ones for guidance.
    def interpret(expr: Expr)(implicit locals: Map[Identifier, Value]): Value = {
      expr match
        case Variable(name) =>
          locals(name)
        case IntLiteral(i) =>
          IntValue(i)
        case BooleanLiteral(b) =>
          BooleanValue(b)
        case StringLiteral(s) =>
          StringValue(s)
        case UnitLiteral() => UnitValue
        case Plus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt + interpret(rhs).asInt)
        case Minus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt - interpret(rhs).asInt)
        case Times(lhs, rhs) =>
          IntValue(interpret(lhs).asInt * interpret(rhs).asInt)
        case Div(lhs, rhs) =>
          val lhsVal = interpret(lhs).asInt
          val rhsVal = interpret(rhs).asInt
          

          if rhsVal == 0 then
            ctx.reporter.fatal("Error: Division by zero")
          else
            IntValue(lhsVal / rhsVal)
        case Mod(lhs, rhs) =>
          val lhsVal = interpret(lhs).asInt
          val rhsVal = interpret(rhs).asInt
          

          if rhsVal == 0 then
            ctx.reporter.fatal("Error: Division by zero")
          else
            IntValue( lhsVal % rhsVal)
        case LessThan(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt < interpret(rhs).asInt)
        case LessEquals(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt <= interpret(rhs).asInt)
        case And(lhs, rhs) =>
          BooleanValue(interpret(lhs).asBoolean && interpret(rhs).asBoolean)
        case Or(lhs, rhs) =>
          BooleanValue(interpret(lhs).asBoolean || interpret(rhs).asBoolean)
        case Equals(lhs, rhs) =>
          BooleanValue((interpret(lhs), interpret(rhs)) match
            case (IntValue(l), IntValue(r)) => l == r
            case (BooleanValue(l), BooleanValue(r)) => l == r
            case (UnitValue, UnitValue) => true
            case (l: StringValue, r: StringValue) => l eq r
            case (l: CaseClassValue, r: CaseClassValue) => l eq r
            case _ => false)
        case Concat(lhs, rhs) =>
          StringValue(interpret(lhs).asString ++ interpret(rhs).asString)
        case Not(e) =>
          BooleanValue(!interpret(e).asBoolean)
        case Neg(e) =>
          IntValue(-(interpret(e).asInt))
        case Call(qname, args) =>
          if isConstructor(qname) then
            CaseClassValue(qname, args.map(interpret(_)))
          else
            val owner = findFunctionOwner(qname)
            builtIns.get(owner, qname.name) match
              case Some(value: ((List[Value]) => Value)) =>
                value(args.map(interpret(_)))
              case None =>
                val func: FunDef = findFunction(owner, qname.name)

                val funcLocals: Map[Identifier, Value] =
                  func.params.map(_.name).zip(args.map(interpret(_))).toMap

                interpret(func.body)(funcLocals)

        case Sequence(e1, e2) =>
          interpret(e1)
          interpret(e2)
        case Let(df, value, body) =>
          interpret(body)(locals + (df.name -> interpret(value)))
        case Ite(cond, thenn, elze) =>
          if interpret(cond).asBoolean then interpret(thenn) else interpret(elze)
        case Match(scrut, cases) =>
          val evS = interpret(scrut)

          // Returns a list of pairs id -> value,
          // where id has been bound to value within the pattern.
          // Returns None when the pattern fails to match.
          // Note: Only works on well typed patterns (which have been ensured by the type checker).
          def matchesPattern(v: Value, pat: Pattern): Option[List[(Identifier, Value)]] = {
            ((v, pat): @unchecked) match {
              case (_, WildcardPattern()) =>
                Some(Nil)
              case (_, IdPattern(name)) =>
                Some(List(name -> v))
              case (IntValue(i1), LiteralPattern(IntLiteral(i2))) =>
                if i1 == i2 then Some(Nil)
                else None
              case (BooleanValue(b1), LiteralPattern(BooleanLiteral(b2))) =>
                if b1 == b2 then Some(Nil)
                else None
              case (StringValue(_), LiteralPattern(StringLiteral(_))) =>
                None
              case (UnitValue, LiteralPattern(UnitLiteral())) =>
                Some(Nil)
              case (CaseClassValue(con1, realArgs), CaseClassPattern(con2, formalArgs)) =>
                if con1 == con2 then
                  val matchCases: List[Option[List[(Identifier, Value)]]] =
                    realArgs.zip(formalArgs).map(matchesPattern)
                  if matchCases.exists(!_.isDefined) then
                    None
                  else
                    Some(matchCases.flatten.flatten)
                else None
            }
          }

          // Main "loop" of the implementation: Go through every case,
          // check if the pattern matches, and if so return the evaluation of the case expression
          for
            MatchCase(pat, rhs) <- cases
            moreLocals <- matchesPattern(evS, pat)
          do
            return interpret(rhs)(locals ++ moreLocals)

          // No case matched: The program fails with a match error
          ctx.reporter.fatal(s"Match error: ${evS.toString}@${scrut.position}")

          
        case Error(msg) =>
          ctx.reporter.fatal(s"Error: ${interpret(msg).asString}")
    }

    // Body of the interpreter: Go through every module in order
    // and evaluate its expression if present
    for {
      m <- program.modules
      e <- m.optExpr
    } {
      interpret(e)(Map())
    }
  }
}
