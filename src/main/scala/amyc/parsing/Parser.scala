package amyc
package parsing

import scala.language.implicitConversions

import amyc.ast.NominalTreeModule._
import amyc.utils._
import Tokens._
import TokenKinds._

import scallion._

// The parser for Amy
object Parser extends Pipeline[Iterator[Token], Program] with Parsers {

  type Token = amyc.parsing.Token
  type Kind = amyc.parsing.TokenKind

  import Implicits._

  override def getKind(token: Token): TokenKind = TokenKind.of(token)

  val eof: Syntax[Token] = elem(EOFKind)
  def op(string: String): Syntax[Token] = elem(OperatorKind(string))

  def op_(string: String): Syntax[String] = accept(OperatorKind(string)) {
    case _ => string
  }

  def kw(string: String): Syntax[Token] = elem(KeywordKind(string))

  def times = op_("*")
  def minus = op_("-")
  def not = op_("!")
  def div = op_("/")
  def plus = op_("+")
  def modulo = op_("%")
  def lt = op_("<")
  def lte = op_("<=")
  def conditionalAnd = op_("&&")
  def conditionalOr = op_("||")
  def conditionalEquals = op_("==")
  def concat = op_("++")
  def sequencing: Syntax[String] = accept(DelimiterKind(";")) { case _ =>
    ";"
  }
  def assign: Syntax[String] = accept(DelimiterKind("=")) { case _ =>
    "="
  }

  implicit def delimiter(string: String): Syntax[Token] = elem(
    DelimiterKind(string)
  )

  // An entire program (the starting rule for any Amy file).
  lazy val program: Syntax[Program] = many1(many1(module) ~<~ eof).map(ms =>
    Program(ms.flatten.toList).setPos(ms.head.head)
  )

  // A module (i.e., a collection of definitions and an initializer expression)
  lazy val module: Syntax[ModuleDef] =
    (kw("object") ~ identifier ~ many(definition) ~ opt(expr) ~ kw(
      "end"
    ) ~ identifier).map { case obj ~ id ~ defs ~ body ~ _ ~ id1 =>
      if id == id1 then ModuleDef(id, defs.toList, body).setPos(obj)
      else
        throw new AmycFatalError(
          "Begin and end module names do not match: " + id + " and " + id1
        )
    }

  // An identifier.
  val identifier: Syntax[String] = accept(IdentifierKind) {
    case IdentifierToken(name) => name
  }

  // An identifier along with its position.
  val identifierPos: Syntax[(String, Position)] = accept(IdentifierKind) {
    case id @ IdentifierToken(name) => (name, id.position)
  }

  lazy val e1: Syntax[Expr] = recursive(valBranch | seqBranch)

  lazy val seqBranch: Syntax[Expr] = (e2 ~ opt(delimiter(";").skip ~ e1)).map {
    case e2_ ~ Some(e1_) => Sequence(e2_, e1_).setPos(e1_)
    case e2_ ~ None      => e2_
  }

  lazy val valBranch: Syntax[Expr] =
    (kw("val") ~ parameter ~ delimiter("=").skip ~ e2 ~ delimiter(
      ";"
    ).skip ~ e1).map { case kw ~ param ~ e1_ ~ e2_ =>
      Let(param, e1_, e2_).setPos(kw)
    }

  lazy val ifBranch: Syntax[Expr] =
    (kw("if") ~ delimiter("(").skip ~ expr ~ delimiter(")").skip ~ delimiter(
      "{"
    ).skip ~ e1 ~ delimiter(
      "}"
    ).skip ~ kw(
      "else"
    ).skip ~ delimiter("{").skip ~ e1 ~ delimiter("}").skip).map {
      case kw ~ cond ~ trueExpr ~ falseExpr =>
        Ite(cond, trueExpr, falseExpr).setPos(kw)
    }

  lazy val amyMatch: Syntax[Match] =
    (e1 ~ kw("match").skip ~ delimiter("{").skip ~ many1(matchCase) ~ delimiter(
      "}"
    ).skip).map { case e ~ matchCases =>
      Match(e, matchCases.toList)
    }

  lazy val matchCase: Syntax[MatchCase] =
    (kw("case") ~ pattern ~ delimiter("=>") ~ e1).map { case kw ~ p ~ _ ~ e =>
      MatchCase(p, e).setPos(kw)
    }

  // lazy val e2: Syntax[Expr] = ifBranch

  lazy val e2: Syntax[Expr] = ((e3 | ifBranch) ~ many(
    kw("match").skip ~ delimiter("{").skip ~ many1(matchCase) ~ delimiter(
      "}"
    ).skip
  ))
    .map { case e ~ matches =>
      if matches.isEmpty then e
      else matches.foldLeft(e)((e, cases) => Match(e, cases.toList))
    }

  // A definition within a module.
  lazy val definition: Syntax[ClassOrFunDef] =
    fundef | caseClassDef | abstractClassDef

  lazy val fundef: Syntax[ClassOrFunDef] =
    (kw("fn").skip ~ identifier ~ delimiter("(").skip ~ parameters ~ delimiter(
      ")"
    ).skip ~ delimiter(":").skip ~ typeTree ~ delimiter("=").skip ~ delimiter(
      "{"
    ).skip ~ e1 ~ delimiter("}").skip).map { case id ~ params ~ typeTr ~ exp =>
      FunDef(id, params, typeTr, exp)
    }

  // lazy val fundef: Syntax[ClassOrFunDef] = (kw("fn")).map{
  //   case f => AbstractClassDef("hi")
  // }

  lazy val caseClassDef: Syntax[ClassOrFunDef] =
    (kw("case") ~ kw("class") ~ identifier ~ delimiter(
      "("
    ) ~ arguments ~ delimiter(")") ~ kw("extends") ~ identifier).map {

      case cas ~ clas ~ id ~ _ ~ params ~ _ ~ extend ~ idparent =>
        CaseClassDef(id, params, idparent)
    }

  lazy val abstractClassDef: Syntax[ClassOrFunDef] =
    (kw("abstract") ~ kw("class") ~ identifier).map { case _ ~ _ ~ id =>
      AbstractClassDef(id)

    }

  // A list of parameter definitions.
  lazy val parameters: Syntax[List[ParamDef]] =
    repsep(parameter, ",").map(_.toList)

  lazy val expressions: Syntax[List[Expr]] = repsep(e2, ",").map(_.toList)
  lazy val expressions1: Syntax[List[Expr]] = repsep(e1, ",").map(_.toList)

  lazy val arguments: Syntax[List[TypeTree]] =
    repsep(onlyTypes, ",").map(_.toList)

  lazy val patterns: Syntax[List[Pattern]] = repsep(pattern, ",").map(_.toList)

  lazy val onlyTypes: Syntax[TypeTree] =
    identifier.skip ~ delimiter(":").skip ~ typeTree

  // A parameter definition, i.e., an identifier along with the expected type.
  lazy val parameter: Syntax[ParamDef] =
    (identifier ~ delimiter(":") ~ typeTree).map { case id ~ _ ~ typeTr =>
      ParamDef(id, typeTr)
    }

  // A type expression.
  lazy val typeTree: Syntax[TypeTree] = primitiveType | identifierType

  // A built-in type (such as `Int`).
  val primitiveType: Syntax[TypeTree] =
    (accept(PrimTypeKind) { case tk @ PrimTypeToken(name) =>
      TypeTree(name match {
        case "Unit"    => UnitType
        case "Boolean" => BooleanType
        case "Int"     => IntType
        case "String"  => StringType
        case "Char"    => CharType
        case _ =>
          throw new java.lang.Error("Unexpected primitive type name: " + name)
      }).setPos(tk)
    } ~ opt("(" ~ literal ~ ")")).map {
      case (prim @ TypeTree(IntType)) ~ Some(_ ~ IntLiteral(32) ~ _) => prim
      case TypeTree(IntType) ~ Some(_ ~ IntLiteral(width) ~ _) =>
        throw new AmycFatalError(
          "Int type can only be used with a width of 32 bits, found : " + width
        )
      case TypeTree(IntType) ~ Some(_ ~ lit ~ _) =>
        throw new AmycFatalError(
          "Int type should have an integer width (only 32 bits is supported)"
        )
      case TypeTree(IntType) ~ None =>
        throw new AmycFatalError(
          "Int type should have a specific width (only 32 bits is supported)"
        )
      case prim ~ Some(_) =>
        throw new AmycFatalError("Only Int type can have a specific width")
      case prim ~ None => prim
    }

  // A user-defined type (such as `List`).
  // lazy val identifierType: Syntax[TypeTree] =(opt( identifier ~ delimiter(".") ) ~ identifier ).map {
  //   case Some( id1 ~ dot  ) ~ id2 =>
  //       TypeTree(ClassType( QualifiedName( Some(id1) , id2   )))
  //   case  None ~ id2 =>
  //       TypeTree(ClassType( QualifiedName(None, id2) ))
  //   }

  lazy val identifierType: Syntax[TypeTree] =
    (identifierPos ~ opt(delimiter(".").skip ~ identifier)).map {
      case (module, pos) ~ Some(name) =>
        TypeTree(ClassType(QualifiedName(Some(module), name))).setPos(pos)
      case (name, pos) ~ None => TypeTree(ClassType(QualifiedName(None, name)))
    }

  // An expression.
  // HINT: You can use `operators` to take care of associativity and precedence

  lazy val e3: Syntax[Expr] = recursive {
    operators(operand)(
      times | div | modulo is LeftAssociative,
      plus | minus | concat is LeftAssociative,
      lt | lte is LeftAssociative,
      conditionalEquals is LeftAssociative,
      conditionalAnd is LeftAssociative,
      conditionalOr is LeftAssociative
    ) {
      case (e1, "+", e2)  => Plus(e1, e2)
      case (e1, "-", e2)  => Minus(e1, e2)
      case (e1, "*", e2)  => Times(e1, e2)
      case (e1, "/", e2)  => Div(e1, e2)
      case (e1, "%", e2)  => Mod(e1, e2)
      case (e1, "<=", e2) => LessEquals(e1, e2)
      case (e1, "<", e2)  => LessThan(e1, e2)
      case (e1, "&&", e2) => And(e1, e2)
      case (e1, "||", e2) => Or(e1, e2)
      case (e1, "==", e2) => Equals(e1, e2)
      case (e1, "++", e2) => Concat(e1, e2)

    }

  }

  lazy val expr: Syntax[Expr] = e1

  // A literal expression.
  lazy val literal: Syntax[Literal[_]] = genLiteral

  lazy val literal2: Syntax[Literal[_]] = genLiteral | unitLiteral

  lazy val genLiteral: Syntax[Literal[_]] = accept(LiteralKind) {
    case BoolLitToken(value) => BooleanLiteral(value)
    case IntLitToken(value)  => IntLiteral(value)
    case CharLitToken(value) =>
      value match
        case x if x.startsWith("\\") => CharLiteral(x.charAt(1), true)
        case _                       => CharLiteral(value.charAt(0))
    case StringLitToken(value) => StringLiteral(value)
  }

  lazy val unitLiteral: Syntax[Literal[_]] =
    (delimiter("(") ~ delimiter(")")).map { case _ ~ _ =>
      UnitLiteral()
    }

  lazy val matchKeyword: Syntax[Expr] =
    (expr ~ kw("match") ~ delimiter("{") ~ many1(matchCase) ~ delimiter("}"))
      .map { case e1 ~ _ ~ _ ~ matchCases ~ _ =>
        Match(e1, matchCases.toList)
      }

  // A pattern as part of a mach case.
  lazy val pattern: Syntax[Pattern] = recursive {
    idPattern | literalPattern | wildPattern
  }

  lazy val operand: Syntax[Expr] = (opt(unaryOperator) ~ simpleExpr).map {
    case Some("!") ~ e => Not(e)
    case Some("-") ~ e => Neg(e)
    case None ~ e      => e
  }

  lazy val literalPattern: Syntax[Pattern] = literal2.map { case lit =>
    LiteralPattern(lit)
  }

  lazy val idPattern: Syntax[Pattern] =
    (identifierPos ~ opt(delimiter(".").skip ~ identifierPos) ~ opt(
      delimiter("(") ~ repsep(pattern, ",") ~ delimiter(")")
    )).map {
      case (module, pos) ~ Some((name, _)) ~ Some(_ ~ args ~ _) =>
        CaseClassPattern(QualifiedName(Some(module), name), args.toList)
          .setPos(pos)
      case (name, pos) ~ None ~ Some(_ ~ args ~ _) =>
        CaseClassPattern(QualifiedName(None, name), args.toList).setPos(pos)
      case (name, pos) ~ None ~ None => IdPattern(name).setPos(pos)
    }

  lazy val wildPattern: Syntax[Pattern] = kw("_").map { case _ =>
    WildcardPattern()
  }

  // HINT: It is useful to have a restricted set of expressions that don't include any more operators on the outer level.
  lazy val simpleExpr: Syntax[Expr] =
    literal.up[Expr] | variableOrCall | leftFactored | errorExpr

  lazy val leftFactored: Syntax[Expr] =
    (delimiter("(") ~ opt(e1) ~ delimiter(")")).map {
      case _ ~ Some(expr) ~ _ => expr
      case _ ~ None ~ _       => UnitLiteral()
    }

  lazy val unaryOperator: Syntax[String] = minus | not

  lazy val factor: Syntax[Expr] = (unaryOperator.opt ~ simpleExpr).map {
    case Some("-") ~ s_expr => Neg(s_expr)
    case Some("!") ~ s_expr => Not(s_expr)
    case None ~ s_expr      => s_expr
    case Some(op) ~ s_expr =>
      throw new AmycFatalError(
        s"Syntax Error: Unknown unary operator - ${op}  "
      )
  }

  lazy val variable: Syntax[Expr] = accept(IdentifierKind) {
    case IdentifierToken(name) => Variable(name)
  }

  lazy val variableOrCall: Syntax[Expr] = (identifier ~ opt(
    delimiter(".").skip ~ identifier
  ) ~ opt(delimiter("(") ~ expressions1 ~ delimiter(")"))).map {

    case id1 ~ Some(id2) ~ Some(_ ~ params ~ _) =>
      Call(QualifiedName(Some(id1), id2), params)
    case id1 ~ None ~ Some(_ ~ params ~ _) =>
      Call(QualifiedName(None, id1), params)

    case id1 ~ None ~ None => Variable(id1)

    case _ => throw new AmycFatalError("match error : variableOrCall")

  }

  lazy val errorExpr: Syntax[Expr] =
    (kw("error") ~ delimiter("(") ~ expr ~ delimiter(")")).map {
      case err ~ _ ~ exp ~ _ => Error(exp)
    }

  // lazy val assignment: Syntax[Expr] = ( kw("val") ~  parameter ~ assign ~ expropWithoutSemicolon ~ delimiter(";") ~ expr   ).map {
  //   case _ ~ param ~ _ ~ e1 ~_~e2 =>
  //     Let(param ,  e1 , e2)
  // }

  // TODO: Other definitions.
  //       Feel free to decompose the rules in whatever way convenient.

  // Ensures the grammar is in LL(1)
  lazy val checkLL1: Boolean = {
    if (program.isLL1) {
      true
    } else {
      // Set `showTrails` to true to make Scallion generate some counterexamples for you.
      // Depending on your grammar, this may be very slow.
      val showTrails = false
      debug(program, showTrails)
      false
    }
  }

  override def run(ctx: Context)(tokens: Iterator[Token]): Program = {
    import ctx.reporter._
    if (!checkLL1) {
      ctx.reporter.fatal("program grammar is not LL1!")
    }

    // ctx.reporter.fatal(s"${fundef.first}")

    val parser = Parser(program)

    parser(tokens) match {
      case Parsed(result, rest) => result
      case UnexpectedEnd(rest)  => fatal("Unexpected end of input.")
      case UnexpectedToken(token, rest) =>
        fatal(
          "Unexpected token: " + token + ", possible kinds: " + rest.first
            .map(_.toString)
            .mkString(", ")
        )
    }
  }
}
