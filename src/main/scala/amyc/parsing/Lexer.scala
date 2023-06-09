package amyc
package parsing

import amyc.utils._
import java.io.File

import silex._

import amyc.utils.Position
import scala.util.Try
import java.nio.charset.Charset

// The lexer for Amy.
object Lexer extends Pipeline[List[File], Iterator[Token]] with Lexers {

  /** Tiny Silex reference: \============================== Silex's lexer
    * essentially allows you to define a list of regular expressions in their
    * order of priority. To tokenize a given input stream of characters, each
    * individual regular expression is applied in turn. If a given expression
    * matches, it is used to produce a token of maximal length. Whenever a
    * regular expression does not match, the expression of next-highest priority
    * is tried. The result is a stream of tokens.
    *
    * Regular expressions `r` can be built using the following operators:
    *   - `word("abc")` matches the sequence "abc" exactly
    *   - `r1 | r2` matches either expression `r1` or expression `r2`
    *   - `r1 ~ r2` matches `r1` followed by `r2`
    *   - `oneOf("xy")` matches either "x" or "y" (i.e., it is a shorthand of
    *     `word` and `|` for single characters)
    *   - `elem(c)` matches character `c`
    *   - `elem(f)` matches any character for which the boolean predicate `f`
    *     holds
    *   - `opt(r)` matches `r` or nothing at all
    *   - `many(r)` matches any number of repetitions of `r` (including none at
    *     all)
    *   - `many1(r)` matches any non-zero number of repetitions of `r`
    *
    * To define the token that should be output for a given expression, one can
    * use the `|>` combinator with an expression on the left-hand side and a
    * function producing the token on the right. The function is given the
    * sequence of matched characters and the source-position range as arguments.
    *
    * For instance,
    *
    * `elem(_.isDigit) ~ word("kg") |> { (cs, range) =>
    * WeightLiteralToken(cs.mkString).setPos(range._1)) }`
    *
    * will match a single digit followed by the characters "kg" and turn them
    * into a "WeightLiteralToken" whose value will be the full string matched
    * (e.g. "1kg").
    */

  // Type of characters consumed.
  type Character = Char

  // Type of positions.
  type Position = SourcePosition

  // Type of tokens produced.
  type Token = parsing.Token

  import Tokens._

  val lexer = Lexer(
    // Keywords,
    word("abstract") | word("case") | word("class") |
      word("fn") | word("else") | word("extends") |
      word("if") | word("match") | word("object") |
      word("val") | word("error") | word("_") | word("end")
      |> { (cs, range) => KeywordToken(cs.mkString).setPos(range._1) },

    // Primitive type names,
    word("Byte") | word("Int") | word("Long") | word("Short") | word(
      "Double"
    ) | word("Float") | word("Boolean") | word("Char") | word("String") | word(
      "Unit"
    )
      |> { (cs, range) => PrimTypeToken(cs.mkString).setPos(range._1) },

    // Boolean literals,
    word("true") | word("false")
      |> { (cs, range) =>
        BoolLitToken(cs.mkString.toBoolean).setPos(range._1)
      },

    // Operators,
    oneOf("+-*/%<!") | word("<=") | word("==") | word("&&") | word("||") | word(
      "++"
    )
      |> { (cs, range) => OperatorToken(cs.mkString).setPos(range._1) },

    // Identifiers,
    elem(_.isLetter) ~ many(elem(_.isDigit) | elem(_.isLetter) | word("_"))
      |> { (cs, range) => IdentifierToken(cs.mkString).setPos(range._1) },

    // Integer literal,
    many1(elem(_.isDigit))
      |> { (cs, range) =>
        val n_val = Try(cs.mkString.toInt).toOption
        n_val match
          case Some(n) => IntLitToken(n).setPos(range._1)
          case None    => ErrorToken(cs.mkString).setPos(range._1)
      },

    // Char literal,
    elem('\'') ~
      (elem(c => c != '\\' && c != '\n' && c != '\'') |
        (elem('\\') ~ oneOf(Character.escapeCharacters))) ~
      elem('\'')
      |> { (cs, range) =>
        CharLitToken(cs.mkString.drop(1).dropRight(1)).setPos(range._1)
      },
    elem('\'') ~
      (elem('\\') ~ elem(!Character.escapeCharacters.contains(_))) ~
      elem('\'')
      |> { (cs, range) =>
        ErrorToken("Invalid escape character").setPos(range._1)
      },

    // String literal,
    elem('"') ~ many(elem(c => c != '\n' && c != '"')) ~ elem('"')
      |> { (cs, range) =>
        StringLitToken(cs.mkString.drop(1).dropRight(1)).setPos(range._1)
      },

    // Delimiters,
    oneOf(".,:;(){}[]=") | word("=>")
      |> { (cs, range) => DelimiterToken(cs.mkString).setPos(range._1) },

    // Whitespace,
    many1(elem(' ') | elem('\n') | elem('\t'))
      |> { (cs, range) => SpaceToken().setPos(range._1) },

    // Single line comment,
    word("//") ~ many(elem(_ != '\n'))
      |> { (cs, range) => CommentToken(cs.mkString).setPos(range._1) },

    // Multi line comment
    word("/*") ~ many(elem(_ != '*')) ~ many1(elem('*')) ~ many(
      elem(c => c != '/' && c != '*') ~ many(elem(_ != '*')) ~ many1(elem('*'))
    ) ~ elem('/')
      |> { (cs, range) => CommentToken(cs.mkString).setPos(range._1) },
    word("/*")
      |> { (cs, range) => ErrorToken(cs.mkString).setPos(range._1) }
  ) onError {
    // We also emit ErrorTokens for Silex-handled errors.
    (cs, range) => ErrorToken(cs.mkString).setPos(range._1)
  } onEnd {
    // Once all the input has been consumed, we emit one EOFToken.
    pos => EOFToken().setPos(pos)
  }

  override def run(
      ctx: amyc.utils.Context
  )(files: List[File]): Iterator[Token] = {
    var it = Seq[Token]().iterator

    for (file <- files) {
      val source = Source.fromFile(
        file.toString,
        SourcePositioner(file),
        Charset.forName(ctx.charset)
      )
      it ++= lexer
        .spawn(source)
        .filter {
          _ match
            case CommentToken(s) => false
            case SpaceToken()    => false
            case _               => true
        }
        .map {
          case token @ ErrorToken(error) =>
            ctx.reporter.fatal(
              "Unknown token at " + token.position + ": " + error
            )
          case token => token
        }
    }
    it
  }
}

/** Extracts all tokens from input and displays them */
object DisplayTokens extends Pipeline[Iterator[Token], Unit] {
  override def run(ctx: Context)(tokens: Iterator[Token]): Unit = {
    tokens.foreach(println(_))
  }
}
