package amyc.utils

import amyc.ast.SymbolicTreeModule.CharLiteral

object Character {
  def escapeCharacters: Seq[Char] = Seq('\'', '\\', 'n')

  def getValue(char: CharLiteral): Char =
    if char.isEscape then
      char.value match
        case '\'' => '\''
        case '\\' => '\\'
        case 'n'  => '\n'
        case _    => char.value
    else char.value
}
