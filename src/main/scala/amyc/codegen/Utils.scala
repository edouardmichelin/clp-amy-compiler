package amyc
package codegen

import amyc.ast.Identifier
import wasm.Function
import wasm.Instructions._

// Utilities for CodeGen
object Utils {

  // The index of the global variable that represents the free memory boundary
  val memoryBoundary: Int = 0
  // # of global variables
  val globalsNo = 1

  // The default imports we will pass to a wasm Module
  val defaultImports: List[String] = List(
    "\"system\" \"printInt\" (func $Std_printInt (param i32) (result i32))",
    "\"system\" \"printChar\" (func $Std_printChar (param i32) (result i32))",
    "\"system\" \"T_fun\" (func $Std_T_fun (param i32) (result i32))",
    "\"system\" \"f2\" (func $StringImpl_f2 (param i32) (result i32))",
    "\"system\" \"length\" (func $StringImpl_length (param i32) (result i32))",
    "\"system\" \"substring0\" (func $StringImpl_substring0 (param i32) (param i32) (param i32) (param i32) (result i32))",
    "\"system\" \"strip0\" (func $StringImpl_strip0 (param i32) (param i32) (param i32)  (result i32))",
    "\"system\" \"toLowerCase0\" (func $StringImpl_toLowerCase0 (param i32) (param i32)  (result i32))",
    "\"system\" \"toUpperCase0\" (func $StringImpl_toUpperCase0 (param i32) (param i32)  (result i32))",
    "\"system\" \"charAt0\" (func $StringImpl_charAt0 (param i32) (param i32) (param i32) (result i32))",
    "\"system\" \"indexOf0\" (func $StringImpl_indexOf0 (param i32) (param i32) (param i32) (result i32))",
    "\"system\" \"replace0\" (func $StringImpl_replace0 (param i32) (param i32) (param i32) (param i32) (param i32) (result i32))",
    "\"system\" \"printString\" (func $Std_printString (param i32) (result i32))",
    "\"system\" \"equals\" (func $StringImpl_equals (param i32) (param i32) (result i32))",
    "\"system\" \"readString0\" (func $js_readString0 (param i32) (result i32))",
    "\"system\" \"readInt\" (func $Std_readInt (result i32))",
    "\"system\" \"mem\" (memory 100)"
  )

  // We don't generate code for these functions in CodeGen (they are hard-coded here or in js wrapper)
  val builtInFunctions: Set[String] = Set(
    "Std_printInt",
    "Std_printChar",
    "Std_printString",
    "Std_digitToString",
    "Std_readInt",
    "Std_readString",
    "Std_T_fun",
    "StringImpl_f2",
    "StringImpl_length",
    "StringImpl_substring",
    "StringImpl_replace",
    "StringImpl_strip",
    "StringImpl_equals",
    "StringImpl_toLowerCase",
    "StringImpl_toUpperCase",
    "StringImpl_charAt",
    "StringImpl_indexOf",
    "Std_charToString"
  )

  /** Utilities */
  // A globally unique name for definitions
  def fullName(owner: Identifier, df: Identifier): String =
    owner.name + "_" + df.name

  // Given a pointer to an ADT on the top of the stack,
  // will point at its field in index (and consume the ADT).
  // 'index' MUST be 0-based.
  def adtField(index: Int): Code = {
    Comment(s"adtField index: $index") <:> Const(4 * (index + 1)) <:> Add
  }

  // Increment a local variable
  def incr(local: Int): Code = {
    GetLocal(local) <:> Const(1) <:> Add <:> SetLocal(local)
  }

  // A fresh label name
  def getFreshLabel(name: String = "label") = {
    Identifier.fresh(name).fullName
  }

  // Creates a known string constant s in memory
  def mkString(s: String): Code = {
    val size = s.length
    val padding = 4 - size % 4

    val completeS = s + 0.toChar.toString * padding

    val setChars = for ((c, ind) <- completeS.zipWithIndex.toList) yield {
      GetGlobal(memoryBoundary) <:> Const(ind) <:> Add <:>
        Const(c.toInt) <:> Store8
    }

    val setMemory =
      GetGlobal(memoryBoundary) <:> GetGlobal(memoryBoundary) <:> Const(
        size + padding
      ) <:> Add <:>
        SetGlobal(memoryBoundary)

    Comment(s"mkString: $s") <:> setChars <:> setMemory
  }

  // Built-in implementation of concatenation
  val concatImpl: Function = {
    Function("String_concat", 2, false) { lh =>
      val ptrS = lh.getFreshLocal()
      val ptrD = lh.getFreshLocal()
      val label = getFreshLabel()

      def mkLoop: Code = {
        val label = getFreshLabel()
        Loop(label) <:>
          // Load current character
          GetLocal(ptrS) <:> Load8_u <:>
          // If != 0
          If_void <:>
          // Copy to destination
          GetLocal(ptrD) <:>
          GetLocal(ptrS) <:> Load8_u <:>
          Store8 <:>
          // Increment pointers
          incr(ptrD) <:> incr(ptrS) <:>
          // Jump to loop
          Br(label) <:>
          Else <:>
          End <:>
          End
      }

      // Instantiate ptrD to previous memory, ptrS to first string
      GetGlobal(memoryBoundary) <:>
        SetLocal(ptrD) <:>
        GetLocal(0) <:>
        SetLocal(ptrS) <:>
        // Copy first string
        mkLoop <:>
        // Set ptrS to second string
        GetLocal(1) <:>
        SetLocal(ptrS) <:>
        // Copy second string
        mkLoop <:>
        //
        // Pad with zeros until multiple of 4
        //
        Loop(label) <:>
        // Write 0
        GetLocal(ptrD) <:> Const(0) <:> Store8 <:>
        // Check if multiple of 4
        GetLocal(ptrD) <:> Const(4) <:> Rem <:>
        // If not
        If_void <:>
        // Increment pointer and go back
        incr(ptrD) <:>
        Br(label) <:>
        Else <:>
        End <:>
        End <:>
        //
        // Put string pointer to stack, set new memory boundary and return
        GetGlobal(memoryBoundary) <:> GetLocal(ptrD) <:> Const(
          1
        ) <:> Add <:> SetGlobal(memoryBoundary)
    }
  }

  val digitToStringImpl: Function = {
    Function("Std_digitToString", 1, false) { lh =>
      // We know we have to create a string of total size 4 (digit code + padding), so we do it all together
      // We do not need to shift the digit due to little endian structure!
      GetGlobal(memoryBoundary) <:> GetLocal(0) <:> Const(
        '0'.toInt
      ) <:> Add <:> Store <:>
        // Load memory boundary to stack, then move it by 4
        GetGlobal(memoryBoundary) <:>
        GetGlobal(memoryBoundary) <:> Const(4) <:> Add <:> SetGlobal(
          memoryBoundary
        )
    }
  }

  val readStringImpl: Function = {
    Function("Std_readString", 0, false) { lh =>
      // We need to use the weird interface of javascript read string:
      // we pass the old memory boundary and get the new one.
      // In the end we have to return the old, where the fresh string lies.
      GetGlobal(memoryBoundary) <:> GetGlobal(memoryBoundary) <:> Call(
        "js_readString0"
      ) <:>
        SetGlobal(memoryBoundary)
    }
  }

  val subStringImpl: Function = {
    Function("StringImpl_substring", 3, false) { lh =>
      // We need to use the weird interface of javascript read string:
      // we pass the old memory boundary and get the new one.
      // In the end we have to return the old, where the fresh string lies.
      GetGlobal(memoryBoundary) <:>
        GetGlobal(memoryBoundary) <:>
        GetLocal(0) <:>
        GetLocal(1) <:>
        GetLocal(2) <:>
        Call("StringImpl_substring0") <:>
        SetGlobal(memoryBoundary)
    }
  }

  val stripImpl: Function = {
    Function("StringImpl_strip", 1, false) { lh =>
      // We need to use the weird interface of javascript read string:
      // we pass the old memory boundary and get the new one.
      // In the end we have to return the old, where the fresh string lies.
      GetGlobal(memoryBoundary) <:>
        GetLocal(0) <:>
        Call("StringImpl_length") <:>
        GetGlobal(memoryBoundary) <:>
        GetLocal(0) <:>
        Call("StringImpl_strip0") <:>
        SetGlobal(memoryBoundary)
    }
  }

  val replaceImpl: Function = {
    Function("StringImpl_replace", 3, false) { lh =>
      // We need to use the weird interface of javascript read string:
      // we pass the old memory boundary and get the new one.
      // In the end we have to return the old, where the fresh string lies.
      GetGlobal(memoryBoundary) <:>
        GetLocal(0) <:>
        Call("StringImpl_length") <:>
        GetGlobal(memoryBoundary) <:>
        GetLocal(0) <:>
        GetLocal(1) <:>
        GetLocal(2) <:>
        Call("StringImpl_replace0") <:>
        SetGlobal(memoryBoundary)
    }
  }

  val toLowerCaseImpl: Function = {
    Function("StringImpl_toLowerCase", 1, false) { lh =>
      // We need to use the weird interface of javascript read string:
      // we pass the old memory boundary and get the new one.
      // In the end we have to return the old, where the fresh string lies.
      GetGlobal(memoryBoundary) <:>
        GetGlobal(memoryBoundary) <:>
        GetLocal(0) <:>
        Call("StringImpl_toLowerCase0") <:>
        SetGlobal(memoryBoundary)
    }
  }

  val toUpperCaseImpl: Function = {
    Function("StringImpl_toUpperCase", 1, false) { lh =>
      // We need to use the weird interface of javascript read string:
      // we pass the old memory boundary and get the new one.
      // In the end we have to return the old, where the fresh string lies.
      GetGlobal(memoryBoundary) <:>
        GetGlobal(memoryBoundary) <:>
        GetLocal(0) <:>
        Call("StringImpl_toUpperCase0") <:>
        SetGlobal(memoryBoundary)
    }
  }

  val charAtImpl: Function = {
    Function("StringImpl_charAt", 2, false) { lh =>
      // We need to use the weird interface of javascript read string:
      // we pass the old memory boundary and get the new one.
      // In the end we have to return the old, where the fresh string lies.

      GetLocal(0) <:>
        GetLocal(1) <:>
        GetLocal(0) <:>
        Call("StringImpl_length") <:>
        Call("StringImpl_charAt0")

    }
  }

  val indexOfImpl: Function = {
    Function("StringImpl_indexOf", 2, false) { lh =>
      // We need to use the weird interface of javascript read string:
      // we pass the old memory boundary and get the new one.
      // In the end we have to return the old, where the fresh string lies.

      GetLocal(0) <:>
        GetLocal(1) <:>
        GetLocal(0) <:>
        Call("StringImpl_length") <:>
        Call("StringImpl_indexOf0")

    }
  }

  val charToStringImpl: Function = {

    Function("Std_charToString", 1, false) { lh =>
      GetGlobal(memoryBoundary) <:> GetLocal(0) <:> Store <:>
        GetGlobal(memoryBoundary) <:>
        GetGlobal(memoryBoundary) <:> Const(4) <:> Add <:> SetGlobal(
          memoryBoundary
        )
    }

  }

  val wasmFunctions = List(
    concatImpl,
    digitToStringImpl,
    readStringImpl,
    subStringImpl,
    stripImpl,
    toLowerCaseImpl,
    toUpperCaseImpl,
    charToStringImpl,
    charAtImpl,
    indexOfImpl,
    replaceImpl
  )

}
