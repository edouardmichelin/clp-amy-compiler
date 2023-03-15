package amyc
package wasm

// A WebAssembly module
case class Module(name: String, imports: List[String], globals: Int, functions: List[Function]) {

  import java.io.{File, FileWriter}

  def writeWasmText(fileName: String) = {
    val fw = new FileWriter(new File(fileName))
    fw.write(ModulePrinter(this))
    fw.flush()
  }

  def writeHtmlWrapper(fileName: String, moduleFile: String) = {
    val wrapperString =
      s"""|<!doctype html>
          |
          |<html>
          |  <head>
          |    <meta charset="utf-8">
          |    <title>$name</title>
          |  </head>
          |
          |  <body>
          |    <p id="htmlText"></p>
          |    <script>
          |
          |      // This library function fetches the wasm module at 'url', instantiates it with
          |      // the given 'importObject', and returns the instantiated object instance
          |      // Taken from https://github.com/WebAssembly/spec
          |      function fetchAndInstantiate(url, importObject) {
          |        return fetch(url).then(response =>
          |          response.arrayBuffer()
          |        ).then(bytes =>
          |          WebAssembly.instantiate(bytes, importObject)
          |        ).then(results =>
          |          results.instance
          |        );
          |      }
          |
          |      const log = (line) => document.getElementById("htmlText").innerHTML += line + "<br>";
          |      const waitInput = () => window.prompt("Input to WASM program:");
          |      const exit = () => log('<span style="color: red">Exited</span>');
          |
          |      const memory = new WebAssembly.Memory({initial:100});
          |
          |      $importObject
          |
          |      fetchAndInstantiate('$moduleFile', importObject).then(function(instance) {
          |""".stripMargin ++
          functions.filter(_.isMain).map { f =>
              s"        instance.exports.${f.name}();\n"
          }.mkString ++
            """|      });
                |    </script>
                |  </body>
                |
                |</html>
                |
            """.stripMargin
    val fw = new FileWriter(new File(fileName))
    fw.write(wrapperString)
    fw.flush()
  }

  def writeNodejsWrapper(fileName: String, moduleFile: String): Unit = {
    val wrapperString =
      s"""function safe_require(module_name) {
         |  try {
         |    return require(module_name);
         |  } catch (e) {
         |    console.log('Error: nodejs module ' + module_name +
         |      ' must be installed (you might want to try: npm install ' + module_name + ')');
         |    process.exit(1);
         |  }
         |}
         |
         |// `Wasm` does **not** understand node buffers, but thankfully a node buffer
         |// is easy to convert to a native Uint8Array.
         |function toUint8Array(buf) {
         |  const u = new Uint8Array(buf.length);
         |  for (let i = 0; i < buf.length; ++i) {
         |    u[i] = buf[i];
         |  }
         |  return u;
         |}
         |// Loads a WebAssembly dynamic library, returns a promise.
         |// imports is an optional imports object
         |function loadWebAssembly(filename, imports) {
         |  // Fetch the file and compile it
         |  const buffer = toUint8Array(require('fs').readFileSync(filename))
         |  return WebAssembly.compile(buffer).then(module => {
         |    return new WebAssembly.Instance(module, imports)
         |  })
         |}
         |
         |const deasync = safe_require('deasync');
         |const rl = require('readline').createInterface({
         |  input: process.stdin,
         |  output: process.stdout
         |});
         |
         |// Newer versions of NodeJS don't seem to buffer line events, so we might lose inputs
         |// when stdin was redirected (e.g. in the automated tests).
         |let inputLines = [];
         |rl.on('line', function(answer) {
         |  inputLines.push(answer);
         |});
         |
         |function waitInput() {
         |  deasync.loopWhile(function(){return inputLines.length <= 0;});
         |  return inputLines.shift();
         |}
         |
         |const log = console.log;
         |const exit = () => process.exit(1);
         |
         |var memory = new WebAssembly.Memory({initial:100});
         |$importObject
         |
         |loadWebAssembly('$moduleFile', importObject).then(function(instance) {
         |""".stripMargin ++
      functions.filter(_.isMain).map { f =>
        s"  instance.exports.${f.name}();\n"
      }.mkString ++
       """  rl.close();
         |}).catch( function(error) {
         |  rl.close();
         |  process.exit(1)
         |})
         |""".stripMargin
    val fw = new FileWriter(new File(fileName))
    fw.write(wrapperString)
    fw.flush()
  }

  /** JavaScript object containing the values to be imported into the WASM instance.
    * Requires a waitInput(), log(line) and exit() function to be in scope.
    */
  private def importObject: String =
    s"""const importObject = {
       |  system: {
       |    mem: memory,
       |
       |    printInt: function(arg) {
       |      log(arg);
       |      0;
       |    },
       |
       |    T_fun: function(arg) {
       |      log(arg);
       |      0;
       |    },
       |
       |
       |    f2: function(arg) {
       |      log(arg);
       |      0;
       |    },
       |
       |    length: function(arg) {
       |      var bufView = new Uint8Array(memory.buffer);
       |      var i = arg;
       |      
       |      var cnt = 0;
       |      while(bufView[i] != 0) {
       |       i = i + 1;
       |       cnt+=1
       |      }
       |
       |      return(cnt);
       |    
       |      
       |    },
       |
       |
       |    replace0: function(len, memB, arg, char1 , char2 ){
       |  
       |      var bufView8 = new Uint8Array(memory.buffer);
       |
       |      var t = 0;
       |
       |      for (var k = arg;  k < arg + len; k++) {
       |        if(bufView8[k] == char1){
       |          bufView8[memB + t] = char2;
       |        }
       |        else {
       |          bufView8[memB + t] = bufView8[k];
       |        }
       |        t = t+1;
       |      }
       |
       |      var size = t;
       |
       |      var padding = 4 - size % 4;
       |      var newMemB = memB + size + padding; 
       |      for (var i = 0; i< padding; i++) {
       |        bufView8[memB + size + i] = "\u0000";
       |      }
       |
       |      return newMemB;
       |
       |    },
       |
       |    strip0: function(len, memB, arg){
       |
       |      var bufView8 = new Uint8Array(memory.buffer);
       |
       |      var start = arg;
       |
       |      while(start < arg + len){
       |        if(String.fromCharCode(bufView8[start]) != " ") break;
       |        start++;
       |      }
       |      var end = arg + len;
       |      while(end>0 && String.fromCharCode(bufView8[end-1]) == " ")end--;
       |
       |      
       |
       |      var t = 0;
       |
       |      for (var k = start;  k < end; k++) {
       |        bufView8[memB + t] = bufView8[k];
       |        t = t+1;
       |      }
       |
       |
       |      var size = t;
       |
       |      var padding = 4 - size % 4;
       |      var newMemB = memB + size + padding; 
       |      for (var i = 0; i< padding; i++) {
       |        bufView8[memB + size + i] = "\u0000";
       |      }
       |
       |      return newMemB;
       |      
       |    },
       |
       |    toLowerCase0: function(memB, arg){
       |  
       |      var i = arg;
       |    
       |      var bufView8 = new Uint8Array(memory.buffer);
       |      while(bufView8[i] != 0) {
       |        if(bufView8[i] >= 65 && bufView8[i]<=90){
       |          bufView8[memB + i - arg] = bufView8[i] + 32;
       |        }
       |        else{
       |          bufView8[memB + i - arg] = bufView8[i];
       |        }
       |        i = i + 1;
       |      }
       |
       |
       |      size = i - arg;
       |
       |      var padding = 4 - size % 4;
       |      var newMemB = memB + size + padding; 
       |      for (var i = 0; i< padding; i++) {
       |        bufView8[memB + size + i] = "\u0000";
       |      }
       |
       |      return newMemB;
       |      
       |    },
       |
       |
       |    toUpperCase0: function(memB, arg){
       |  
       |      var i = arg;
       |    
       |      var bufView8 = new Uint8Array(memory.buffer);
       |      while(bufView8[i] != 0) {
       |        if(bufView8[i] >= 97 && bufView8[i]<=122){
       |          bufView8[memB + i - arg] = bufView8[i] - 32;
       |        }
       |        else{
       |          bufView8[memB + i - arg] = bufView8[i];
       |        }
       |        i = i + 1;
       |      }
       |
       |
       |      size = i - arg;
       |
       |      var padding = 4 - size % 4;
       |      var newMemB = memB + size + padding; 
       |      for (var i = 0; i< padding; i++) {
       |        bufView8[memB + size + i] = "\u0000";
       |      }
       |
       |      return newMemB;
       |      
       |    },
       |
       |
       |    substring0: function(memB, arg, start, stop) {
       |      
       |      var size = stop - start;
       |      var padding = 4 - size % 4;
       |      
       |      var newMemB = memB + size + padding;
       |      var bufView8 = new Uint8Array(memory.buffer);
       |      for (var i = 0; i < size; i++) {
       |        bufView8[memB + i] = bufView8[arg + start + i];
       |      }
       |      for (var i = 0; i< padding; i++) {
       |        bufView8[memB + size + i] = "\u0000";
       |      } 
       |      return newMemB;
       |    },
       |
       |    printChar: function(arg) {
       |
       |      log(String.fromCharCode(arg));
       |
       |      0;
       |
       |    },
       |
       |    printString: function(arg) {
       |      var bufView = new Uint8Array(memory.buffer);
       |      var i = arg;
       |      var result = "";
       |      while(bufView[i] != 0) {
       |       result += String.fromCharCode(bufView[i]);
       |       i = i + 1;
       |      }
       |      log(result);
       |      0;
       |    },
       |
       |    equals: function(s1, s2) {
       |      var bufView = new Uint8Array(memory.buffer);
       |      while(true){
       |        if(bufView[s1] == 0 &&  bufView[s2] == 0) return true;
       |        if(bufView[s1] != bufView[s2]) return false;
       |        s1++;s2++;
       |      }
       |    },
       |
       |    charAt0: function(arg, index, len ) {
       |
       |
       |
       |      if(index < 0 || index >= len ){
       |        return 48;
       |      }
       |
       |
       |      var bufView = new Uint8Array(memory.buffer);
       |      return bufView[arg + index];
       |
       |
       |      
       |    },
       |
       |    indexOf0: function(arg, ch, len ) {
       |
       |
       |      var bufView = new Uint8Array(memory.buffer);
       |      
       |      var i = 0;
       |      while(i < len){
       |        if(bufView[arg+i] == ch) return i;
       |        i++;
       |      }
       |
       |      return -1;
       |
       |
       |      
       |
       |
       |      
       |    },
       |
       |    readInt: function() {
       |      var res = parseInt(waitInput());
       |      if (isNaN(res)) {
       |        log("Error: Could not parse int");
       |        exit();
       |      } else {
       |        return res;
       |      }
       |    },
       |
       |    // This function has a weird signature due to restrictions of the current WASM format:
       |    // It takes as argument the position that it needs to store the string to,
       |    // and returns the first position after the new string.
       |    readString0: function(memB) {
       |      var s = waitInput();
       |      var size = s.length;
       |      var padding = 4 - size % 4;
       |      var fullString = s + "\u0000".repeat(padding);
       |      var newMemB = memB + size + padding;
       |      var bufView8 = new Uint8Array(memory.buffer);
       |      for (var i = 0; i < fullString.length; i++) {
       |        bufView8[memB + i] = fullString.charCodeAt(i);
       |      }
       |      return newMemB;
       |    }
       |  }
       |};
       |""".stripMargin
}
