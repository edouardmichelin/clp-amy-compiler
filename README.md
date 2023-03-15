# Amy Compiler

##### Extension

###### Improve string support

Report can be seen by clicking [here](./report/report.pdf) or by navigating to `report/report.pdf`

Slides from the presentation can be seen by clicking [here](./slides/CLP-G07-3.5.pdf) or by navigating to `slides/CLP-G07-3.5.pdf`

Final grade: `20/20`

---

### Run the compiler and execute the output

1.  Requirements

    -   [Node.js](https://nodejs.org/en/) v12 or later
    -   [Wat2Wasm](https://github.com/WebAssembly/wabt/releases/tag/1.0.31)

    <br/>

2.  Clone the repository and move the downloaded `wat2wasm` to `<root of the project>/bin/`
    <br/>
3.  Run the following command in order to install the required packets

        ~/$                 npm install

    <br/>

4.  Generate WebAssembly bytecode for amy source files (with sbt)
    `run [LIBFILES ...] [REQUIRED_FILES ...] Main.amy [--charset=CHARSET]`
    <br/>

        ~/cs320-group07$    sbt
        sbt:epfl-amyc>      run ./library/Std.amy ./examples/Hello.amy

    <br/>

5.  Run the compiler output
    `node ./wasmout/Main.js`
    <br/>

    ```
    ~/cs320-group07$        node ./wasmout/Hello.js
    Hello World!
    ```

### Examples

You will find generic examples in the `/examples` folder and examples more specific to the extension we proposed in the `/extension-examples` folder.
The same process as above applies for running the programs. Programs in `extension-examples` need the following libraries: `./library/Std.amy ./library/StringImpl.amy`. More dependencies can be required, see table below.

| Program           | Description                                                                                                                                               |
| ----------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Hotel.amy         | Small program for hotel room reservation. Requires `./library/List.amy` and `./library/Option.amy`.                                                       |
| NamePrinter.amy   | Asks for user's last name. Formats it by stripping and capitalizing it then outputs it to the console.                                                    |
| HelloChars.amy    | Declares two Char, checks for equality and ouputs the result. Then transforms them to string, concatenate them and output the result.                     |
| EscapeChars.amy   | Declares two escape sequence characters and outputs them. Uncommenting the commented out "invalidEscapeChar" will generate an error in the parsing phase. |
| StringOps.amy     | Demonstration of length, equals, charAt, substring, strip, toLowerCase and indexOf.                                                                       |
| StringNewLine.amy | Demonstration of a string that is split into two lines with the new-line character.                                                                       |
| IndexOf.amy       | Demonstration of indexOf.                                                                                                                                 |
| UpperCase.amy     | Demonstration of toUpperCase.                                                                                                                             |
| More examples     | Which are also available in the `extension-examples` folder.                                                                                              |

### Contributors

Edouard Michelin

Suhas Shankar
