SubScript command-line tool
==========================-

To run the tool,

  stack exec runhaskell -- -W Main.hs [<option>] <path>

or 
  stack build
  stack exec subscript-parser -- [<option>] <path>

<option> can be -i for interpreter, or -p for parser.
The default (no option) is to parse *and* interpret.
