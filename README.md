# Haskell-Subscript-Parser-Interpreter
This is a subscript parser & interpreter for a subset of JavaScript, especially Array comprehensions, written in Haskell.

## Interpreter
The implementation of the interpreter can be found in `src/Interpreter/Impl.hs`.

## Parser
The implementation of the parser can be found in `src/Parser/Impl.hs`

## Main
The `src/Main.hs` file combines the two. Given a set of expressions, the program first parses the JavaScript expressions and then interprets and evaluates those expressions.
