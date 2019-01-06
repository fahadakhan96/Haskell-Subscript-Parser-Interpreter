-- put your tests here, and/or in other files in the tests/ directory

import Test.Tasty
import Test.Tasty.HUnit

import SubsAst
import SubsParser

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "tiny" $
      parseString "2+3" @?= Right (Call "+" [Number 2, Number 3]),
    testCase "intro" $
      do act <- parseFile "examples/intro.js"
         exp <- fmap read $ readFile "examples/intro-ast.txt"
         act @?= Right exp
  ]

