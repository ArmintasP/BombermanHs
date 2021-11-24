module Test where

import Test.HUnit
import Data.Either
import Parser3

main :: IO ()
main = do
  putStrLn "Loading acceptance tests..."
  a <- runTestTT acceptanceTests
  putStrLn "Loading rejection tests..."

acceptanceTests = TestList 
  [
    TestLabel "aTest1" aTest1,
    TestLabel "aTest2" aTest2,
    TestLabel "aTest3" aTest3,
    TestLabel "aTest4" aTest4,
    TestLabel "aTest5" aTest5,
    TestLabel "aTest6" aTest6,
    TestLabel "aTest7" aTest7,
    TestLabel "aTest8" aTest8,
    TestLabel "aTest9" aTest9,
    TestLabel "aTest10" aTest10,
    TestLabel "aTest11" aTest11,
    TestLabel "aTest12" aTest12,
    TestLabel "aTest16" aTest16,
    TestLabel "aTest18" aTest18,
    TestLabel "aTest20" aTest20,
    TestLabel "aTest21" aTest21,
    TestLabel "aTest22" aTest22,
    TestLabel "aTest23" aTest23,
    TestLabel "aTest31" aTest31,
    TestLabel "aTest33" aTest33,
    TestLabel "aTest34" aTest34,
    TestLabel "aTest35" aTest35,
    TestLabel "aTest36" aTest36,
    TestLabel "aTest37" aTest37,
    TestLabel "aTest38" aTest38,
    TestLabel "aTest39" aTest39,
    TestLabel "aTest41" aTest41,
    TestLabel "aTest42" aTest42,
    TestLabel "aTest43" aTest43,
    TestLabel "aTest44" aTest44,
    TestLabel "aTest45" aTest45,
    TestLabel "aTest46" aTest46,
    TestLabel "aTest47" aTest47,
    TestLabel "aTest48" aTest48,
    TestLabel "aTest49" aTest49,
    TestLabel "aTest50" aTest50,
    TestLabel "aTest51" aTest51,
    TestLabel "aTest52" aTest52,
    TestLabel "aTest53" aTest53,
    TestLabel "aTest54" aTest54,
    TestLabel "aTest55" aTest55,
    TestLabel "aTest56" aTest56,
    TestLabel "aTest57" aTest57,
    TestLabel "aTest58" aTest58,
    TestLabel "aTest59" aTest59,
    TestLabel "aTest60" aTest60,
    TestLabel "aTest61" aTest61,
    TestLabel "aTest62" aTest62,
    TestLabel "aTest63" aTest63,
    TestLabel "aTest64" aTest64,
    TestLabel "aTest65" aTest65,
    TestLabel "aTest66" aTest66,
    TestLabel "aTest67" aTest67,
    TestLabel "aTest68" aTest68,
    TestLabel "aTest69" aTest69,
    TestLabel "aTest70" aTest70,
    TestLabel "aTest71" aTest71,
    TestLabel "aTest72" aTest72,
    TestLabel "aTest73" aTest73,
    TestLabel "aTest74" aTest74,
    TestLabel "aTest75" aTest75,
    TestLabel "aTest76" aTest76,
    TestLabel "aTest77" aTest77,
    TestLabel "aTest79" aTest79,
    TestLabel "aTest80" aTest80,
    TestLabel "aTest81" aTest81,
    TestLabel "aTest82" aTest82,
    TestLabel "aTest83" aTest83,
    TestLabel "aTest84" aTest84,
    TestLabel "aTest85" aTest85,
    TestLabel "aTest86" aTest86
  ]

aTest1 = TestCase (assertBool "[[]   ]" (isRight(runParser "[[]   ]")))
aTest2 = TestCase (assertBool "[\"\"]" (isRight(runParser "[\"\"]")))
aTest3 = TestCase (assertBool "[]" (isRight(runParser "[]")))
aTest4 = TestCase (assertBool "[\"a\"]" (isRight(runParser "[\"a\"]")))
aTest5 = TestCase (assertBool "[false]" (isRight(runParser "[false]")))
aTest6 = TestCase (assertBool "[null, 1, \"1\", {}]" (isRight(runParser "[null, 1, \"1\", {}]")))
aTest7 = TestCase (assertBool "[null]" (isRight(runParser "[null]")))
aTest8 = TestCase (assertBool "[1" (isRight(runParser "[1")))
aTest9 = TestCase (assertBool "]" (isRight(runParser "]")))
aTest10 = TestCase (assertBool " [1]" (isRight(runParser " [1]")))
aTest11 = TestCase (assertBool "[1,null,null,null,2]" (isRight(runParser "[1,null,null,null,2]")))
aTest12 = TestCase (assertBool "[2] " (isRight(runParser "[2] ")))
aTest16 = TestCase (assertBool "[ 4]" (isRight(runParser "[ 4]")))
aTest18 = TestCase (assertBool "" (isRight(runParser "")))
aTest20 = TestCase (assertBool "[-0]" (isRight(runParser "[-0]")))
aTest21 = TestCase (assertBool "[-123]" (isRight(runParser "[-123]")))
aTest22 = TestCase (assertBool "[-1]" (isRight(runParser "[-1]")))
aTest23 = TestCase (assertBool "[-0]" (isRight(runParser "[-0]")))
aTest31 = TestCase (assertBool "[123]" (isRight(runParser "[123]")))
aTest33 = TestCase (assertBool "{\"asd\":\"sdf\", \"dfg\":\"fgh\"}" (isRight(runParser "{\"asd\":\"sdf\", \"dfg\":\"fgh\"}")))
aTest34 = TestCase (assertBool "{\"asd\":\"sdf\"}" (isRight(runParser "{\"asd\":\"sdf\"}")))
aTest35 = TestCase (assertBool "{\"a\":\"b\",\"a\":\"c\"}" (isRight(runParser "{\"a\":\"b\",\"a\":\"c\"}")))
aTest36 = TestCase (assertBool "{\"a\":\"b\",\"a\":\"b\"}" (isRight(runParser "{\"a\":\"b\",\"a\":\"b\"}")))
aTest37 = TestCase (assertBool "{}" (isRight(runParser "{}")))
aTest38 = TestCase (assertBool "{\"\":0}" (isRight(runParser "{\"\":0}")))
aTest39 = TestCase (assertBool "{\"foobar\": 42}" (isRight(runParser "{\"foobar\": 42}")))
aTest41 = TestCase (assertBool "{\"x\":[{\"id\": \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}], \"id\": \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}" (isRight(runParser "{\"x\":[{\"id\": \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}], \"id\": \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}")))
aTest42 = TestCase (assertBool "{\"a\":[]}" (isRight(runParser "{\"a\":[]}")))
aTest43 = TestCase (assertBool "{\"title\":\"Полтора Землекопа\" }" (isRight(runParser "{\"title\":\"Полтора Землекопа\" }")))
aTest44 = TestCase (assertBool "{" (isRight(runParser "{")))
aTest45 = TestCase (assertBool "\"a\": \"b\"" (isRight(runParser "\"a\": \"b\"")))
aTest46 = TestCase (assertBool "}" (isRight(runParser "}")))
aTest47 = TestCase (assertBool "[\"`Īካ\"]" (isRight(runParser "[\"`Īካ\"]")))
aTest48 = TestCase (assertBool "[\"??????\"]" (isRight(runParser "[\"??????\"]")))
aTest49 = TestCase (assertBool "[\"????????????\"]" (isRight(runParser "[\"????????????\"]")))
aTest50 = TestCase (assertBool "[\"\\u0000\"]" (isRight(runParser "[\"\\u0000\"]")))
aTest51 = TestCase (assertBool "[\"\"\"]" (isRight(runParser "[\"\"\"]")))
aTest52 = TestCase (assertBool "[\"a/*b*/c/*d//e\"]" (isRight(runParser "[\"a/*b*/c/*d//e\"]")))
aTest53 = TestCase (assertBool "[\"\a\"]" (isRight(runParser "[\"\a\"]")))
aTest54 = TestCase (assertBool "[\"\n\"]" (isRight(runParser "[\"\n\"]")))
aTest55 = TestCase (assertBool "[\"\"]" (isRight(runParser "[\"\"]")))
aTest56 = TestCase (assertBool "[\"asd\"]" (isRight(runParser "[\"asd\"]")))
aTest57 = TestCase (assertBool "[ \"asd\"]" (isRight(runParser "[ \"asd\"]")))
aTest58 = TestCase (assertBool "[\"??????\"]" (isRight(runParser "[\"??????\"]")))
aTest59 = TestCase (assertBool "[\"new line\"]" (isRight(runParser "[\"new line\"]")))
aTest60 = TestCase (assertBool "[\"\"]" (isRight(runParser "[\"\"]")))
aTest61 = TestCase (assertBool "[\",\"]" (isRight(runParser "[\",\"]")))
aTest62 = TestCase (assertBool "[\"π\"]" (isRight(runParser "[\"π\"]")))
aTest63 = TestCase (assertBool "[\"asd \"]" (isRight(runParser "[\"asd \"]")))
aTest64 = TestCase (assertBool "\" \"" (isRight(runParser "\" \"")))
aTest65 = TestCase (assertBool "[\"??????\"]" (isRight(runParser "[\"??????\"]")))
aTest66 = TestCase (assertBool "[\"ࠡ\"]" (isRight(runParser "[\"ࠡ\"]")))
aTest67 = TestCase (assertBool "[\"ģ\"]" (isRight(runParser "[\"ģ\"]")))
aTest68 = TestCase (assertBool "[\"aクリス\"]" (isRight(runParser "[\"aクリス\"]")))
aTest69 = TestCase (assertBool "[\"\"]" (isRight(runParser "[\"\"]")))
aTest70 = TestCase (assertBool "[\"ꙭ\"]" (isRight(runParser "[\"ꙭ\"]")))
aTest71 = TestCase (assertBool "[\"⍂㈴⍂\"]" (isRight(runParser "[\"⍂㈴⍂\"]")))
aTest72 = TestCase (assertBool "[\"??????\"]" (isRight(runParser "[\"??????\"]")))
aTest73 = TestCase (assertBool "[\"??????\"]" (isRight(runParser "[\"??????\"]")))
aTest74 = TestCase (assertBool "[\"€𝄞\"]" (isRight(runParser "[\"€𝄞\"]")))
aTest75 = TestCase (assertBool "[\"aa\"]" (isRight(runParser "[\"aa\"]")))
aTest76 = TestCase (assertBool "false" (isRight(runParser "false")))
aTest77 = TestCase (assertBool "42" (isRight(runParser "42")))
aTest79 = TestCase (assertBool "null" (isRight(runParser "null")))
aTest80 = TestCase (assertBool "\"asd\"" (isRight(runParser "\"asd\"")))
aTest81 = TestCase (assertBool "true" (isRight(runParser "true")))
aTest82 = TestCase (assertBool "\"\"" (isRight(runParser "\"\"")))
aTest83 = TestCase (assertBool "[\"a\"]" (isRight(runParser "[\"a\"]")))
aTest84 = TestCase (assertBool "" (isRight(runParser "")))
aTest85 = TestCase (assertBool "[true]" (isRight(runParser "[true]")))
aTest86 = TestCase (assertBool " [] " (isRight(runParser " [] ")))
