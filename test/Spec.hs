module Main where

import Test.HUnit
import Data.Either
import Parser4
import MapElements (MapElements (MapElements), createMapElements)

main :: IO ()
main = do
  aJResults <- loadTests acceptanceJsonTests "json acceptance"
  rJResults <- loadTests rejectionJsonTests "json rejection"
  gResults <- loadTests gameTests "game"
  let
    totalFailures = sum $ map failures [aJResults, rJResults, gResults]
    totalErrors = sum $ map errors [aJResults, rJResults, gResults]
  if totalErrors + totalFailures == 0
    then
      putStrLn "Tests have been completed succesfully.\n"
    else
      putStrLn ("Total number of tests failed: " ++ show totalFailures ++ ".\n")

loadTests :: Test -> String -> IO Counts
loadTests tests name = do
  let bar = "\n**************************\n"
  putStrLn ("\nLoading " ++ name ++ " tests..." ++ bar)
  runTestTT tests
acceptanceJsonTests = TestList
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
    TestLabel "aTest13" aTest13,
    TestLabel "aTest14" aTest14,
    TestLabel "aTest15" aTest15,
    TestLabel "aTest16" aTest16,
    TestLabel "aTest17" aTest17,
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
    TestLabel "aTest47" aTest47,
    TestLabel "aTest48" aTest48,
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
    TestLabel "aTest74" aTest74,
    TestLabel "aTest75" aTest75,
    TestLabel "aTest77" aTest77,
    TestLabel "aTest79" aTest79,
    TestLabel "aTest80" aTest80,
    TestLabel "aTest82" aTest82,
    TestLabel "aTest83" aTest83,
    TestLabel "aTest86" aTest86
  ]

aTest1 = TestCase (assertBool "[[]   ]" (isRight(runParser "[[]   ]")))
aTest2 = TestCase (assertBool "[\"\"]" (isRight(runParser "[\"\"]")))
aTest3 = TestCase (assertBool "[]" (isRight(runParser "[]")))
aTest4 = TestCase (assertBool "[\"a\"]" (isRight(runParser "[\"a\"]")))
aTest5 = TestCase (assertBool "{\"id\":1,\"address\":{\"addr1\":\"123 Main\",\"addr2\":null,\"city\":\"Houston\",\"state\":\"TX\"}}" (isRight(runParser "{\"id\":1,\"address\":{\"addr1\":\"123 Main\",\"addr2\":null,\"city\":\"Houston\",\"state\":\"TX\"}}")))
aTest6 = TestCase (assertBool "[null, 1, \"1\", {}]" (isRight(runParser "[null, 1, \"1\", {}]")))
aTest7 = TestCase (assertBool "[null]" (isRight(runParser "[null]")))
aTest8 = TestCase (assertBool "{\"id\":1,\"pets\":[\"dog\",\"cat\",\"fish\"]}" (isRight(runParser "{\"id\":1,\"pets\":[\"dog\",\"cat\",\"fish\"]}")))
aTest9 = TestCase (assertBool "{\"a\":{\"b\":{\"c\":{\"d\":{\"e\":{\"f\":{\"g\":{\"h\":{\"i\":{\"j\":{\"k\":{\"l\":{\"m\":{\"n\":{\"o\":{\"p\":\"blah\"}}}}}}}}}}}}}}}}" (isRight(runParser "{\"a\":{\"b\":{\"c\":{\"d\":{\"e\":{\"f\":{\"g\":{\"h\":{\"i\":{\"j\":{\"k\":{\"l\":{\"m\":{\"n\":{\"o\":{\"p\":\"blah\"}}}}}}}}}}}}}}}}")))
aTest10 = TestCase (assertBool " [1]" (isRight(runParser " [1]")))
aTest11 = TestCase (assertBool "[1,null,null,null,2]" (isRight(runParser "[1,null,null,null,2]")))
aTest12 = TestCase (assertBool "[2] " (isRight(runParser "[2] ")))
aTest13 = TestCase (assertBool "{\"stuff\":[321, \"abc\"]}" (isRight(runParser "{\"stuff\":[321, \"abc\"]}")))
aTest14 = TestCase (assertBool "{\"stuff\":[{\"pet\":\"cat\"},{\"car\":\"Ford\"}]}" (isRight(runParser "{\"stuff\":[{\"pet\":\"cat\"},{\"car\":\"Ford\"}]}")))
aTest15 = TestCase (assertBool "{\"id\":1,\"name\":\"Joe\",\"friends\":[{\"id\":2,\"name\":\"Pat\",\"pets\":[\"dog\"]},{\"id\":3,\"name\":\"Sue\",\"pets\":[\"bird\",\"fish\"]}],\"pets\":[]}" (isRight(runParser "{\"id\":1,\"name\":\"Joe\",\"friends\":[{\"id\":2,\"name\":\"Pat\",\"pets\":[\"dog\"]},{\"id\":3,\"name\":\"Sue\",\"pets\":[\"bird\",\"fish\"]}],\"pets\":[]}")))
aTest16 = TestCase (assertBool "[ 4]" (isRight(runParser "[ 4]")))
aTest17 = TestCase (assertBool "{\"id\":1,\"stuff\":[[1,2],[2,  3],  [ ],[3,4]]}" (isRight(runParser "{\"id\":1,\"stuff\":[[1,2],[2,  3],  [ ],[3,4]]}")))
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
aTest43 = TestCase (assertBool "{\"title\":\"?????????????? ??????????????????\" }" (isRight(runParser "{\"title\":\"?????????????? ??????????????????\" }")))
aTest47 = TestCase (assertBool "[\"`?????\"]" (isRight(runParser "[\"`?????\"]")))
aTest48 = TestCase (assertBool "[\"??????\"]" (isRight(runParser "[\"??????\"]")))
aTest50 = TestCase (assertBool "[\"\\u0000\"]" (isRight(runParser "[\"\\u0000\"]")))
aTest51 = TestCase (assertBool "[\"\\\"\"]" (isRight(runParser "[\"\\\"\"]")))
aTest52 = TestCase (assertBool "[\"a/*b*/c/*d//e\"]" (isRight(runParser "[\"a/*b*/c/*d//e\"]")))
aTest53 = TestCase (assertBool "[\"\a\"]" (isRight(runParser "[\"\a\"]")))
aTest54 = TestCase (assertBool "[\"\n\"]" (isRight(runParser "[\"\n\"]")))
aTest55 = TestCase (assertBool "[\"\"]" (isRight(runParser "[\"\"]")))
aTest56 = TestCase (assertBool "[\"asd\"]" (isRight(runParser "[\"asd\"]")))
aTest57 = TestCase (assertBool "[ \"asd\"]" (isRight(runParser "[ \"asd\"]")))
aTest58 = TestCase (assertBool "[\"??????\"]" (isRight(runParser "[\"??????\"]")))
aTest59 = TestCase (assertBool "[\"new??line\"]" (isRight(runParser "[\"new??line\"]")))
aTest60 = TestCase (assertBool "[\"\"]" (isRight(runParser "[\"\"]")))
aTest61 = TestCase (assertBool "[\",\"]" (isRight(runParser "[\",\"]")))
aTest62 = TestCase (assertBool "[\"??\"]" (isRight(runParser "[\"??\"]")))
aTest63 = TestCase (assertBool "[\"asd \"]" (isRight(runParser "[\"asd \"]")))
aTest64 = TestCase (assertBool "\" \"" (isRight(runParser "\" \"")))
aTest65 = TestCase (assertBool "[\"??????\"]" (isRight(runParser "[\"??????\"]")))
aTest66 = TestCase (assertBool "[\"\"]" (isRight(runParser "[\"\"]")))
aTest67 = TestCase (assertBool "[\"??\"]" (isRight(runParser "[\"??\"]")))
aTest68 = TestCase (assertBool "[\"a?????????\"]" (isRight(runParser "[\"a?????????\"]")))
aTest69 = TestCase (assertBool "[\"\"]" (isRight(runParser "[\"\"]")))
aTest70 = TestCase (assertBool "[\"???\"]" (isRight(runParser "[\"???\"]")))
aTest71 = TestCase (assertBool "[\"?????????\"]" (isRight(runParser "[\"?????????\"]")))
aTest72 = TestCase (assertBool "[\"??????\"]" (isRight(runParser "[\"??????\"]")))
aTest74 = TestCase (assertBool "[\"???????\"]" (isRight(runParser "[\"???????\"]")))
aTest75 = TestCase (assertBool "[\"aa\"]" (isRight(runParser "[\"aa\"]")))
aTest77 = TestCase (assertBool "42" (isRight(runParser "42")))
aTest79 = TestCase (assertBool "null" (isRight(runParser "null")))
aTest80 = TestCase (assertBool "\"asd\"" (isRight(runParser "\"asd\"")))
aTest82 = TestCase (assertBool "\"\"" (isRight(runParser "\"\"")))
aTest83 = TestCase (assertBool "[\"a\"]" (isRight(runParser "[\"a\"]")))
aTest86 = TestCase (assertBool " [] " (isRight(runParser " [] ")))


rejectionJsonTests = TestList
  [
    TestLabel "rTest1" rTest1,
    TestLabel "rTest2" rTest2,
    TestLabel "rTest3" rTest3,
    TestLabel "rTest4" rTest4,
    TestLabel "rTest5" rTest5,
    TestLabel "rTest6" rTest6,
    TestLabel "rTest7" rTest7,
    TestLabel "rTest8" rTest8,
    TestLabel "rTest9" rTest9,
    TestLabel "rTest10" rTest10,
    TestLabel "rTest11" rTest11,
    TestLabel "rTest12" rTest12,
    TestLabel "rTest13" rTest13,
    TestLabel "rTest14" rTest14,
    TestLabel "rTest15" rTest15,
    TestLabel "rTest16" rTest16,
    TestLabel "rTest17" rTest17,
    TestLabel "rTest19" rTest19,
    TestLabel "rTest20" rTest20,
    TestLabel "rTest21" rTest21,
    TestLabel "rTest22" rTest22,
    TestLabel "rTest23" rTest23,
    TestLabel "rTest24" rTest24,
    TestLabel "rTest25" rTest25,
    TestLabel "rTest26" rTest26,
    TestLabel "rTest28" rTest28,
    TestLabel "rTest29" rTest29,
    TestLabel "rTest30" rTest30,
    TestLabel "rTest31" rTest31,
    TestLabel "rTest32" rTest32,
    TestLabel "rTest33" rTest33,
    TestLabel "rTest34" rTest34,
    TestLabel "rTest35" rTest35,
    TestLabel "rTest35" rTest36,
    TestLabel "rTest35" rTest37,
    TestLabel "rTest35" rTest38,
    TestLabel "rTest63" rTest63,
    TestLabel "rTest64" rTest64,
    TestLabel "rTest65" rTest65,
    TestLabel "rTest66" rTest66,
    TestLabel "rTest67" rTest67,
    TestLabel "rTest68" rTest68,
    TestLabel "rTest69" rTest69,
    TestLabel "rTest70" rTest70,
    TestLabel "rTest71" rTest71,
    TestLabel "rTest72" rTest72,
    TestLabel "rTest73" rTest73,
    TestLabel "rTest74" rTest74,
    TestLabel "rTest75" rTest75,
    TestLabel "rTest76" rTest76,
    TestLabel "rTest77" rTest77,
    TestLabel "rTest78" rTest78,
    TestLabel "rTest79" rTest79,
    TestLabel "rTest80" rTest80,
    TestLabel "rTest81" rTest81,
    TestLabel "rTest82" rTest82,
    TestLabel "rTest85" rTest85,
    TestLabel "rTest87" rTest87,
    TestLabel "rTest88" rTest88,
    TestLabel "rTest89" rTest89,
    TestLabel "rTest90" rTest90,
    TestLabel "rTest92" rTest92,
    TestLabel "rTest93" rTest93,
    TestLabel "rTest95" rTest95,
    TestLabel "rTest96" rTest96,
    TestLabel "rTest97" rTest97,
    TestLabel "rTest98" rTest98,
    TestLabel "rTest99" rTest99,
    TestLabel "rTest100" rTest100,
    TestLabel "rTest101" rTest101,
    TestLabel "rTest102" rTest102,
    TestLabel "rTest103" rTest103,
    TestLabel "rTest104" rTest104,
    TestLabel "rTest105" rTest105,
    TestLabel "rTest106" rTest106,
    TestLabel "rTest107" rTest107,
    TestLabel "rTest108" rTest108,
    TestLabel "rTest109" rTest109,
    TestLabel "rTest110" rTest110,
    TestLabel "rTest111" rTest111,
    TestLabel "rTest112" rTest112,
    TestLabel "rTest113" rTest113,
    TestLabel "rTest114" rTest114,
    TestLabel "rTest115" rTest115,
    TestLabel "rTest136" rTest136,
    TestLabel "rTest137" rTest137,
    TestLabel "rTest138" rTest138,
    TestLabel "rTest141" rTest141,
    TestLabel "rTest142" rTest142,
    TestLabel "rTest145" rTest145,
    TestLabel "rTest148" rTest148,
    TestLabel "rTest149" rTest149,
    TestLabel "rTest150" rTest150,
    TestLabel "rTest151" rTest151,
    TestLabel "rTest152" rTest152,
    TestLabel "rTest153" rTest153,
    TestLabel "rTest154" rTest154,
    TestLabel "rTest156" rTest156,
    TestLabel "rTest158" rTest158,
    TestLabel "rTest159" rTest159,
    TestLabel "rTest160" rTest160,
    TestLabel "rTest161" rTest161,
    TestLabel "rTest162" rTest162,
    TestLabel "rTest163" rTest163,
    TestLabel "rTest165" rTest165,
    TestLabel "rTest166" rTest166,
    TestLabel "rTest167" rTest167,
    TestLabel "rTest168" rTest168,
    TestLabel "rTest170" rTest170,
    TestLabel "rTest171" rTest171,
    TestLabel "rTest173" rTest173,
    TestLabel "rTest174" rTest174,
    TestLabel "rTest175" rTest175,
    TestLabel "rTest176" rTest176,
    TestLabel "rTest177" rTest177,
    TestLabel "rTest178" rTest178,
    TestLabel "rTest179" rTest179,
    TestLabel "rTest180" rTest180,
    TestLabel "rTest181" rTest181,
    TestLabel "rTest182" rTest182,
    TestLabel "rTest184" rTest184,
    TestLabel "rTest185" rTest185,
    TestLabel "rTest186" rTest186,
    TestLabel "rTest188" rTest188,
    TestLabel "rTest189" rTest189,
    TestLabel "rTest190" rTest190,
    TestLabel "rTest191" rTest191,
    TestLabel "rTest192" rTest192,
    TestLabel "rTest193" rTest193
  ]

rTest1 = TestCase (assertBool "[a??]" (isLeft(runParser "[a??]")))
rTest2 = TestCase (assertBool "[\"\": 1]" (isLeft(runParser "[\"\": 1]")))
rTest3 = TestCase (assertBool "[\"\"]," (isLeft(runParser "[\"\"],")))
rTest4 = TestCase (assertBool "[,1]" (isLeft(runParser "[,1]")))
rTest5 = TestCase (assertBool "[1,,2]" (isLeft(runParser "[1,,2]")))
rTest6 = TestCase (assertBool "[\"x\",,]" (isLeft(runParser "[\"x\",,]")))
rTest7 = TestCase (assertBool "[\"x\"]]" (isLeft(runParser "[\"x\"]]")))
rTest8 = TestCase (assertBool "[\"\",]" (isLeft(runParser "[\"\",]")))
rTest9 = TestCase (assertBool "[\"x\"" (isLeft(runParser "[\"x\"")))
rTest10 = TestCase (assertBool "[x" (isLeft(runParser "[x")))
rTest11 = TestCase (assertBool "[3[4]]" (isLeft(runParser "[3[4]]")))
rTest12 = TestCase (assertBool "[??]" (isLeft(runParser "[??]")))
rTest13 = TestCase (assertBool "[1:2]" (isLeft(runParser "[1:2]")))
rTest14 = TestCase (assertBool "[,]" (isLeft(runParser "[,]")))
rTest15 = TestCase (assertBool "[-]" (isLeft(runParser "[-]")))
rTest16 = TestCase (assertBool "[   , \"\"]" (isLeft(runParser "[   , \"\"]")))
rTest17 = TestCase (assertBool "[\"a\"," (isLeft(runParser "[\"a\",")))
rTest19 = TestCase (assertBool ",1," (isLeft(runParser ",1,")))
rTest20 = TestCase (assertBool "[1,]" (isLeft(runParser "[1,]")))
rTest21 = TestCase (assertBool "[1,,]" (isLeft(runParser "[1,,]")))
rTest22 = TestCase (assertBool "" (isLeft(runParser "")))
rTest23 = TestCase (assertBool "[*]" (isLeft(runParser "[*]")))
rTest24 = TestCase (assertBool "[\"\"" (isLeft(runParser "[\"\"")))
rTest25 = TestCase (assertBool "[1," (isLeft(runParser "[1,")))
rTest26 = TestCase (assertBool "[1," (isLeft(runParser "[1,")))
rTest28 = TestCase (assertBool ",1" (isLeft(runParser ",1")))
rTest29 = TestCase (assertBool "[{}" (isLeft(runParser "[{}")))
rTest30 = TestCase (assertBool "{" (isLeft(runParser "{")))
rTest31 = TestCase (assertBool "[nul]" (isLeft(runParser "[nul]")))
rTest32 = TestCase (assertBool "\"a\": \"b\"" (isLeft(runParser "\"a\": \"b\"")))
rTest33 = TestCase (assertBool "}" (isLeft(runParser "}")))
rTest34 = TestCase (assertBool "[++1234]" (isLeft(runParser "[++1234]")))
rTest35 = TestCase (assertBool "[+1]" (isLeft(runParser "[+1]")))
rTest36 = TestCase (assertBool "{\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}}}" (isLeft(runParser "{\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}}}")))
rTest37 = TestCase (assertBool "{\"key\":\"bad_value }\"" (isLeft(runParser "{\"key\":\"bad_value }\"")))
rTest38 = TestCase (assertBool "{badkey : 2}" (isLeft(runParser "{badkey : 2}")))
rTest63 = TestCase (assertBool "[1+2]" (isLeft(runParser "[1+2]")))
rTest64 = TestCase (assertBool "[0x1]" (isLeft(runParser "[0x1]")))
rTest65 = TestCase (assertBool "[0x42]" (isLeft(runParser "[0x42]")))
rTest66 = TestCase (assertBool "[Infinity]" (isLeft(runParser "[Infinity]")))
rTest67 = TestCase (assertBool "[0e+-1]" (isLeft(runParser "[0e+-1]")))
rTest68 = TestCase (assertBool "[-123.123foo]" (isLeft(runParser "[-123.123foo]")))
rTest69 = TestCase (assertBool "[123??]" (isLeft(runParser "[123??]")))
rTest70 = TestCase (assertBool "[1e1??]" (isLeft(runParser "[1e1??]")))
rTest71 = TestCase (assertBool "[0??]" (isLeft(runParser "[0??]")))
rTest72 = TestCase (assertBool "" (isLeft(runParser "")))
rTest73 = TestCase (assertBool "[-Infinity]" (isLeft(runParser "[-Infinity]")))
rTest74 = TestCase (assertBool "[-foo]" (isLeft(runParser "[-foo]")))
rTest75 = TestCase (assertBool "[- 1]" (isLeft(runParser "[- 1]")))
rTest76 = TestCase (assertBool "[-012]" (isLeft(runParser "[-012]")))
rTest77 = TestCase (assertBool "[-.123]" (isLeft(runParser "[-.123]")))
rTest78 = TestCase (assertBool "[-1x]" (isLeft(runParser "[-1x]")))
rTest79 = TestCase (assertBool "[1ea]" (isLeft(runParser "[1ea]")))
rTest80 = TestCase (assertBool "[1e??]" (isLeft(runParser "[1e??]")))
rTest81 = TestCase (assertBool "[1.]" (isLeft(runParser "[1.]")))
rTest82 = TestCase (assertBool "[.123]" (isLeft(runParser "[.123]")))
rTest85 = TestCase (assertBool "[012]" (isLeft(runParser "[012]")))
rTest87 = TestCase (assertBool "{[: \"x\"}" (isLeft(runParser "{[: \"x\"}")))
rTest88 = TestCase (assertBool "" (isLeft(runParser "")))
rTest89 = TestCase (assertBool "{\"x\", null}" (isLeft(runParser "{\"x\", null}")))
rTest90 = TestCase (assertBool "{\"x\"::\"b\"}" (isLeft(runParser "{\"x\"::\"b\"}")))
rTest92 = TestCase (assertBool "{\"a\":\"a\" 123}" (isLeft(runParser "{\"a\":\"a\" 123}")))
rTest93 = TestCase (assertBool "{key: 'value'}" (isLeft(runParser "{key: 'value'}")))
rTest95 = TestCase (assertBool "{\"a\" b}" (isLeft(runParser "{\"a\" b}")))
rTest96 = TestCase (assertBool "{:\"b\"}" (isLeft(runParser "{:\"b\"}")))
rTest97 = TestCase (assertBool "{\"a\" \"b\"}" (isLeft(runParser "{\"a\" \"b\"}")))
rTest98 = TestCase (assertBool "{\"a\":" (isLeft(runParser "{\"a\":")))
rTest99 = TestCase (assertBool "{\"a\"" (isLeft(runParser "{\"a\"")))
rTest100 = TestCase (assertBool "{1:1}" (isLeft(runParser "{1:1}")))
rTest101 = TestCase (assertBool "{9999E9999:1}" (isLeft(runParser "{9999E9999:1}")))
rTest102 = TestCase (assertBool "{null:null,null:null}" (isLeft(runParser "{null:null,null:null}")))
rTest103 = TestCase (assertBool "{\"id\":0,,,,,}" (isLeft(runParser "{\"id\":0,,,,,}")))
rTest104 = TestCase (assertBool "{'a':0}" (isLeft(runParser "{'a':0}")))
rTest105 = TestCase (assertBool "{\"id\":0,}" (isLeft(runParser "{\"id\":0,}")))
rTest106 = TestCase (assertBool "{\"a\":\"b\"}/**/" (isLeft(runParser "{\"a\":\"b\"}/**/")))
rTest107 = TestCase (assertBool "{\"a\":\"b\"}/**//" (isLeft(runParser "{\"a\":\"b\"}/**//")))
rTest108 = TestCase (assertBool "{\"a\":\"b\"}//" (isLeft(runParser "{\"a\":\"b\"}//")))
rTest109 = TestCase (assertBool "{\"a\":\"b\"}/" (isLeft(runParser "{\"a\":\"b\"}/")))
rTest110 = TestCase (assertBool "{\"a\":\"b\",,\"c\":\"d\"}" (isLeft(runParser "{\"a\":\"b\",,\"c\":\"d\"}")))
rTest111 = TestCase (assertBool "{a: \"b\"}" (isLeft(runParser "{a: \"b\"}")))
rTest112 = TestCase (assertBool "{\"a\":\"a" (isLeft(runParser "{\"a\":\"a")))
rTest113 = TestCase (assertBool "{ \"foo\" : \"bar\", \"a\" }" (isLeft(runParser "{ \"foo\" : \"bar\", \"a\" }")))
rTest114 = TestCase (assertBool "{\"a\":\"b\"}#" (isLeft(runParser "{\"a\":\"b\"}#")))
rTest115 = TestCase (assertBool " " (isLeft(runParser " ")))
rTest126 = TestCase (assertBool "[\"\"]" (isLeft(runParser "[\"\"]")))
rTest134 = TestCase (assertBool "[ \"asd\"]" (isLeft(runParser "[ \"asd\"]")))
rTest136 = TestCase (assertBool "\"" (isLeft(runParser "\"")))
rTest137 = TestCase (assertBool "['single quote']" (isLeft(runParser "['single quote']")))
rTest138 = TestCase (assertBool "abc" (isLeft(runParser "abc")))
rTest141 = TestCase (assertBool "[\"new" (isLeft(runParser "[\"new")))
rTest142 = TestCase (assertBool "line\"]" (isLeft(runParser "line\"]")))
rTest145 = TestCase (assertBool "\"\"x" (isLeft(runParser "\"\"x")))
rTest148 = TestCase (assertBool "??????" (isLeft(runParser "??????")))
rTest149 = TestCase (assertBool "<.>" (isLeft(runParser "<.>")))
rTest150 = TestCase (assertBool "[<null>]" (isLeft(runParser "[<null>]")))
rTest151 = TestCase (assertBool "[1]x" (isLeft(runParser "[1]x")))
rTest152 = TestCase (assertBool "[1]]" (isLeft(runParser "[1]]")))
rTest153 = TestCase (assertBool "[\"asd]" (isLeft(runParser "[\"asd]")))
rTest154 = TestCase (assertBool "a????" (isLeft(runParser "a????")))
rTest156 = TestCase (assertBool "1]" (isLeft(runParser "1]")))
rTest158 = TestCase (assertBool "[][]" (isLeft(runParser "[][]")))
rTest159 = TestCase (assertBool "]" (isLeft(runParser "]")))
rTest160 = TestCase (assertBool "????{}" (isLeft(runParser "????{}")))
rTest161 = TestCase (assertBool "??" (isLeft(runParser "??")))
rTest162 = TestCase (assertBool "[" (isLeft(runParser "[")))
rTest163 = TestCase (assertBool "" (isLeft(runParser "")))
rTest165 = TestCase (assertBool "2@" (isLeft(runParser "2@")))
rTest166 = TestCase (assertBool "{}}" (isLeft(runParser "{}}")))
rTest167 = TestCase (assertBool "{\"\":" (isLeft(runParser "{\"\":")))
rTest168 = TestCase (assertBool "{\"a\":/*comment*/\"b\"}" (isLeft(runParser "{\"a\":/*comment*/\"b\"}")))
rTest170 = TestCase (assertBool "['" (isLeft(runParser "['")))
rTest171 = TestCase (assertBool "[," (isLeft(runParser "[,")))
rTest173 = TestCase (assertBool "" (isLeft(runParser "")))
rTest174 = TestCase (assertBool "[{" (isLeft(runParser "[{")))
rTest175 = TestCase (assertBool "[\"a" (isLeft(runParser "[\"a")))
rTest176 = TestCase (assertBool "[\"a\"" (isLeft(runParser "[\"a\"")))
rTest177 = TestCase (assertBool "{" (isLeft(runParser "{")))
rTest178 = TestCase (assertBool "{]" (isLeft(runParser "{]")))
rTest179 = TestCase (assertBool "{," (isLeft(runParser "{,")))
rTest180 = TestCase (assertBool "{[" (isLeft(runParser "{[")))
rTest181 = TestCase (assertBool "{\"a" (isLeft(runParser "{\"a")))
rTest182 = TestCase (assertBool "{'a'" (isLeft(runParser "{'a'")))
rTest184 = TestCase (assertBool "??" (isLeft(runParser "??")))
rTest185 = TestCase (assertBool "*" (isLeft(runParser "*")))
rTest186 = TestCase (assertBool "{\"a\":\"b\"}#{}" (isLeft(runParser "{\"a\":\"b\"}#{}")))
rTest188 = TestCase (assertBool "[1" (isLeft(runParser "[1")))
rTest189 = TestCase (assertBool "[ nul" (isLeft(runParser "[ nul")))
rTest190 = TestCase (assertBool "[1" (isLeft(runParser "[1")))
rTest191 = TestCase (assertBool "]" (isLeft(runParser "]")))
rTest192 = TestCase (assertBool "{\"asd\":\"asd\"" (isLeft(runParser "{\"asd\":\"asd\"")))
rTest193 = TestCase (assertBool "????" (isLeft(runParser "????")))


instance Eq MapElements where
  (MapElements x0 x1 x2 x3 x4 x5 x6 x7) == (MapElements y0 y1 y2 y3 y4 y5 y6 y7) =
    and [x0 == y0, x1 == y1, x2 == y2, x3 == y3, x4 == y4, x5 == y5, x6 == x6, x7 == y7]


gameTests = TestList
  [
    TestLabel "gTest1" gTest1,
    TestLabel "gTest2" gTest2,
    TestLabel "gTest3" gTest3,
    TestLabel "gTest4" gTest4,
    TestLabel "gTest5" gTest5,
    TestLabel "gTest6" gTest6,
    TestLabel "gTest7" gTest7,
    TestLabel "gTest8" gTest8,
    TestLabel "gTest9" gTest9,
    TestLabel "gTest10" gTest10,
    TestLabel "gTest11" gTest11,        
    TestLabel "gTest12" gTest12,
    TestLabel "gTest13" gTest13,
    TestLabel "gTest14" gTest14
  ]

gTest1string = "{\"bomb\":null,\"surrounding\":null,\"bomb_surrounding\":null}"
gTest1 = TestCase (assertEqual gTest1string
  (Right (MapElements [] [] [] [] [] [] [] []))
  (runGameParser gTest1string))

gTest2string = "{\"bomb_surrounding\":null,\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}}}}"
gTest2 = TestCase (assertEqual gTest2string
  (Right (MapElements [] [[1,1]] [] [] [] [] [] []))
  (runGameParser gTest2string))

gTest3string = "{\"bomb_surrounding\":null,\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[0,4],\"tail\":{\"head\":null,\"tail\":null}},\
            \\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":null,\"tail\":null}}}}}"
gTest3 = TestCase (assertEqual gTest3string
  (Right (MapElements [] [[0,4]] [[8,7], [8,3]] [] [] [] [] []))
  (runGameParser gTest3string))

gTest4string  = "{\"bomb_surrounding\":null,\"bomb\":null,\"surrounding\":{\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":null,\"tail\":null}}},\
            \\"bombermans\":{\"head\":[0,4],\"tail\":{\"head\":null,\"tail\":null}}}}"
gTest4 = TestCase (assertEqual gTest4string
  (Right (MapElements [] [[0,4]] [[8,7], [8,3]] [] [] [] [] []))
  (runGameParser gTest4string))

gTest5string = "{\"bomb_surrounding\":null,\"surrounding\":{\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":null,\"tail\":null}}},\
            \\"bombermans\":{\"head\":[0,4],\"tail\":{\"head\":null,\"tail\":null}}}, \"bomb\":[5, 5]}"
gTest5 = TestCase (assertEqual gTest5string
  (Right (MapElements [[5,5]] [[0,4]] [[8,7], [8,3]] [] [] [] [] []))
  (runGameParser gTest5string))

gTest6string = "\"random\""
gTest6 = TestCase (assertEqual gTest6string
  (Left "Error: there should be 3 objects with keys: bomb, surrounding, bomb_surrounding")
  (runGameParser gTest6string))

gTest7string = "{\"bomb\":null}"
gTest7 = TestCase (assertEqual gTest7string
  (Left "Error: there should be 3 objects with keys: bomb, surrounding, bomb_surrounding")
  (runGameParser gTest7string))


gTest8string = "{\"surrounding\":null}"
gTest8 = TestCase (assertEqual gTest8string
  (Left "Error: there should be 3 objects with keys: bomb, surrounding, bomb_surrounding")
  (runGameParser gTest8string))

gTest9string = "{\"bomb\":null,\"bomb_surrounding\":null,\"surrounding\":[]}"
gTest9 = TestCase (assertEqual gTest9string
  (Left "Error: invalid type after key \"surrounding\".")
  (runGameParser gTest9string))

gTest10string = "{\"bomb\":[1,2,3],\"surrounding\":{}, \"bomb_surrounding\":null}"
gTest10 = TestCase (assertEqual gTest10string
  (Left "Error: list with key `bomb` should contain only two coordinates that are numbers.")
  (runGameParser gTest10string))

gTest11string = "{\"bomb\":[1],\"surrounding\":null, \"bomb_surrounding\":null}"
gTest11 = TestCase (assertEqual gTest11string
  (Left "Error: list with key `bomb` should contain only two coordinates that are numbers.")
  (runGameParser gTest11string))

gTest12string = "{\"bomb\":[1,2,3],\"surrounding\":null, \"bomb_surrounding\":null}"
gTest12 = TestCase (assertEqual gTest12string
  (Left "Error: list with key `bomb` should contain only two coordinates that are numbers.")
  (runGameParser gTest12string))

gTest13string = "{\"bomb_surrounding\":null, \"bomb\":null,\"surrounding\":{\"bombermans\":{\"BADHEADDDDDDDDD\":[0,4],\"tail\":{\"head\":null,\"tail\":null}},\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":null,\"tail\":null}}}}}"
gTest13 = TestCase (assertEqual gTest13string
  (Left "Error: linked list should have \"head\" with a list (or null, if empty) as a value, and \"tail\" with an object (or null) as a value.")
  (runGameParser gTest13string))

gTest14string = "{\"bomb_surrounding\":null, \"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[0,4, 5],\"tail\":{\"head\":null,\"tail\":null}}}}"
gTest14 = TestCase (assertEqual gTest14string
  (Left "Error: \"head\" must have 2 and only 2 elements that are positive integers that represent coordinates of an object.")
  (runGameParser gTest14string))


runGameParser str = do
  json <- runParser str
  createMapElements json