module Tests where
import Parser2
import Data.Either

runJsonTests :: IO ()
runJsonTests = do putStrLn "Loading tests from testsR..."
                  failedR <- loadTests testsR testsRLineNum
                  putStrLn "Loading tests from testsL..."
                  failedL <- loadTests testsL testsLLineNum
                  putStrLn "******************************************"
                  putStrLn ("Total number of tests failed: " ++ show (failedL + failedR) ++ ".")


loadTests :: [Bool] -> Int -> IO Int
loadTests [] n = return 0
loadTests [x] n
    | x = do putStrLn ("\ESC[32;40mTest (line " ++ show n ++ ") passed.\ESC[0m")
             return 0
    | otherwise = do putStrLn ("\ESC[31;40mTest (line " ++ show n ++ ") failed.\ESC[0m")
                     return 1
loadTests (x:xs) n = do value <- loadTests [x] n
                        values <- loadTests xs (n + 1) 
                        return (values + value)


-- | All tests from testsR should pass as they are valid JSONs.

testsRLineNum = 30
testsR = [
    aPass "{}",  -- change testsRLineNum to the line number of the first list element.
    aPass "  { \n } \t ",
    aPass "{\"a\": {  }}",
    aPass "{\"a\": \"b\"}",
    aPass "{\"adomo obuolys\": 2}",
    aPass "[]",
    aPass "[{}]",
    aPass "{\"a\":[]}",
    aPass "[1,    \"hi\", null, {}, []]",
    aPass "0",
    aPass "-15",
    aPass "22222222222222222222",
    aPass' "123456789012345678901234567890" (JsonLikeInteger 123456789012345678901234567890),
    aPass "\"\\\"\"",
    aPass' "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\"" (JsonLikeString "\\\"\\\\\\/\\b\\f\\n\\r\\t"),
    aPass "\"\\u260E\"",
    aPass "\"string\"",
    aPass "null",
    aPass "[1, 2, 2]",
    aPass "[1, 2, 2, \"hi\", null]",
    aPass "{  \"id\"  :1}",
    aPass "{\"name\":\"Joe\",\"id\":1}",
    aPass "{\"id\":1,\"address\":{\"addr1\":\"123 Main\",\"addr2\":null,\"city\":\"Houston\",\"state\":\"TX\"}}",
    aPass "{\"a\":{\"b\":{\"c\":{\"d\":{\"e\":{\"f\":{\"g\":{\"h\":{\"i\":{\"j\":{\"k\":{\"l\":{\"m\":{\"n\":{\"o\":{\"p\":\"blah\"}}}}}}}}}}}}}}}}",
    aPass "{\"id\":1,\"pets\":[\"dog\",\"cat\",\"fish\"]}",
    aPass "{\"stuff\":[321, \"abc\"]}",
    aPass "{\"stuff\":[{\"pet\":\"cat\"},{\"car\":\"Ford\"}]}",
    aPass "{\"id\":1,\"name\":\"Joe\",\"friends\":[{\"id\":2,\"name\":\"Pat\",\"pets\":[\"dog\"]},{\"id\":3,\"name\":\"Sue\",\"pets\":[\"bird\",\"fish\"]}],\"pets\":[]}",
    aPass "{\"id\":1,\"stuff\":[[1,2],[2,  3],  [ ],[3,4]]}"
    ]


-- | testsL contains strings that are not valid JSONs.
-- | All of them should fail (aFail should return true for each).
testsLLineNum = 79
testsL = [
    aFail "badstring", -- change testsLLineNum to the line number of the first list element.
    aFail "\"halfstringgood",
    aFail "{",
    aFail "}",
    aFail "{\"validstringbutnoending}\"",
    aFail "{\"validstring but no ending }\"",
    aFail "{\"key\":\"bad_value }\"",
    aFail "{badkey : 2}",
    aFail "{}{}",
    aFail "[",
    aFail "[1, 2, 3",
    aFail "[1, ",
    aFail "[1, ]",
    aFail "-",
    aFail "\"\\a\"",
    aFail "\"\\u26\"",
    aFail "\"\\uZ000\"",
    aFail "02",
    aFail "{\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}}}"
    ]


aPass :: String -> Bool
aPass = isRight . runParser
testL1 = isLeft (runParser "[1, 2, 2, hi, null]")


aPass' :: String -> JsonLike -> Bool
aPass' str expected
    | null result = False
    | head result == expected = True
    | otherwise = False
    where result = rights [runParser str]
aFail = not . aPass
aFail' s ex = not (aPass' s ex)


-- | Do not rely heavily on this; is for testing purposes only.
instance Eq JsonLike where
  (JsonLikeInteger i) == (JsonLikeInteger i') = i == i'
  (JsonLikeString s) == (JsonLikeString s') = s == s'
  (JsonLikeList []) == (JsonLikeList []) = True
  (JsonLikeList [js]) == (JsonLikeList [js']) = js == js'
  (JsonLikeList (x:xs)) == (JsonLikeList (x':xs')) = (length xs == length xs' && x == x') && xs == xs'
  (JsonLikeObject []) == (JsonLikeObject []) = True
  (JsonLikeObject [(s, js)]) == (JsonLikeObject [(s', js')]) = s == s' && js == js'
  (JsonLikeObject xs) == (JsonLikeObject xs') = xs == xs'
  JsonLikeNull == JsonLikeNull = True
  _ == _ = False

