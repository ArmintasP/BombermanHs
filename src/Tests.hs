module Tests where
import Parser2
import MapElements (createMapElements, MapElements (MapElements))
import MapRender
import Data.Either
import Lib3 (InitData(InitData))

-- | Run both, runJsonTests and runGameTests to make sure parsing is less likely to fail.

runJsonTests :: IO ()
runJsonTests = loadTestsGeneral testsR testsRLineNum testsL testsLLineNum

runGameTests :: IO ()
runGameTests = loadTestsGeneral testsRGame testsRGameLineNum testsLGame testsLGameLineNum

loadTestsGeneral :: [IO Bool] -> Int -> [IO Bool] -> Int -> IO ()
loadTestsGeneral t1 size1 t2 size2 = do putStr "\ESC[2J" -- Cleans terminal.
                                        putStrLn "Loading tests from testsRGame..."
                                        failedR <- loadTests t1 size1
                                        putStrLn "\nLoading tests from testsLGame with error messages...\n"
                                        failedL <- loadTests t2 size2
                                        putStrLn "******************************************"
                                        putStrLn ("Total number of tests failed: " ++ show (failedL + failedR) ++ ".\n")


-- | All tests from testsR should pass as they are valid JSONs.

testsRLineNum = 29 -- use `0` as a value if tests are spanning multiple lines.
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
testsLLineNum = 65 -- use `0` as a value if tests are spanning multiple lines.
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
testsRGameLineNum = 88

testsRGame = [
    gAPass "{\"bomb\":null,\"surrounding\":null,\"bomb_surrounding\":null}" (MapElements [] [] [] [] [] [] [] []),
    gAPass "{\"bomb_surrounding\":null,\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[1,1],\"tail\":{\"head\":null,\"tail\":null}}}}" (MapElements [] [[1,1]] [] [] [] [] [] []),

    gAPass "{\"bomb_surrounding\":null,\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[0,4],\"tail\":{\"head\":null,\"tail\":null}},\
            \\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":null,\"tail\":null}}}}}" (MapElements [] [[0,4]] [[8,7], [8,3]] [] [] [] [] []),

    gAPass "{\"bomb_surrounding\":null,\"bomb\":null,\"surrounding\":{\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":null,\"tail\":null}}},\
            \\"bombermans\":{\"head\":[0,4],\"tail\":{\"head\":null,\"tail\":null}}}}" (MapElements [] [[0,4]] [[8,7], [8,3]] [] [] [] [] []),

    gAPass "{\"bomb_surrounding\":null,\"surrounding\":{\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":null,\"tail\":null}}},\
            \\"bombermans\":{\"head\":[0,4],\"tail\":{\"head\":null,\"tail\":null}}}, \"bomb\":[5, 5]}" (MapElements [[5,5]] [[0,4]] [[8,7], [8,3]] [] [] [] [] [])
    ]

testsLGameLineNum = 103
testsLGame = [
    gAFail "\"random\"",
    gAFail "{\"bomb\":null}",
    gAFail "{\"surrounding\":null}",
    gAFail "{\"bomb\":null,\"surrounding\":[]}",
    gAFail "{\"bomb\":[1,2,3],\"surrounding\":null}",
    gAFail "{\"bomb\":[1],\"surrounding\":null}",
    gAFail "{\"bomb\":[1,2,3],\"surrounding\":null}",
    gAFail "{\"bomb\":null,\"surrounding\":{\"bombermans\":{\"BADHEADDDDDDDDD\":[0,4],\"tail\":{\"head\":null,\"tail\":null}},\"bricks\":{\"head\":[8,7],\"tail\":{\"head\":[8,3],\"tail\":{\"head\":null,\"tail\":null}}}}}",
    gAFail "{\"bomb\":null,\"surrounding\":{\"bombermans\":{\"head\":[0,4, 5],\"tail\":{\"head\":null,\"tail\":null}}}}"
    ]

instaRender json = do putStr (MapRender.render (MapRender.init (InitData 10 10) json))

gAFail :: String -> IO Bool
gAFail str = case runParser str of
    (Left e) -> do putStr $ e ++ "\n\t^"
                   return True
    (Right json) -> case createMapElements json of
        (Left e) -> do putStr $ e ++ "\n\t^"
                       return True
        (Right xs) -> return False


gAPass :: String -> MapElements -> IO Bool
gAPass str me = case runParser str of
    (Left e) -> return False
    (Right json) -> case createMapElements json of
        (Left e) -> return False
        (Right xs) -> return (xs == me)

aPass :: String -> IO Bool
aPass = return . isRight . runParser


aPass' :: String -> JsonLike -> IO Bool
aPass' str expected
    | null result = return False
    | head result == expected = return True
    | otherwise = return False
    where result = rights [runParser str]
    
aFail :: String -> IO Bool
aFail str = case runParser str of
    (Left e) -> do putStr $ e ++ "\n\t^"
                   return True
    (Right xs) -> return False
aFail' s ex = do not <$> aPass' s ex

loadTests :: [IO Bool] -> Int -> IO Int
loadTests [] n = return 0
loadTests [x] n = do x' <- x
                     if x' then
                         do putStrLn ("\ESC[32;40mTest (line " ++ show n ++ ") passed.\ESC[0m")
                            return 0
                     else
                         do putStrLn ("\ESC[31;40mTest (line " ++ show n ++ ") failed.\ESC[0m")
                            return 1
loadTests (x:xs) n = do value <- loadTests [x] n
                        values <- loadTests xs (n + 1)
                        return (values + value)


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


instance Eq MapElements where
    (MapElements x0 x1 x2 x3 x4 x5 x6 x7) == (MapElements y0 y1 y2 y3 y4 y5 y6 y7) = and [x0 == y0, x1 == y1, x2 == y2, x3 == y3, x4 == y4, x5 == y5, x6 == x6, x7 == y7]
