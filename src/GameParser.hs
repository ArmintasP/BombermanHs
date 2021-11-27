module GameParser where
import Parser3 (JsonLike(..))
import Data.Either
import Data.List (intercalate, delete)

-- Pass JsonLike and get a list of coordinates with keys (map element names) on success.
jsonToCoordinates :: JsonLike -> Either String [(String, [[Int]])]
jsonToCoordinates json = xs >>= jsonExtract
  where xs  = jsonGetMapObjects json

-- | Extracts values from values of "bomb" and "surrounding". Parses linked list into [[Int]].
jsonExtract :: [(String, JsonLike)] -> Either String [(String, [[Int]])]
jsonExtract [] = Right []
jsonExtract [([], _)] = Left "Error: object key should not be an empty string."

-- | IMPORTANT: This "bomb" and "bomb_surrounding" require a separate case since their coordinates are given not in a linked list for some reason.
jsonExtract [("bomb", js@(JsonLikeList [JsonLikeInteger x, JsonLikeInteger x']))] = do
    res <- getListElements [js] "bomb"
    return [("bomb", res)]
jsonExtract [(key, JsonLikeList xs)] = do
    res <- getListElements xs key
    return [(key, res)]
jsonExtract [(key, JsonLikeNull)] = Right [(key, [])]
jsonExtract [(key, JsonLikeObject xs)] = do
  coordinates <- constructList xs
  return [(key, coordinates)]
jsonExtract [(key, _)] = Left ("Error: \"" ++ key ++ "\" should have an object (a linked list) as a value, not a list, string or integer.")
jsonExtract (x:xs) = do
  pair <- jsonExtract [x]
  pairs <- jsonExtract xs
  return $ pair ++ pairs

getListElements :: [JsonLike] -> String -> Either String [[Int]]
getListElements [JsonLikeList [JsonLikeInteger x, JsonLikeInteger x']] key =
    if signum x >= 0 && signum x' >= 0 then Right [[fromInteger x, fromInteger x']]
    else Left $ "Error: bomb coordinates should be positive in llist with key: " ++ key
getListElements (x:y:xs) key = (++) <$> getListElements [x] key <*> getListElements (y:xs) key
getListElements [] key = Right []
getListElements _ key = Left ("Error: list with key `" ++ key ++ "` should contain only two coordinates that are numbers.")

-- | Constructs a list (that is formed with 'head' and 'tail') of coordinates.
constructList :: [(String, JsonLike)] -> Either String [[Int]]
constructList [("head", JsonLikeNull) , ("tail", JsonLikeNull)] = Right []
constructList [("head", JsonLikeList jval), ("tail", JsonLikeObject obj)] = do
  listH <- constructListHead jval
  listT <- constructList obj
  return $ listH ++ listT
constructList _  = Left "Error: linked list should have \"head\" with a list (or null, if empty) as a value, and \"tail\" with an object (or null) as a value."

-- | Takes values from a linked list's head.
constructListHead :: [JsonLike] -> Either String [[Int]]
constructListHead [] = Left "Error: if \"head\" has an empty list, its value should be null. Otherwise populate it with coordinates (2 positive integers)."
constructListHead [js, js'] = case (js, js') of
  (JsonLikeInteger x, JsonLikeInteger x') -> if signum x >= 0 && signum x' >= 0 then Right [[fromInteger x, fromInteger x']]
    else Left "Error: \"head\" has a list with negative integer(s)."
  (_, _) -> Left "Error: \"head\" has a list containing other types than positive integers."
constructListHead js = Left "Error: \"head\" must have 2 and only 2 elements that are positive integers that represent coordinates of an object."

-- | Checks if passed json has "surrounding" and "bomb" field. They are crucial for our current program.
jsonGetMapObjects :: JsonLike -> Either String [(String, JsonLike)]
jsonGetMapObjects (JsonLikeObject [o1, o2, o3]) = handleInitialObjects initialObjects [o1, o2, o3]
jsonGetMapObjects _ = Left initialObjectsErrorMessage

handleBombAndSurroundings :: (String, JsonLike) -> (String, JsonLike) -> Either String [(String, JsonLike)]
handleBombAndSurroundings (b, js) (s, js') = case js' of
  (JsonLikeObject js'') -> Right (("bomb", js) : js'')
  JsonLikeNull -> Right [("bomb", js)]
  _ -> Left "Error: \"surrounding\" doesn't have correct value, it should be null or Json object."



handleInitialObjects :: [String] -> [(String, JsonLike)] -> Either String [(String, JsonLike)]
handleInitialObjects ["bomb"] [(k, js)] = Right [(k, js)]
handleInitialObjects [key] [(k, JsonLikeObject js)] = Right $ appendKey js key
handleInitialObjects [key] [(k, JsonLikeNull)] = Right []
handleInitialObjects [key] _ = Left $ "Error: invalid type after key \"" ++ key ++ "\"."
handleInitialObjects keys ((k, js):xs)
  | k `elem` keys = (++) <$> handleInitialObjects [k] [(k, js)] <*> handleInitialObjects (delete k keys) xs
  | otherwise = Left initialObjectsErrorMessage
handleInitialObjects _ _ = Left initialObjectsErrorMessage

appendKey :: [(String, JsonLike)] -> String -> [(String, JsonLike)]
appendKey [] _ = []
appendKey [(str, js)] key = [(key ++ "_" ++ str, js)]
appendKey (x:xs) key = appendKey [x] key ++ appendKey xs key


initialObjects :: [String]
initialObjects = ["bomb", "surrounding", "bomb_surrounding"]

initialObjectsErrorMessage :: String
initialObjectsErrorMessage = "Error: there should be " ++ show (length initialObjects) ++ " objects with keys: " ++ intercalate ", " initialObjects

singleton :: a -> [a]
singleton a = [a]