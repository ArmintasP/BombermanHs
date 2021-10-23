<<<<<<< HEAD
![Bomberman logo](logo.png)

Contents
===
- [Usage](#usage)
- [Current state](#current-state)
- [How it's Done?](#how-its-done)
  - [Parsing](#parsing)
  - [Rendering](#rendering)
<hr>


## Usage
Open terminal in project's directory & write `stack run`.
<hr>

## Current state
![Bomberman demo](preview.gif) 
- Received JSON data is parsed using our own  `/src/Parser.hs` functions.
- Colored rendering is done in `/src/Lib1.hs`.
<hr>

## How it's Done?
### Parsing
1. Before JSON string is finally parsed, it must be modified - we get rid of '\' and '"' and change ':' into '='. 
```haskell
changeSyms :: String -> String
changeSyms str = modifyString ':' '=' (ms '\"' (ms '\\' str))
  where ms ch = modifyString ch space
```
2. We get rid drop first 15 symbols and the last one from the modified string and add "Surrounding" to it so it could be read using `Prelude.read`.
```haskell
getSurrounding :: String -> Surrounding
getSurrounding str = read ("Surrounding " ++ init (drop 15 (changeSyms str )))::Surrounding
```
3. Voilà, we have parsed JSON string and now important data lies in the object of type Surrounding.

However, this specific parsing method will change in the future, as it is not universal enough and does not tolerate changes.

### Rendering
1. `init`, responsible for initializing the game, calls generateEmptyMap which populates an array with default symbols.
```haskell
generateEmptyMap :: InitData -> [(Int, String)]
generateEmptyMap (InitData w h) = [(entryNumber, defaultSym) | entryNumber <- [0 .. (w * h)]]
```
2. `getSurroundingEntries` gets all entries (coordinate, rendering symbol) from a `Surrounding` object.
```haskell
getSurroundingEntries :: Surrounding -> InitData -> [[Entry]]
getSurroundingEntries surr iData = map (getEntries surr iData) surrFuns
```
4. `update` cals `updateMap` only if the newest JSON string is different from the last one. UpdateMaps collects all entries from a `Surrounding` object, puts them into the map and passes bombermans' coordiantes to the state, as they should not be in the map.
```haskell
updateMap :: State -> State
updateMap (State str iData prevMap _ ) = State str iData newMap bMans 
  where
    surr = getSurrounding str
    entries = concat (getSurroundingEntries surr iData)
    newMap = inserEntriesToMap entries prevMap
    bMans = getEntries surr iData (bombermans, bombermansSym)

```
5. Finally, if `render` is called, it makes a copy of the map with bomberman inserted and joins every entry into a single `String`. It also shows `str`, which is useful for debugging.
```haskell
render (State str iData gMap bMan) = mapToString ++ str
  where
    mapWithBMan = inserEntriesToMap bMan gMap
    w = gameWidth iData
    mapToString = concat [str ++ p | (i, str) <- mapWithBMan,
                          let p = if (i + 1) `mod` w == 0 then newlineSym else ""]
```