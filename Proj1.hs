{-
  File     : Proj1.hs
  Author   : Bowen Huang
  Origin   : Thu Aug 31 23:05:20 2017
  Purpose  : Provide guess program for 3 combination of different pitches
-}

module Proj1 (initialGuess,nextGuess, GameState) where

--import Data.Map (Map, (!))
import qualified Data.Map as Map

-- GameState is a collection of all possible combinations of 3 pitches 
type GameState = [[String]]

allPitches = ["A1","A2","A3","B1","B2","B3","C1","C2","C3","D1","D2","D3","E1","E2","E3","F1","F2","F3","G1","G2","G3"]

{-
  Provide initial guess and generate initial game state
-}
-- Initial guess provide a predefined answer for first guess
-- Initial GameState is all possible combinations of 21 pitches
initialGuess :: ([String], GameState)
initialGuess =(initialPitches,allCombPitches)
  where
    initialPitches = ["A1","B2","C3"]
    allCombPitches = allComb 3 allPitches


-- allComb n lst return all n-sub combinations of elements in lst
allComb :: Int -> [a] -> [[a]]
allComb _ [] = [[]]
allComb 0 _ = [[]]
allComb n (p:ps) =
  phead ++ others
  where
    phead = [p:rest | rest <- allComb (n-1) ps]
    others =
      if n<=length ps then allComb n ps
      else []

{-
  Provide guess result depends on last guess and response from test code
-}
-- Return modified game state and guess made on previews guess and response
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (guess,gamestate) (i,j,k) =
  (selectReturn modifygamestate,modifygamestate)
  where
    modifygamestate = modifyGameState (guess,gamestate) (i,j,k)


-- Modify game state by calculate the response of last guess with all possible target, and remove the potential target who give the different result
modifyGameState :: ([String],GameState) -> (Int,Int,Int) -> GameState
modifyGameState (_,[]) _ = []
modifyGameState (guess,gs:gss) (i,j,k)
  | feedback guess gs == (i,j,k) = gs:modifyGameState (guess,gss) (i,j,k)
  | otherwise = modifyGameState (guess,gss) (i,j,k)


-- Select the best guess from current game state 
selectReturn :: GameState -> [String]
selectReturn [] = []
selectReturn gs =
  if length gs == 1 then head gs
  else
    minFeedback (generateFeedback gs gs)

-- Generate a list of (guess,expectation)
generateFeedback :: GameState -> GameState -> [([String],Int)]
generateFeedback [] _ = []
generateFeedback (gs:gss) origs = (gs,getExpectation gs origs) : generateFeedback gss origs


-- Get Expectation of guess numbers for one guess
getExpectation :: [String] -> GameState -> Int
getExpectation guess gs = sum $ map (^2) allFreq
  where
    allfeedback = getFeedback guess gs
    allFreq = getFbFreqLst (countFeedback allfeedback)

-- Generate a list of feedback frequency 
getFbFreqLst :: [((Int,Int,Int),Int)] -> [Int]
getFbFreqLst [] = []
getFbFreqLst (gs:gss) = getSec gs : getFbFreqLst gss
  where
    getSec (_,a) = a

-- Generate a list of all feedback with different elements in potential target
getFeedback :: [String] -> GameState -> [(Int,Int,Int)]
getFeedback _ [] = []
getFeedback guess (ts:tss) =
  if guess == ts then getFeedback guess tss
  else feedback guess ts:getFeedback guess tss

-- Count number of feedback from all potential targets
countFeedback :: [(Int,Int,Int)] -> [((Int,Int,Int),Int)]
countFeedback xs = Map.toList (Map.fromListWith (+) [(x,1)|x<-xs])

-- Find the minimum expectation guess
minFeedback :: [([String],Int)] -> [String]
minFeedback [] = error "empty list has no maximum"
minFeedback (fs:fss) = findMin fs fss
  where
    findMin ts [] = fst ts
    findMin ts (gs:gss) =
      if snd ts < snd gs then findMin ts gss
      else findMin gs gss

-- Compute the feedback from guess to target
feedback :: [String] -> [String] -> (Int,Int,Int)
feedback guess target = (mPitch, mNote, mOctave)
  where
    mGuess = matchPitch guess target
    mTarget = matchPitch target guess
    mPitch = 3-length mGuess
    mNote = length mGuess - length (matchPitch (remainBy 0 mGuess) (remainBy 0 mTarget))
    mOctave = length mGuess - length (matchPitch (remainBy 1 mGuess) (remainBy 1 mTarget))

-- Compute the number of match elements in two lists 
matchPitch :: [String] -> [String] -> [String]
matchPitch [] _ = []
matchPitch (gs:gss) target =
  if gs `elem` target then matchPitch gss (dropEle gs target)
  else gs:matchPitch gss target

-- dropEle target list if list contains target element, then drop it from list
dropEle :: String -> [String] -> [String]
dropEle _ [] = []
dropEle a (ts:tss) =
  if a == ts then tss
  else ts:dropEle a tss

-- remainBy p list remain the part in p position for every element in list
remainBy :: Int -> [String] -> [String]
remainBy _ [] = []
remainBy pos (ss:sss) =
  [ss!!pos]:remainBy pos sss

is_static :: Int  -> Bool
is_static n =
  case n of
    1 -> True
    2 -> True
    3 -> False





