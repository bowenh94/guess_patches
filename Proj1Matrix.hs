module Proj1 (initialGuess, nextGuess, GameState) where

type GameState = [[String]]

allPitches = ["A1","A2","A3","B1","B2","B3","C1","C2","C3","D1","D2","D3","E1","E2","E3","F1","F2","F3","G1","G2","G3"]

-- Initial guess provide a predefined answer
-- Initial GameState is empty
initialGuess :: ([String], GameState)
initialGuess = (["A1","B2","C3"], [allPitches,allPitches,allPitches])


-- Input: previous guess and game state, and feedback from composer as (correct,notes and octaves).
-- Return: pair of next guess
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess ([_],_) (3,0,0) = error "Guess already success!"
nextGuess (guess,gamestate) (mPitch,mNote,mOctave) =
  (take 3 (head mgamestate),mgamestate)
  where
    mgamestate = modifyGameState (guess,gamestate) (mPitch,mNote,mOctave)

modifyGameState :: ([String],GameState) -> (Int,Int,Int) -> GameState
modifyGameState ([_],_) (3,0,0) = error "Guess already success!"
modifyGameState (guess,gamestate) (mPitch,mNote,mOctave) =
  verifyGameState [gamestateR0,gamestateR1,gamestateR2]
  where
    gamestateR0 = genGameStateRow (mPitch,mNote,mOctave) 0 guess (gamestate!!0)
    gamestateR1 = genGameStateRow (mPitch,mNote,mOctave) 1 guess (gamestate!!1)
    gamestateR2 = genGameStateRow (mPitch,mNote,mOctave) 2 guess (gamestate!!2)

genGameStateRow :: (Int,Int,Int) -> Int -> [String] -> [String] -> [String]
genGameStateRow  (mPitch,mNote,mOctave) idx guess gamestateRow
  | mPitch==0 =
    modifyGameStateTail ((guess),(modifyGameStateTail ((guess),(deletesub guess gamestateRow)) (mPitch,mNote) 0)) (mPitch,mOctave) 1
  | mPitch==1 && idx==0 =
    guess!!0:modifyGameStateTail ((tail guess),(modifyGameStateTail ((tail guess),(deletesub guess gamestateRow)) (mPitch,mNote) 0)) (mPitch,mOctave) 1
  | mPitch==1 && idx==1 =
    guess!!1:modifyGameStateTail ((head guess:drop 2 guess),(modifyGameStateTail ((head guess:drop 2 guess),(deletesub guess gamestateRow)) (mPitch,mNote) 0)) (mPitch,mOctave) 1
  | mPitch==1 && idx==2 =
    guess!!2:modifyGameStateTail ((take 2 guess),(modifyGameStateTail ((take 2 guess),(deletesub guess gamestateRow)) (mPitch,mNote) 0)) (mPitch,mOctave) 1
  | mPitch==2 && idx==0 =
    take 2 guess ++ modifyGameStateTail (([guess!!2]),(modifyGameStateTail (([guess!!2]),(deletesub guess gamestateRow)) (mPitch,mNote) 0)) (mPitch,mOctave) 1
  | mPitch==2 && idx==1 =
    head guess:drop 2 guess ++ modifyGameStateTail (([guess!!1]),(modifyGameStateTail (([guess!!1]),(deletesub guess gamestateRow)) (mPitch,mNote) 0)) (mPitch,mOctave) 1
  | mPitch==2 && idx==2 =
    tail guess ++ modifyGameStateTail ([guess!!0],(modifyGameStateTail ([guess!!0],(deletesub guess gamestateRow)) (mPitch,mNote) 0)) (mPitch,mOctave) 1

-- Modify tail for game state
modifyGameStateTail :: ([String],[String]) -> (Int,Int) -> Int -> [String]
modifyGameStateTail (rmGuess,gsTail) (mPitch,mOther) idx
  | mOther==0 =
    deletesubBy (idx,rmGuess) gsTail
  | (mPitch+mOther)==3 =
    deletesubByNeg (idx,rmGuess) gsTail
  | otherwise =
    gsTail   -- keep original for now 

-- Remove game state rows less than three pitches 
verifyGameState :: GameState -> GameState
verifyGameState [] = []
verifyGameState (gs:gss)
  | length gs < 3 = verifyGameState gss
  | otherwise = gs:verifyGameState gss

















-- Delete a sub list from list by position 
deletesubBy :: (Int,[String]) -> [String] -> [String]
deletesubBy (_,[]) games = games
deletesubBy (pos,(g:gs)) games =
  deletesubBy (pos,gs) gamesModify
  where
    gamesModify = deleteBy (pos,g) games

-- Delete Strings by char in pos from a list 
deleteBy :: (Int,String) -> [String] -> [String]
deleteBy _ [] = []
deleteBy (pos,g) (games:gamess) =
  if g!!pos == games!!pos then
    deleteBy (pos,g) gamess
  else
    games:deleteBy (pos,g) gamess

-- Delete a sub list from list
-- input: sub lst
deletesub :: [String] -> [String] -> [String]
deletesub [] lst = lst
deletesub _ [] = []
deletesub sub lst = filter (`notElem` sub) lst

-- Delete a sub list from list by position 
deletesubByNeg :: (Int,[String]) -> [String] -> [String]
deletesubByNeg (_,[]) _ = []
deletesubByNeg (pos,g:gs) games =
  deleteByNeg (pos,g) games++deletesubByNeg (pos,gs) games

-- Delete Strings by char in pos from a list 
deleteByNeg :: (Int,String) -> [String] -> [String]
deleteByNeg _ [] = []
deleteByNeg (pos,g) (games:gamess) =
  if g!!pos /= games!!pos then
    deleteByNeg (pos,g) gamess
  else
    games:deleteByNeg (pos,g) gamess

{-first Guess generate gamestate from first response
firstGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
firstGuess (guess,gamestate) (mPitch,mNote,mOctave)
  | mPitch == 0 = (take 3 deleAll,[deleAll]) -- no one in this guess is correct.
  | mPitch == 1 = (take 3 (modifGS1!!0),modifGS1) -- Three possible gamestate.
  | mPitch == 2 = (take 3 (modifGS2!!0),modifGS2) -- Three possible gamestate.
  where
    deleAll = deletesub guess allPitches
    modifGS1_0 = guess!!0:deleAll
    modifGS1_1 = guess!!1:deleAll
    modifGS1_2 = guess!!2:deleAll
    modifGS1 = [modifGS1_0,modifGS1_1,modifGS1_2]
    modifGS2_0 = take 2 guess++deleAll
    modifGS2_1 = drop 1 guess++deleAll
    modifGS2_2 = (head guess:drop 2 guess)++deleAll
    modifGS2 = [modifGS2_0,modifGS2_1,modifGS2_2]
-}
