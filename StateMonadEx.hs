{- StateMonadEx.hs

Sean Welleck

Simple state monad examples.
-}

module LDA where

import Control.Monad.State

type Stack = [Int]

-- declare a function and pass it into the state constructor
-- to define a State Stack Int
pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  _ <- pop
  pop

pushAndGetFirst :: Int -> State Stack Int
pushAndGetFirst x = do
	push x
	(y:_) <- get
	return y

stackAlternative :: State Stack ()
stackAlternative = do
	curr <- get
	when (length curr < 5) $ put $ curr ++ [5, 55, 555]

-- run in GHCI with:
-- 	runState stackManip [1,2,3,4]
-- 
-- 	runState (pushAndGetFirst 10) [1,2,3,4]
--
-- 	runState stackAlternative [1,2,3,4]
--
-- 	runState stackAlternative [1,2,3,4,5]

main :: IO ()
main = print $ runState (pushAndGetFirst 10) [1,2,3,4]