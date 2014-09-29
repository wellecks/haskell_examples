{- StateRandomEx.hs

Sean Welleck

An example of using the StateT monad transformer and the RVar
monad to create a 'RandomModel' with state and random variable capabilities.
-}

module StateTRandomEx where

import Control.Monad.State
import Data.Random

data Model = Model {
	vector :: [Double],
	count  :: Int
} deriving (Show)


type RandomModel a = StateT Model RVar a

initial :: Model
initial = Model { vector = [0.0,0.0,0.0], count = 0 }

makeRandomModel :: Int -> RandomModel Model
makeRandomModel n = do
	m <- get
	v <- randomListM n
	return Model { vector = v, count = count m + 1 }

-- read the type as "A RandomModel computation that returns a Model"
reverseModel :: RandomModel Model
reverseModel = do
	m <- get
	return Model { vector = reverse (vector m), count = count m + 1 }

makeAndReverse :: Int -> RandomModel Model
makeAndReverse n = do
	m <- makeRandomModel n
	put m
	m2 <- reverseModel
	put m2
	m3 <- reverseModel
	put m3
	reverseModel

randomGamma :: RVar Double
randomGamma = gamma 100.0 (1.0/100.0)

-- Note that since RVar is the inner monad, we lift the RVar computation into
-- the StateT monad.
randomListM :: Int -> RandomModel [Double]
randomListM n = lift $ replicateM n $ sample randomGamma

main :: IO ()
main = do 
	m <- runRVar (evalStateT (makeAndReverse 10) initial) StdRandom
	print m
