{-# LANGUAGE FlexibleContexts #-}

module RandomEx where

import Data.Random
import Control.Monad (replicateM)
import Data.Matrix

randomGamma :: RVar Double
randomGamma = gamma 100.0 (1.0/100.0)

randomMatrix :: (MonadRandom m) => Int -> Int -> m (Matrix Double)
randomMatrix r c = do
	xs  <- replicateM (r*c) $ sample randomGamma
	return $ fromList r c xs

randomList :: (MonadRandom m) => Int -> m [Double]
randomList n = replicateM n $ sample randomGamma

logNormal :: Double -> Double -> RVar Double
logNormal mu sigmaSq = do
    x <- normal mu sigmaSq
    return (exp x)

main :: IO ()
main = do
	mat <- randomMatrix 5 5
	print mat