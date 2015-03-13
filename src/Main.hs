{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Applicative ((<$>), (<*>), Applicative)
import Control.Monad (foldM, replicateM)
import Control.Monad.Random
--import Control.Monad.Random.Class (MonadRandom)
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.State.Lazy
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (unfoldr)
import Data.Monoid ((<>))

randrand :: (MonadRandom m) => m Int
randrand = getRandomR (0, 4)

findDistribution :: forall a g . (RandomGen g, Eq a, Hashable a)
                              => Rand g a
                              -> Rand g (HashMap a Int)
findDistribution randFunc =
    foldM (\hash _ -> fmap (insertItem hash) randFunc)
          HashMap.empty
          ([1..100000] :: [Integer])
  where
    insertItem :: HashMap a Int -> a -> HashMap a Int
    insertItem hashmap key = HashMap.insertWith (+) key 1 hashmap

findDistribution' :: forall a g . (RandomGen g, Eq a, Hashable a)
                              => Rand g a
                              -> Rand g (HashMap a Int)
findDistribution' randFunc = go 0 HashMap.empty
  where
    go :: Int -> HashMap a Int -> Rand g (HashMap a Int)
    go 100000 hashmap = return hashmap
    go n hashmap = do
      key <- randFunc
      go (n+1) $ HashMap.insertWith (+) key 1 hashmap


randrandrand :: (RandomGen g) => State g Int
randrandrand = do
    oldGen <- get
    let (randVal, newGen) = randomR (0, 4) oldGen
    put newGen
    return randVal

findDistribution'' ::
    forall a g . (RandomGen g, Eq a, Hashable a) => State g a -> State g (HashMap a Int)
findDistribution'' randFunc = go 0 HashMap.empty
  where
    go :: Int -> HashMap a Int -> State g (HashMap a Int)
    go 100000 hashmap = return hashmap
    go n hashmap = do
      randVal <- randFunc
      go (n+1) $ HashMap.insertWith (+) randVal 1 hashmap

totalRolls :: Int
totalRolls = 10000000

ones :: [Integer]
ones = repeat 1

genRandom :: (RandomGen g) => g -> (Integer, g)
genRandom = randomR (0,4)

listOfRandomRolls :: (RandomGen g) => g -> [Integer]
listOfRandomRolls randGen =
    let allRolls = unfoldr (Just . genRandom) randGen
    in take totalRolls allRolls

createProbDistHashMap :: (RandomGen g) => g ->  HashMap Integer Integer
createProbDistHashMap randGen =
    -- let replicatedRandomFunc = replicateM totalRolls randrandrand
    --     listOfRandomRolls = evalState replicatedRandomFunc randGen
    let zippedRolls = zip (listOfRandomRolls randGen) ones
        hashmap = HashMap.fromListWith (+) zippedRolls
    in hashmap

genRandom' :: (RandomGen g, MonadReader g m, Functor m) => m (Integer, g)
genRandom' = do
    oldGen <- ask
    let (num :: Integer, newGen) = randomR (1, 25) oldGen
    case num of
        1 -> return (1, newGen)
        2 -> return (2, newGen)
        3 -> return (3, newGen)
        4 -> return (4, newGen)
        5 -> return (5, newGen)
        6 -> return (6, newGen)
        7 -> return (7, newGen)
        8 -> return (1, newGen)
        9 -> return (2, newGen)
        10 -> return (3, newGen)
        11 -> return (4, newGen)
        12 -> return (5, newGen)
        13 -> return (6, newGen)
        14 -> return (7, newGen)
        15 -> return (1, newGen)
        16 -> return (2, newGen)
        17 -> return (3, newGen)
        18 -> return (4, newGen)
        19 -> return (5, newGen)
        20 -> return (6, newGen)
        21 -> return (7, newGen)
        _ -> return $ runReader genRandom' newGen

listOfRandomRolls' :: (RandomGen g, MonadReader g m, Functor m) => m [Integer]
listOfRandomRolls' = do
    allRolls <- unfoldr (Just . genRandom') <$> ask
    return $ take totalRolls allRolls

createProbDistHashMap' :: (RandomGen g, MonadReader g m, Functor m, Applicative m)
                       => m (HashMap Integer Integer)
createProbDistHashMap' = do
    zippedRolls <- zip <$> (listOfRandomRolls' <$> ask) <*> return ones
    let hashmap = HashMap.fromListWith (+) zippedRolls
    return hashmap

printDistribution :: HashMap Integer Integer -> IO ()
printDistribution hashmap =
    forM_ (HashMap.toList hashmap) $ \(key, value) -> do
      putStrLn $ show key
              <> ": "
              <> show ((fromInteger value * 100) / (fromInteger $ toInteger totalRolls))
              <> "% ("
              <> show value
              <> ")"

main :: IO ()
main = do
    -- print =<< (evalRandIO $ findDistribution' randrand)
    randGen <- getStdGen
    -- hashmap <- createProbDistHashMap randGen
    let hashmap = runReader createProbDistHashMap' randGen
    printDistribution hashmap
