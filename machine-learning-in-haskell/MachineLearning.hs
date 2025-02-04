module MachineLearning where

import Control.Monad (join, replicateM)
import Debug.Trace (trace)
import System.Random (Random, StdGen, mkStdGen, random)

sigmoid :: (Floating a) => a -> a
sigmoid x = 1 / (1 + exp (-x))

randomList :: (Random a) => Int -> StdGen -> ([a], StdGen)
randomList n gen = go n gen []
  where
    go 0 g acc = (reverse acc, g)
    go m g acc =
      let (value, newGen) = random g :: (Random a) => (a, StdGen)
       in go (m - 1) newGen (value : acc)

-- definition of a neuron

data Neuron = Neuron
  { bias :: Float,
    activate :: Float -> Output,
    weights :: [Weight]
  }

instance Show Neuron where
  show neuron = "w = " ++ show (weights neuron) ++ ", b = " ++ show (bias neuron)

type Weight = Float

type Output = Float

type Input = [Float]

type TrainingData = [(Input, Output)]

run :: Neuron -> Input -> Output
run (Neuron {activate, weights, bias}) input = activate $ bias + sum (zipWith (*) weights input)

modifyWeights :: [Weight] -> Neuron -> Neuron
modifyWeights dw neuron = neuron {weights = zipWith (+) dw (weights neuron)}

modifyBias :: Weight -> Neuron -> Neuron
modifyBias db neuron = neuron {bias = db + bias neuron}

initializeNeuron :: Int -> Int -> Neuron
initializeNeuron seed nWeights =
  let gen = mkStdGen seed
      (weights, gen') = randomList nWeights gen
      (bias, gen'') = random gen'
   in Neuron {bias = bias, activate = id, weights = weights}

-- prerequisites for gradient descent

epsilon :: (Fractional a) => a
epsilon = 1e-3

meanSquaredError :: (Floating a) => [a] -> [a] -> a
meanSquaredError xs ys = mean $ zipWith (\x y -> (x - y) ** 2) xs ys
  where
    mean xs = sum xs / fromIntegral (length xs)

cost :: Neuron -> TrainingData -> Float
cost neuron trainingData =
  let actual = map (run neuron . fst) trainingData
      expected = map snd trainingData
   in meanSquaredError actual expected

oneHotVectors :: Int -> [[Weight]]
oneHotVectors n = [oneHot i | i <- [0 .. n - 1]]
  where
    oneHot index = [if j == index then epsilon else 0 | j <- [0 .. n - 1]]

differential :: Neuron -> TrainingData -> Neuron
differential neuron trainingData =
  let c = cost neuron trainingData
      weightUpdates = oneHotVectors $ length $ weights neuron
      dws =
        map
          ( \weightUpdate ->
              (cost (modifyWeights weightUpdate neuron) trainingData - c) / epsilon
          )
          weightUpdates
      db = (cost (modifyBias epsilon neuron) trainingData - c) / epsilon
   in trace (show neuron ++ ", cost = " ++ show c) $
        neuron
          { weights = dws,
            bias = db
          }

learn :: Float -> Neuron -> (Neuron -> Neuron)
learn learningRate differential =
  modifyBias (-learningRate * bias differential)
    . modifyWeights (map (\dw -> -learningRate * dw) (weights differential))

epoch :: TrainingData -> Neuron -> Neuron
epoch trainingData neuron =
  let neuron' = learn learningRate (differential neuron trainingData) neuron
   in neuron'
  where
    learningRate = 1e-3

-- concrete example

trainingDataAdd, trainingDataDouble, trainingDataOr, trainingDataAnd :: TrainingData
trainingDataDouble = [([x], x * 2) | x <- [0 .. 4]]
trainingDataAdd = [([x, y], x + y) | x <- [1 .. 10], y <- [1 .. 10]]
trainingDataOr = [([0, 0], 0), ([1, 0], 1), ([0, 1], 1), ([1, 1], 1)]
trainingDataAnd = [([0, 0], 0), ([1, 0], 0), ([0, 1], 0), ([1, 1], 1)]

trainDouble n =
  let neuron = initializeNeuron 69 1
   in iterate (epoch trainingDataDouble) neuron !! n

trainAdd n =
  let neuron = initializeNeuron 69 2
   in iterate (epoch trainingDataAdd) neuron !! n

trainOr n =
  let neuron = (initializeNeuron 69 2) {activate = sigmoid}
   in iterate (epoch trainingDataOr) neuron !! n

trainAnd n =
  let neuron = (initializeNeuron 69 2) {activate = sigmoid}
   in iterate (epoch trainingDataAnd) neuron !! n

evaluateNeuron :: Neuron -> TrainingData -> [(Input, Output)]
evaluateNeuron neuron = map (\(x, _) -> (x, run neuron x))
