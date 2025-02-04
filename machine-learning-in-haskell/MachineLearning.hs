module MachineLearning where

import Control.Monad (replicateM)
import System.Random (Random, StdGen, mkStdGen, random)

randomList :: (Random a) => Int -> StdGen -> ([a], StdGen)
randomList n gen = go n gen []
  where
    go 0 g acc = (reverse acc, g)
    go m g acc =
      let (value, newGen) = random g :: (Random a) => (a, StdGen)
       in go (m - 1) newGen (value : acc)

-- definition of a neuron

data Neuron = Neuron
  { bias :: Double,
    activate :: Double -> Output,
    weights :: [Weight]
  }

instance Show Neuron where
  show neuron = show (weights neuron, bias neuron)

type Weight = Double

type Output = Double

type Input = [Double]

type TrainingData = [(Input, Output)]

run :: Neuron -> Input -> Output
run (Neuron {activate, weights, bias}) input = activate $ bias + sum (zipWith (*) weights input)

modifyWeights :: [Weight -> Weight] -> Neuron -> Neuron
modifyWeights fs neuron = neuron {weights = zipWith id fs (weights neuron)}

modifyBias :: (Weight -> Weight) -> Neuron -> Neuron
modifyBias f neuron = neuron {bias = f (bias neuron)}

initializeNeuron :: Int -> Int -> Neuron
initializeNeuron seed nWeights =
  let gen = mkStdGen seed
      (weights, gen') = randomList nWeights gen
      (bias, gen'') = random gen'
   in Neuron {bias = bias * 5, activate = id, weights = map (* 10) weights}

-- prerequisites for gradient descent

epsilon :: (Fractional a) => a
epsilon = 1e-3

meanSquaredError :: (Floating a) => [a] -> [a] -> a
meanSquaredError xs ys = mean $ zipWith (\x y -> (x - y) ** 2) xs ys
  where
    mean xs = sum xs / fromIntegral (length xs)

cost :: Neuron -> TrainingData -> Double
cost neuron trainingData =
  let actual = map (run neuron . fst) trainingData
      expected = map snd trainingData
   in meanSquaredError actual expected

differential :: Neuron -> TrainingData -> Neuron
differential neuron trainingData =
  let c = cost neuron trainingData
      dw = (cost (modifyWeights (repeat (+ epsilon)) neuron) trainingData - c) / epsilon
      db = (cost (modifyBias (+ epsilon) neuron) trainingData - c) / epsilon
   in neuron
        { weights = repeat dw,
          bias = db
        }

learn :: Double -> Neuron -> (Neuron -> Neuron)
learn learningRate differential =
  modifyBias (\b -> b - learningRate * bias differential)
    . modifyWeights (map (\dw w -> w - learningRate * dw) (weights differential))

epoch :: TrainingData -> Neuron -> Neuron
epoch trainingData neuron =
  let neuron' = learn learningRate (differential neuron trainingData) neuron
   in neuron'
  where
    learningRate = 1e-3

-- concrete example

trainingData :: TrainingData
trainingData =
  [ ([0], 0),
    ([1], 2),
    ([2], 4),
    ([3], 6),
    ([4], 8)
  ]

main = do
  let neuron = initializeNeuron 69 1
  print neuron
  let c = cost neuron trainingData
  print c

  let neuron' = iterate (epoch trainingData) neuron !! 10000

  print neuron'
  print $ cost neuron' trainingData
