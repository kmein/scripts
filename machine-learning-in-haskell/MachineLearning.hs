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

data NeuralNetwork = NeuralNetwork
  { layers :: [[Neuron]],
    finalLayer :: Neuron
  }

runNetwork :: NeuralNetwork -> Input -> Output
runNetwork NeuralNetwork {layers, finalLayer} input =
  run finalLayer $ runLayers layers input
  where
    runLayers [] input = input
    runLayers (l : ls) input = runLayers ls $ map (`run` input) l

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

initializeNeuron :: StdGen -> Int -> (Neuron, StdGen)
initializeNeuron gen nWeights =
  let (weights, gen') = randomList nWeights gen
      (bias, gen'') = random gen'
   in ( Neuron
          { bias = bias,
            activate = id,
            weights = weights
          },
        gen''
      )

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

costNetwork :: NeuralNetwork -> TrainingData -> Float
costNetwork network trainingData =
  let actual = map (runNetwork network . fst) trainingData
      expected = map snd trainingData
   in meanSquaredError actual expected

oneHotVectors :: Int -> [[Weight]]
oneHotVectors n = [oneHot i | i <- [0 .. n - 1]]
  where
    oneHot index = [if j == index then epsilon else 0 | j <- [0 .. n - 1]]

differential :: (Neuron -> TrainingData -> Float) -> Neuron -> TrainingData -> Neuron
differential costFunc neuron trainingData =
  let c = costFunc neuron trainingData
      weightUpdates = oneHotVectors $ length $ weights neuron
      dws =
        map
          ( \weightUpdate ->
              (costFunc (modifyWeights weightUpdate neuron) trainingData - c) / epsilon
          )
          weightUpdates
      db = (costFunc (modifyBias epsilon neuron) trainingData - c) / epsilon
   in neuron {weights = dws, bias = db}

differentialNetwork :: NeuralNetwork -> TrainingData -> NeuralNetwork
differentialNetwork network trainingData =
  let costFunc = costNetwork
      updatedLayers = map (map (\neuron -> differential costFunc neuron trainingData)) (layers network)
      updatedFinalLayer = differential costFunc (finalLayer network) trainingData
   in NeuralNetwork {layers = updatedLayers, finalLayer = updatedFinalLayer}

learn :: Float -> Neuron -> (Neuron -> Neuron)
learn learningRate differential =
  modifyBias (-learningRate * bias differential)
    . modifyWeights (map (\dw -> -learningRate * dw) (weights differential))

learnNetwork :: Float -> NeuralNetwork -> (NeuralNetwork -> NeuralNetwork)
learnNetwork learningRate differential network =
  NeuralNetwork
    { finalLayer = learn learningRate (finalLayer differential) (finalLayer network),
      layers = zipWith (zipWith (learn learningRate)) (layers differential) (layers network)
    }

epoch :: TrainingData -> Neuron -> Neuron
epoch trainingData neuron =
  let neuron' = learn learningRate (differential cost neuron trainingData) neuron
   in neuron'
  where
    learningRate = 1e-3

-- concrete example

trainingDataAdd, trainingDataDouble, trainingDataOr, trainingDataAnd, trainingDataNand, trainingDataNot, trainingDataXor :: TrainingData
trainingDataDouble = [([x], x * 2) | x <- [0 .. 4]]
trainingDataAdd = [([x, y], x + y) | x <- [1 .. 10], y <- [1 .. 10]]
trainingDataOr = [([0, 0], 0), ([1, 0], 1), ([0, 1], 1), ([1, 1], 1)]
trainingDataAnd = [([0, 0], 0), ([1, 0], 0), ([0, 1], 0), ([1, 1], 1)]
trainingDataNand = [([0, 0], 1), ([1, 0], 1), ([0, 1], 1), ([1, 1], 0)]
trainingDataXor = [([0, 0], 0), ([1, 0], 1), ([0, 1], 1), ([1, 1], 0)]
trainingDataNot = [([0], 1), ([1], 0)]

gen = mkStdGen 69

trainDouble n =
  let (neuron, _) = initializeNeuron gen 1
   in iterate (epoch trainingDataDouble) neuron !! n

trainAdd n =
  let (neuron, _) = initializeNeuron gen 2
   in iterate (epoch trainingDataAdd) neuron !! n

trainOr n =
  let (neuron, _) = initializeNeuron gen 2
      neuron' = neuron {activate = sigmoid}
   in iterate (epoch trainingDataOr) neuron' !! n

trainAnd n =
  let (neuron, _) = initializeNeuron gen 2
      neuron' = neuron {activate = sigmoid}
   in iterate (epoch trainingDataAnd) neuron' !! n

trainXor n =
  let (neuron, _) = initializeNeuron gen 2
      neuron' = neuron {activate = sigmoid}
   in iterate (epoch trainingDataXor) neuron' !! n

trainNand n =
  let (neuron, _) = initializeNeuron gen 2
      neuron' = neuron {activate = sigmoid}
   in iterate (epoch trainingDataNand) neuron' !! n

trainNot n =
  let (neuron, _) = initializeNeuron gen 2
      neuron' = neuron {activate = sigmoid}
   in iterate (epoch trainingDataNot) neuron' !! n

evaluateNeuron :: Neuron -> TrainingData -> [(Input, Output)]
evaluateNeuron neuron = map (\(x, _) -> (x, run neuron x))

evaluateNetwork :: NeuralNetwork -> TrainingData -> [(Input, Output)]
evaluateNetwork network = map (\(x, _) -> (x, runNetwork network x))
