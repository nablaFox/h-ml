module ML.Model where

type Loss h z = h -> z -> Double

data Model h z = Model {predict :: h, loss :: z -> Double}

newModel :: h -> Loss h z -> Model h z
newModel h l = Model h (l h)

objective :: Model h z -> [z] -> Double
objective model env = sum $ map (loss model) env

-- supervised learning
type SupervisedModel a b = Model (a -> b) (a, b)

-- (hyper-parameters, loss function, training set) -> (model, learned-parameters, objective)
type SupervisedTrainerWithLoss hp lp a b = hp -> Loss (a -> b) (a, b) -> [(a, b)] -> (SupervisedModel a b, lp, Double)

type SupervisedTrainer hp lp a b = hp -> [(a, b)] -> (SupervisedModel a b, lp, Double)

makePredictions :: SupervisedModel a b -> [a] -> [(a, b)]
makePredictions model = map (\d -> (d, predict model d))

trainedModel :: SupervisedModel a b -> lp -> [(a, b)] -> (SupervisedModel a b, lp, Double)
trainedModel model params trset = (model, params, objective model trset)

se :: (Num b, Real b) => Loss (a -> b) (a, b)
se h (x, y) =
  let diff = h x - y
   in realToFrac (diff * diff)
