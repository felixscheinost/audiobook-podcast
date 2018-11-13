{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}

module Conversion.Types where

import           Control.Concurrent.STM.TVar (TVar)

-- | State of the step
data StepState a
    = Waiting
    | Running (Maybe a)
    | Finished

-- | Step action that takes i as input, produces o as output and has progress information p
type StepAction i o p =  ((Maybe p -> Maybe p) -> IO ()) -> i -> IO o

class StepProgress a where
    asPercentDouble :: a -> Double

instance StepProgress (Integer, Integer) where
    asPercentDouble (delta, total) = 100 * fromIntegral delta / fromIntegral total

instance StepProgress Double where
    asPercentDouble = id

-- | Holds the data for one conversion step
data StepData i o p d = StepData
    { action      :: StepAction i o p
    , description :: i -> Maybe d
    , state       :: TVar (StepState p)
    }

-- | Conversion step that takes i, produces o, stores its progress as p and description as d
data Step i o p d
    = Single (StepData i o p d)
    | forall u p'. Nested (StepData i u p d) (Step u o p' d)
