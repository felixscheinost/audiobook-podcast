{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}

module Conversion.Step where

import           Control.Concurrent          (threadDelay)
-- import qualified Algorithms.NaturalSort      as NaturalSort
import qualified Control.Concurrent.STM      as STM
import           Control.Concurrent.STM.TVar
import           Control.Monad               (liftM2)
import           Control.Monad.Trans.Except  (ExceptT, throwE)
import qualified Data.List                   as List
import qualified Zip


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

-- | Run two steps after each other
andThen :: Step i u p d -> Step u o p' d -> Step i o p d
andThen (Single c) next   = Nested c next
andThen (Nested c d) next = Nested c (andThen d next)

andThenM :: Monad m => m (Step i u p d) -> m (Step u o p' d) -> m (Step i o p d)
andThenM = liftM2 andThen

-- | Convert an action to a step
createStep :: (i -> Maybe d) -> StepAction i o p -> IO (Step i o p d)
createStep desc action = do
    m <- newTVarIO Waiting
    return $ Single $ StepData
        { action = action
        , description = desc
        , state = m
        }

runSingleStep :: i -> StepData i o p d -> IO o
runSingleStep i sd@StepData{state=tvar, action=action} = do
    STM.atomically $ writeTVar tvar (Running Nothing)
    o <- action updateProgress i
    STM.atomically $ writeTVar tvar Finished
    return o
    where
        updateProgress update = STM.atomically $ do
            status <- readTVar tvar
            case status of
                (Running val) -> writeTVar tvar $ Running $ update val
                _             -> return ()

runStep :: i -> Step i o p d -> IO o
runStep i (Single sd)          = runSingleStep i sd
runStep i (Nested sd nextStep) = runSingleStep i sd >>= flip runStep nextStep

