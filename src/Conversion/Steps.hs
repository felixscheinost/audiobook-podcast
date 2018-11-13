{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}

module Conversion.Steps where

import           Control.Concurrent          (threadDelay)
-- import qualified Algorithms.NaturalSort      as NaturalSort
import qualified Control.Concurrent.STM      as STM
import           Control.Concurrent.STM.TVar
import           Control.Monad               (liftM2)
import           Control.Monad.Trans.Except  (ExceptT, throwE)
import           Conversion.Types
import qualified Data.List                   as List
import qualified Zip

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

