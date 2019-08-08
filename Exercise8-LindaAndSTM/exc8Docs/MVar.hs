import Control.Concurrent.STM

type MVar a = TVar (Maybe a)

newEmptyMVarSTM :: STM (MVar a)
newEmptyMVarSTM = newTVar Nothing

takeMVarSTM :: MVar a -> STM a
takeMVarSTM mvar = do
  mv <- readTVar
  case mv of
    Nothing -> retry
    Just v  -> do
        writeTVar Nothing
        return v

putMVarSTM :: MVar a -> a -> STM ()
putMVarSTM mvar v = do
  mv <- readTVar
  case mv of
    Nothing -> writeTVar mvar (Just v)
    Just v  -> writeTVar mvar (Just v) >> retry

tryPutMVar :: MVar a -> a -> IO Bool
tryPutMVar mvar v =
       putMVar mvar v >>
       return True
  `orElse`       
       return False

