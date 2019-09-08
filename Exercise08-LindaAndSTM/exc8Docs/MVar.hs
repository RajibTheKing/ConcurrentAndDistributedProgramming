import Control.Concurrent.STM

type MVar a = TVar (Maybe a)

newEmptyMVarSTM :: STM (MVar a)
newEmptyMVarSTM = newTVar Nothing

takeMVarSTM :: MVar a -> STM a
takeMVarSTM mvar = do
  mv <- readTVar mvar
  case mv of
    Nothing -> retry
    Just v  -> do
        writeTVar mvar Nothing
        return v

putMVarSTM :: MVar a -> a -> STM ()
putMVarSTM mv val = do
  v <- readTVar mv
  case v of
      Nothing -> writeTVar mv (Just val)
      Just val -> retry 


tryPutMVar :: MVar a -> a -> STM Bool
tryPutMVar mv val = do 
    putMVarSTM mv val >> return True
    `orElse`
    return False

-- My Own analysis: 
tryTakeMVar :: MVar a -> STM (Maybe a)
tryTakeMVar mv = 
    (takeMVarSTM mv >>= \value -> return (Just value)) 
    `orElse` 
    return Nothing


ourPutMVar :: MVar a -> a -> STM ()
ourPutMVar mvar value = do
  curValue <- readTVar mvar
  case curValue of
    Nothing -> do
                 writeTVar mvar (Just value)
                 return ()
    Just _ -> retry
    