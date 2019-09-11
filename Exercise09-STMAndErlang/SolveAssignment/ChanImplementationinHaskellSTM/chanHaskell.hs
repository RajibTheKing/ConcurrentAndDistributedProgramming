{-# LANGUAGE GADTs #-}
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



data ChanEntry a = ChanEntry (MVar (a,ChanEntry a))

type Chan a = ((MVar (ChanEntry a)), (MVar (ChanEntry a)))


newChan :: STM (Chan a)
newChan = do
    hole <- newEmptyMVarSTM
    read <- newEmptyMVarSTM
    write <- newEmptyMVarSTM
    putMVarSTM read (ChanEntry hole)
    putMVarSTM write (ChanEntry hole)
    return (read, write)


readChan :: Chan a -> STM a
readChan (read, write) = do
    ChanEntry reader_end <- takeMVarSTM read
    (v, hole) <- takeMVarSTM reader_end
    putMVarSTM read  hole
    return v

writeChan :: a -> Chan a -> STM (Chan a)
writeChan v (read, write) = do
    ChanEntry hole <- takeMVarSTM write
    new_hole <- newEmptyMVarSTM
    putMVarSTM hole (v, ChanEntry new_hole)
    putMVarSTM write (ChanEntry new_hole)
    return (read , write)

isEmpty :: Chan a -> STM Bool
isEmpty (read, write) = do
    ChanEntry readEnd <- takeMVarSTM read
    ChanEntry writeEnd <- takeMVarSTM write
    putMVarSTM read (ChanEntry readEnd)
    putMVarSTM write (ChanEntry writeEnd)
    if readEnd == writeEnd then 
        return True
    else
        return False
