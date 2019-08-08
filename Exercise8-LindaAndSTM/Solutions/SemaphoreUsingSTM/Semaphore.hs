module Semaphore where

    import Control.Concurrent.STM
    
    type Semaphore = TVar Int
    
    newSem :: Int -> IO Semaphore
    newSem n = newTVarIO n
    
    p :: Semaphore -> STM ()
    p sem = do
      n <- readTVar sem
      if n <=0 then do
        retry
      else do
        writeTVar sem (n-1)
    
    v :: Semaphore -> STM ()
    v sem = do
      n <- readTVar sem
      writeTVar sem (n+1)
    
    l :: Semaphore -> STM Int
    l sem = readTVar sem