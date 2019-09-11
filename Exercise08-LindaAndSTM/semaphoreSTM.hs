import Control.Concurrent.STM

type Semaphore = TVar (Int, Int)

newSem :: Int -> STM Semaphore
newSem n = newTVar (n, n) 


p :: Semaphore -> STM Bool
p sem = do
    (cur, mx) <- readTVar sem
    case cur of 
        0 -> retry
        x -> do 
            writeTVar sem (cur - 1, mx)
            return True

v :: Semaphore -> STM ()
v sem = do
    (cur,mx) <- readTVar sem
    case (compare cur mx) of 
        EQ -> retry
        _  -> writeTVar sem (cur + 1, mx) >> return ()

l :: Semaphore -> STM Int
l sem  = do
    (cur, mx) <- readTVar sem
    return cur



        