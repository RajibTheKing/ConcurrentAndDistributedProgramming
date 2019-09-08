
import Control.Concurrent

type ChanIO a = MVar [a]

newChanIO :: IO (ChanIO a)
newChanIO = newMVar []

writeChan :: ChanIO a -> a -> IO ()
writeChan ch v = do
  queue <- takeMVar ch
  putMvar (queue ++ [v])

readChan :: ChanIO a -> IO a
readChan ch v = do
  queue <- takeMVar ch
  case queue of
    (x:xs) -> do 
              putMVar ch xs
              return x
    []     -> error "no suspension possible here"


type Chan a = TVar [a]

newChan :: STM (Chan a)
newChan = newTVar []

writeChan :: Chan a -> a -> STM ()
writeChan ch v = do
  queue <- readTVar ch
  writeTVar (queue ++ [v])

readChan :: Chan a -> STM a
readChan ch v = do
  queue <- readTVar ch
  case queue of
    (x:xs) -> do 
              pwriteTVar ch xs
              return x
    []     -> retry

isEmptyChan :: Chan a -> STM Bool
isEmptyChan ch = do
  queue <- readTVar ch
  return (null queue)

readChan' :: Chan a -> STM a
readChan' ch = do
  empty <- isEmptyChan ch
  if empty then
    retry
  else do
    (x:xs) <- do
      readTVar ch
      pwriteTVar ch xs
      return x

writer :: Chan Int -> Int -> IO ()
writer ch 0 = return ()
writer ch n = do
  atomically $ do
        v <- readChan ch
        writeChan ch (v + 1)
     `orElse`
               do
        writeChan ch 42
  writer ch (n-1)

main = do
  ch <- atomically $ newChan
  forkIO (writer ch 10)
  writer ch 30


