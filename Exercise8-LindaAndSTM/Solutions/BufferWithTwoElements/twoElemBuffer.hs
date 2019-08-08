import Control.Concurrent.STM

type Buffer2 a = TVar [a]

newBuffer2 :: STM (Buffer2 a)
newBuffer2 = newTVar []

readBuffer2 :: Buffer2 a -> STM a
readBuffer2 buffer = do
  xs <- readTVar buffer
  case xs of
    [] -> retry
    (y:ys) -> do
      writeTVar buffer ys
      return y

writeBuffer2 :: Buffer2 a -> a -> STM ()
writeBuffer2 buffer elem = do
  xs <- readTVar buffer
  case xs of
    (_:_:_) -> retry
    _       -> writeTVar buffer (xs ++ [elem])