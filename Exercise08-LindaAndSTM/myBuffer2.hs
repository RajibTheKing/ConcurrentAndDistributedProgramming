import Control.Concurrent.STM

type Buf2 a = TVar [a]



newBuf2 :: STM (Buf2 a)
newBuf2 = newTVar []


putBuf2 :: a -> Buf2 a -> STM ()
putBuf2 v buf = do
    xs <- readTVar buf
    if length xs >= 2 then
        retry
    else do
        writeTVar buf (xs ++ [v])
        return ()

takeBuf2 :: Buf2 a -> STM a
takeBuf2 buf = do
    myData <- readTVar buf
    case myData of
        [] -> retry
        (x:xs) -> do
            writeTVar buf xs
            return x

