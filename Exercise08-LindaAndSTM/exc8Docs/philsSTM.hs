import Control.Concurrent
import Control.Concurrent.STM

type Stick = TVar Bool

main :: IO ()
main = do
  s1 <- atomically newStick
  s2 <- atomically newStick
  s3 <- atomically newStick
  s4 <- atomically newStick
  s5 <- atomically newStick
  forkIO (phil s1 s2 1)
  forkIO (phil s2 s3 2)
  forkIO (phil s3 s4 3)
  forkIO (phil s4 s5 4)
  getLine
  phil s5 s1 5
  
newStick :: STM Stick
newStick = do
  newTVar True

takeStick :: Stick -> STM ()
takeStick stick = do
  available <- readTVar stick
  if available then
    writeTVar stick False
  else
    retry

putStick stick = do
  writeTVar stick True

phil :: Stick -> Stick -> Int -> IO ()
phil sl sr nr = do
  putStrLn (show nr ++ " is thinking")
  atomically $ do takeStick sl
                  takeStick sr
  --yield
  --atomically $ takeStick sr
  putStrLn (show nr ++ " is eating")
  atomically $ putStick sl
  atomically $ putStick sr
  phil sl sr nr

