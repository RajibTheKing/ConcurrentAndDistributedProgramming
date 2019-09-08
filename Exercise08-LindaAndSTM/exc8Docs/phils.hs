import Control.Concurrent

type Stick = MVar ()

main :: IO ()
main = do
  s1 <- newStick
  s2 <- newStick
  s3 <- newStick
  s4 <- newStick
  s5 <- newStick
  forkIO (phil s1 s2 1)
  forkIO (phil s2 s3 2)
  forkIO (phil s3 s4 3)
  forkIO (phil s4 s5 4)
  getLine
  phil s5 s1 5
  
newStick :: IO Stick
newStick = do
  newMVar ()

takeStick stick = do
  takeMVar stick

putStick stick = do
  putMVar stick ()

phil :: Stick -> Stick -> Int -> IO ()
phil sl sr nr = do
  putStrLn (show nr ++ " is thinking")
  takeStick sl
  --yield
  takeStick sr
  putStrLn (show nr ++ " is eating")
  putStick sl
  putStick sr
  phil sl sr nr

