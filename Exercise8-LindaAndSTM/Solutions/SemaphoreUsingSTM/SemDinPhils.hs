module SemDinPhils where

    import Control.Concurrent
    import Control.Concurrent.STM
    import Control.Monad (replicateM)
    
    import Semaphore
    
    main :: IO ()
    main = replicateM 5 (newSem 1) >>= startPhils
    
    startPhils :: [Semaphore] -> IO ()
    startPhils sems = start 1 sems
      where
      start n [s]        = philosopher n s (head sems)
      start n (s1:s2:ss) = do forkIO $ philosopher n s1 s2
                              start (n+1) (s2:ss)
    
    philosopher :: Int -> Semaphore -> Semaphore -> IO ()
    philosopher n left right = do
      putStrLn $ "Philosopher " ++ show n ++ " is thinking."
      atomically $ do
        p left
        p right
      threadDelay 2000
      putStrLn $ "Philosopher " ++ show n ++ " is eating."
      atomically $ v right
      atomically $ v left
      philosopher n left right

    testOther :: IO ()
    testOther = do
      sem <- newSem 2
      ret <- atomically $ do 
                   v sem
                   v sem
                   l sem
      putStrLn $ "Here: " ++ show ret    
      
      return ()
