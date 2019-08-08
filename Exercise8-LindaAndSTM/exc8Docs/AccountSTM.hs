import Control.Concurrent.STM
import Control.Concurrent

type Account = TVar Int

newAccount :: Int -> STM Account
newAccount am = newTVar am 

getBalance :: Account -> STM Int
getBalance acc = readTVar acc

withdraw :: Account -> Int -> STM Bool
withdraw acc am = do 
  bal <- getBalance acc
  if bal >= am then do
    writeTVar acc (bal-am)
    return True
  else do
    writeTVar acc bal
    return False

deposit :: Account -> Int -> STM ()
deposit acc am = do
  bal <- getBalance acc
  writeTVar acc (bal + am)

transfer :: Account -> Account -> Int -> STM Bool
transfer from to am = do
  succ <- withdraw from am
  if succ then do
    deposit to am
    return True
  else
    return False

main :: IO ()
main = do
  acc1 <- atomically (newAccount 1000)
  acc2 <- atomically (newAccount 1000)
  --forkIO (checkAmount acc1 acc2 2000)
  --forkIO (testTransfer acc1 acc2 500)
  testTransfer acc2 acc1 500
  putStrLn("Transaction Completed\nPress any key...")
  getLine
  return ()

testTransfer :: Account -> Account -> Int -> IO ()
testTransfer _    _  0 = return ()
testTransfer from to n = do
  succ <- atomically (transfer from to 1)
  if not succ then do
    putStr "F"
  else
    return ()
  testTransfer from to (n-1)

checkAmount :: Account -> Account -> Int -> IO ()
checkAmount acc1 acc2 am = do
  okay <- atomically $ do
    bal1 <- getBalance acc1
    bal2 <- getBalance acc2
    return (bal1 + bal2 == am)
  if not okay then
    putStrLn "LOST MONEY"
  else
    return ()
  checkAmount acc1 acc2 am

