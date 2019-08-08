import Control.Concurrent
import Control.Concurrent.MVar

type Account = MVar Int

newAccount :: Int -> IO Account
newAccount am = newMVar am 

getBalance :: Account -> IO Int
getBalance acc = readMVar acc

withdraw :: Account -> Int -> IO Bool
withdraw acc am = do 
  bal <- takeMVar acc
  if bal >= am then do
    putMVar acc (bal-am)
    return True
  else do
    putMVar acc bal
    return False

deposit :: Account -> Int -> IO ()
deposit acc am = do
  bal <- takeMVar acc
  putMVar acc (bal+am)

transfer1 :: Account -> Account -> Int -> IO Bool
transfer1 from to am = do
  succ <- withdraw from am
  if succ then do
    deposit to am
    return True
  else
    return False

transferDeadlock :: Account -> Account -> Int -> IO Bool
transferDeadlock from to am = do
  bal <- takeMVar from
  yield
  if bal >= am then do
    deposit to am
    putMVar from (bal-am)
    return True
  else do
    putMVar from bal
    return False

transferPutBack :: Account -> Account -> Int -> IO Bool
transferPutBack from to am = do
  bal <- takeMVar from
  if bal >= am then do
    empty <- isEmptyMVar to
    if empty then do
      putMVar from bal
      transferPutBack from to am
    else do
      deposit to am
      putMVar from (bal-am)
      return True
  else do
    putMVar from bal
    return False

{- does not work, because there is no instance of Ord for MVars
transferOrder :: Account -> Account -> Int -> IO Bool
transferOrder from to am =
  if from < to then do
    balFrom <- takeMVar from
    if balFrom >= am then do
      balTo <- takeMVar to
      putMVar from (balFrom - am)
      putMVar to (balTo + am)
      return True
    else do
      putMVar from balFrom
      return False
  else do
    balTo <- takeMVar to
    balFrom <- takeMVar from
    if balFrom >= am then do
      putMVar from (balFrom - am)
      putMVar to (balTo + am)
      return True
    else do
      putMVar from balFrom
      putMVar to balTo
      return False
-}


main :: IO ()
main = do
  acc1 <- newAccount 1000
  acc2 <- newAccount 1000
  --forkIO (checkAmount acc1 acc2 2000)
  forkIO (testTransfer acc1 acc2 500 1)
  testTransfer acc2 acc1 5000 2
  getLine
  return ()

testTransfer :: Account -> Account -> Int -> Int -> IO ()
testTransfer _    _  0 _   = return ()
testTransfer from to n num = do
  succ <- transferDeadlock from to 10
  if not succ then do
    putStr ("F"++show(num))
  else
    return ()
  testTransfer from to (n-1) num

checkAmount :: Account -> Account -> Int -> IO ()
checkAmount acc1 acc2 am = do
  bal1 <- getBalance acc1
  bal2 <- getBalance acc2
  if (bal1 + bal2 /= am) then
    putStrLn "LOST MONEY"
  else
    return ()
  checkAmount acc1 acc2 am

