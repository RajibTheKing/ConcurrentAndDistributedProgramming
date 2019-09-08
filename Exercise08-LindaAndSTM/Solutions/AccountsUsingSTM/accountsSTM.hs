
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

--collectedLimitedTransfer [a,b,c] d 120

collectedLimitedTransfer :: [Account] -> Account -> Int -> IO Bool
collectedLimitedTransfer [] _ am | am > 0 = return False
                                 | otherwise = return True
collectedLimitedTransfer (x:xs) to am = do
    bal <- takeMVar x
    if bal >= am then do
        putMVar x (bal - am)
        curBal <- takeMVar to
        putMVar to (curBal + am)
        return True
    else do
        ret <- collectedLimitedTransfer xs to (am - bal)
        if ret == False then do
            putMVar x bal
            return False
        else do
            putMVar x 0
            curBal <- takeMVar to
            putMVar to (curBal + bal)
            return True
        
checkAllAccount :: [Account] -> IO ()
checkAllAccount [] = return ()
checkAllAccount (x:xs) = do
    bal <- takeMVar x
    putStrLn (show bal)
    putMVar x bal
    checkAllAccount xs
    
main :: IO ()
main = do
  acc1 <- newAccount 50
  acc2 <- newAccount 100
  acc3 <- newAccount 35
  acc4 <- newAccount 0

  let accList = [acc1, acc2, acc3]
  collectedLimitedTransfer accList acc4 120
  checkAllAccount accList
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

