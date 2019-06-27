module Main where

import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (replicateM_, forever, void)
import Lib
import Say

makeCounter :: IO (IO Int)
makeCounter = do
  var <- newTVarIO 1
  return $ atomically $ stateTVar var (\s -> (s, s + 1))

mainCounter :: IO ()
mainCounter = do
  counter <- makeCounter
  replicateM_ 10 $ counter >>= print

mainPayAliceFromBob :: IO ()
mainPayAliceFromBob = do
  aliceAccount <- newTVarIO 0
  bobAccount <- newTVarIO 0
  _ <- forkIO $ payAlice aliceAccount
  atomically $ do
    currentAliceBalance <- readTVar aliceAccount
    check (currentAliceBalance >= 20)
    writeTVar aliceAccount (currentAliceBalance - 20)
    currentBobBalance <- readTVar bobAccount
    writeTVar bobAccount (currentBobBalance + 20)
  finalAliceBalance <- readTVarIO aliceAccount
  finalBobBalance <- readTVarIO bobAccount
  sayString $ "Final Alice Balance :" ++ show finalAliceBalance
  sayString $ "Final Bob Balance :" ++ show finalBobBalance

payAlice :: TVar Int -> IO ()
payAlice aliceAccountBalance =
  forever $ do
    threadDelay 1000000
    atomically $ modifyTVar aliceAccountBalance (5 +)
    sayString "Paid Alice"

aliceCharlieBob :: IO ()
aliceCharlieBob = do
  aliceAccount <- newTVarIO 0
  bobAccount <- newTVarIO 0
  charlieAccount <- newTVarIO 0
  payThread aliceAccount (Interval 1000000) (Amount 5)
  payThread bobAccount (Interval 1500000) (Amount 8)
  atomically $
    transfer (Amount 20) bobAccount charlieAccount <|>
    transfer (Amount 20) aliceAccount charlieAccount
  finalAlice <- readTVarIO aliceAccount
  finalBob <- readTVarIO bobAccount
  finalCharlie <- readTVarIO charlieAccount
  sayString $ "Final Alice : " ++ show finalAlice
  sayString $ "Final Bob : " ++ show finalBob
  sayString $ "Final Charlie : " ++ show finalCharlie

newtype Interval =
  Interval Int

newtype Amount =
  Amount Int

payThread :: TVar Int -> Interval -> Amount -> IO ()
payThread account (Interval a) (Amount b) =
  void $
  forkIO $
  forever $ do
    threadDelay a
    atomically $ modifyTVar account (b +)

{-
 This function will not work since it wait until the transfer happens from one account and later checks for the next one.
-}
transferAtomically :: Amount -> TVar Int -> TVar Int -> IO ()
transferAtomically amount fromAccount toAccount = atomically $ transfer amount fromAccount toAccount

transfer :: Amount -> TVar Int -> TVar Int -> STM ()
transfer (Amount a) fromAccount toAccount = do
  fromAccountBalance <- readTVar fromAccount
  check (fromAccountBalance >= a)
  writeTVar fromAccount (fromAccountBalance - a)
  modifyTVar toAccount (a +)

main :: IO ()
main = putStrLn "Main"
