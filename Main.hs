{- -*- coding: utf-8 -*- -}
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.List
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, dupChan, newChan, readChan, writeChan)
import Control.Monad
import Network (HostName, PortID(PortNumber), connectTo)
import Network.IRC (Channel, Password, Parameter, ServerName, UserName, RealName, Message(..))
import qualified Network.IRC as I
import qualified Data.ByteString.Char8 as C
import System.IO (BufferMode(NoBuffering, LineBuffering), Handle, hClose, hGetLine, hPutChar, hSetBuffering)
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar
import Control.Exception

import Data
import Config

passMsg :: Password -> Message
passMsg p = Message Nothing "PASS" [p]

sender :: TMVar Handle -> Chan Message -> IO ThreadId
sender hTM chan =
    forkIO $ forever $ do
      msg <- readChan chan
      C.putStrLn $ C.pack "send: " `C.append` (I.encode msg)
      h <- atomically $ readTMVar hTM
      C.hPutStr h $ I.encode msg

reciever :: TMVar Handle -> Chan Message -> IO ThreadId
reciever hTM chan =
    forkIO $ forever $ do
      h <- atomically $ readTMVar hTM
      msg <- C.hGetLine h
      C.putStrLn  $ C.pack "rec: " `C.append` msg
      case I.decode msg of
        Nothing  -> fail ("message parse error" ++ show msg)
        Just msg -> do
          writeChan chan msg

connect :: Connect -> IO ((Chan Message, Chan Message), [ThreadId])
connect c = do
  hTM <- atomically $ newTMVar undefined

  senderChan   <- newChan :: IO (Chan Message)
  recieverChan <- newChan :: IO (Chan Message)
  recieverTh <- forkIO $ do
                  h <- connectTo <$> host <*> port $ c
                  hSetBuffering h LineBuffering
                  atomically $ swapTMVar hTM h
                  writeChan recieverChan undefined
                  forever $ do
                    h <- atomically $ readTMVar hTM
                    msg <- C.hGetLine h
                    C.putStrLn $ C.pack "rec: " `C.append` msg
                    case I.decode msg of
                      Nothing  -> fail ("message parse error" ++ show msg)
                      Just msg -> do
                        writeChan recieverChan msg
  senderTh <- forkIO $ forever $ do
                  msg <- readChan senderChan
                  let encMsg = I.encode msg
                  C.putStrLn $ "send: " <> encMsg
                  h <- atomically $ readTMVar hTM
                  C.hPutStrLn h $ encMsg

  readChan recieverChan
  writeChan senderChan $ passMsg <$> pass $ c
  writeChan senderChan $ I.user  <$> username
                                 <*> hostname
                                 <*> servername
                                 <*> realname $ user c
  writeChan senderChan $ I.nick  <$> nick $ c 
  return ((senderChan, recieverChan), [senderTh, recieverTh])

makeBot :: (Chan Message, Chan Message) -> ((Chan Message, Chan Message) -> IO ()) -> IO ThreadId
makeBot (senderChan, recieverChan) bot = do
  chans' <- (,) <$> dupChan senderChan
                <*> dupChan recieverChan
  forkIO $ forever $ bot chans'
         
pingBot :: (Chan Message, Chan Message) -> IO ()
pingBot (senderChan, recieverChan) = do
  msg <- readChan recieverChan
  when (msg_command msg == "PING") $ do
    writeChan senderChan $ I.pong $ head $ msg_params $ msg 

transBot :: [(C.ByteString, C.ByteString)] -> Chan Message -> (Chan Message, Chan Message) -> IO ()
transBot channnels transToChan (_, recieverChan) = do
  msg <- readChan recieverChan
  when (msg_command msg == "PRIVMSG") $ do
    let fromCh = head $ msg_params msg
    let name = case msg_prefix msg of
                 Just (I.Server x)       -> x
                 Just (I.NickName x _ _) -> x
                 Nothing                 -> "anyone"
    let newmsg = C.concat [name, ": ", (msg_params msg) !! 1]
    case lookup fromCh channnels of
      Nothing   -> return ()
      Just toCh -> do
        writeChan transToChan $ I.privmsg toCh $ newmsg

joinChannnel :: (Chan Message, Chan Message) -> C.ByteString -> IO ()
joinChannnel (senderChan, recieverChan) = writeChan senderChan . I.joinChan
joinAll :: (Chan Message, Chan Message) -> [C.ByteString] -> IO ()
joinAll chann = mapM_ (joinChannnel chann)

main = do
  ((s,r), th) <- connect serv1
  th <- (:th) <$> makeBot (s,r) pingBot
  readChan r
  readChan r
  let channels = map fst transChannel
  joinAll (s, r) channels
  joinAll (s, r) $ map snd transChannel
  
  th <- (:th) <$> makeBot (s,r) (transBot transChannel s)

  getLine
  return ()
