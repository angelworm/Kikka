{- -*- coding: utf-8 -*- -}
{-# LANGUAGE OverloadedStrings #-}

{-
  Kikka configuration file.
  PLEASE edit this file and rename into "Config.hs"
-}

module Config (serv1, serv2, transChannel) where
import Network
import Network.IRC hiding(nick, user)
import qualified Data.ByteString.Char8 as C
import Data

{- 
   connection server
 -}
serv1 = Connect {
          host = "irc.ircnet.ne.jp"
        , port = PortNumber 6667
        , nick = "kikka"
        , user = User {
                   username   = "kikka"
                 , hostname   = "irc.ircnet.ne.jp"
                 , servername = "localhost"
                 , realname   = "Nakajima Kikka"
                 }
        , pass = "xxx"
        }
serv2 = {
          host = "irc.ircnet.ne.jp"
        , port = PortNumber 6667
        , nick = "kikka"
        , user = User {
                   username   = "kikka"
                 , hostname   = "irc.ircnet.ne.jp"
                 , servername = "localhost"
                 , realname   = "Nakajima Kikka"
                 }
        , pass = "xxx"
        }

{-
  serv1 channnel into serv2 channel
 -}
transChannel :: [(C.ByteString, C.ByteString )]
transChannel = [("#serv1-channel", "#serv2-channel")]

