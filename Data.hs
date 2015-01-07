module Data (User(..)
            ,Connect(..)) where

import Network (HostName, PortID(..))
import Network.IRC hiding(nick, user)

data User = User {
      username   :: UserName
    , hostname   :: ServerName
    , servername :: ServerName
    , realname   :: RealName
    }
    
data Connect = Connect {
      host :: HostName
    , port :: PortID
    , nick :: UserName
    , user :: User
    , pass :: Password
    }
