module Data.Conduit.RawSocket
  ( sourceTCP
  , TCPPacket (..)
  ) where

import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as B
import Data.IP hiding (flags)
import Data.TCP
import Data.Serialize
import Data.Conduit hiding (Done)

import Control.Monad.IO.Class (liftIO)

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.BSD (getProtocolNumber)

data TCPPacket = TCPPacket
    { ip    :: IPv4Header
    , tcp   :: TCPHeader
    , opt   :: B.ByteString -- FIXME structure for tcp options
    , payload :: B.ByteString
    } deriving (Show)

tcpProtocol :: ProtocolNumber
tcpProtocol = unsafePerformIO (getProtocolNumber "tcp")

runGetPart :: Get a -> B.ByteString -> Either String (a, B.ByteString)
runGetPart get s =
    case runGetPartial get s of
        Done a s' -> Right (a, s')
        Fail err -> Left err
        Partial _ -> Left "not enough input."

recvTCPPacket :: Socket -> IO TCPPacket
recvTCPPacket sock = loop where
  loop = do
    (s, addr) <- recvFrom sock 2048
    let r = do (ip, s') <- runGetPart (get::Get IPv4Header) s
               if protocol ip == fromIntegral tcpProtocol
                 then do
                   (tcp, s'') <- runGetPart (get::Get TCPHeader) s'
                   let optLen = dataOffset tcp * 4 - 20 -- 20 is length of tcp header
                       (opt, payload) = B.splitAt optLen s''
                   return (TCPPacket ip tcp opt payload)
                 else
                   fail "not tcp packet."
    either ((>> loop) . putStrLn) return r

sourceTCP :: Source (ResourceT IO) TCPPacket
sourceTCP =
    sourceIO
        (socket AF_INET Raw tcpProtocol)
        sClose
        (fmap IOOpen . liftIO . recvTCPPacket)
