{-# LANGUAGE RecordWildCards, ViewPatterns, BangPatterns #-}
import System.Environment (getArgs)
import System.IO (stdout)

import qualified Data.ByteString.Char8 as S
import Data.IP
import Data.TCP
import Data.Conduit
import Data.Conduit.Binary (sinkHandle)
import qualified Data.Conduit.List as CL
import qualified Data.Map as M

import Control.Monad.IO.Class (liftIO)

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Network.BSD
import Data.Conduit.RawSocket

sinkRemote :: MonadResource m => HostName -> PortNumber -> Sink TCPPacket m ()
sinkRemote remoteIp remotePort = do
    he <- liftIO $ getHostByName remoteIp
    liftIO $ print (he, remotePort)
    let remoteAddr = SockAddrInet remotePort (hostAddress he)
    loop M.empty remoteAddr
  where
    loop m addr = do
        r <- await
        case r of
            Nothing -> loop m addr
            Just TCPPacket{..} -> do
                let key = (source ip, srcPort tcp)
                case M.lookup key m of
                    Nothing -> do
                        sock <- liftIO $ do
                            sock <- socket AF_INET Stream tcpProtocol
                            connect sock addr
                            sendAll sock payload
                            return sock
                        let m' = M.insert key sock m
                        loop m' addr
                    Just sock -> do
                        liftIO $ sendAll sock payload
                        loop m addr

main :: IO ()
main = do
    [localIp', read -> !localPort, remoteIp, fromInteger . read -> !remotePort] <- getArgs
    localIp <- inet_addr localIp'
    runResourceT $ sourceTCP
                $= CL.filter (validatePacket localIp localPort)
                -- $= CL.map (S.pack . (++"\n") . show)
                -- $$ sinkHandle stdout
                $$ sinkRemote remoteIp remotePort
  where
    validatePacket localIp localPort TCPPacket{..} =
        destination ip==IPv4 localIp
        && dstPort tcp==TCPPort localPort
        && not (S.null payload)
