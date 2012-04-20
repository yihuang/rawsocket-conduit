{-# LANGUAGE RecordWildCards, ViewPatterns, BangPatterns #-}
import System.Environment (getArgs)
import System.IO (stdout)

import qualified Data.ByteString.Char8 as S
import Data.IP
import Data.TCP
import Data.Conduit
import Data.Conduit.Binary (sinkHandle)
import qualified Data.Conduit.List as CL

import Network.Socket (inet_addr)
import Data.Conduit.RawSocket

main :: IO ()
main = do
    [bindIp', read -> !bindPort] <- getArgs
    bindIp <- inet_addr bindIp'
    runResourceT $ sourceTCP
                $= CL.filter (validatePacket bindIp bindPort)
                $= CL.map (S.pack . (++"\n") . show)
                $$ sinkHandle stdout
  where
    validatePacket bindIp bindPort TCPPacket{..} =
        destination ip==IPv4 bindIp
        || dstPort tcp==TCPPort bindPort
