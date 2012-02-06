{-# LANGUAGE OverloadedStrings #-}
import Blaze.ByteString.Builder
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans
import Data.String
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.Printf
import qualified Data.ByteString as B

-- a history of heartbeats
data Ekg = Ekg
    { thread :: ThreadId
    , pings  :: Int
    }
online = (<0) . pings
offline = not . online

data Direction = Incr | Decr

intervalDead = minutes 11

main = do
    verifyArgs
    putStrLn "Listening on http://localhost:15000"
    dummy <- forkIO $ threadDelay (seconds 1)
    ekg <- newMVar $ Ekg dummy (negate 1)
    run 15000 (app ekg)

verifyArgs :: IO ()
verifyArgs = do
    args <- getArgs
    when (null args) (error "You must provide a callback script path")
    ec <- rawSystem (head args) ["test"]
    case ec of
        ExitSuccess -> return ()
        ExitFailure c -> error $ "Callback script test exited with " ++ show c

runScript :: String -> IO ()
runScript argument = do
    script <- head `fmap` getArgs
    ec <- rawSystem script [argument]
    case ec of
        ExitSuccess -> return ()
        ExitFailure c -> hPutStrLn stderr $ "Callback script exitid with " ++ show c

app :: MVar Ekg -> Application
app ekg req = case rawPathInfo req of
    "/ping" -> pong ekg
    _ -> index

-- receive heartbeats
pong ekg = do
    liftIO $ do
        putStrLn "I was pinged."
        awaitDeath ekg
        e <- readMVar ekg
        when (offline e) (incrPings ekg)
    okText "pong"
index = okText "Try /ping"

-- assume the device will die shortly
awaitDeath :: MVar Ekg -> IO ()
awaitDeath ekg = do
    e <- takeMVar ekg
    killThread $ thread e
    t <- forkIO $ do
        threadDelay intervalDead
        markOffline ekg
    putMVar ekg $ e{thread=t}

-- increment the pings count
incrPings :: MVar Ekg -> IO ()
incrPings ekg = do
    e <- readMVar ekg
    when (online e) (error "Shouldn't increment pings when online")
    let n = pings e + 1
    if n >= 2
        then markOnline ekg
        else (swapMVar ekg $ e{pings=n}) >> return ()

-- the device just went offline
markOffline :: MVar Ekg -> IO ()
markOffline ekg = do
    e <- takeMVar ekg
    when (online e) $ do
        putStrLn "status -> offline"
        runScript "offline"
    putMVar ekg $ e{pings=0}

-- the device came back online
markOnline :: MVar Ekg -> IO ()
markOnline ekg = do
    e <- takeMVar ekg
    when (offline e) $ do
        putStrLn "status -> online"
        runScript "online"
    putMVar ekg $ e{pings=negate 1}

-- convert seconds into microseconds (for threadDelay)
seconds = (1000000*)
minutes = (1000000*) . (60*)

textResponse status body = return $ ResponseBuilder
    status
    [("Content-Type","text/plain"),("Content-Length", len body)]
    $ fromByteString body
    where len = fromString . show . B.length
okText = textResponse status200
