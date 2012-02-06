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
import System.Locale
import System.Process
import Text.Printf
import qualified Data.ByteString as B

-- a history of heartbeats
data Ekg = Ekg
    { missed :: Int
    , online :: Bool
    }
offline = not . online

data Direction = Incr | Decr

intervalBeat = 60
ratio = 3
intervalCheck = intervalBeat `div` ratio

main = do
    verifyArgs
    putStrLn "Listening on http://localhost:15000"
    ekg <- newMVar $ Ekg 0 True
    forkIO $ checkUp ekg
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
        maybeOnline ekg Decr
    okText "pong"
index = okText "Try /ping"

-- look for missing heartbeats
checkUp :: MVar Ekg -> IO ()
checkUp ekg = do
    threadDelay $ seconds intervalCheck
    _ <- forkIO $ maybeOnline ekg Incr
    checkUp ekg

-- decide whether our online status has changed
maybeOnline :: MVar Ekg -> Direction -> IO ()
maybeOnline ekg dir = do
    previous <- takeMVar ekg
    let m = missed previous
    let n = case dir of
                Incr -> m+1
                Decr -> bound 0 ratio (m-2*ratio)
    let new = previous{missed=n}
    putMVar ekg new
    putStrLn $ "maybe online. " ++ showEkg new
    when (n == ratio+1 && online previous) (markOffline ekg)
    when (n == 0 && offline previous) (markOnline ekg)

markOffline :: MVar Ekg -> IO ()
markOffline ekg = do
    previous <- takeMVar ekg
    putStrLn "status -> offline"
    putMVar ekg $ previous{online=False}
    runScript "offline"

markOnline :: MVar Ekg -> IO ()
markOnline ekg = do
    previous <- takeMVar ekg
    putStrLn "status -> online"
    putMVar ekg $ previous{online=True}
    runScript "online"

-- convert seconds into microseconds (for threadDelay)
seconds = (1000000*)

-- keep 'n' between upper ('u') and lower ('l') bounds, respectively
bound :: Int -> Int -> Int -> Int
bound l u n =
    case (n < l, n > u) of
        (True,_) -> l
        (_, True) -> u
        (False,False) -> n

textResponse status body = return $ ResponseBuilder
    status
    [("Content-Type","text/plain"),("Content-Length", len body)]
    $ fromByteString body
    where len = fromString . show . B.length
okText = textResponse status200

showEkg :: Ekg -> String
showEkg ekg = printf "%d missed" (missed ekg)
