module Client
    ( getServerStatus
    , stopServer
    , serverCommand

-- This is for testing only:
    , connect
    , clientReadLoop
    ) where

import Control.Exception (tryJust)
import Control.Monad (guard)
import Network (PortID(UnixSocket), connectTo)
import System.Exit (exitFailure, ExitCode)
import System.IO (Handle, hClose, hFlush, hGetLine, hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)

import Daemonize (daemonize)
import Server (startServer)
import Types (ClientDirective(..), Command(..), CommandExtra(..), ServerDirective(..))
import Util (readMaybe)

connect :: FilePath -> IO Handle
connect sock = do
  connectTo "" (UnixSocket sock)

getServerStatus :: FilePath -> IO ExitCode
getServerStatus sock = do
    h <- connect sock
    hPutStrLn h $ show SrvStatus
    hFlush h
    startClientReadLoop h

stopServer :: FilePath -> IO ExitCode
stopServer sock = do
    h <- connect sock
    hPutStrLn h $ show SrvExit
    hFlush h
    startClientReadLoop h

serverCommand :: FilePath -> Command -> CommandExtra -> IO ExitCode
serverCommand sock cmd cmdExtra = do
    r <- tryJust (guard . isDoesNotExistError) (connect sock)
    case r of
        Right h -> do
            hPutStrLn h $ show (SrvCommand cmd cmdExtra)
            hFlush h
            startClientReadLoop h
        Left _ -> do
            daemonize False $ startServer sock Nothing cmdExtra
            serverCommand sock cmd cmdExtra

clientReadLoop :: (String -> IO ()) -> (String -> IO ()) -> Handle -> IO ExitCode
clientReadLoop printOut printErr h = do
    msg <- hGetLine h
    let clientDirective = readMaybe msg
    case clientDirective of
        Just (ClientStdout out) -> printOut out >> clientReadLoop printOut printErr h
        Just (ClientStderr err) -> printErr err >> clientReadLoop printOut printErr h
        Just (ClientExit exitCode) -> hClose h >> return exitCode -- This should just return  >> exitWith exitCode
        Just (ClientUnexpectedError err) -> hClose h >> unexpectedError err
        Nothing -> do
            hClose h
            unexpectedError $
                "The server sent an invalid message to the client: " ++ show msg

startClientReadLoop :: Handle -> IO ExitCode
startClientReadLoop = clientReadLoop putStrLn (hPutStrLn stderr)

unexpectedError :: String -> IO a
unexpectedError err = do
    hPutStrLn stderr banner
    hPutStrLn stderr err
    hPutStrLn stderr banner
    exitFailure
    where banner = replicate 78 '*'
