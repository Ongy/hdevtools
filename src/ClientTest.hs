import Network (Socket, accept)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.IORef (newIORef)
import System.Exit (exitSuccess, exitFailure, ExitCode(..))
import System.IO (Handle, hGetLine, hPutStrLn, stderr)
import Text.Read (readMaybe)
import Control.Concurrent.MVar (newEmptyMVar, readMVar, putMVar)
import System.Timeout (timeout)


import Client
import Server (withServer, clientSend)
import Types (ClientDirective(..), ServerDirective(..))

socketPath :: String
socketPath = ".test-sock"

getNext :: Handle -> IO ServerDirective
getNext h = do
    msg <- hGetLine h
    case readMaybe msg of
      Just x -> return x
      Nothing -> do
        hPutStrLn stderr "Client send an invalid server directive: msg"
        exitFailure

-- Fork out the action, which starts the client with a command
-- Tell the client to exit and test the command received from the client
-- against the command given as expected
testSimpleCommand :: Socket -> IO () -> ServerDirective -> IO Bool
testSimpleCommand sock act exp = do
    _ <- forkIO act
    (h, _, _) <- accept sock
    cmd <- getNext h
    ref <- newIORef (Just h)
    clientSend ref (ClientExit ExitSuccess)
    if (cmd == exp)
       then putStrLn "OK" >> return True
       else putStrLn "Fail" >> return False

-- This should be rather obvious :)
testStopServer :: Socket -> IO Bool
testStopServer sock = do
    putStr "Starting testStopServer: "
    testSimpleCommand sock (void $ stopServer socketPath) SrvExit

-- This should be rather obvious :)
testServerStatus :: Socket -> IO Bool
testServerStatus sock = do
    putStr "Starting testServerStatus: "
    testSimpleCommand sock (void $ getServerStatus socketPath) SrvStatus

-- Test the client actions for Stdout out:
testClientStdout :: Socket -> IO Bool
testClientStdout sock = do
    putStr "Startint testClientStdout: "
    var <- newEmptyMVar
    client <- connect socketPath
    _ <- forkIO . void $ clientReadLoop (putMVar var) (\_ -> return ()) client
    (h, _, _) <- accept sock
    ref <- newIORef (Just h)
    clientSend ref (ClientStdout "test string")
    clientSend ref (ClientExit ExitSuccess)

    -- safety timeout. We will wait up to 5 seconds for the client thread
    ret <- timeout (5 * 1000 * 1000) $ readMVar var
    case ret of
      (Just "test string") -> putStrLn "OK" >> return True
      _ -> do
          putStrLn "Fail"
          putStrLn "Got in ClientStdout:"
          putStrLn (show ret)
          return False

-- Test the client actions for Stderr out:
testClientStderr :: Socket -> IO Bool
testClientStderr sock = do
    putStr "Startint testClientStderr: "
    var <- newEmptyMVar
    client <- connect socketPath
    _ <- forkIO . void $ clientReadLoop (\_ -> return ()) (putMVar var) client
    (h, _, _) <- accept sock
    ref <- newIORef (Just h)
    clientSend ref (ClientStderr "test string")
    clientSend ref (ClientExit ExitSuccess)

    -- safety timeout. We will wait up to 5 seconds for the client thread
    ret <- timeout (5 * 1000 * 1000) $ readMVar var
    case ret of
      (Just "test string") -> putStrLn "OK" >> return True
      _ -> do
          putStrLn "Fail"
          putStrLn "Got in ClientStderr:"
          putStrLn (show ret)
          return False


testExitCode :: Socket -> ExitCode -> IO Bool
testExitCode sock exp = do
    client <- connect socketPath
    (h, _, _) <- accept sock
    ref <- newIORef (Just h)
    clientSend ref (ClientExit exp)
    code <- clientReadLoop (\_ -> return ()) (\_ -> return ()) client
    return (code == exp)

testExitCodes :: Socket -> IO Bool
testExitCodes sock = do
    putStr "Startin testExitCodes: "
    ret <- mapM (testExitCode sock)
                [ExitSuccess, ExitFailure 1]
    if and ret
       then putStrLn "OK"
       else putStrLn "Fail"
    return (and ret)

testCases :: [Socket -> IO Bool]
testCases =
    [ testStopServer
    , testServerStatus
    , testClientStdout
    , testClientStderr
    , testExitCodes
    ]


runClientTests :: Socket -> IO Bool
runClientTests = fmap and . sequence . sequence testCases


main :: IO ()
main = do
    ret <- withServer socketPath Nothing runClientTests
    case ret of
      True -> exitSuccess
      False -> exitFailure
