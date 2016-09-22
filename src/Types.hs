module Types
    ( ServerDirective(..)
    , ClientDirective(..)
    , Command(..)
    , CommandExtra(..)
    , emptyCommandExtra
    ) where

import System.Exit (ExitCode)

-- TODO: Should we keep these Eq instances? Or define something on our own

data CommandExtra = CommandExtra
  { cePath :: Maybe FilePath
  , ceGhcOptions :: [String]
  , ceCabalFilePath :: Maybe FilePath
  , ceCabalOptions :: [String]
  , ceStackYamlPath :: Maybe FilePath
  , ceTemplateHaskell :: Bool
  } deriving (Read, Show, Eq)

emptyCommandExtra :: CommandExtra
emptyCommandExtra = CommandExtra { cePath = Nothing
                                 , ceGhcOptions  = []
                                 , ceCabalFilePath = Nothing
                                 , ceCabalOptions = []
                                 , ceStackYamlPath = Nothing
                                 , ceTemplateHaskell = True
                                 }

data ServerDirective
    = SrvCommand Command CommandExtra
    | SrvStatus
    | SrvExit
    deriving (Read, Show, Eq)

data ClientDirective
    = ClientStdout String
    | ClientStderr String
    | ClientExit ExitCode
    | ClientUnexpectedError String -- ^ For unexpected errors that should not happen
    deriving (Read, Show, Eq)

data Command
    = CmdCheck FilePath
    | CmdModuleFile String
    | CmdInfo FilePath String
    | CmdType FilePath (Int, Int)
    | CmdFindSymbol String [String]
    deriving (Read, Show, Eq)
