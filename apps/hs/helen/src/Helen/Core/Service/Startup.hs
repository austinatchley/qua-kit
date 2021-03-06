{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}

module Helen.Core.Service.Startup where

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.State

import System.Exit
import System.IO.Error
import System.Process

import Path
import Path.IO

import Helen.Core.OptParse.Types
import Helen.Core.Types

startupServices :: HelenWorld ()
startupServices = do
    bcs <- gets $ settingsBins . helenSettings
    mapM_ startBinaryWithRestarts bcs

data BinState = BinState
    { binStateProcessHandle :: ProcessHandle
    , binStateRestartsLeft  :: Int
    }

startBinaryWithRestarts :: BinConfig -> HelenWorld ()
startBinaryWithRestarts bc@BinConfig {..} = do
    ph <- startBinary bc
    nras <- gets $ settingsBinRestartAttempts . helenSettings
    let bs = BinState {binStateProcessHandle = ph, binStateRestartsLeft = nras}
    forkHelen $ go bs
  where
    go BinState {..} = do
        ec <- liftIO $ waitForProcess binStateProcessHandle
        case ec of
            ExitSuccess -> pure ()
            ExitFailure c -> do
                logOtherNS binConfigName LevelError $
                    T.unwords
                        [ binConfigName
                        , "failed with exit code"
                        , T.pack (show c) <> ","
                        , T.pack (show binStateRestartsLeft)
                        , "restarts left."
                        ]
                when (binStateRestartsLeft > 0) $ do
                    logOtherNS binConfigName LevelInfo $
                        T.unwords ["restarting", binConfigName]
                    ph <- startBinary bc
                    go
                        BinState
                        { binStateProcessHandle = ph
                        , binStateRestartsLeft = binStateRestartsLeft - 1
                        }

startBinary :: BinConfig -> HelenWorld ProcessHandle
startBinary BinConfig {..} = do
    dirPath <- getCurrentDir
    let execPath = dirPath </> binConfigPath
        cp = (proc (toFilePath execPath) binConfigArgs)
          { cwd = Just $ fromAbsDir (parent execPath)
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
    (_, mouth, merrh, ph) <- liftIO $ createProcess_ (T.unpack binConfigName) cp
    setupOutputHandler LevelInfo mouth
    setupOutputHandler LevelWarn merrh
    pure ph
  where
    setupOutputHandler _ Nothing  = pure ()
    setupOutputHandler level (Just h) = forkHelen loop
      where
        loop = do
          el <- liftIO . try $ T.hGetLine h
          case el of
            Left e -> if isEOFError e
                      then pure ()
                      else logErrorNS binConfigName $ T.pack (show e)
            Right line -> do
              logOtherNS binConfigName level line
              loop
