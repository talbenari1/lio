{-# LANGUAGE OverloadedStrings #-}

module LIO.HTTP.Server.Frankie.Static (static) where

import           Control.Exception          (SomeException, try)
import           Control.Monad.Trans.Class  (lift)
import qualified Data.ByteString.Lazy.Char8 as L8
import           LIO.HTTP.Server.Frankie
import           LIO.TCB                    (ioTCB)
import           Prelude                    hiding (log)
import           System.Directory           (makeAbsolute)
import           System.FilePath            ((</>))

static :: FilePath -> FilePath -> DCController ()
static path route = do
  absPath <- lift . ioTCB $ makeAbsolute path
  -- TODO: ensure that absPath is a prefix of fullPath
  let fullPath = absPath </> route
  log DEBUG $ "Static file lookup: " ++ fullPath
  fileOrEx <- lift . ioTCB . try $ L8.readFile fullPath
  case fileOrEx of
    Left ex -> handleEx ex
    Right file -> do
      log DEBUG $ "File found: " ++ fullPath
      respond $ okHtml file

handleEx :: SomeException -> DCController ()
handleEx ex = do
  log ERROR $ show ex
  respond notFound
