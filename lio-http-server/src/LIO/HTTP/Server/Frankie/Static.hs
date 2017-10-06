{-# LANGUAGE OverloadedStrings #-}

module LIO.HTTP.Server.Frankie.Static (static) where

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
  let fullPath = absPath </> route
  log DEBUG $ "Static file lookup: " ++ fullPath
  file <- lift . ioTCB $ L8.readFile fullPath
  log DEBUG $ "File found: " ++ fullPath
  respond $ okHtml file
