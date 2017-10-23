{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LIO.HTTP.Server.Frankie.Static (fileHandler) where

import           Control.Exception          (SomeException, try)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.List                  (stripPrefix)
import qualified Data.Text                  as Text
import           LIO.DCLabel                (DC)
import           LIO.HTTP.Server.Frankie    (StaticHandler (..))
import           LIO.TCB                    (ioTCB)
import           Prelude                    hiding (log)
import           System.Directory           (canonicalizePath)
import           System.FilePath            ((</>))

fileHandler :: StaticHandler DC
fileHandler = StaticHandler $ \(vprefix, path) parts ->
  case stripPrefix vprefix parts of
    Just suffix -> do
      p <- ioTCB . canonicalizePath $ Text.unpack path
      let fullPath = p </> (Text.unpack . Text.intercalate "/") suffix
      fileOrEx :: Either SomeException L8.ByteString <- ioTCB $ try $ L8.readFile fullPath
      case fileOrEx of
        Left _     -> return Nothing
        Right file -> return $ Just file
    Nothing -> return Nothing
