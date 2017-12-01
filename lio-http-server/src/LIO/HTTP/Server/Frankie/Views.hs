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
import           System.Directory           (canonicalizePath)
import           System.FilePath            ((</>))

fileHandler :: Text -> ViewHandler DC
fileHandler path filename = do
    p <- ioTCB . canonicalizePath $ Text.unpack path
    let fullPath = p </> (Text.unpack . Text.intercalate "/") filename
    fileOrEx :: Either SomeException L8.ByteString <- ioTCB $ try $ L8.readFile fullPath
    case fileOrEx of
        Left _     -> return Nothing
        Right file -> return $ Just file
