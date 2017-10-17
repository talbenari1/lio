{-# LANGUAGE OverloadedStrings #-}

import           LIO.HTTP.Server.Frankie
import           LIO.HTTP.Server.Frankie.Loggers
import           LIO.HTTP.Server.Frankie.Static
import           Prelude                         hiding (log)

main :: IO ()
main = runFrankieServer "dev" $ do
  mode "dev" $ do
    host "127.0.0.1"
    port 3000
    appState ()
    logger DEBUG colorStdOutLogger
  dispatch $ do
    -- TODO: Convert to "/+" once supported
    get "/:file" $ static "examples/public"
    fallback $ respond notFound
  onError $ \err -> do
    log ERROR $ "Controller failed with " ++ displayException err
    respond notFound
