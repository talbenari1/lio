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
    static "/assets" "examples/public"
    logger DEBUG colorStdOutLogger
  dispatch $ fallback $ respond notFound
  onError $ \err -> do
    log ERROR $ "Controller failed with " ++ displayException err
    respond notFound
