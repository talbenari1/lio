
module Main (-- module LIO.LIO
             module LIO.TCB
            , module LIO.DCLabel
            , module LIO.FS
            , module LIO.Handle
            , module Main
            ) where

import LIO.LIO
import LIO.TCB
import LIO.Handle
import qualified LIO.Handle as LH
import LIO.DCLabel
import LIO.FS

import Control.Exception
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO

cat1 = DCat (Set.fromList [Principal "my@address.com"
                          , Principal "your@address.com"])
cat2 = DCat (Set.fromList [Principal "my@example.com"
                          , Principal "your@address.com"])

e = DCLabel (Set.singleton cat1) (Set.fromList [cat1, cat2])
d = DCLabel (Set.fromList [cat1, cat2]) (Set.fromList [cat1, cat2])
h = DCLabel (Set.empty) (Set.fromList [cat1, cat2])

rl :: String -> [(DCLabel, String)]
rl = reads

md = evalDC $ mkDir NoPrivs h rootDir "high"

main = return ()
