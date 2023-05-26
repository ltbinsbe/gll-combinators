module Main where

import Control.Arrow
import Data.List
import System.Environment

import Text.PrettyPrint (render)

import ART.Parsing.Concrete as ART
import AAG.Parsing.Concrete as AAG
import AAG.Parsing.Abstract as AAG
import AAG.Morphing.AssignPid as PID
import AAG.Morphing.Merge as MERGE
import AAG.Printing.DataTypes as  DT
import GLL.Parsing.Abstract as GLL
import GLL.Parsing.ExtractModule as EM
import GLL.Printing.Module as MOD

main = do   
    args <- getArgs
    case args of
        []          -> putStrLn "Please provide me with a grammar file"
        [_]         -> putStrLn "Where would you like me to generate your parser?"
        (_:_:[])    -> putStrLn "What should the module be called?"
        (i:o:m:_)   -> go i o m ("art" `isSuffixOf` i)
 where
    go fp out mod isArt = 
        readFile fp 
            >>= (scanNparse 
             >>> PID.morph
             >>> MERGE.morph
             >>>  (AAG.abstract >>> DT.printer)
              &&& (GLL.abstract >>> EM.extract mod)
             >>> MOD.printer
             >>> render 
             >>> writeFile out)
     where scanNparse | isArt     = ART.scan >>> ART.concretize
                      | otherwise = AAG.scan >>> AAG.concretize
