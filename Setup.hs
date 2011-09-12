module Main ( main ) where

import System.Directory    (findExecutable)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, postInst)
import System.Directory    (findExecutable)
import System.Exit         (exitWith, ExitCode(..))

main :: IO ()
main = defaultMainWithHooks simpleUserHooks{ postInst = checkDefSolver }
 where checkDefSolver _ _ _ _ = do
                mbP <- findExecutable "cbmc"
                case mbP of
                   Nothing -> do putStrLn "***"
                                 putStrLn "*** The copilot-cbmc library requires the solver cbcm to be installed."
                                 putStrLn "*** The executable CBMC must be in your path."
                                 putStrLn "*** Please install CBMC and put it in your path!"
                                 putStrLn "*** CBMC can downloaded at http://www.cprover.org/cbmc/"
                                 putStrLn "***"
                   Just _  -> return ()
                exitWith ExitSuccess

