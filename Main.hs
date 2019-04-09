module Main where

import BasicBlocks
import HW4
import CodeGen
import Common
import Control.Monad.Identity
import Control.Monad.State
import Data.Graph
import ImpParser
import ImpSyntax
import System.Process
import ThreeAddrParser
import ThreeAddrSyntax


codegen :: FilePath -> IO ()
codegen imp = do
  impprog <- parseImp imp
  putStrLn $ show $ runM $ compileImp impprog

-- Defined for you in ImpParser.hs
-- parseImp  :: FilePath -> IO Prog
-- parseProg :: String -> Prog

-- Defined for you in CodeGen.hs
-- compileImp :: Prog -> StateT Label (StateT VirtualReg Identity) ThreeAddrProg

imp2threeaddr :: String -> StateT Label (StateT VirtualReg Identity) ThreeAddrProg
imp2threeaddr = (return . parseProg) Common.<> compileImp 

bb :: FilePath -> IO [[ThreeAddr]]
bb imp = do
  impprog <- parseImp imp
  let ThreeAddrProg tac = runM $ compileImp impprog
  return $ blockify tac []

cfg imp = do
  impprog <- parseImp imp
  let ThreeAddrProg tac = runM $ compileImp impprog
  let nodes             = genNodes $ blockify tac []
  writeFile (imp ++ ".dot") $ show $ nodes2digraph nodes
  system $ "dot -T png -O " ++ imp ++ ".dot"
  system $ "open " ++ imp ++ ".dot.png"               -- maybe MacOS specific.i

main = do
    undefined
