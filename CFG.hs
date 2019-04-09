module CFG where

-- CFG == control-flow graph.

import Common
import Data.Graph
import Prelude hiding (Word)
import ThreeAddrSyntax

--------------------------------
-- Basic Blocks
--------------------------------

-- ***
-- *** Here's the output of bb for foobar.imp:
-- ***
-- [[0:,mov R0 #99,mov Rx R0],
--  [1:,mov R1 #0,sub R2 Rx R1,brnz R2 #3],
--  [mov R2 #0,jmp #4],
--  [3:,mov R2 #1],
--  [4:,brz R2 #2],
--  [mov R3 #1,sub R4 Rx R3,mov Rx R4,jmp #1],
--  [2:,exit]]

type Block = [ThreeAddr]

--------------------------------
-- Graphs in Haskell
--------------------------------

--
-- Building Graphs in Haskell
--

directedgraph :: (Graph, Vertex -> ([Char], Integer, [Integer]), Integer -> Maybe Vertex)
directedgraph@(g,vm,l2v)
       = graphFromEdges [("hey pal",1,[2,3]),("dingo",2,[1]),("jose",3,[2,1])]

-- Basic Usage
--
-- ghci> vertices g
-- [0,1,2]
-- ghci> map vm $ vertices g
-- [("hey pal",1,[2,3]),("dingo",2,[1]),("jose",3,[2,1])]

----------------------------------
-- Visualizing Graphs in GraphViz
----------------------------------

-- Basic Usage of GraphViz
--
-- Sample dot file in file "digraph.dot"
--
-- digraph d {
--  1 [label="Hello"]
--  2 [label="World"]
--  3 [label="Everyone"]
--  1 -> { 2 3 }
-- }

-- From dot to png: digraph.dot.png
--
-- dot -T png -O digraph.dot 
--

----------------------------------
-- Converting basic blocks into CFG. Warning: it's ugly code.
----------------------------------

-- ***
-- *** Here's the output of bb for foobar.imp:
-- ***
-- [[0:,mov R0 #99,mov Rx R0],
--  [1:,mov R1 #0,sub R2 Rx R1,brnz R2 #3],
--  [mov R2 #0,jmp #4],
--  [3:,mov R2 #1],
--  [4:,brz R2 #2],
--  [mov R3 #1,sub R4 Rx R3,mov Rx R4,jmp #1],
--  [2:,exit]]

alllabels :: [Block] -> [Int]
alllabels []     = []
alllabels (b:bs) = case b of
  (Label i : _) -> i : alllabels bs
  (_ : _)       -> alllabels bs
  []            -> error "alllabels: this is impossible!"

entry k b = case b of
  Label i : _ -> (i,k)
  _ : _       -> (k,k+1)
  []          -> error "entry: can't happen"

exit :: Int -> [Int] -> ThreeAddr -> [Int]
exit nxt als (Jmp (Immediate _))    = als
exit nxt als (Jmp (Literal w))      = [w]
exit nxt als (BrZ _ (Immediate _))  = als
exit nxt als (BrZ _ (Literal w))    = [w,nxt]
exit nxt als (BrNZ _ (Immediate _)) = als
exit nxt als (BrNZ _ (Literal w))   = [w,nxt]
exit nxt als (BrGT _ (Immediate _)) = als
exit nxt als (BrGT _ (Literal w))   = [w,nxt]
exit nxt als (BrGE _ (Immediate _)) = als
exit nxt als (BrGE _ (Literal w))   = [w,nxt]
exit nxt als (Call (Immediate _))   = als
exit nxt als (Call (Literal w))     = [w,nxt]
exit nxt als Ret                    = []
exit nxt als Exit                   = []
exit nxt _ _                        = [nxt]

belabel :: Int -> [Block] -> [(Block,Int)]
belabel _ []     = []
belabel k (b:bs) = case b of
   Label i : _ -> (b,i) : belabel k bs
   _ : _       -> (b,k) : belabel (k+1) bs
   []          -> error "belabel: this can't happen"

genNodes :: [Block] -> [(Block, Int, [Int])]
genNodes blks = outlabel $ belabel newlabel blks
  where als      = alllabels blks
        newlabel = maximum als + 1
        --
        --
        outlabel :: [(Block, Int)] -> [(Block, Int, [Int])]
        outlabel ((b,l):ns) = case ns of
                 (_,nxt) : _ -> (b,l,exit nxt als (last b)) : outlabel ns
                 []          -> [(b,l,[])]

----------------------------
-- Converting node list into GraphViz digraph
----------------------------    

data Digraph = Digraph String [(Int,String)] [(Int,[Int])]

d = Digraph "d" [(1,"Hello"),(2,"World"),(3,"Everyone")] [(1,[2,3])]

nodes2digraph :: [(Block, Int, [Int])] -> Digraph
nodes2digraph nodes = Digraph "d" labels links
  where
    labels = map (\ (b,i,_) -> (i,showBlock b)) nodes
    links  = map (\ (_,i,is) -> (i,is)) nodes
    showBlock :: Block -> String
    showBlock = foldr (\ b bs -> show b ++ "\n" ++ bs) "" 

instance Show Digraph where
  show (Digraph d labels links) =
    "digraph " ++ d ++ " {\n" ++
       foldr (\ (i,a) ls -> show i ++ " [label=\"" ++ a ++ "\"]\n" ++ ls) "" labels ++
       foldr (\ (i,ls) lnks -> show i ++ " -> { " ++ intseq ls ++ "}\n" ++ lnks) "" links ++
       "}"
          where
            intseq :: [Int] -> String
            intseq is = foldr (\ i is -> show i ++ " " ++ is) "" is
