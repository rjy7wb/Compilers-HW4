module HW4 where

{-
2 Questions/ 50 points each.

Due Monday, April 8th by 11:59pm.

General Requirements.
---------------------
1. Your solutions to these questions are to be included in this file only.
2. Turn in your homework on blackboard only. Emailing your answers to me or 
   the TAs or turning in a print out in class is the same as not turning it
   in.
3. IMPORTANT: your homework must load into ghci without errors to receive
   any credit. That is, if what you turn in does not load into ghci
   then you will receive a zero for this assignment. No exceptions.
4. If you don't finish a problem and have something that you'd like to have
   considered as partial credit, put it within a comment. This isn't a 
   guarantee that you'll get partial credit, though.
5. Type declarations must be included with every definition you give.

Turning In Your Solution.
-------------------------
To turn in your solution, please email me the code (harrisonwl@missouri.edu)
with the subject "CS4430 HW4". It is important that you get this small detail
right because otherwise I may miss your submitted solution. Your solution
should be in the form of a single Haskell file named "Last-name_HW4.hs".
So, if I did this homework, I’d turn in "Harrison_HW4.hs".

-}

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



--
-- Problem 1. The output of the genNodes function is a list of blocks and
-- their connections; i.e., something of the following type:
--     [(Block, Int, [Int])]
-- For this problem, reformat things of that type as a Digraph, where
-- a Digraph is defined as:                 

data Digraph = Digraph String [(Int,String)] [(Int,[Int])]

-- Here's an example Digraph:
d = Digraph "d" [(1,"Hello"),(2,"World"),(3,"Everyone")] [(1,[2,3])]

-- The reformatting function you must use is called "nodes2digraph".
-- Complete Problem 1 by defining labels and links below.
nodes2digraph :: [(Block, Int, [Int])] -> Digraph
nodes2digraph nodes = Digraph "d" labels links
  where
    labels = map (\ (b,i,_) -> (i,showBlock b)) nodes
    links  = map (\ (_,i,is) -> (i,is)) nodes
    showBlock :: Block -> String
    showBlock = foldr (\ b bs -> show b ++ "\n" ++ bs) ""
--
-- Problem 2. This show instance should format the Digraph as a dot file.
-- See the sample dot files for inspiration.    
--    
instance Show Digraph where
  show (Digraph d labels links) =
      "digraph " ++ d ++ " {\n" ++
       foldr (\ (i,a) ls -> show i ++ " [label=\"" ++ a ++ "\"]\n" ++ ls) "" labels ++ foldr (\ (i,ls) lnks -> show i ++ " -> { " ++ intseq ls ++ "}\n" ++ lnks) "" links ++ "}"
       where
         intseq :: [Int] -> String
         intseq is = foldr (\ i is -> show i ++ " " ++ is) "" is

--
-- Watch HW4video.mov to see how all this should fit together.
--
