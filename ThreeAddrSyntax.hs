module ThreeAddrSyntax where

import Prelude hiding (Word)
import Common

data ThreeAddrProg = ThreeAddrProg [ThreeAddr]
     
data ThreeAddr = Mov Register Arg
               | Load Register Register   -- (Load rtarg raddr) is rtarg := contents(raddr)
               | Store Register Register  -- (Store rtarg rsrc) stores contents(rsrc) in address rtarg
               | Add Register Arg Arg
               | Sub Register Arg Arg
               | Div Register Arg Arg
               | Mul Register Arg Arg
               | Negate Register Arg
               | Equal Register Arg Arg
               | LogNot Register Arg 
               | GThan Register Arg Arg
               | Jmp Arg
               | BrZ Register Arg 
               | BrNZ Register Arg
               | BrGT Register Arg
               | BrGE Register Arg
               | Label Int
               | Read Register 
               | Write Arg
               | Call Arg
               | Ret
               | Exit

instance Show ThreeAddrProg where
   show (ThreeAddrProg ts) = pretty ts
     where
       pretty []         = ""
       pretty (t : rest) = case t of
         Label _ -> show t ++ pretty rest
         _       -> '\t' : show t ++ ";\n" ++ pretty rest

instance Show ThreeAddr where
    show (Mov ra a)      = "mov " ++ show ra ++ " "++show a 
    show (Load r1 r2)    = "load " ++ show r1 ++ " "++show r2
    show (Store r1 r2)   = "store " ++ show r1 ++ " "++show r2
    show (Add ra a1 a2)  = "add " ++ show ra ++" " ++ show a1 ++ " " ++ show a2
    show (Sub ra a1 a2)  = "sub "++ show ra ++ " " ++ show a1 ++" " ++ show a2
    show (Div ra a1 a2)  = "div " ++ show ra ++ " " ++ show a1 ++ " " ++ show a2
    show (Mul ra a1 a2)  = "mul " ++ show ra++" "++show a1++" "++show a2
    show (Negate ra a)   = "neg "++show ra++" "++show a
    show (Equal r a1 a2) = "equal " ++show r++" "++show a1++" "++show a2
    show (LogNot ra a)   = "neg "++show ra++" "++show a
    show (GThan r a1 a2) = "equal " ++show r++" "++show a1++" "++show a2
    show (Jmp a)         = "jmp "++show a
    show (BrZ r a)       = "brz "++show r++" "++show a
    show (BrNZ r a)      = "brnz "++show r++" "++show a
    show (BrGT r a)      = "brgt "++show r++" "++show a
    show (BrGE r a)      = "brge "++show r++" "++show a
    show (Label i)       = show i ++ ":"
    show (Read ra)       = "read "++show ra
    show (Write a)       = "write "++show a
    show (Call a)        = "call "++show a
    show Ret             = "ret"
    show Exit            = "exit"
