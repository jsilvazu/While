module Main where

import UU.Parsing as Par
import UU.Scanner
import UU.Pretty
import State
import SemExpr

import System.Environment(getArgs)

pExpr  = pAsig
        <|> pSecu
        <|> pIf
        <|> Skp <$> pKey "skip"
        <|> pWh
         <|> pKey "(" *> pExpr <* pKey ")"


pAsig  = Asig <$> pVar <* pKey ":=" <*> pArit
pSecu  = Secu <$  pKey ";" <*> pExpr <*> pExpr
pIf    = If   <$  pKey "if" <*> pBool <*  pKey "then" <*> pExpr <*  pKey "else" <*> pExpr
pWh    = Wh   <$  pKey "while" <*> pBool <* pKey "do" <*> pExpr

pRoot  = RootAbs <$> pExpr

pArit   = pInt
          <|> pVar
          <|> Sum <$ pKey "+" <*> pArit  <*> pArit
          <|> Mul <$ pKey "*" <*> pArit  <*> pArit
          <|> Res <$ pKey "-" <*> pArit  <*> pArit

pInt   = Num <$> pInteger
pVar   = Var <$> pVarid

pBool  = Bol <$> (pKey "True" <|> pKey "False")
            <|> pEqu
            <|> pGeq

pEqu   = Equ <$ pKey "==" <*> pArit <*> pArit
pGeq   = Geq <$ pKey "<=" <*> pArit <*> pArit
--
--
--pLambda = Abs . HNm <$ pKey "\\" <*> pVarid <* pKey "->" <*> pExpr
-- :t Expr
--pLet   = Let . HNm <$ pKey "let" <*> pVarid
--        <* pKey "=" <*> pExpr
--        <* pKey "in" <*> pExpr

-------------------------------------------------------------

lmbdScanTxt :: String -> [Token]
lmbdScanTxt = lmbdScan (Pos 0 0 "")

lmbdScanFl :: FilePath -> String -> [Token]
lmbdScanFl f s = lmbdScan (Pos 1 0 f) s

process :: FilePath -> IO ()
process f = do
  s <- readFile f
  t pRoot (lmbdScan (Pos 0 0 "") s)

t p inp -- test parser p on input inp :t HsName
  = do  c <- parseIO p inp
        let   inh = Inh_RootAbs {}
              syn = wrap_RootAbs (sem_RootAbs c) inh
              fo  = fo_Syn_RootAbs syn
        if foHasErrs fo
           then
                putStrLn $ "Errores tiene el programa" ++ (show $ foErrL fo)
           else
                let inm = fov fo
                    inn = gamma2Var inm
                in
                putStrLn $ "Ok: " ++ (show $ inn)

use :: String
use = "Use: wh <path>"


mail = do
        args <- getArgs
        if length args /= 1
         then
            print use
         else
            print use--process (head args)

-- IO <$
main = do
          let
            s = --"; x:= 6 if <= x 6 then x := + x 1 else y := x"
                --"; ; x := 0 y:=0 while <= x 6 do ; x := + x 1 y := + x y"
                --"; ; y := 1 x:=2 y := 3"
                ";  "
          t pRoot (lmbdScanTxt s )
          a <- parseIO pExpr (lmbdScanTxt s)
          print a

kywrdtxt = ["True","False", "if", "then", "else", "skip", "while", "do"]
kywrdops = [ "==", "<=",  "=", "+", ":=", "-", ";", "*"]
spcchrs  = "()[]{}|"
opchrs   = "-:=>+,;*<="
lmbdScan = scan kywrdtxt kywrdops spcchrs opchrs






















