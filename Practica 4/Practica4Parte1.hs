{-
P R A C T I C A  4 | PUNTO 1.
Integrantes:
- DJLP
- ADLG
- JGJMP
-}

module Practica4 where
    
import Data.Char

data Token = Assign | If | Then | Else | Seq | While | Do | Skip |
             Boolean Bool | Equal | And | Not |
             Loc Int | Number Int | LP | RP | Sum deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs 
lexer ('L':rest) = parseLoc rest
lexer ('n':rest) = parseNumber rest
lexer (':':'=':rest) = Assign : lexer rest
lexer ('(':rest) = LP : lexer rest
lexer (')':rest) = RP : lexer rest
lexer ('=':rest) = Equal : lexer rest
lexer ('+':rest) = Sum : lexer rest
lexer (';':rest) = Seq : lexer rest
lexer ('-':'(':rest) = Not : LP : lexer rest
lexer ('t':'r':'u':'e':rest) = Boolean True : lexer rest
lexer ('f':'a':'l':'s':'e':rest) = Boolean False : lexer rest
lexer ('i':'f':rest) = If : lexer rest
lexer ('t':'h':'e':'n':rest) = Then : lexer rest
lexer ('e':'l':'s':'e':rest) = Else : lexer rest
lexer ('w':'h':'i':'l':'e':rest) = While : lexer rest
lexer ('d':'o':rest) = Do : lexer rest
lexer ('s':'k':'i':'p':rest) = Skip : lexer rest
lexer ('&':rest) = And : lexer rest 
lexer (c:cs)
    | isDigit c = parseNumber (c:cs)
    | isAlpha c = parseLoc (c:cs)
    | otherwise = error $ "Token desconocido: " ++ [c]

parseLoc :: String -> [Token]
parseLoc cs = Loc (read loc) : lexer rest
    where (loc, rest) = span isDigit cs

parseNumber :: String -> [Token]
parseNumber cs = Number (read num) : lexer rest
    where (num, rest) = span isDigit cs

--lexer "L2 :=1; L3 :=0; while -( L2 = L2 ) do L2 := L2 +1; L3 := L3 +1"
--lexer " if -( true & false ) then skip else skip "

data Content = T Token | S | C | B | E deriving Show
type Input = [Token]
type Stack = [Content]

parserAux :: Input -> Stack -> Bool
parserAux [] [] = True

parserAux (Assign:xs) (T Assign:ys) = parserAux xs ys
parserAux (If:xs) (T If:ys) = parserAux xs ys
parserAux (Then:xs) (T Then:ys) = parserAux xs ys
parserAux (Else:xs) (T Else:ys) = parserAux xs ys
parserAux (Seq:xs) (T Seq:ys) = parserAux xs ys
parserAux (While:xs) (T While:ys) = parserAux xs ys
parserAux (Do:xs) (T Do:ys) = parserAux xs ys
parserAux (Skip:xs) (T Skip:ys) = parserAux xs ys
parserAux (Equal:xs) (T Equal:ys) = parserAux xs ys
parserAux (And:xs) (T And:ys) = parserAux xs ys
parserAux (Not:xs) (T Not:ys) = parserAux xs ys
parserAux (LP:xs) (T LP:ys) = parserAux xs ys
parserAux (RP:xs) (T RP:ys) = parserAux xs ys
parserAux (Sum:xs) (T Sum:ys) = parserAux xs ys
parserAux (Boolean True:xs) (T (Boolean True):ys) = parserAux xs ys
parserAux (Boolean False:xs) (T (Boolean False):ys) = parserAux xs ys
parserAux (Loc n:xs) (T (Loc m):ys) = parserAux xs ys
parserAux (Number n:xs) (T (Number m):ys) = parserAux xs ys

parserAux (If:xs) (S:ys) = parserAux (If:xs) (C:ys)
parserAux (If:xs) (C:ys) = parserAux (If:xs) (T If:B:T Then:C:T Else:C:ys)
parserAux (Loc n:xs) (S:ys) = parserAux (Loc n:xs) (C:ys)
parserAux (Loc n:xs) (C:ys) = parserAux (Loc n:xs) (T (Loc n):T Assign:E:ys)
parserAux (Loc n:xs) (B:ys) = parserAux (Loc n:xs) (E:T Equal:E:ys)
parserAux (Loc n:xs) (E:ys) = parserAux (Loc n:xs) (T (Loc n):ys)
parserAux (Number n:xs) (B:ys) = parserAux (Number n:xs) (E:T Equal:E:ys)
parserAux (Number n:xs) (E:ys) = parserAux (Number n:xs) (T (Number n):ys)
parserAux (LP:xs) (S:ys) = parserAux (LP:xs) (C:ys)
parserAux (LP:xs) (C:ys) = parserAux (LP:xs) (T LP:C:T Seq:C:T RP:ys)
parserAux (LP:xs) (B:ys) = parserAux (LP:xs) (E:T Equal:E:ys)
parserAux (LP:xs) (E:ys) = parserAux (LP:xs) (T LP:E:T Sum:E:T RP:ys)
parserAux (While:xs) (S:ys) = parserAux (While:xs) (C:ys)
parserAux (While:xs) (C:ys) = parserAux (While:xs) (T While:B:T Do:C:ys)
parserAux (Skip:xs) (S:ys) = parserAux (Skip:xs) (C:ys)
parserAux (Skip:xs) (C:ys) = parserAux (Skip:xs) (T Skip:ys)
parserAux (Boolean True:xs) (B:ys) = parserAux (Boolean True:xs) (T (Boolean True):ys)
parserAux (Boolean False:xs) (B:ys) = parserAux (Boolean False:xs) (T (Boolean False):ys)
parserAux (And:xs) (B:ys) = parserAux (And:xs) (T And:B:B:ys)
parserAux (Not:xs) (B:ys) = parserAux (Not:xs) (T Not:B:ys)

parserAux (x:xs) _ = False


parser :: Input -> Bool
parser input = parserAux input [S]