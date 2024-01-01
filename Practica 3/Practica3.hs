{-
P R A C T I C A  3 | PUNTO 1.
Integrantes:
- DJLP
- ADLG
- JGJMP
-}

module Practica3 where
    
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
lexer ('s':'k':'i':'p':rest) = Skip : lexer rest
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
