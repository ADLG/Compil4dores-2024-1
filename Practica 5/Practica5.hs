{-
P R A C T I C A  5
Integrantes:
- DJLP
- ADLG
- JGJMP
-}

module Practica5 where
    
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
data State = Q Int deriving Show

type Input = [Token]
type Stack = [State]
type Symbols = [Content]

parserAux :: Input -> Stack -> Symbols -> Bool
parserAux [] [Q 0] [] = True
parserAux (t:ts) (Q 0:stack) symbols = case t of
    Loc _ -> parserAux ts (Q 1 : Q 2 : stack) (C : symbols)
    If -> parserAux ts (Q 7 : stack) (B : symbols)
    Skip -> parserAux ts (Q 0 : stack) (S : symbols)
    _ -> False
parserAux (t:ts) (Q 1:stack) (C:symbols) = case t of
    Assign -> parserAux ts (Q 6 : stack) (E : symbols)
    _ -> False
parserAux (t:ts) (Q 2:stack) (C:symbols) = case t of
    Seq -> parserAux ts (Q 3 : stack) (C : symbols)
    Do -> parserAux ts (Q 5 : stack) (B : symbols)
    _ -> False
parserAux (t:ts) (Q 3:stack) (C:symbols) = case t of
    Loc _ -> parserAux ts (Q 1 : Q 2 : stack) (C : symbols)
    If -> parserAux ts (Q 7 : stack) (B : symbols)
    Skip -> parserAux ts (Q 0 : stack) (S : symbols)
    _ -> False
parserAux (t:ts) (Q 5:stack) (B:symbols) = case t of
    Not -> parserAux ts (Q 7 : stack) (B : symbols)
    _ -> False
parserAux (t:ts) (Q 6:stack) (E:symbols) = case t of
    Loc _ -> parserAux ts (Q 4 : stack) (E : symbols)
    Number _ -> parserAux ts (Q 4 : stack) (E : symbols)
    _ -> False
parserAux (t:ts) (Q 4:stack) (E:symbols) = case t of
    Seq -> parserAux ts (Q 3 : stack) (C : symbols)
    Do -> parserAux ts (Q 5 : stack) (B : symbols)
    _ -> False
parserAux (t:ts) (Q 7:stack) (B:symbols) = case t of
    Boolean _ -> parserAux ts (Q 8 : stack) (B : symbols)
    And -> parserAux ts (Q 9 : stack) (B : B : symbols)
    _ -> False
parserAux (t:ts) (Q 8:stack) (B:symbols) = case t of
    Seq -> parserAux ts (Q 3 : stack) (C : symbols)
    Do -> parserAux ts (Q 5 : stack) (B : symbols)
    _ -> False
parserAux (t:ts) (Q 9:Q 5:stack) (B:B:symbols) = case t of
    Boolean _ -> parserAux ts (Q 8 : stack) (B : symbols)
    And -> parserAux ts (Q 9 : stack) (B : B : symbols)
    _ -> False
parserAux _ _ _ = True

-- parserAux [ Loc 2 , Assign , Number 1 , Seq , Loc 3 , Assign , Number 0 , Seq , While , Not , Loc 2 , Equal , Loc 2 , Do , Loc 2 , Assign , LP , Loc 2 , Sum , Number 1 , Seq , Loc 3 , Assign , LP , Loc 3 , Sum , Number 1] [ Q 0] []
-- parserAux [ Loc 1 , Assign , Number 1 , Seq , Loc 2 , Assign , Number 2 , Seq , Skip ] [ Q 0] []

parser :: Input -> Bool
parser input = parserAux input [Q 0] []

-- parser [ Loc 2 , Assign , Number 1 , Seq , Loc 3 , Assign , Number 0 , Seq , While , Not , Loc 2 , Equal , Loc 2 , Do , Loc 2 , Assign , LP , Loc 2 , Sum , Number 1 , Seq , Loc 3 , Assign , LP , Loc 3 , Sum , Number 1]
-- parser [ Loc 1 , Assign , Number 1 , Seq , Loc 2 , Assign , Number 2 , Seq , Skip ]
