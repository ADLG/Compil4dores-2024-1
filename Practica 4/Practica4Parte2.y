{
module Practica4Parte2 where

import Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
      bool            { Boolean $$ }
      L               { Loc $$ }
      int             { Number $$ }
      Assign          { Assign }
      If              { If }
      Then            { Then }
      Else            { Else }
      Seq             { Seq }
      While           { While }
      Do              { Do }
      Skip            { Skip }
      Equal           { Equal }
      And             { And }
      Not             { Not }
      LP              { LP }
      RP              { RP }
      Sum             { Sum }

%%

C     : E Assign E            { AssignASA $1 $3 }
      | If B Then C Else C    { IfThenElse $2 $4 $6 }
      | LP C Seq C RP         { SeqASA $2 $4 }
      | While B Do C          { WhileDo $2 $4 }
      | Skip                  { SkipASA }

B     : bool                  { BoolASA $1 }
      | E Equal E             { EqualASA $1 $3 }
      | And B B               { AndASA $2 $3 }
      | Not B                 { NotASA $2 }

E     : L                     { LocASA $1 }
      | int                   { NumberASA $1 }
      | LP E Sum E RP         { SumASA $2 $4 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token = Assign
          | If 
          | Then 
          | Else 
          | Seq 
          | While 
          | Do 
          | Skip 
          | Boolean Bool 
          | Equal 
          | And 
          | Not 
          | Loc Int 
          | Number Int 
          | LP 
          | RP 
          | Sum deriving Show

data Content = T Token | S | C | B | E deriving Show
type Input = [Token]
type Stack = [Content]

data C = AssignASA E E | IfThenElse B C C | SeqASA C C | WhileDo B C | SkipASA deriving Show
data B = BoolASA Bool | EqualASA E E | AndASA B B | NotASA B deriving Show
data E = LocASA Int | NumberASA Int | SumASA E E deriving Show

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

main = getContents >>= print . parse . lexer

}
