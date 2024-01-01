-- P R A C T I C A  3 | PUNTO 2.
-- Integrantes:
-- - DJLP
-- - ADLG
-- - JGJMP
{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+                        ;
  "--".*                         ;
  ":="                           { \s -> Assign }
  ";"                            { \s -> Seq }
  $digit+                        { \s -> Number (read s) }
  "L" $digit+                    { \s -> Loc (read (tail s) :: Int) }
  "true"                         { \s -> Boolean True }
  "false"                        { \s -> Boolean False }
  "if"                           { \s -> If }
  "then"                         { \s -> Then }
  "else"                         { \s -> Else }
  "while"                        { \s -> While }
  "do"                           { \s -> Do }
  "skip"                         { \s -> Skip }
  "="                            { \s -> Equal }
  "&"                            { \s -> And }
  "+"                            { \s -> Sum }
  "-"                            { \s -> Not }
  "("                            { \s -> LP }
  ")"                            { \s -> RP }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = Number Int
  | Loc Int
  | Boolean Bool
  | Assign
  | If
  | Then
  | Else
  | Seq
  | While
  | Do
  | Skip
  | Equal
  | And
  | Not
  | LP
  | RP
  | Sum
  deriving (Eq, Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
