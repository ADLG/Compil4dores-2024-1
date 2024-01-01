{-
P R A C T I C A  2
Integrantes:
- DJLP
- ADLG
- JGJMP
-}

module Practica2 where

import Data.List
import Data.Char


{------------------------------------------- Análisis Léxico -------------------------------------------}
data Token = Var String | Number Int | Boolean Bool | Sum | Subs | And | Or | Equal deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('t' : rstr) = Boolean True : lexer rstr
lexer ('f' : rstr) = Boolean False : lexer rstr
lexer ('+' : rstr) = Sum : lexer rstr
lexer ('-' : rstr) = Subs : lexer rstr
lexer ('&' : '&' : rstr) = And : lexer rstr
lexer ('|' : '|' : rstr) = Or : lexer rstr
lexer ('=' : '=' : rstr) = Equal : lexer rstr
lexer (str : rstr) | isSpace str = lexer rstr
lexer (x:xs)
    | isDigit x = lexNum(x:xs)
    | isAlpha x = lexAlph(x:xs)

lexNum :: String -> [Token]
lexNum cs = Number (read num): lexer rest
      where (num,rest) = span isDigit cs

lexAlph :: String -> [Token]
lexAlph cs = Var var : lexer rest
      where (var,rest) = span isAlpha cs

--lexer " 22 3 + var == t && "
--lexer " 22 + var var f t == && "


{------------------------------------------- Análisis Sintáctico -------------------------------------------}
data ASA = VarASA String | NumberASA Int | BooleanASA Bool | Op Token ASA ASA deriving Show
type Stack = [ASA]

scanner :: [Token] -> ASA
scanner (x:xs) = scannerAux (x:xs) []

--scanner [ Number 22 , Number 3 , Sum , Var " var " , Equal , Boolean True , And ]
--scanner [ Number 22 , Sum , Var " var " , Var " var " , Boolean False , Boolean True , Equal , And ]


scannerAux :: [Token] -> Stack -> ASA
scannerAux [Var s] [] = VarASA s
scannerAux [Number s] [] = NumberASA s
scannerAux [Boolean s] [] = BooleanASA s
scannerAux (Var s:xs) [] = scannerAux xs [VarASA s]
scannerAux (Var s:xs) (b:stk) = scannerAux xs (VarASA s : b : stk)
scannerAux (Number a:xs) [] = scannerAux xs [NumberASA a]
scannerAux (Number a:xs) (b:stk) = scannerAux xs (NumberASA a : b : stk)
scannerAux (Boolean a:xs) [] = scannerAux xs [BooleanASA a]
scannerAux (Boolean a:xs) (b:stk) = scannerAux xs (BooleanASA a : b : stk)
scannerAux (Sum:xs) (a:b:stk) = scannerAux xs (Op Sum a b : stk)
scannerAux (Subs:xs) (a:b:stk) = scannerAux xs (Op Subs a b : stk)
scannerAux (And:xs) (a:b:stk) = scannerAux xs (Op And a b : stk)
scannerAux (Or:xs) (a:b:stk) = scannerAux xs (Op Or a b : stk)
scannerAux (Equal:xs) (a:b:stk) = scannerAux xs (Op Equal a b : stk)
scannerAux [] [result] = result
scannerAux _ _ = error "Expresion mal formada."

--scannerAux [ Number 22 , Number 3 , Sum , Var " var " , Equal , Boolean True , And ] []
--scannerAux [ Number 22 , Sum , Var " var " , Var " var " , Boolean False , Boolean True , Equal , And ] []



{------------------------------------------- Análisis Semántico -------------------------------------------}
data Type = Num | Bool deriving Show

instance Eq Type where
    Num == Num = True
    Bool == Bool = True
    _ == _ = False

typeCheckerAux :: ASA -> Type
typeCheckerAux (VarASA _) = Num 
typeCheckerAux (NumberASA _) = Num
typeCheckerAux (BooleanASA _) = Bool
typeCheckerAux (Op token a b) = case token of
    Sum -> checkBinaryOp Num a b
    Subs -> checkBinaryOp Num a b
    And -> checkBinaryOp Bool a b
    Or -> checkBinaryOp Bool a b
    Equal -> checkEqualOp a b
  where
    checkBinaryOp expectedType left right =
        let leftType = typeCheckerAux left
            rightType = typeCheckerAux right
        in if leftType == expectedType && rightType == expectedType
            then expectedType
            else error $ "El tipo de los argumentos " ++ show left ++ " y " ++ show right ++
                         " no son los esperados para el operador " ++ show token
    checkEqualOp left right =
        let leftType = typeCheckerAux left
            rightType = typeCheckerAux right
        in if leftType == rightType
            then Bool
            else error $ "El tipo de los argumentos " ++ show left ++ " y " ++ show right ++
                         " no son iguales para el operador Equal"

--typeCheckerAux ( Op And ( BooleanASA True ) ( Op Equal ( VarASA " var " ) ( Op Sum ( NumberASA 3) ( NumberASA 22) ) ) )
--typeCheckerAux ( Op And ( NumberASA 43) ( Op Equal ( VarASA " var " ) ( Op Sum ( NumberASA 3) ( NumberASA 22) ) ) )


typeChecker :: ASA -> ASA
typeChecker (VarASA s) = VarASA s  
typeChecker (NumberASA n) = NumberASA n  
typeChecker (BooleanASA b) = BooleanASA b 
typeChecker (Op token a b) =
    let a' = typeChecker a
        b' = typeChecker b
    in case token of
        And -> case (typeCheckerAux a', typeCheckerAux b') of
                  (Bool, Bool) -> Op token a' b'
                  _ -> error $ "El tipo de los argumentos " ++ show a' ++ " y " ++ show b' ++
                               " no son los esperados para el operador " ++ show token
        Or -> case (typeCheckerAux a', typeCheckerAux b') of
                 (Bool, Bool) -> Op token a' b'
                 _ -> error $ "El tipo de los argumentos " ++ show a' ++ " y " ++ show b' ++
                              " no son los esperados para el operador " ++ show token
        Equal -> case (typeCheckerAux a', typeCheckerAux b') of
                    (Num, Num) -> Op token a' b'
                    (Bool, Bool) -> Op token a' b'
                    _ -> error $ "El tipo de los argumentos " ++ show a' ++ " y " ++ show b' ++
                                 " no son los esperados para el operador " ++ show token
        Sum -> case (typeCheckerAux a', typeCheckerAux b') of
                  (Num, Num) -> Op token a' b'
                  _ -> error $ "El tipo de los argumentos " ++ show a' ++ " y " ++ show b' ++
                               " no son los esperados para el operador " ++ show token
        Subs -> case (typeCheckerAux a', typeCheckerAux b') of
                   (Num, Num) -> Op token a' b'
                   _ -> error $ "El tipo de los argumentos " ++ show a' ++ " y " ++ show b' ++
                                " no son los esperados para el operador " ++ show token

--typeChecker ( Op And ( BooleanASA True ) ( Op Equal ( VarASA " var " ) ( Op Sum ( NumberASA 3) ( NumberASA 22) ) ) )
--typeChecker ( Op And ( NumberASA 43) ( Op Equal ( VarASA " var " ) ( Op Sum ( NumberASA 3) ( NumberASA 22) ) ) )



{------------------------------------------- Optimización de Código Fuente -------------------------------------------}
data Value = N Int | B Bool | S String
instance Show Value where
  show (N n) = show n
  show (B b) = show b
  show (S s) = show s

data ThreeAddress = Assign String Value | Operation String String Token String
instance Show ThreeAddress where
    show (Assign t v) = show t ++ " = " ++ show v
    show (Operation t a op b) = show t ++ " = " ++ show a ++ tokenThreeAddress op ++ show b

tokenThreeAddress :: Token -> String
tokenThreeAddress Sum = " + "
tokenThreeAddress Subs = " - "
tokenThreeAddress And = " && "
tokenThreeAddress Or = " || "
tokenThreeAddress Equal = " == "

constantFolding :: ASA -> ASA
constantFolding (VarASA s) = VarASA s
constantFolding (NumberASA n) = NumberASA n
constantFolding (BooleanASA b) = BooleanASA b
constantFolding (Op op a b) = aplica op (map constantFolding [a,b])

aplica :: Token -> [ASA] -> ASA
aplica Sum [(VarASA a),b] = (Op Sum (VarASA a) b)
aplica Sum [b,(VarASA a)] = (Op Sum b (VarASA a))
aplica Subs [(VarASA a),b] = (Op Subs (VarASA a) b)
aplica Subs [b,(VarASA a)] = (Op Subs b (VarASA a))

aplica Sum l = NumberASA (foldr1 (+) (map extraeI l))
aplica Subs l = NumberASA (foldr1 (-) (map extraeI l))

aplica Equal [(VarASA a),b] = Op Equal (VarASA a) b
aplica Equal [b,(VarASA a)] = Op Equal b (VarASA a)
aplica Equal l = BooleanASA (multiparam "=" (map extraeI l))

aplica And [BooleanASA True,xs] = xs
aplica And [x,BooleanASA True] = x
aplica And [x,xs] = Op And x xs

aplica Or [BooleanASA True,xs] = BooleanASA True
aplica Or [x,BooleanASA True] = BooleanASA True
aplica Or [x,xs] = Op Or x xs

extraeI :: ASA -> Int
extraeI (NumberASA n) = n

extraeB :: ASA -> Bool
extraeB (BooleanASA b) = b 

multiparam :: String -> [Int] -> Bool
multiparam _ [] = True
multiparam _ [x] = True
multiparam "=" (x:y:xs) = (x == y) && (multiparam "=" (y:xs))

--constantFolding ( Op And ( BooleanASA True ) ( Op Equal ( VarASA " var " ) ( Op Sum ( NumberASA 3) ( NumberASA 22) ) ) )
--constantFolding ( Op And ( Op Equal ( VarASA " var " ) ( VarASA " var " ) ) ( Op Equal ( VarASA " var " ) ( Op Sum ( VarASA " var " ) ( NumberASA 22) ) ) )


fresh :: [Int] -> Int
fresh xs = head $ [0..] \\ xs

--fresh [1 ,2 ,3]
--fresh [4 ,2 ,3 ,0]


threeAddressAux :: ASA -> [Int] -> ([ThreeAddress],String,[Int])
threeAddressAux (VarASA v) ts = (code,temp,i:ts)
 where
 i = fresh ts
 temp = "t" ++ show i
 code = [Assign temp (S v)]
threeAddressAux (NumberASA n) ts = (code,temp,i:ts)
 where
 i = fresh ts
 temp = "t" ++ show i
 code = [Assign temp (N n)]
threeAddressAux (BooleanASA b) ts = (code,temp,i:ts)
 where
 i = fresh ts
 temp = "t" ++ show i
 code = [Assign temp (B b)]
threeAddressAux (Op op a b) ts = (ai++ad++code,temp,i:idTder)
 where
 (ai,tempA,idTizq) = threeAddressAux a ts
 (ad,tempB,idTder) = threeAddressAux b idTizq
 i = fresh idTder
 temp = "t" ++ show i
 code = [Operation temp tempA op tempB]

threeAddress :: ASA -> [ThreeAddress]
threeAddress ast =
    let (code, _, _) = threeAddressAux ast []
    in code

--threeAddressAux ( Op Equal ( VarASA " var " ) ( NumberASA 25) ) []
--threeAddressAux ( Op Equal ( NumberASA 50) ( VarASA " var " ) ) []



{------------------------------------------- Generación de Código -------------------------------------------}
assembly :: [ThreeAddress] -> String
assembly code = unlines (map translateInstruction code)

assemblyConSapce :: [ThreeAddress] -> IO ()
assemblyConSapce code = putStrLn(assembly code)

translateInstruction :: ThreeAddress -> String
translateInstruction (Assign t v) = "MOV " ++ t ++ " " ++ showValue v 
translateInstruction (Operation t a op b) = case op of
    Sum    -> "ADD " ++ t ++ " " ++ a ++ " " ++ b
    Subs   -> "SUB " ++ t ++ " " ++ a ++ " " ++ b
    And    -> "AND " ++ t ++ " " ++ a ++ " " ++ b
    Or     -> "OR " ++ t ++ " " ++ a ++ " " ++ b
    Equal  -> "EQ " ++ t ++ " " ++ a ++ " " ++ b

showValue :: Value -> String
showValue (N n) = show n
showValue (B b) = if b then "True" else "False"
showValue (S s) = s

--assembly [Assign "t0" (S "var"), Assign "t1" (N 25), Operation "t2" "t0" Equal "t1"]
--assembly [Assign "t0" (N 50), Assign "t1" (S "var"), Operation "t2" "t0" Equal "t1"]



{------------------------------------------- Extra -------------------------------------------}
compile :: String -> String
compile input =
    let tokens = lexer input
        ast = scanner tokens
        foldedAst = constantFolding ast
        threeAddrCode = threeAddress foldedAst
    in assembly threeAddrCode

compileConSapce :: String -> IO ()
compileConSapce input = putStrLn(compile input)

--compile "22 3 + var == t &&"
