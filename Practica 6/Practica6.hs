{-
P R A C T I C A  6
Integrantes:
- DJLP
- ADLG
- JGJMP
-}

module Practica6 where

data ASA = Assign ASA ASA
         | IfThenElse ASA ASA ASA
         | Seq ASA ASA
         | WhileDo ASA ASA
         | Skip
         | Boolean Bool
         | Equal ASA ASA
         | And ASA ASA
         | Not ASA
         | Loc Int
         | Number Int
         | Sum ASA ASA
         deriving Show

data Type = Num | Bool | Void deriving (Show, Eq)

typeCheckerAux :: ASA -> Type
typeCheckerAux (Sum e1 e2) =
    case (typeCheckerAux e1, typeCheckerAux e2) of
        (Num, Num) -> Num
        _          -> error "La operación de suma debe tener operandos de tipo Num"
typeCheckerAux (Equal e1 e2) =
    case (typeCheckerAux e1, typeCheckerAux e2) of
        (Num, Num) -> Bool
        _          -> error "La operación de igualdad debe tener operandos de tipo Num"
typeCheckerAux (And b1 b2) =
    case (typeCheckerAux b1, typeCheckerAux b2) of
        (Bool, Bool) -> Bool
        _            -> error "La operación AND debe tener operandos de tipo Bool"
typeCheckerAux (Not b) =
    case typeCheckerAux b of
        Bool -> Bool
        _    -> error "La operación NOT debe tener un operando de tipo Bool"
typeCheckerAux (Loc _) = Num
typeCheckerAux (Number _) = Num
typeCheckerAux Skip = Void
typeCheckerAux (Assign _ _) = Void
typeCheckerAux (IfThenElse cond thenBranch elseBranch) =
    case typeCheckerAux cond of
        Bool ->
            let thenType = typeCheckerAux thenBranch
                elseType = typeCheckerAux elseBranch
            in
                if thenType == elseType
                then thenType
                else error $ "El tipo de (" ++ show cond ++ ") no es el esperado"
        _    -> error $ "El tipo de (" ++ show cond ++ ") no es el esperado"
typeCheckerAux (Seq c1 c2) =
    case (typeCheckerAux c1, typeCheckerAux c2) of
        (_, Void) -> typeCheckerAux c1
        _         -> error "La secuencia de comandos debe tener un segundo comando de tipo Void"
typeCheckerAux (WhileDo cond body) =
    case typeCheckerAux cond of
        Bool ->
            if typeCheckerAux body == Void
            then Void
            else error "El cuerpo del bucle while debe tener tipo Void"
        _    -> error "La condición en la instrucción while debe tener tipo Bool"

-- typeCheckerAux ( Sum ( Loc 2) ( Number 5) )
-- typeCheckerAux ( IfThenElse ( Number 2) ( Not ( Number 5) ) ( Skip ) )


typeChecker :: ASA -> ASA
typeChecker program =
    case typeCheckerAux program of
        Void -> program
        _    -> error "Las expresiones aritmeticas no son programas validos en el lenguaje"

-- typeChecker ( Sum ( Loc 2) ( Number 5) )
-- typeChecker ( IfThenElse ( Number 2) ( Not ( Number 5) ) ( Skip ) )