module Lab1 where
import Data.Sequence (ViewL)

type Var = String
type Con = String
type Rama = (Con, Exp)
type Sust = [(Var, Exp)]

data Exp = Variable Var
        | Const Con
        | Funcion [Var] Exp
        | Aplicacion Exp [Exp]
        | Case Exp [Rama]
        | Rec Var Exp deriving Show

data Val = ValConst Con [Val] | Lambda [Var] Exp deriving Show

sustitucion :: Sust -> Exp -> Exp
sustitucion susts (Variable x)  = busqueda susts x
sustitucion susts (Const c) = Const c
sustitucion susts (Funcion xs e) = Funcion xs (sustitucion (bajas susts xs) e)
sustitucion susts (Aplicacion e es) = Aplicacion (sustitucion susts e) (map (sustitucion susts) es)
sustitucion susts (Case e rs) = Case (sustitucion susts e) (sustitucionEnListaRamas susts rs)
sustitucion susts (Rec x e) = Rec x (sustitucion (bajas susts [x]) e)

---------------------------------------------------------------------------------
-- Auxiliares de sustitución

busqueda :: Sust -> Var -> Exp
busqueda s x = case lookup x s of {
    Just e -> e;
    Nothing -> Variable x;
}

bajas :: Sust -> [Var] -> Sust
bajas = foldl bajaUnElem

bajaUnElem :: Sust -> Var -> Sust
bajaUnElem [] x = []
bajaUnElem ((var, exp):es) x
    | x == var = es
    | otherwise = (var, exp):bajaUnElem es x

sustitucionEnListaRamas :: Sust -> [Rama] -> [Rama]
sustitucionEnListaRamas [] listaRamas = listaRamas
sustitucionEnListaRamas sust ((c, e):rs) = sustitucionEnUnaRama sust (c, e):sustitucionEnListaRamas sust rs

sustitucionEnUnaRama :: Sust -> Rama -> Rama
sustitucionEnUnaRama [] r = r
sustitucionEnUnaRama sust (c, e) = (c, sustitucion sust e)

---------------------------------------------------------------------------------
-- Función de evaluación

eval :: Exp -> Val
eval (Const c) = ValConst c []
eval (Funcion xs e) = Lambda xs e
eval (Aplicacion exp es) = case eval exp of {
    ValConst c vs -> case map eval es of {
                        v2 -> ValConst c (vs ++ v2)
                    };
    Lambda vs expLambda -> case map eval es of {
        [] -> Lambda vs expLambda;
        (x:xs) -> (if length vs == length es then eval (sustitucion (listaVariablesYValoresAListaDePares vs (map eval es)) expLambda) else error "La cantidad de variables es distinta a la de expresiones")
    }
};
eval (Case exp ramas) = case eval exp of {
    ValConst c valores -> case lkupRama c ramas of {
        e' -> eval (Aplicacion e' (map valoresAExpresiones valores));
    }
}
eval (Rec var exp) = eval (sustitucion [(var, Rec var exp)] exp)

listaVariablesYValoresAListaDePares :: [Var] -> [Val] -> [(Var, Exp)]
listaVariablesYValoresAListaDePares [] xs = []
listaVariablesYValoresAListaDePares xs [] = []
listaVariablesYValoresAListaDePares (vr:vrs) (vl:vls) = (vr, valoresAExpresiones vl):listaVariablesYValoresAListaDePares vrs vls

valoresAExpresiones :: Val -> Exp
valoresAExpresiones (ValConst c vs) = Aplicacion (Const c) (map valoresAExpresiones vs)
valoresAExpresiones (Lambda vs exp) = Funcion vs exp

lkupRama :: Con -> [Rama] -> Exp
lkupRama c [] = error "no se encontro constructor en los casos"
lkupRama c ((cons, exp):rs) = if c == cons then exp else lkupRama c rs

---------------------------------------------------------------------------------

--------------------------------
-- Funciones en Chi
--------------------------------

iguales :: Exp
iguales =
    Funcion ["b1", "b2"]
    (Case (Variable "b1") [
        ("True",
            Case (Variable "b2") [
                ("True", Const "True"),
                ("False", Const "False")
            ]
        ),
        ("False",
            Case (Variable "b2") [
                ("True", Const "False"),
                ("False", Const "True")
            ]
        )
    ])

suma :: Exp
suma =
    Rec "suma" (
        Funcion ["n1", "n2"]
        (Case (Variable "n1") [
            ("O", Variable "n2"),
            ("S", Funcion ["x"] (
                    Aplicacion (Const "S") [Aplicacion suma [Variable "x", Variable "n2"]]
            ))
        ])
    )

sumi :: Exp
sumi =
    Rec "sumi" (
        Funcion ["n"]
        (Case (Variable "n") [
            ("O", Const "O"),
            ("S", Funcion ["x"] (
                    Aplicacion suma [Variable "n", Aplicacion sumi [Variable "x"]]
            ))
        ])
    )

unir :: Exp
unir =
    Rec "unir" (
        Funcion ["l1", "l2"] (
            Case (Variable "l1") [
                ("[]", Variable "l2"),
                (":", Funcion ["x", "xs"] (
                    Aplicacion (Const ":") [Variable "x", Aplicacion unir [Variable "xs", Variable "l2"]]
                ))
    ]))

inOrder :: Exp
inOrder =
    Rec "inOrder" (
        Funcion ["t"] (
            Case (Variable "t") [
                ("Hoja", Const "[]"),
                ("Nodo", Funcion ["i", "x", "d"] (
                    Aplicacion unir [
                        Aplicacion inOrder [Variable "i"],
                        Aplicacion (Const ":") [Variable "x", Aplicacion inOrder [Variable "d"]]
                    ]
                ))
    ]))

---------------------------------------------------------------------------------

--------------------------------
-- Pruebas
--------------------------------

------------------
-- Pruebas suma y sumi
------------------
-- Valores
cero :: Exp
cero = Const "O"

uno :: Exp
uno = Aplicacion (Const "S") [Const "O"]

dos :: Exp
dos = Aplicacion (Const "S") [uno]

tres :: Exp
tres = Aplicacion (Const "S") [dos]

cuatro :: Exp
cuatro = Aplicacion (Const "S") [tres]

ocho :: Exp
ocho = Aplicacion suma [cuatro, cuatro]

-- Evaluaciones suma
sumaCeroCero :: Val
sumaCeroCero = eval (Aplicacion suma [cero, cero])

sumaCeroTres :: Val
sumaCeroTres = eval (Aplicacion suma [cero, tres])

sumaUnoDos :: Val
sumaUnoDos = eval (Aplicacion suma [uno, dos])

-- Evaluaciones sumi
sumiCero :: Val
sumiCero = eval (Aplicacion sumi [cero])

sumiDos :: Val
sumiDos = eval (Aplicacion sumi [dos])

sumiTres :: Val
sumiTres = eval (Aplicacion sumi [tres])

sumiOcho :: Val
sumiOcho = eval (Aplicacion sumi [ocho]) 

------------------
-- Pruebas unir
------------------
-- Valores
listaVacia :: Exp
listaVacia = Const "[]"

listaUno :: Exp
listaUno = Aplicacion (Const ":") [uno, Const "[]"]

listaDos :: Exp
listaDos = Aplicacion (Const ":") [cero, Aplicacion (Const ":") [cero, listaVacia]]

-- Evaluaciones
listaVaciaYListaVacia :: Val
listaVaciaYListaVacia = eval (Aplicacion unir [listaVacia, listaVacia])

listaVaciaYListaDos :: Val
listaVaciaYListaDos = eval (Aplicacion unir [listaVacia, listaDos])

listaDosYListaDos :: Val
listaDosYListaDos = eval (Aplicacion unir [listaDos, listaDos])
------------------
-- Pruebas inOrder
------------------
-- Valores
arbolHoja :: Exp
arbolHoja = Const "Hoja"

arbolUnNodo :: Exp
arbolUnNodo = Aplicacion (Const "Nodo") [arbolHoja, Const "9", arbolHoja]

arbolDosNodos :: Exp
arbolDosNodos = Aplicacion (Const "Nodo") [arbolHoja, Const "3", arbolUnNodo]

arbolTresNodos :: Exp
arbolTresNodos = Aplicacion (Const "Nodo") [arbolHoja, Const "5", arbolDosNodos]

-- Evaluaciones
arbolHojaInOrder :: Val
arbolHojaInOrder = eval (Aplicacion inOrder [arbolHoja])

arbolUnNodoInOrder :: Val
arbolUnNodoInOrder = eval (Aplicacion inOrder [arbolUnNodo])

arbolDosNodosInOrder :: Val
arbolDosNodosInOrder = eval (Aplicacion inOrder [arbolDosNodos])

arbolTresNodosInOrder :: Val
arbolTresNodosInOrder = eval (Aplicacion inOrder [arbolTresNodos])

arbolHojaInOrderError :: Val
arbolHojaInOrderError = eval (Aplicacion inOrder [arbolHoja, arbolHoja])


--convierte una lista de valores a un arbol implementado en chi
tree2chi :: Show a => [a] -> Exp
tree2chi [] = Const "Hoja"
tree2chi (x:xs) = Aplicacion (Const "Nodo") [tree2chi xs, Const (show x), tree2chi xs]
