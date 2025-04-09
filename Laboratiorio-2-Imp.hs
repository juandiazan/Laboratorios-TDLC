type Var = String
type Con = String

data Exp = Variable Var | ConstEnExp Con [Exp] deriving Show
data Valor = Const Con [Valor] | Null deriving Show

type Prog = [Instruccion]
type Rama = (Con, [Var], Prog)
type Mem = [(Var, Valor)]

data Instruccion = Asig [Var] [Exp]
                 | Local [Var] Prog
                 | Case Var [Rama]
                 | While Var [Rama]
                 | Return Exp deriving Show

busqueda :: Mem -> Var -> Maybe Valor
busqueda [] x = Nothing
busqueda ((var, val):ms) x = if var == x then Just val else busqueda ms x

actualizar :: Mem -> [(Var, Valor)] -> Mem
actualizar m [] = m
actualizar [] (v:vs) = actualizar (alta [] (map fst (v:vs))) (v:vs)
actualizar (m:ms) (upd:upds) = if fst m == fst upd then upd:actualizar ms upds else m:actualizar ms (upd:upds)

alta :: Mem -> [Var] -> Mem
alta = foldr (\ v -> (:) (v, Null))

baja :: Mem -> [Var] -> Mem
baja m [] = m
baja [] v = []
baja mem (v:vs) = baja (bajaUnElemento mem v) vs

bajaUnElemento :: Mem -> Var -> Mem
bajaUnElemento [] x = []
bajaUnElemento ((var, val):ms) x
    | x == var = ms
    | otherwise = (var, val):bajaUnElemento ms x

eval :: Mem -> Exp -> Valor
eval m (Variable x) = case busqueda m x of {
    Just v -> v;
    Nothing -> Null
}
eval m (ConstEnExp c exps) = Const c (map (eval m) exps)

ejecutar :: Mem -> Prog -> Mem
ejecutar mem [] = mem

ejecutar mem ((Asig variables expresiones):ps) =
    case map (eval mem) expresiones of {
        valores -> ejecutar (actualizar mem (zip variables (map (eval mem) expresiones))) ps
    };

ejecutar mem ((Local variables prog):ps) =
    case ejecutar (alta mem variables) prog of{
        m' -> ejecutar (baja m' variables) ps
    };

ejecutar mem ((Case var ramas):ps) =
    case eval mem (Variable var) of {
        (Const con valores) ->
        case buscarEnRama con ramas of {
            Just (variables, prog) ->
                if length variables == length valores
                then ejecutar mem (Local variables (Asig variables (map v2e valores):prog):ps)
                else error "largo de variables distinto al de valores";
            Nothing -> ejecutar mem ps;
        };
        Null -> error "no se recibio rama valida"
    };

ejecutar mem ((While var ramas):ps) =
    case eval mem (Variable var) of {
        Null -> ejecutar mem ps;
        (Const con valores) ->
        case buscarEnRama con ramas of {
            Just (variables, prog) ->
                if length variables == length valores
                then ejecutar mem (Local variables (Asig variables (map v2e valores):prog):While var ramas:ps)
                else error "largo de variables distinto al de valores";
            Nothing -> ejecutar mem ps;
        };
    };

ejecutar mem ((Return exp):ps) =
    case eval mem exp of {
    valor -> actualizar mem [("result", valor)]
}

v2e :: Valor -> Exp
v2e (Const c []) = ConstEnExp c []
v2e (Const c v) = ConstEnExp c (map v2e v)

buscarEnRama :: Con -> [Rama] -> Maybe ([Var], Prog)
buscarEnRama c [] = Nothing
buscarEnRama c ((const, vars, prog):rs) = if c == const then Just (vars, prog) else buscarEnRama c rs

------------------------------------------------------------------------------------------------
--
-- Funciones
--
------------------------------------------------------------------------------------------------
par :: Var -> Prog
par n =
    [
        Local ["n'"] [
            Asig ["n'", "result"] [Variable n, ConstEnExp "True" []],
            While "n'"
            [
                ("S", ["x"], [
                    Asig ["n'"] [Variable "x"],
                    Case "result" [
                        ("True", [], [Return (ConstEnExp "False" [])]),
                        ("False", [], [Return (ConstEnExp "True" [])])
                    ],
                    Return (Variable "result")
                ])
            ],
            Return (Variable "result")
        ],
        Return (Variable "result")
    ]

suma :: Var -> Var -> Prog
suma m n =
    [
        Asig ["result"] [Variable n],
        Local ["m'", "n'"] [
            Asig ["m'"] [Variable m],
            While "m'" [
                ("S", ["x"], [
                    Asig ["m'"] [Variable "x"],
                    Return (ConstEnExp "S" [Variable "result"])
                ])
            ],
            Return (Variable "result")
        ],
        Return (Variable "result")
    ]

largo :: Var -> Prog
largo l = 
    [
        Local ["l'"] [
            Asig ["l'", "result"] [Variable l, ConstEnExp "O" []],
            While "l'" [
                (":", ["x", "xs"], [
                    Asig ["l'", "result"] [Variable "xs", ConstEnExp "S" [Variable "result"]],
                    Return (Variable "result")
                ])
            ],
            Return (Variable "result")
        ],
        Return (Variable "result")
    ]

igualdadN :: Var -> Var -> Prog
igualdadN m n =
    [
        Local ["m'", "n'", "iter"] [
            Asig ["m'", "n'", "iter", "result"] [Variable m, Variable n, ConstEnExp "True" [], ConstEnExp "True" []],
            While "iter" [
                ("True", [], [
                    Case "m'" [
                        ("O", [], [
                            Case "n'" [
                                ("O", [], [
                                    Asig ["iter", "result"] [ConstEnExp "False" [], ConstEnExp "True" []],
                                    Return (Variable "result")
                                ]),
                                ("S", ["x"], [
                                    Asig ["iter", "result"] [ConstEnExp "False" [], ConstEnExp "False" []],
                                    Return (Variable "result")
                                ])
                            ],
                            Return (Variable "result")
                        ]),
                        ("S", ["x"], [
                            Case "n'" [
                                ("O", [], [
                                    Asig ["iter", "result"] [ConstEnExp "False" [], ConstEnExp "False" []],
                                    Return (Variable "result")
                                ]),
                                ("S", ["y"], [
                                    Asig ["m'", "n'", "result"] [Variable "x", Variable "y", ConstEnExp "False" []],
                                    Return (Variable "result")
                                ])
                            ],
                            Return (Variable "result")
                        ])
                    ],
                    Return (Variable "result")
                ])
            ],
            Return (Variable "result")
        ],
        Return (Variable "result")
    ]

concatenar :: Var -> Var -> Prog
concatenar l1 l2 =
    [
        Local ["l1'"] [
            Asig ["l1'", "result"] [Variable l1, Variable l2],
            While "l1'" [
                (":", ["x", "xs"], [
                    Asig ["l1'", "result"] [Variable "xs", ConstEnExp ":" [Variable "x", Variable "result"]],
                    Return (Variable "result")
                ])
            ],
            Return (Variable "result")
        ],
        Return (Variable "result")
    ]

------------------------------------------------------------------------------------------------
--
-- Datos de prueba
--
------------------------------------------------------------------------------------------------
m :: Mem
m = [
    ("cero",Const "O" []),
    ("uno", Const "S" [Const "O" []]),
    ("dos", Const "S" [Const "S" [Const "O" []]]),
    ("tres", Const "S" [Const "S" [Const "S" [Const "O" []]]]),
    ("T", Const "True" []),
    ("F", Const "False" []),
    ("l1", Const ":" [Const "True" [], Const "[]" []]),
    ("l2", Const ":" [Const "False" [], Const ":" [Const "True" [], Const "[]" []]]),
    ("l3", Const ":" [Const "False" [], Const ":" [Const "True" [], Const ":" [Const "False" [], Const "[]" []]]])
    ]

ejecutarTest :: Mem -> Valor
ejecutarTest unTest = eval unTest (Variable "result")

parTestUno :: Mem
parTestUno = ejecutar m (par "uno")
parTestDos :: Mem
parTestDos = ejecutar m (par "dos")

sumaTestCeroDos :: Mem
sumaTestCeroDos = ejecutar m (suma "cero" "dos")
sumaTestUnoTres :: Mem
sumaTestUnoTres = ejecutar m (suma "uno" "tres")

largoTestl1 :: Mem
largoTestl1 = ejecutar m (largo "l1")
largoTestl2 :: Mem
largoTestl2 = ejecutar m (largo "l2")
largoTestl3 :: Mem
largoTestl3 = ejecutar m (largo "l3")

igualdadNTestCeroDos :: Mem
igualdadNTestCeroDos = ejecutar m (igualdadN "cero" "dos")
igualdadNTestDosUno :: Mem
igualdadNTestDosUno = ejecutar m (igualdadN "dos" "uno")

igualdadNTestCeroCero :: Mem
igualdadNTestCeroCero = ejecutar m (igualdadN "cero" "cero")
igualdadNTestTresTres :: Mem
igualdadNTestTresTres = ejecutar m (igualdadN "tres" "tres")

concatenarTestl1l2 :: Mem
concatenarTestl1l2 = ejecutar m (concatenar "l1" "l2")
concatenarTestl3l1 :: Mem
concatenarTestl3l1 = ejecutar m (concatenar "l3" "l1")
concatenarTestl3l2 :: Mem
concatenarTestl3l2 = ejecutar m (concatenar "l3" "l2")

------------------------------------------------------------------------------------------------
--
-- Ver resultados de pruebas
--
------------------------------------------------------------------------------------------------
instance Eq Valor where
    (==) :: Valor -> Valor -> Bool
    (Const con1 []) == (Const con2 []) = con1 == con2
    (Const con1 (x:xs)) == (Const con2 []) = False
    (Const con1 []) == (Const con2 (x:xs)) = False
    (Const con1 (x:xs)) == (Const con2 (y:ys)) = con1 == con2 && x == y && xs == ys

pasanTestsPar :: Bool
pasanTestsPar = ejecutarTest parTestUno == Const "False" [] && ejecutarTest parTestDos == Const "True" []
pasanTestsSuma :: Bool
pasanTestsSuma = ejecutarTest sumaTestCeroDos == Const "S" [Const "S" [Const "O" []]] && ejecutarTest sumaTestUnoTres == Const "S" [Const "S" [Const "S" [Const "S" [Const "O" []]]]]
pasanTestsLargo :: Bool
pasanTestsLargo = ejecutarTest largoTestl1 == Const "S" [Const "O" []] && ejecutarTest largoTestl2 == Const "S" [Const "S" [Const "O" []]] && ejecutarTest largoTestl3 == Const "S" [Const "S" [Const "S" [Const "O" []]]]
pasanTestsIgualdadN :: Bool
pasanTestsIgualdadN = ejecutarTest igualdadNTestCeroDos == Const "False" [] && ejecutarTest igualdadNTestDosUno == Const "False" [] && ejecutarTest igualdadNTestCeroCero == Const "True" [] && ejecutarTest igualdadNTestTresTres == Const "True" []
pasanTestsConcatenar :: Bool
pasanTestsConcatenar = ejecutarTest concatenarTestl1l2
    == Const ":" [Const "True" [],Const ":" [Const "False" [],Const ":" [Const "True" [],Const "[]" []]]]
    && ejecutarTest concatenarTestl3l1 == Const ":" [Const "False" [],Const ":" [Const "True" [],Const ":" [Const "False" [],Const ":" [Const "True" [],Const "[]" []]]]]

pasanTests :: Bool
pasanTests = pasanTestsPar && pasanTestsSuma && pasanTestsLargo && pasanTestsIgualdadN && pasanTestsConcatenar