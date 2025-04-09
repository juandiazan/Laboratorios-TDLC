--1.1
type Symbol = String

--1.2
blank :: Symbol
blank = "#"

--1.3
type Tape = ([Symbol], Symbol, [Symbol])

---------------------------------------------

--2.1
type State = String

--2.2
ini :: State
ini = "i"
h :: State
h = "h"

--2.3
data Movement = L | R | Defer deriving Show

--2.4
type Action = (Symbol, Movement)

--2.5
type Code = [Step]

---------------------------------------------

--3
type Config = (State, Tape)

---------------------------------------------

type Step = (State, Symbol, Action, State) 

buscarEstadoEnCodigo :: Code -> State -> Symbol -> (Action, State)
buscarEstadoEnCodigo [] state symbol = error ("no existe la pareja " ++ state ++ " " ++ symbol)
buscarEstadoEnCodigo ((estadoActual, simboloActual, accion, estadoSig):cs) estadoBuscado simboloBuscado = 
    if estadoActual == estadoBuscado && simboloActual == simboloBuscado then (accion, estadoSig) else buscarEstadoEnCodigo cs estadoBuscado simboloBuscado 

step :: Code -> Config -> Config
step codigo (estadoCinta, (cintaIzq, cabezal, cintaDer)) = 
    case buscarEstadoEnCodigo codigo estadoCinta cabezal of {
    ((simboloNuevo, accion), estadoSiguiente) -> case accion of {
        
        L -> case cintaIzq of {
            [] -> (estadoSiguiente, ([], blank, simboloNuevo:cintaDer));
            c:cs -> (estadoSiguiente, (cs, c, simboloNuevo:cintaDer));
        };
        
        R -> case cintaDer of{
            [] -> (estadoSiguiente, (simboloNuevo:cintaIzq, blank, []));
            c:cs -> (estadoSiguiente, (simboloNuevo:cintaIzq, c, cs));
        };
        
        Defer -> (estadoSiguiente, (cintaIzq, simboloNuevo, cintaDer));
    }
}

execAux :: Code -> Tape -> State -> Tape
execAux codigo (cintaIzq, cabezal, cintaDer) estado = if estado == h
    then (reverse cintaIzq, cabezal, cintaDer)
    else execAux codigo tape state where (state, tape) = step codigo (estado, (cintaIzq, cabezal, cintaDer))  

execAux' :: Code -> Config -> Tape
execAux' codigo (estado, (cintaIzq, cabezal, cintaDer)) = if estado == h
    then (cintaIzq, cabezal, cintaDer)
    else execAux' codigo (step codigo (estado, (cintaIzq, cabezal, cintaDer)))

exec :: Code -> Tape -> Tape
exec codigo (cintaIzq, cabezal, cintaDer) = execAux codigo (reverse cintaIzq, cabezal, cintaDer) ini

impar :: Code
impar = 
    [
        (ini, blank, (blank, L), "par"),

        ("par", "1", ("1", L), "impar"), 
        ("par", "#", (blank, R), "derechaHastaBlankPar"),
        
        ("impar", "1", ("1", L), "par"),
        ("impar", "#", (blank, R), "derechaHastaBlankImpar"),
        
        ("derechaHastaBlankImpar", "1", ("1", R), "derechaHastaBlankImpar"),
        ("derechaHastaBlankImpar", "#", (blank, R), "finImpar"),
        ("finImpar", "#", ("T", R), "h"),

        ("derechaHastaBlankPar", "1", ("1", R), "derechaHastaBlankPar"),
        ("derechaHastaBlankPar", "#", (blank, R), "finPar"),
        ("finPar", "#", ("F", R), "h")
    ]

suma :: Code
suma = 
    [
        (ini, blank, (blank, L), "izqHastaPrincipioSegundoNumero"),
        
        ("izqHastaPrincipioSegundoNumero", "1", ("1", L), "izqHastaPrincipioSegundoNumero"),
        ("izqHastaPrincipioSegundoNumero", blank, (blank, Defer), "izq"),

        ("izq", blank, (blank, L), "izqHastaPrincipioPrimerNumero"),

        ("izqHastaPrincipioPrimerNumero", "1", ("1", L), "izqHastaPrincipioPrimerNumero"),
        ("izqHastaPrincipioPrimerNumero", blank, (blank, Defer), "der"),

        ("der", blank, (blank, R), "estoyEnBlank?"),

        --El primer numero es distinto de 0 o ya llegue al final
        ("estoyEnBlank?", "1", ("X", R), "derHastaBlankSegundoNumero"),


        ("derHastaBlankSegundoNumero", "1", ("1", R), "derHastaBlankSegundoNumero"),
        ("derHastaBlankSegundoNumero", blank, (blank, R), "derHastaBlankInicio"),

        ("derHastaBlankInicio", "1", ("1", R), "derHastaBlankInicio"),
        ("derHastaBlankInicio", blank, (blank, R), "derPorSiNoHayCinta"),

        ("derPorSiNoHayCinta", blank, (blank, R), "derHastaFinSuma"),

        ("derHastaFinSuma", "1", ("1", R), "derHastaFinSuma"),
        ("derHastaFinSuma", blank, ("1", R), "izqHastaX"),

        ("izqHastaX", "1", ("1", L), "izqHastaX"),
        ("izqHastaX", blank, (blank, L), "izqHastaX"),
        ("izqHastaX", "X", ("1", R), "estoyEnBlank?"),

        ("estoyEnBlank?", blank, (blank, R), "estoyEnBlank2doNumero?"),

        ("estoyEnBlank2doNumero?", "1", ("X", R), "derHastaBlankInicioSegundoNum"),

        ("derHastaBlankInicioSegundoNum", "1", ("1", R), "derHastaBlankInicioSegundoNum"),
        ("derHastaBlankInicioSegundoNum", blank, (blank, R), "derHastaFinSumaSegundoNum"),

        ("derHastaFinSumaSegundoNum", "1", ("1", R), "derHastaFinSumaSegundoNum"),
        ("derHastaFinSumaSegundoNum", blank, ("1", R), "izqHastaXSegundoNum"),

        ("izqHastaXSegundoNum", "1", ("1", L), "izqHastaXSegundoNum"),
        ("izqHastaXSegundoNum", blank, (blank, L), "izqHastaXSegundoNum"),
        ("izqHastaXSegundoNum", "X", ("1", R), "estoyEnBlank2doNumero?"),

        --El primer numero es distinto de 0 o ya llegue al final
        ("estoyEnBlank2doNumero?", blank, (blank, R), "derechaHastaFin"),

        ("derechaHastaFin", "1", ("1", R), "derechaHastaFin"),
        ("derechaHastaFin", blank, (blank, Defer), h)
    ]

copia :: Code
copia = 
    [
        (ini, blank, (blank, L), "izquierdaHastaBlank"),

        ("izquierdaHastaBlank", "o1", ("o1", L), "izquierdaHastaBlank"),
        ("izquierdaHastaBlank", "o2", ("o2", L), "izquierdaHastaBlank"),
        ("izquierdaHastaBlank", blank, (blank, R), "estoyEnBlank?"),

        ("estoyEnBlank?", blank, (blank, R), "derHastaBlankFinal"),
        ("estoyEnBlank?", "o1", ("X", R), "derechaHastaBlankInicioEscribirO1"),
        ("estoyEnBlank?", "o2", ("X", R), "derechaHastaBlankInicioEscribirO2"), 

        -- Escritura en caso de o1 (cuando estoyEnBlank? da o1)

        ("derechaHastaBlankInicioEscribirO1", "o1", ("o1", R), "derechaHastaBlankInicioEscribirO1"), 
        ("derechaHastaBlankInicioEscribirO1", "o2", ("o2", R), "derechaHastaBlankInicioEscribirO1"), 
        ("derechaHastaBlankInicioEscribirO1", blank, (blank, R), "derechaHastaEscribirO1"),

        ("derechaHastaEscribirO1", "o1", ("o1", R), "derechaHastaEscribirO1"),
        ("derechaHastaEscribirO1", "o2", ("o2", R), "derechaHastaEscribirO1"),
        ("derechaHastaEscribirO1", blank, ("o1", R), "izquierdaHastaXLuegoDeEscribirO1"),

        ("izquierdaHastaXLuegoDeEscribirO1", "o1", ("o1", L), "izquierdaHastaXLuegoDeEscribirO1"),
        ("izquierdaHastaXLuegoDeEscribirO1", "o2", ("o2", L), "izquierdaHastaXLuegoDeEscribirO1"),
        ("izquierdaHastaXLuegoDeEscribirO1", blank, (blank, L), "izquierdaHastaXLuegoDeEscribirO1"),
        ("izquierdaHastaXLuegoDeEscribirO1", "X", ("o1", R), "estoyEnBlank?"),

        -- Escritura en caso de o2 (cuando estoyEnBlank? da o2)

        ("derechaHastaBlankInicioEscribirO2", "o1", ("o1", R), "derechaHastaBlankInicioEscribirO2"), 
        ("derechaHastaBlankInicioEscribirO2", "o2", ("o2", R), "derechaHastaBlankInicioEscribirO2"), 
        ("derechaHastaBlankInicioEscribirO2", blank, (blank, R), "derechaHastaEscribirO2"),

        ("derechaHastaEscribirO2", "o1", ("o1", R), "derechaHastaEscribirO2"),
        ("derechaHastaEscribirO2", "o2", ("o2", R), "derechaHastaEscribirO2"),
        ("derechaHastaEscribirO2", blank, ("o2", R), "izquierdaHastaXLuegoDeEscribirO2"),

        ("izquierdaHastaXLuegoDeEscribirO2", "o1", ("o1", L), "izquierdaHastaXLuegoDeEscribirO2"),
        ("izquierdaHastaXLuegoDeEscribirO2", "o2", ("o2", L), "izquierdaHastaXLuegoDeEscribirO2"),
        ("izquierdaHastaXLuegoDeEscribirO2", blank, (blank, L), "izquierdaHastaXLuegoDeEscribirO2"),
        ("izquierdaHastaXLuegoDeEscribirO2", "X", ("o2", R), "estoyEnBlank?"),

        -- Llegué al final (cuando estoyEnBlank? da blank)

        ("derHastaBlankFinal", "o1", ("o1", R), "derHastaBlankFinal"),
        ("derHastaBlankFinal", "o2", ("o2", R), "derHastaBlankFinal"),
        ("derHastaBlankFinal", blank, (blank, Defer), h)
    ]

reversa :: Code
reversa = 
    [
        (ini, blank, (blank, L), "estoyEnBlank?"),

        ("estoyEnBlank?", blank, (blank, R), "derechaHastaBlankInicio"),

        ("estoyEnBlank?", "o1", ("X", R), "derechaHastaBlankInicioO1"),

        -- Copio o1

        ("derechaHastaBlankInicioO1", "o1", ("o1", R), "derechaHastaBlankInicioO1"),
        ("derechaHastaBlankInicioO1", "o2", ("o2", R), "derechaHastaBlankInicioO1"),
        ("derechaHastaBlankInicioO1", blank, (blank, R), "derechaHastaFinPalabraO1"),

        ("derechaHastaFinPalabraO1", "o1", ("o1", R), "derechaHastaFinPalabraO1"),
        ("derechaHastaFinPalabraO1", "o2", ("o2", R), "derechaHastaFinPalabraO1"),
        ("derechaHastaFinPalabraO1", blank, ("o1", R), "izquierdaHastaXO1"),

        ("izquierdaHastaXO1", "o1", ("o1", L), "izquierdaHastaXO1"),
        ("izquierdaHastaXO1", "o2", ("o2", L), "izquierdaHastaXO1"),
        ("izquierdaHastaXO1", blank, (blank, L), "izquierdaHastaXO1"),
        ("izquierdaHastaXO1", "X", ("o1", L), "estoyEnBlank?"),

        -- Copio o2

        ("estoyEnBlank?", "o2", ("X", R), "derechaHastaBlankInicioO2"),

        ("derechaHastaBlankInicioO2", "o1", ("o1", R), "derechaHastaBlankInicioO2"),
        ("derechaHastaBlankInicioO2", "o2", ("o2", R), "derechaHastaBlankInicioO2"),
        ("derechaHastaBlankInicioO2", blank, (blank, R), "derechaHastaFinPalabraO2"),

        ("derechaHastaFinPalabraO2", "o1", ("o1", R), "derechaHastaFinPalabraO2"),
        ("derechaHastaFinPalabraO2", "o2", ("o2", R), "derechaHastaFinPalabraO2"),
        ("derechaHastaFinPalabraO2", blank, ("o2", R), "izquierdaHastaXO2"),

        ("izquierdaHastaXO2", "o1", ("o1", L), "izquierdaHastaXO2"),
        ("izquierdaHastaXO2", "o2", ("o2", L), "izquierdaHastaXO2"),
        ("izquierdaHastaXO2", blank, (blank, L), "izquierdaHastaXO2"),
        ("izquierdaHastaXO2", "X", ("o2", L), "estoyEnBlank?"),

        -- Fin, si ya termine de copiar

        ("derechaHastaBlankInicio", "o1", ("o1", R), "derechaHastaBlankInicio"),
        ("derechaHastaBlankInicio", "o2", ("o2", R), "derechaHastaBlankInicio"),
        ("derechaHastaBlankInicio", blank, (blank, Defer), "derechaParaPasarInicio"),

        ("derechaParaPasarInicio", blank, (blank, R), "derechaHastaFinReverse"),
        
        ("derechaHastaFinReverse", "o1", ("o1", R), "derechaHastaFinReverse"),
        ("derechaHastaFinReverse", "o2", ("o2", R), "derechaHastaFinReverse"),
        ("derechaHastaFinReverse", blank, (blank, Defer), "h")
    ]

    
-------------------------------------------------------------------------------------
-- Pruebas
-------------------------------------------------------------------------------------
-------------------------------------
--Impar
-------------------------------------
impartest1 :: Bool
impartest1 = exec impar ([blank], blank, []) == ([blank, blank, "F"], blank, [])

impartest2 :: Bool
impartest2 = exec impar ([blank, "1"], blank, []) == ([blank, "1", blank, "T"], blank, [])

impartest3 :: Bool
impartest3 = exec impar ([blank, "1", "1"], blank, []) == ([blank, "1", "1", blank, "F"], blank, [])

impartest4 :: Bool
impartest4 = exec impar ([blank, "1", "1", "1"], blank, []) == ([blank, "1", "1", "1", blank, "T"], blank, [])

impartest5 :: Bool
impartest5  = exec impar ([blank, "1", "1", "1", "1"], blank, []) == ([blank, "1", "1", "1", "1", blank, "F"], blank, [])   

impartest :: Bool
impartest = impartest1 && impartest2 && impartest3 && impartest4 && impartest5
-------------------------------------
--Suma
-------------------------------------
sumatest1 :: Bool
sumatest1 = exec suma ([blank, blank], blank, []) == ([blank, blank, blank], blank, [])

sumatest2 :: Bool
sumatest2 = exec suma ([blank, "1", "1", blank, "1", "1"], blank, []) == ([blank, "1", "1", blank, "1", "1", blank, "1", "1", "1", "1"], blank, [])

sumatest3 :: Bool
sumatest3 = exec suma ([blank, "1", blank, "1", "1", "1"], blank, []) == ([blank, "1", blank, "1", "1", "1", blank, "1", "1", "1", "1"], blank, [])

sumatest4 :: Bool
sumatest4 = exec suma ([blank, "1", blank, blank], blank, []) == ([blank, "1", blank, blank, blank], blank, [])

sumatest :: Bool
sumatest = sumatest1 && sumatest2 && sumatest3 && sumatest4
-------------------------------------
--Copia
-------------------------------------
copiatest1 :: Bool
copiatest1 = exec copia ([blank, "o1", "o1", "o2", "o2"], blank, []) == ([blank, "o1", "o1", "o2", "o2", blank, "o1", "o1", "o2", "o2"], blank, [])

copiatest2 :: Bool
copiatest2 = exec copia ([blank, "o1"], blank, []) == ([blank, "o1", blank, "o1"], blank, [])

copiatest3 :: Bool
copiatest3 = exec copia ([blank, "o2", "o1"], blank, []) == ([blank, "o2", "o1", blank, "o2", "o1"], blank, [])

copiatest4 :: Bool
copiatest4 = exec copia ([blank], blank, []) == ([blank, blank], blank, [])

copiatest :: Bool
copiatest = copiatest1 && copiatest2 && copiatest3 && copiatest4
-------------------------------------
--Reversa
-------------------------------------
reversatest1 :: Bool
reversatest1 = exec reversa ([blank, "o1", "o1", "o2", "o2"], blank, []) == ([blank, "o1", "o1", "o2", "o2", blank, "o2", "o2", "o1", "o1"], blank, [])

reversatest2 :: Bool
reversatest2 = exec reversa ([blank, "o1", "o2", "o1", "o2"], blank, []) == ([blank, "o1", "o2", "o1", "o2", blank, "o2", "o1", "o2", "o1"], blank, []) 

reversatest3 :: Bool
reversatest3 = exec reversa ([blank, "o2", "o1"], blank, []) == ([blank, "o2", "o1", blank, "o1", "o2"], blank, [])

reversatest4 :: Bool
reversatest4 = exec reversa ([blank], blank, []) == ([blank, blank], blank, [])

reversatest5 :: Bool
reversatest5 = exec reversa ([blank, "o2"], blank, []) == ([blank, "o2", blank, "o2"], blank, [])

reversatest :: Bool
reversatest = reversatest1 && reversatest2 && reversatest3 && reversatest4 && reversatest5
-------------------------------------------------------------------------------------
-- Todas las pruebas
-------------------------------------------------------------------------------------
test :: Bool
test = impartest && sumatest && copiatest && reversatest
-------------------------------------------------------------------------------------
-- Fin pruebas
-------------------------------------------------------------------------------------