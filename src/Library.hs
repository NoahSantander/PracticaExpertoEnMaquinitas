module Library where
import PdePreludat

-- Defino mis alias
type Nombre = String
type Dinero = Number
type Suerte = Number
type Factor = (String, Number)
type Factores = [Factor]
type SeGana = Number -> Number
type CriterioParaGanar = Persona -> Bool
type ApuestaInicial = Number
type Juegos = [Juego]
type Personas = [Persona]
type ModificarSaldo = Dinero -> Persona -> Persona

-- Defino mis tipos
data Persona = UnaPersona {
    nombre :: Nombre,
    dinero :: Dinero,
    suerte :: Suerte,
    factores :: Factores
} deriving Show

data Juego = UnJuego {
    nombreJuego :: Nombre,
    seGana :: SeGana,
    criterioParaGanar :: CriterioParaGanar
} deriving Show

-- Defino algunas personas
nico = UnaPersona "Nico" 100.0 30 [("amuleto", 3), ("manos magicas",100)]
maiu = UnaPersona "Maiu" 100.0 420 [("inteligencia",55), ("paciencia",50)]

-- Defino la suerte total de una persona
tieneValor :: Factor -> Bool
tieneValor = (> 0).snd

esUnAmuleto :: Factor -> Bool
esUnAmuleto = (=="amuleto").fst

valorAmuletos :: Persona -> [Number]
valorAmuletos persona = ((map (snd)).(filter (esUnAmuleto))) (factores persona)

suerteTotal :: Persona -> Suerte
suerteTotal persona = foldl (*) (suerte persona) (valorAmuletos persona)

-- Defino algunas funciones auxiliares
tiene :: String -> CriterioParaGanar
tiene algoATener persona = elem algoATener (map (fst) (factores persona))

condicionDoble :: CriterioParaGanar -> CriterioParaGanar -> CriterioParaGanar
condicionDoble criterio1 criterio2 persona = criterio1 persona && criterio2 persona

suerteMayorA :: Suerte -> CriterioParaGanar
suerteMayorA suerteMenor = (> suerteMenor).suerte

-- Defino algunos juegos
ruleta = UnJuego "Ruleta" (*37) (suerteMayorA 80)
maquinita :: Number -> Juego
maquinita jackpot = UnJuego "Maquinita" (+ jackpot) (condicionDoble (suerteMayorA 95) (tiene "paciencia"))

-- Defino si una persona puede ganar un juego
puedeGanar :: Persona -> Juego -> Bool
puedeGanar persona juego = (criterioParaGanar juego) persona

-- Defino el dinero que puede ganar una persona en un juego
dineroGanado :: Persona -> ApuestaInicial -> Juego -> Dinero
dineroGanado persona apuesta juego 
    | puedeGanar persona juego = (seGana juego) apuesta
    | otherwise = apuesta

-- Definir cuanto dinero puede ganar una persona dada una apuesta inicial y una lista de juegos // Orden superior, aplicación parcial y composición
{-dineroConseguido :: ApuestaInicial -> Persona -> Juegos -> Dinero
dineroConseguido apuestaInicial persona juegos = foldl (dineroGanado persona) apuestaInicial juegos -}

-- Definir cuanto dinero puede ganar una persona dada una apuesta inicial y una lista de juegos // Recursividad
dineroConseguido :: ApuestaInicial -> Persona -> Juegos -> Dinero
dineroConseguido apuestaInicial _ [] = apuestaInicial
dineroConseguido apuestaInicial persona [juego] = dineroGanado persona apuestaInicial juego
dineroConseguido apuestaInicial persona (juego:juegosRestantes) = dineroConseguido (dineroGanado persona apuestaInicial juego) persona juegosRestantes

-- Defino las personas que que no pueden ganar ningun juego de una lista de juegos
noGanaNinguno :: Juegos -> Persona -> Bool
noGanaNinguno juegos persona = all (not.puedeGanar persona) juegos

noPuedenGanarNinguno :: Personas -> Juegos -> Personas
noPuedenGanarNinguno personas juegos = filter (noGanaNinguno juegos) personas

-- Defino modificar dinero de una persona despues de apostar
modificarSaldo :: ModificarSaldo
modificarSaldo saldo persona = persona {dinero = dinero persona + saldo}

apostar :: Dinero -> Persona -> Juego -> Persona
apostar dineroApostado persona juego
    | puedeGanar persona juego = modificarSaldo ((dineroGanado persona dineroApostado juego) - dineroApostado) persona
    | otherwise = modificarSaldo (-dineroApostado) persona

-- Inferir tipo de la funcion
-- elCocoEstaEnLaCasa :: (a, [Number]) -> (t -> [Number]) -> Number -> [([Number] -> [Number], t)] -> Bool
elCocoEstaEnLaCasa x y z = all ((>z).(+42)).foldl (\a (b,c) -> y c ++ b a) (snd x)