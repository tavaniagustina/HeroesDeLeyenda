--------------
-- Punto 01 --
--------------

data Heroe = Heroe {
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea]
}

data Artefacto = Artefacto {
    nombre :: String,
    rareza :: Int
}

type Tarea = Heroe -> Heroe

--------------
-- Punto 02 --
--------------

pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria unHeroe
    | (reconocimiento unHeroe) > 1000 = cambiarEpiteto "El mitico" unHeroe
    | (reconocimiento unHeroe) >= 500 = cambiarEpiteto "El magnifico" . agregarArtefacto lanzaDelOlimpo $ unHeroe
    | (reconocimiento unHeroe) >  100 = cambiarEpiteto "Hoplita" . agregarArtefacto xipohos $ unHeroe
    | otherwise                       = unHeroe
    

cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto unEpiteto unHeroe = unHeroe { epiteto = unEpiteto}

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto unArtefacto = mapArtefacto (unArtefacto :) 

mapArtefacto :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
mapArtefacto unaFuncion unHeroe = unHeroe { artefactos = unaFuncion (artefactos unHeroe) }

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = Artefacto "Lanza del Olimpo" 100

xipohos :: Artefacto
xipohos = Artefacto "Xipohos" 50

--------------
-- Punto 03 --
--------------

encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto unArtefacto = ganarReconocimiento (rareza unArtefacto) . agregarArtefacto unArtefacto 

ganarReconocimiento :: Int -> Heroe -> Heroe
ganarReconocimiento unReconocimiento unHeroe = unHeroe { reconocimiento = reconocimiento unHeroe + unReconocimiento }

escalarElOlimpo :: Tarea
escalarElOlimpo = agregarArtefacto relampagoDeZeus . desecharArtefactos . triplicarRarezaArtefactos . ganarReconocimiento 500   

relampagoDeZeus :: Artefacto
relampagoDeZeus = Artefacto "El relampago de Zeus" 500

triplicarRarezaArtefactos :: Heroe -> Heroe
triplicarRarezaArtefactos = mapArtefacto (map triplicarRareza) 
 
triplicarRareza :: Artefacto -> Artefacto
triplicarRareza unArtefacto = unArtefacto { rareza = rareza unArtefacto * 3}

desecharArtefactos :: Heroe -> Heroe
desecharArtefactos = mapArtefacto (filter $ (>= 1000) . rareza)

type CantidadDeCuadras = Int

ayudarACruzarLaCalle :: CantidadDeCuadras -> Tarea
ayudarACruzarLaCalle cantidadDeCuadras unHeroe = cambiarEpiteto ("Gros" ++ replicate cantidadDeCuadras 'o') unHeroe

matarUnaBestia :: Bestia -> Tarea
matarUnaBestia unaBestia unHeroe
    | debilidad unaBestia unHeroe = cambiarEpiteto ("El asesino de " ++ nombreBestia unaBestia) unHeroe
    | otherwise                   = cambiarEpiteto "El cobarde" . mapArtefacto (drop 1) $ unHeroe

data Bestia = Bestia {
    nombreBestia :: String,
    debilidad :: Debilidad 
}

type Debilidad = Heroe -> Bool

--------------
-- Punto 04 --
--------------

heracles :: Heroe
heracles = Heroe "El guardian del Olimpo" 700 [relampagoDeZeus, pistola] [matarAlLeonDeNemea]

pistola :: Artefacto
pistola = Artefacto "Pistola" 1000

--------------
-- Punto 05 --
--------------

matarAlLeonDeNemea :: Tarea
matarAlLeonDeNemea = matarUnaBestia leonDeNemea

leonDeNemea :: Bestia
leonDeNemea = Bestia "Leon de Nemea" ((>= 20) . length . epiteto)

--------------
-- Punto 06 --
--------------

hacer :: Tarea -> Heroe -> Heroe
hacer unaTarea unHeroe = (unaTarea unHeroe) { tareas = unaTarea : tareas unHeroe }

--------------
-- Punto 07 --
--------------

presumir :: Heroe -> Heroe -> (Heroe, Heroe)
presumir heroe1 heroe2 
    | leGana heroe1 heroe2 = (heroe1, heroe2)
    | leGana heroe2 heroe1 = (heroe2, heroe1)
    | otherwise            = presumir (realizarTareas (tareas heroe2) heroe1) (realizarTareas (tareas heroe1) heroe2)

leGana :: Heroe -> Heroe -> Bool
leGana heroe1 heroe2 = (reconocimiento heroe1 > reconocimiento heroe2) || (reconocimiento heroe1 == reconocimiento heroe2 && sumatoriaRarezas heroe1 > sumatoriaRarezas heroe2)

sumatoriaRarezas :: Heroe -> Int
sumatoriaRarezas = sum . map rareza . artefactos

realizarTareas :: [Tarea] -> Heroe -> Heroe
realizarTareas tareas unHeroe = foldl (flip hacer) unHeroe tareas  

--------------
-- Punto 08 --
--------------

-- loopea

--------------
-- Punto 09 --
--------------

realizarLabor :: [Tarea] -> Heroe -> Heroe
realizarLabor = realizarTareas

--------------
-- Punto 10 --
--------------

-- loopea