import Text.Show.Functions()

-- Punto 1 --

data Heroe = Heroe {
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tarea :: [Tarea]
} deriving (Show)

data Bestia = Bestia {
    nombre :: String,
    debilidad :: Debilidad
} deriving (Show)

type Tarea = Heroe -> Heroe
type Artefacto = (String, Rareza) 
type Rareza = Int
type Debilidad = Heroe -> Bool


-- Mapeos ----------

mapEpiteto :: (String -> String) -> Heroe -> Heroe
mapEpiteto unaFuncion heroe = heroe { epiteto = unaFuncion (epiteto heroe) }

mapArtefacto :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
mapArtefacto unaFuncion heroe = heroe { artefactos = unaFuncion (artefactos heroe) }

-- Punto 2 ---------

paseALaHistoria :: Heroe -> Heroe
paseALaHistoria unHeroe 
                        | reconocimiento unHeroe > 1000                                 = cambiarEpiteto "El mitico" unHeroe
                        | reconocimiento unHeroe >= 500                                 = anadirArtefacto ("Lanza del Olimpo", 100) $ cambiarEpiteto "El magnifico" unHeroe
                        | reconocimiento unHeroe >= 100 && reconocimiento unHeroe < 500 = anadirArtefacto ("Xiphos", 50) $ cambiarEpiteto "Hoplita" unHeroe
                        | otherwise                                                     = unHeroe
cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto nuevoEpiteto = mapEpiteto (const nuevoEpiteto)

anadirArtefacto :: Artefacto -> Heroe -> Heroe
anadirArtefacto nuevoArtefacto = mapArtefacto (nuevoArtefacto : )

-- Punto 3 ---------

encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto (nombre, rareza) = anadirArtefacto (nombre, rareza) . ganarReconocimiento rareza

escalarElOlimpo :: Tarea
escalarElOlimpo = anadirArtefacto ("El relámpago de Zeus", 500) . desecharArtefactos . triplicarRareza . ganarReconocimiento 500

matarAUnaBestia :: Bestia -> Tarea
matarAUnaBestia unaBestia unHeroe 
                                | (debilidad unaBestia) unHeroe = cambiarEpiteto ("El asesino de " ++ unaBestia ) unHeroe
                                | otherwise                     = pierdePrimerArtefacto $ cambiarEpiteto ("El cobarde" ) unHeroe


anadirArtefacto 
anadirArtefacto 

ganarReconocimiento
ganarReconocimiento

pierdePrimerArtefacto
pierdePrimerArtefacto


-- Punto 4 ---------

-- heracles :: Heroe
-- heracles = Heroe "Guardian del Olimpo" 700 --