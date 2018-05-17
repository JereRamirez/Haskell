import Text.Show.Functions

data Alumno = Alumno {
	nombre :: String,
	fechaNacimiento :: (Int, Int, Int),
	legajo :: Int,
	materiasCursando :: [String],
	criterioEstudio :: CriterioEstudio
} deriving (Show)

data Parcial = Parcial {
	materia :: String,
	cantidadPreguntas :: Int
} deriving (Show)

materia :: (String, _) -> String
materia (mat, _) = mat

cantidadPreguntas :: (_, Int) -> Int
cantidadPreguntas (_, cant) = cant

type CriterioEstudio = Parcial -> Bool

estudioso :: CriterioEstudio
estudioso _ = True

hijoDelRigor :: Int -> CriterioEstudio
hijoDelRigor n (Parcial _ preguntas) = preguntas > n

cabulero :: CriterioEstudio
cabulero (Parcial materia _) = (odd . length) materia

nico = Alumno {
	nombre = "Nico",
	fechaNacimiento = (10, 2, 1992),
	legajo = 143393
	materiasCursando = ["sysop", "proyecto"],
	criterioEStudio = estudioso
}

cambiarCriterioEstudio :: CriterioEstudio -> Alumno -> Alumno
cambiarCriterioEstudio nuevoCriterio alumno = alumno {
	criterioEstudio = nuevoCriterio
}

estudia :: Parcial -> Alumno -> Bool
estudia parcial alumno = (criterioEstudio alumno) parcial