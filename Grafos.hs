---------------------------------- Inicio de Grafos completos-----------------------------------------

-- Desarrollado por: Samuel Josue Mila de la Roca Guerra
-- Cedula: 31.632.023
-- Correo: miladelaroca@gmail.com
-- Fecha de creación: 27 de diciembre del 2025
-- compilar con: ghc Grafo.hs -o Grafo
------------------------------------------------------------------------------------------------------

module Main where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

type Vertice = Int
type Grafo = [[Vertice]]

-- =========================
-- Construcción del grafo
-- =========================

crearGrafo :: Int -> Grafo
crearGrafo n = replicate n []

agregarArista :: Grafo -> Vertice -> Vertice -> Grafo
agregarArista g a b =
    let agregar i v
            | i == a    = b : v
            | i == b    = a : v
            | otherwise = v
    in zipWith agregar [0..] g  

-- Acceso seguro a vecinos (evita `!!` fuera de rango)
vecinos :: Grafo -> Vertice -> [Vertice]
vecinos g v
    | v >= 0 && v < length g = g !! v
    | otherwise              = []

-- =========================
-- DFS con detección de ciclos
-- =========================

dfsArbol :: Grafo -> Vertice -> Vertice -> [Vertice] -> Bool
dfsArbol g v padre visitados =
    let visitados' = v : visitados
    in any (tieneCiclo visitados') (vecinos g v)
    where
    tieneCiclo vis u
        | u == padre   = False
        | u `elem` vis = True
        | otherwise    = dfsArbol g u v vis

-- =========================
-- DFS simple
-- =========================

dfs :: Grafo -> Vertice -> [Vertice] -> [Vertice]
dfs g v visitados
    | v `elem` visitados = visitados
    | otherwise =
        foldl (\vis u -> dfs g u vis) (v : visitados) (vecinos g v)

-- =========================
-- Propiedades del grafo
-- =========================

esConexo :: Grafo -> Bool
esConexo [] = True
esConexo g  = length (dfs g 0 []) == length g

esArbol :: Grafo -> Bool
esArbol [] = False
esArbol g  =
    esConexo g && not (dfsArbol g 0 (-1) [])

esEuleriano :: Grafo -> Bool
esEuleriano g =
    esConexo g && all even (map length g)

-- =========================
-- Bipartito (BFS)
-- =========================

type Color = Int
type Coloreado = [(Vertice, Color)]

esBipartito :: Grafo -> Bool
esBipartito g = all (bfsBipartito g) [0 .. length g - 1]

bfsBipartito :: Grafo -> Vertice -> Bool
bfsBipartito g inicio = bfs [(inicio, 0)] []
    where 
    bfs [] _ = True 
    bfs ((v, c):cola) colores = 
        case lookup v colores of
            Just c' -> c == c' && bfs cola colores
            Nothing ->
                let nuevos = [(u, 1 - c) | u <- vecinos g v]
                in bfs (nuevos ++ cola) ((v, c) : colores)

-- =========================
-- Lectura del archivo data.io
-- =========================

leerGrafoDesdeArchivo :: FilePath -> IO Grafo
leerGrafoDesdeArchivo ruta = do
    contenido <- readFile ruta
    let ls = lines contenido

    case ls of
        [] -> return []
        (nLine:rest) ->
            case readMaybe nLine :: Maybe Int of
                Nothing -> error "Formato inválido: número de vértices"
                Just n ->
                    let aristas = mapMaybe parseArista rest
                        grafoInicial = crearGrafo n
                        grafoFinal = foldl (\g (u,v) -> agregarArista g u v) grafoInicial aristas
                    in return grafoFinal

parseArista :: String -> Maybe (Vertice, Vertice)
parseArista linea =
    case mapM readMaybe (words linea) of
        Just [u,v] -> Just (u,v)
        _ -> Nothing

-- =========================
-- MAIN
-- =========================

main :: IO ()
main = do
    grafo <- leerGrafoDesdeArchivo "data.io"

    putStrLn ("Grafo es conexo: " ++ show (esConexo grafo))
    putStrLn ("Grafo es arbol: " ++ show (esArbol grafo))
    putStrLn ("Grafo es euleriano: " ++ show (esEuleriano grafo))
    putStrLn ("Grafo es bipartito: " ++ show (esBipartito grafo))

---------------------------------- Fin de Grafos completos A ver que tal -----------------------------------------