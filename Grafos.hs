---------------------------------- Inicio de Grafos completos-----------------------------------------

-- Desarrollado por: Samuel Josue Mila de la Roca Guerra
-- Cedula: 31.632.023
-- Correo: miladelaroca@gmail.com
-- Fecha de creación: 27 de diciembre del 2025
-- compilar con: ghc Grafo.hs -o Grafo
------------------------------------------------------------------------------------------------------

module Main where

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

-- =========================
-- DFS con detección de ciclos
-- =========================

dfsArbol :: Grafo -> Vertice -> Vertice -> [Vertice] -> Bool
dfsArbol g v padre visitados =
    let visitados' = v : visitados
    in any (tieneCiclo visitados') (g !! v)
    where
    tieneCiclo vis u
        | u == padre   = False        -- CORREGIDO
        | u `elem` vis = True
        | otherwise    = dfsArbol g u v vis

-- =========================
-- DFS simple
-- =========================

dfs :: Grafo -> Vertice -> [Vertice] -> [Vertice]
dfs g v visitados
    | v `elem` visitados = visitados      -- CORREGIDO
    | otherwise =
        foldl (\vis u -> dfs g u vis) (v : visitados) (g !! v)

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
bfsBipartito g inicio = bfs [(inicio, 0)] []     -- CORREGIDO
    where 
    bfs [] _ = True 
    bfs ((v, c):cola) colores = 
        case lookup v colores of
            Just c' -> c == c' && bfs cola colores
            Nothing ->
                let nuevos = [(u, 1 - c) | u <- g !! v]
                in bfs (nuevos ++ cola) ((v, c) : colores)  -- CORREGIDO

-- =========================
-- MAIN
-- =========================

main :: IO ()
main = do
    let g0 = crearGrafo 4
        g1 = agregarArista g0 0 1
        g2 = agregarArista g1 1 2
        g3 = agregarArista g2 2 3
        g4 = agregarArista g3 3 0
        
    putStrLn ("Grafo g4 es conexo: " ++ show (esConexo g4))
    putStrLn ("Grafo g4 es arbol: " ++ show (esArbol g4))
    putStrLn ("Grafo g4 es euleriano: " ++ show (esEuleriano g4))
    putStrLn ("Grafo g4 es bipartito: " ++ show (esBipartito g4))

---------------------------------- Fin de Grafos completos A ver que tal -----------------------------------------