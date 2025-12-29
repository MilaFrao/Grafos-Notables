---------------------------------- Inicio de Grafos completos-----------------------------------------

-- Desarrollado por: Samuel Josue Mila de la Roca Guerra
-- Cedula: 31.632.023
-- Correo: miladelaroca@gmail.com
-- Fecha de creación: 27 de diciembre del 2025

------------------------------------------------------------------------------------------------------

Module Grafo where

type Vertice = Int
type Grafo = [[Vertice]]

crearGrafo :: Int -> Grafo
crearGrafo n = replicate n []

agregarArista :: Grafo -> Vertice -> Vertice -> Grafo
agregarArista g a b =
    let agregar i v
            | i == a = b:v
            | i == b = a:v
            | otherwise = v
    in zipWith agregar [0..] g  

DFS_Arbol :: Grafo -> Vertice -> Vertice -> [Vertice] -> Bool
DFS_Arbol g v padre visitados =
    let visitados' = v :visitados
    in any (tieneCiclo visitados') (g !! v)
    where
        tieneCiclo vis u
            | u == padre = True
            | u `elem` vis = True
            | otherwise = DFS_Arbol g u v vis

DFS :: Grafo -> Vertice -> [Vertice] -> [Vertice]  
DFS g v visitados =
    |v 'elem' visitados = visitados
    |otherwise = foldl (\vis u -> DFS g u vis) (v : visitados) (g !! v)

esConexo :: Grafo -> Bool
esConexo [] = True
esConexo g = length (DFS g 0 []) == length g

esArbol :: Grafo -> Bool
esArbol [] = False
esArbol g  =
    esConexo g && not (DFS_Arbol g 0 (-1) [])

esEuleriano :: Grafo -> Bool
esEuleriano g =
    esConexo g && all even (map length g)

type Color = Int
type Coloreado = [(Vertice, Color)]

esBipartito :: Grafo -> Bool
esBipartito g = all (BFS_Bipartito g) [0..length g - 1]

BFS_Bipartito :: Grafo -> Vertice -> Bool
BFS_Bipartito g inicio = BFS [(inicio, )] []
    where 
        BFS [] _ = True 
        BFS ((v, c):cola) colores = 
            case lookup v colores of
                Just c' -> c == c' && BFS cola colores
                Nothing ->
                    let nuevos = [(u, 1 - c) | u <- g !! v]
                    BFS (nuevos ++ cola) ((v, c) : colores)

Main :: IO ()
Main = do
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