//--------------------------------- Inicio de Grafos completos -----------------------------------------
/*
    Grafos.c

    Desarrollado por: Samuel Josue Mila de la Roca Guerra
    Cédula: 31.632.023
    Correo: miladelaroca@gmail.com
    Fecha de creación: 27 de diciembre del 2025
    Fecha de culminación: 5 de enero del 2026



    Compilar:
    gcc Grafos.c -o Grafos
    (En Visual Studio Code con Code Runner: Ctrl+Alt+N)

    Estructura del archivo:
    - Definición de tipos `Nodo` y `Grafo`.
    - Funciones para crear, modificar y liberar el grafo.
    - Algoritmos DFS y BFS para detectar ciclos, comprobar conexidad,
        bipartición y propiedad euleriana.
*/

#include <stdio.h>
#include <stdlib.h>


// =========================
// Tipos de datos
// =========================


typedef struct Nodo {
    int vertice;
    struct Nodo* siguiente;
} Nodo;

typedef struct Grafo {
    int numVertices;
    Nodo** listaAdyacencia;
} Grafo;


Grafo* crearGrafo(int vertices) {
    Grafo* grafo = (Grafo*)malloc(sizeof(Grafo));
    grafo->numVertices = vertices;

    grafo->listaAdyacencia = (Nodo**)malloc(vertices * sizeof(Nodo*));
    for (int i = 0; i < vertices; i++) {
        grafo->listaAdyacencia[i] = NULL;
    }

    return grafo;
}
 
/*
    crearGrafo:
    Crea e inicializa un grafo con `vertices` vértices. Devuelve un
    puntero a `Grafo` con todas las listas de adyacencia inicializadas a NULL.
    El llamador debe liberar la memoria con `liberarGrafo`.
*/

void agregarArista(Grafo* grafo, int origen, int destino) {
    Nodo* nuevo = (Nodo*)malloc(sizeof(Nodo));
    nuevo->vertice = destino;
    nuevo->siguiente = grafo->listaAdyacencia[origen];
    grafo->listaAdyacencia[origen] = nuevo;

    Nodo* nuevo2 = (Nodo*)malloc(sizeof(Nodo));
    nuevo2->vertice = origen;
    nuevo2->siguiente = grafo->listaAdyacencia[destino];
    grafo->listaAdyacencia[destino] = nuevo2;
}

/*
    agregarArista:
    Añade una arista no dirigida entre `origen` y `destino` insertando
    nodos al principio de cada listas de adyacencia.
*/

int DFS_ciclo(Grafo* grafo, int v, int* visitado, int padre) {
    visitado[v] = 1;

    Nodo* temp = grafo->listaAdyacencia[v];
    while (temp != NULL) {
        int u = temp->vertice;

        if (!visitado[u]) {
            if (DFS_ciclo(grafo, u, visitado, v)) {
                return 1;
            }
        } else if (u != padre) {
            return 1;
        }

        temp = temp->siguiente;
    }

    return 0;
}

/*
    DFS_ciclo:
    DFS recursivo que marca vértices como visitados y detecta ciclos.
    Parámetros:
        - grafo: puntero al grafo
        - v: vértice actual
        - visitado: arreglo de tamaño numVertices (0/1)
        - padre: vértice desde el que se llegó a `v` (evita falso positivo de ciclo)
    Devuelve 1 si se detecta un ciclo en la componente, 0 en caso contrario.
*/

int esConexo(Grafo* grafo) {
    if (grafo->numVertices == 0) return 1;

    int* visitado = (int*)calloc(grafo->numVertices, sizeof(int));
    DFS_ciclo(grafo, 0, visitado, -1);

    for (int i = 0; i < grafo->numVertices; i++) {
        if (!visitado[i]) {
            free(visitado);
            return 0;
        }
    }

    free(visitado);
    return 1;
}

/*
    esConexo:
    Comprueba si el grafo es conexo: ejecuta un DFS desde el vértice 0 y
    verifica que todos los vértices hayan sido visitados.
*/

int esArbol(Grafo* grafo) {
    if (grafo->numVertices == 0) return 0;

    int* visitado = (int*)calloc(grafo->numVertices, sizeof(int));

    if (DFS_ciclo(grafo, 0, visitado, -1)) {
        free(visitado);
        return 0;
    }

    for (int i = 0; i < grafo->numVertices; i++) {
        if (!visitado[i]) {
            free(visitado);
            return 0;
        }
    }

    free(visitado);
    return 1;
}

/*
    esArbol:
    Devuelve 1 si el grafo es un árbol: es conexo y no tiene ciclos.
    Para ello se utiliza `DFS_ciclo` para detectar ciclos y luego se
    comprueba que todos los vértices estén visitados.
*/

int esEuleriano(Grafo* grafo) {
    if (!esConexo(grafo)) return 0;

    for (int i = 0; i < grafo->numVertices; i++) {
        int grado = 0;
        Nodo* temp = grafo->listaAdyacencia[i];
        while (temp != NULL) {
            grado++;
            temp = temp->siguiente;
        }
        if (grado % 2 != 0) return 0;
    }

    return 1;
}

/*
    esEuleriano:
    Un grafo no dirigido es euleriano si es conexo y todos sus vértices
    tienen grado par. Esta función calcula el grado de cada vértice
    recorriendo su lista de adyacencia.
*/

/*
    esBipartito:
    Comprueba si el grafo es bipartito mediante BFS por componentes.
    Se colorea cada componente con dos colores (0/1). Si se encuentra
    una arista entre dos vértices del mismo color, no es bipartito.
*/

int esBipartito(Grafo* grafo) {
    int* color = (int*)malloc(grafo->numVertices * sizeof(int));
    for (int i = 0; i < grafo->numVertices; i++) {
        color[i] = -1;
    }

    for (int inicio = 0; inicio < grafo->numVertices; inicio++) {
        if (color[inicio] == -1) {
            int* cola = (int*)malloc(grafo->numVertices * sizeof(int));
            int frente = 0, fin = 0;

            color[inicio] = 0;
            cola[fin++] = inicio;

            while (frente < fin) {
                int v = cola[frente++];

                Nodo* temp = grafo->listaAdyacencia[v];
                while (temp != NULL) {
                    int u = temp->vertice;

                    if (color[u] == -1) {
                        color[u] = 1 - color[v];
                        cola[fin++] = u;
                    } else if (color[u] == color[v]) {
                        free(cola);
                        free(color);
                        return 0;
                    }

                    temp = temp->siguiente;
                }
            }

            free(cola);
        }
    }

    free(color);
    return 1;
}

/*
    esBipartito:
    Comprueba si el grafo es bipartito mediante BFS por componentes.
    Se colorea cada componente con dos colores (0/1). Si se encuentra
    una arista entre dos vértices del mismo color, no es bipartito.
*/

/*
    liberarGrafo:
    Libera toda la memoria asociada al grafo: las listas de adyacencia
    y la estructura `Grafo` en sí.
*/

void liberarGrafo(Grafo* grafo) {
    for (int i = 0; i < grafo->numVertices; i++) {
        Nodo* temp = grafo->listaAdyacencia[i];
        while (temp != NULL) {
            Nodo* borrar = temp;
            temp = temp->siguiente;
            free(borrar);
        }
    }
    free(grafo->listaAdyacencia);
    free(grafo);
}

Grafo* leerArchivo(const char* nombreArchivo){
/*
        leerArchivo:
        Lee el grafo desde un archivo con formato:
            n         (número de vértices)
            u v       (una arista por línea)
        Valida rangos de vértices y añade las aristas válidas.
*/
    FILE* archivo = fopen(nombreArchivo, "r");
    if (archivo == NULL)
    {
    printf("Error: no se pudo abrir el archivo %s\n", nombreArchivo);
        return NULL;
    }

    int n;
    if (fscanf(archivo, "%d", &n) != 1 || n <= 0) 
    {
        printf("Error: número de vértices inválido\n");
        fclose(archivo);
        return NULL;
    }

    Grafo* grafo = crearGrafo(n);
    int u, v;
    
    while (fscanf(archivo, "%d %d", &u, &v) == 2)
    {
        if(u >= 0 && u < n && v >= 0 && v < n)
        {
            agregarArista(grafo, u, v);
        }
        else
        {
            printf("Advertencia: arista inválida (%d, %d) ignorada\n", u, v);
        }
    }
    
    fclose(archivo);
    return grafo;
}


int main() {
/*
    main:
    Punto de entrada. Lee `data.io`, imprime las propiedades del grafo y
    libera la memoria. Devuelve 0 en caso de éxito, 1 si falla la lectura.
*/
    
    Grafo* grafo = leerArchivo("data.io");
    if (grafo == NULL)
    {
        return 1;
    }
    
    printf("Conexo: %s\n", esConexo(grafo) ? "SI" : "NO");
    printf("Arbol: %s\n", esArbol(grafo) ? "SI" : "NO");
    printf("Euleriano: %s\n", esEuleriano(grafo) ? "SI" : "NO");
    printf("Bipartito: %s\n", esBipartito(grafo) ? "SI" : "NO");

    liberarGrafo(grafo);
    
    return 0;
    
}



