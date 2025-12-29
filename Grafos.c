//--------------------------------- Inicio de Grafos completos -----------------------------------------
/*

Desarrollado por: Samuel Josue Mila de la Roca Guerra
Cedula: 31.632.023
Correo: miladelaroca@gmail.com
Fecha de creación: 27 de diciembre del 2025

Compilar:
gcc Grafos.c -o Grafos
O en visual studio code: ctrl+alt+N (Con la extensión Code Runner)
*/

#include <stdio.h>
#include <stdlib.h>


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



int main() {
    
    /*
    Grafo* grafo = crearGrafo(4);

        agregarArista(grafo, 0, 1);
        agregarArista(grafo, 1, 2);
        agregarArista(grafo, 2, 3);
        agregarArista(grafo, 3, 0);

        printf("Conexo: %s\n", esConexo(grafo) ? "SI" : "NO");
        printf("Arbol: %s\n", esArbol(grafo) ? "SI" : "NO");
        printf("Euleriano: %s\n", esEuleriano(grafo) ? "SI" : "NO");
        printf("Bipartito: %s\n", esBipartito(grafo) ? "SI" : "NO");

        liberarGrafo(grafo);
        return 0;
    */
}



