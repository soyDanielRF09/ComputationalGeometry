# Práctica 9: Coloración de grafos y triangulación

Este directorio reúne la implementación de la práctica de Geometría Computacional centrada en dos temas principales:

- triangulación de polígonos
- coloración de grafos con algoritmo DSATUR

La práctica incluye ejemplos con diferentes tipos de polígonos y la aplicación de la coloración a mapas geográficos (China y EEUU).

## Archivos incluidos

- `Practica9.R`: script principal con la implementación completa de la práctica.
- `ENTREGA_PRACTICA_9_GC_DanielRF_DavidHP.pdf`: entrega final en formato PDF de la práctica.

## Contenido de la práctica

El script `Practica9.R` contiene:

- funciones geométricas para orientación, distancia y detección de convexidad
- algoritmo recursivo de triangulación de un polígono
- visualización gráfica de triangulaciones
- construcción de grafos a partir de aristas
- algoritmo de coloración por saturación (`DSATUR`)
- representación gráfica de la coloración de varios grafos
- coloración de mapas de China y EEUU usando datos geográficos

## Requisitos

Para ejecutar el script es necesario tener instalado R y algunas librerías:

- `igraph`
- `rnaturalearth`
- `rnaturalearthdata`
- `sf`

Se pueden instalar con:

```r
install.packages("igraph")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("sf")
```

## Ejecución

Desde R o RStudio, abre el archivo `Practica9.R` y ejecútalo:

```r
source("Practica9.R")
```

Esto generará varias ventanas gráficas con:

- triangulaciones de distintos polígonos
- coloraciones de grafos
- mapas coloreados según la adyacencia de provincias

## Autores

- Daniel Rodríguez Fernández
- David Hernández Paradela

## Nota

Esta práctica forma parte del repositorio de proyectos de la asignatura de Geometría Computacional y está enfocada a la visualización y aplicación de algoritmos geométricos y de teoría de grafos.
