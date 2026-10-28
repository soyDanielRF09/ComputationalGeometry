# Práctica de triangulación: Star Wars

Este proyecto forma parte de la asignatura de Geometría Computacional y consiste en calcular el área de una figura mediante triangulación, usando puntos seleccionados sobre una imagen y una escala definida por el usuario.

## Objetivo

El objetivo principal es:

- cargar una imagen de referencia,
- seleccionar los puntos que delimitan las componentes convexas de la figura,
- escalar la imagen a unidades reales,
- calcular el área total mediante triangulación,
- guardar el proyecto con los puntos y la configuración de escala.

## Archivos principales

- `area_figura_componentes_convexas.R`: script principal para seleccionar puntos sobre la imagen, calcular áreas y guardar el proyecto en un archivo `.rds`.
- `lectura_puntos_componentes_convexas.R`: script para cargar un proyecto ya generado y reconstruir el cálculo del área a partir de los puntos guardados.
- `archivo_puntos_atat.rds`: ejemplo de archivo generado con los datos del proyecto.
- `PRACTICA_1_STAR_WARS_GC.pdf`: enunciado de la práctica.

## Requisitos

Necesitarás tener instalado R y, en preferencia, RStudio.

Además, instala los paquetes:

```r
install.packages(c("imager", "pracma", "rstudioapi"))
```

## Flujo de trabajo

1. Abre `area_figura_componentes_convexas.R` en RStudio.
2. Ejecuta el script.
3. Selecciona la imagen que contiene la figura.
4. Define una escala marcando dos puntos con una diferencia de altura conocida.
5. Elige los puntos que forman cada componente convexa de la figura.
6. Confirma la selección y guarda el proyecto.
7. Abre `lectura_puntos_componentes_convexas.R` para cargar el proyecto y calcular el área nuevamente.

## Descripción técnica

El proyecto usa varios conceptos de geometría computacional:

- ordenación de puntos alrededor de un punto de referencia,
- eliminación de puntos redundantes,
- cálculo de áreas por triangulación,
- manejo de componentes convexas,
- representación gráfica en coordenadas reales y en píxeles.

## Resultado esperado

El programa calcula una aproximación del área total de la figura como suma de áreas de las componentes convexas, usando triangulación. También permite visualizar en la imagen los puntos y la estructura geométrica asociada.

## Nota

Este repositorio contiene una práctica académica y está pensado como material de estudio y demostración, no como una librería general de uso productivo.

## Autoría

- David Hernández Paradela
- Daniel Rodríguez Fernández

## Licencia

Consulta el repositorio para verificar la licencia aplicada, si procede.
