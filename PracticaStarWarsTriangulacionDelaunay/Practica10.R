##########################################################################
# Práctica de Geometría Commputacional 10: Triangulaciones y coloraciones
# Daniel Rodríguez Fernández, David Hernández Paradela
##########################################################################


# Instalar paquetes
#######################

install.packages("deldir")
install.packages("tripack")
install.packages("RColorBrewer")
install.packages("pracma")
install.packages("imager")

# Librerías
#####################
library(deldir)
library(tripack)
library(RColorBrewer)
library(imager)
library(pracma)


graphics.off()


# Delaunay / Voronoi
########################

x <- c(2.3, 3.0, 7.0, 1.0, 3.0, 8.0)
y <- c(2.3, 3.0, 2.0, 5.0, 8.0, 9.0)

# dpl = estructura de puntos “dummy” (normalmente NULL)
# rw  = región rectangular (bounding box)
dxy1 <- deldir(x, y, dpl = NULL, rw = NULL, plotit = TRUE)
plot(dxy1)



# Centroides de las celdas de Voronoi
##########################################

l <- tile.list(dxy1)        # lista de polígonos de Voronoi
g <- tile.centroids(l)      # centroides de cada celda

plot(l, close = TRUE)
points(g, pch = 20, col = "red")



# Ejemplo: nube de puntos
########################

set.seed(1)

pts <- cbind(
  X = rnorm(500, rep(seq(1, 9, by = 2) / 10, 100), 0.022),
  Y = rnorm(500, 0.5, 0.15)
)
plot(pts)


# Clustering con Voronoi
###########################

km1 <- kmeans(pts, centers = 5, nstart = 30, algorithm = "Lloyd")

CL5 <- brewer.pal(5, "Pastel1")

# Voronoi de los centroides del k-means
V <- voronoi.mosaic(km1$centers[,1], km1$centers[,2])

P <- voronoi.polygons(V)


# Visualización
########################

plot(
  pts,
  pch = 19,
  xlim = 0:1,
  ylim = 0:1,
  xlab = "",
  ylab = "",
  col = CL5[km1$cluster]
)

points(
  km1$centers[,1],
  km1$centers[,2],
  pch = 3,
  cex = 1.5,
  lwd = 2
)

plot(V, add = TRUE)



###########################################
# Práctica de Star Wars (el regreso)
###########################################
# Paquetes
###########################################

# Cargar la imagen
###########################################
cargarImagen <- function(imagen, ancho_px, alto_px, etiquetas_x, etiquetas_y){
  plot(imagen, axes = FALSE, ylim = c(alto_px, 0), xaxs = "i", yaxs = "i") #Quitamos los ejes para reestablecerlos en unidades reales
  #Ejes con etiquetas nuevas:
  axis(1, at = seq(0, ancho_px, length.out = length(etiquetas_x)), labels = etiquetas_x)
  axis(2, at = seq(alto_px, 0, length.out = length(etiquetas_y)), labels = etiquetas_y)
  title(main = "Modelo (Unidades Reales)", xlab = "Distancia X", ylab = "Altura Y")
}



# Función: área de un triángulo a partir de 3 puntos (2D)
###########################################
areaTriangulo <- function(p1, p2, p3){
  return(abs(det(cbind(c(p2 - p1), c(p3 - p1)))) / 2)
}

# Función: área total por triangulación de Delaunay
###########################################
areaDelaunay <- function(puntos){
  x <- puntos[1,]
  y <- puntos[2,]
  
  tess <- deldir(x, y)
  triangulos <- triang.list(tess)
  
  A <- 0
  for(tri in triangulos){
    p1 <- c(tri$x[1], tri$y[1])
    p2 <- c(tri$x[2], tri$y[2])
    p3 <- c(tri$x[3], tri$y[3])
    
    A <- A + areaTriangulo(p1, p2, p3)
  }
  
  return(list(areaTotal = A, triangulos = triangulos))
}

###########################################
# PROCESO PRINCIPAL
###########################################
ruta_archivo <- rstudioapi::selectFile(
  caption = "Abrir Proyecto de Puntos",
  path = paste0(getwd(), "/"),
  label = "Abrir",
  filter = "R Data (*.rds)",
  existing = TRUE
)
# Verificar si el usuario seleccionó algo
continuar <- !is.null(ruta_archivo)
if(continuar){
  datos_cargados <- readRDS(ruta_archivo)
  comps_leidas <- datos_cargados$componentes
  comps_px_leidas <- datos_cargados$componentes_px
  escala_leida  <- datos_cargados$escala
  dimensiones_leidas <- datos_cargados$dimensiones
  ruta_img_leida  <- datos_cargados$ruta_imagen
  if(file.exists(ruta_img_leida)){
    colores <- c("cyan", "red", "green", "blue", "violet", "yellow", "orange", "brown", "black")
    img <- load.image(ruta_img_leida)
    anchoPixel <- dimensiones_leidas[1]
    altoPixel <- dimensiones_leidas[2]
    etiquetasX <- round(seq(0, anchoPixel*escala_leida, length.out = 4), 2)
    etiquetasY <- round(seq(0, altoPixel*escala_leida, length.out = 4), 2)
    
    cargarImagen(img, anchoPixel, altoPixel, etiquetasX, etiquetasY)
    
    areaTotal <- 0
    # Procesar cada componente
    for(j in seq_along(comps_leidas)){
      comp <- comps_leidas[[j]]
      # Convertir lista en matriz
      if(is.list(comp)) comp <- simplify2array(comp)
      # Calcular triangulación Delaunay y área
      resultado <- areaDelaunay(comp)
      areaComp <- resultado$areaTotal
      areaTotal <- areaTotal + areaComp
      message(paste0("La componente ", j, "tiene área = ", round(areaComp, 4), " (u^2)"))
      # Dibujar triangulación sobre la imagen
      triangulos <- resultado$triangulos
      color <- colores[(j-1) %% length(colores) + 1]
      
      # Dibujar triangulación
      for(tri in triangulos){
        x <- tri$x
        y <- tri$y
        polygon(x/escala_leida, altoPixel - y/escala_leida, border = color, lty = 3)
      }
      # Dibujar contorno base
      
      # Calcular los índices del perímetro ordenados usando la función chull()
      indices_hull <- chull(comp[1, ], comp[2, ])
      # Filtramos la matriz original usando solo esos índices ordenados
      comp_cerrada <- comp[, indices_hull]
      
      # Dibujar contorno base
      polygon(comp_cerrada[1,]/escala_leida, altoPixel - comp_cerrada[2,]/escala_leida, border = color, lty = 1)
    }
    rstudioapi::showDialog("Resultado",paste0("Área total (Delaunay) = ", round(areaTotal, 4), " unidades^2"))
  }else{
    rstudioapi::showDialog("Aviso", "No se encontró la imagen asociada al proyecto.")
  }
}else{
  message("Carga cancelada o error con el archivo.")
}