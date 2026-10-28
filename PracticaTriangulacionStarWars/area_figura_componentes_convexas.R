##################################################################
# Práctica 3: Star Wars. Parte 1
# Fecha: 25 de Febrero de 2026
# Autores: David Hernández Paradela y Daniel Rodríguez Fernández
##################################################################

#Función para intercambiar puntos de la lista según el orden
intercambiarPts <- function(listaPts, i, j){
  aux <- listaPts[[i]]
  listaPts[[i]] <- listaPts[[j]]
  listaPts[[j]] <- aux
  return(listaPts)
}
#Devolver el índice del punto más alto en una lista:
puntoMasAlto <- function(listaPts){
  index <- 1
  PtSol <- listaPts[[index]]
  
  for(i in 1:length(listaPts)){
    if(listaPts[[i]][2] > PtSol[2]){
      index <- i
      PtSol <- listaPts[[index]]
    }
  }
  return(index)
}
#Tomamos dos vectores y devolvemos su ángulo, salvo nulidad
angulo <- function(vector1, vector2){
  prod_escalar <- sum(vector1*vector2)
  modulo1 <- sqrt(sum(vector1^2))
  modulo2 <- sqrt(sum(vector2^2))
  if(modulo1 == 0 || modulo2 == 0){
    stop("No hay ángulo porque alguno de los veectores es nulo")
  }
  cos = prod_escalar/(modulo1*modulo2)
  cos <- max(-1, min(1, cos))   #Evitar errores de redondeo
  return(acos(cos))
}
#Dada una lista de puntos y dos índices, la función devuelve el índice de menor distancia a una referencia
ptMenorDistancia <- function(ptRef, listaPts, index1, index2){
  v1 <- (listaPts[[index1]]-ptRef)
  v2 <- (listaPts[[index2]]-ptRef)
  d1 <- sum(v1^2)
  d2 <- sum(v2^2)
  
  if(d1 < d2){
    return(index1)
  }else{
    return(index2)
  }
}
#Ordenamos los puntos como en el algoritmo de scanner de Graham
ordenarPts_Reloj <- function(listaPts){
  listaAngulos <- list()
  listaPtsNueva<- list()
  
  listaPts <- intercambiarPts(listaPts, puntoMasAlto(listaPts), 1) #Cambio de lugar (el más alto queda el primero)
  ptAlto <- listaPts[[1]]
  listaPtsNueva[[1]] <- ptAlto
  listaAngulos[[1]] <- 0   #En el primer índice, tiene 0 (forma 0 grados consigo)
  
  #Ordenamos por inserción... O(n^2)
  for(i in 2:length(listaPts)){
    listaAngulos[[i]] <- angulo(ptAlto-listaPts[[i]], c(1,0)) #Ángulo con la horizontal, de cada punto con el primero
    listaPtsNueva[[i]] <- listaPts[[i]] #Inserto nuevo punto
    j = i
    while(j>2 && listaAngulos[[j-1]] < listaAngulos[[j]]){
      listaPtsNueva<-intercambiarPts(listaPtsNueva, j, j-1)
      listaAngulos<-intercambiarPts(listaAngulos, j, j-1)
      j<-j-1
    }
  }
  #Si hay ángulos repetidos, eliminamos los puntos que están en medio:
  indices_a_borrar <- c()
  for(i in length(listaPtsNueva):3){
    if(abs(listaAngulos[[i]] - listaAngulos[[i-1]]) <= 1e-7){
      # Decide cuál borrar (al más cercano)
      menorIndex <- ptMenorDistancia(ptAlto, listaPtsNueva, i, i-1)
      indices_a_borrar <- c(indices_a_borrar, menorIndex)
    }
  }
  
  if(length(indices_a_borrar) > 0){
    listaPtsNueva <- listaPtsNueva[-indices_a_borrar]
  }
  
  return(listaPtsNueva)
}
#Devuelve el ángulo de un triángulo por determinantes
areaTriangulo <- function(u,v){
  return(abs(u[1]*v[2]- u[2]*v[1])/2)
}
#Combinamos las funciones anteriores:
triangulacionAreas<- function(listaPts){
  #Ordenamos los puntos:
  listaPts <- ordenarPts_Reloj(listaPts)
  listaVectores <- list()
  #Creamos la lista de vectores desde el primero
  for(i in 2:length(listaPts)){
    listaVectores[[i-1]] <- listaPts[[i]] - listaPts[[1]]
  }
  #Calculamos áreas, y sumamos:
  sumaArea <- 0
  for(j in 1:(length(listaPts)-2)){   #Un elemento menos que los puntos, y el último no lo tiene que verificar
    sumaArea <- sumaArea + areaTriangulo(listaVectores[[j]], listaVectores[[j+1]])
  }
  return(sumaArea)
}


#Ejemplo de funcionamiento
J <- c(1,3)
H <- c(17/2,3)
G <- c(4,0)
P <- c(6,5)
B <- c(7.5,5)
C <- c(0,2)
D <- c(0,0)
A <- c(3,5)
E <- c(10,0)
lista <- list(J,H,G,P,B,C,D,A,E)
area <- triangulacionAreas(lista)
print(area)



####### Segunda parte: seleccionar y usar la imagen correcta
#install.packages("imager")
library(imager)
#install.packages("pracma")
library(pracma)
graphics.off()

#Seleccionar imagen
rutaImg <- rstudioapi::selectFile(
  caption = "Selecciona el plano de tu figura",
  path = paste0(getwd(), "/"),
  label = "Abrir",
  filter = "Imágenes (*.png;*.jpg;*.jpeg;*.bmp)",
  existing = TRUE
)
#Cargar imagen
if(!is.null(rutaImg)){
  img <- load.image(rutaImg)
  plot(img)
}

##Escalado de la imagen---------------------------------------------------
confirmado <- FALSE
while (!confirmado) {
  #Redibuja la imagen (se borran los puntos anteriores)
  plot(img)
  rstudioapi::showDialog("Calibración de escala", "Selecciona (haciendo click) dos puntos que tengan una DIFERENCIA DE ALTURA conocida:")
  calib <- locator(n = 2, type = "p", col = "red", pch = 19)
  points(calib$x[1], calib$y[2], col = "white", pch = 19, cex = 1)
  abline(h = calib$y[2], col = "grey", lty=2)
  segments(x0 = calib$x[1], y0 = calib$y[2], x1 = calib$x[1], y1 = calib$y[1], col = "white", lwd = 3)
  
  confirmado <- rstudioapi::showQuestion(
    title = "Confirmar puntos",
    message = "¿Están los puntos en el lugar correcto?",
    ok = "Sí, continuar",
    cancel = "No, repetir"
  ) && (calib$y[2] != calib$y[1])
  
  if(calib$y[2] == calib$y[1]){
    rstudioapi::showDialog("Selección no válida", "Los puntos deben tener alturas distintas")
  }
}
confirmado <- FALSE
alturaReal <- 0
#Altura real entre los pntos escogidos
while(!confirmado){
  alturaReal <- as.numeric(rstudioapi::showPrompt(
    title = "Escalado de imagen",
    message = "Introduce la diferencia de altura real en las unidades que consideres: ",
    default = ""
  ))
  confirmado <- !is.nan(alturaReal) && (alturaReal > 0)
  if(is.nan(alturaReal)){
    rstudioapi::showDialog("Error numérico", "La altura debe ser un número")
  }else if(alturaReal <= 0){
    rstudioapi::showDialog("Error numérico", "La altura debe ser mayor a 0")
  }
}

alturaPixel <- abs(calib$y[2] - calib$y[1])
factorEscala <- alturaReal/alturaPixel

#Ancho y alto de imagen, para determinar límites del gráfico luego
anchoPixel <- width(img)
altoPixel <- height(img)

#Etiquetas para los ejes en unidades reales
etiquetasX <- round(seq(0, anchoPixel*factorEscala, length.out = 4), 2)
etiquetasY <- round(seq(0, altoPixel*factorEscala, length.out = 4), 2)

cargarImagen <- function(imagen, ancho_px, alto_px, etiquetas_x, etiquetas_y){
  plot(img, axes = FALSE, ylim = c(alto_px, 0), xaxs = "i", yaxs = "i") #Quitamos los ejes para reestablecerlos en unidades reales
  
  #Ejes con etiquetas nuevas:
  axis(1, at = seq(0, ancho_px, length.out = length(etiquetas_x)), labels = etiquetas_x)
  axis(2, at = seq(alto_px, 0, length.out = length(etiquetas_y)), labels = etiquetas_y)
  
  title(main = "Modelo (Unidades Reales)", xlab = "Distancia X", ylab = "Altura Y")
}
cargarImagen(img, anchoPixel, altoPixel, etiquetasX, etiquetasY)


##Selección de omponentes de la imagen-------------------------------------
colores <- c("cyan", "red", "green", "blue", "violet", "yellow", "orange", "brown", "black")
lista_componentes <- list()
lista_componentes_pixeles <- list()
i<- 1
continuar <- TRUE
while(continuar){
  mensaje <- paste(
    "Escoge una componente convexa de tu figura, y selecciona puntos del BORDE de esa componente.",
    "\nSi la componente es un polígono, se recomienda escoger los vértices. Si tiene curvas, se recomienda escoger muchos puntos de las mismas, por precisión.",
    "\nEl orden no afectará al cálculo posterior, pero se recomienda escoger puntos 'consecutivos' en orden para facilidar la visualización."
  )
  rstudioapi::showDialog("Selección de puntos", mensaje)
  confirmado <- FALSE
  while(!confirmado){
    puntos <- locator(type = "p", col = colores[(i-1)%%9+1], lwd = 2, pch = 19)
    
    # Dataframe para eliminar puntos repetidos
    df_puntos <- data.frame(x = puntos$x, y = puntos$y)
    df_limpio <- df_puntos[!duplicated(df_puntos), ]
    # Vuelta al formato anterior:
    puntos$x <- df_limpio$x
    puntos$y <- df_limpio$y
    # Se explica qué puntos se han elminado:
    puntos_eliminados <- nrow(df_puntos) - nrow(df_limpio)
    
    if(puntos_eliminados > 0) {
      message(paste("Se han eliminado", puntos_eliminados, "puntos duplicados."))
    }
    
    confirmado <- rstudioapi::showQuestion(
      title = "Confirmar puntos",
      message = "¿Estás seguro de tu selección? Recuerda que no debes coger figuras no convexas",
      ok = "Sí, continuar",
      cancel = "No, repetir"
    )
    if(length(puntos$x) <= 2){
      confirmado <- FALSE
      rstudioapi::showDialog("Error", "Debes escoger al menos tres puntos no alineados")
    }
    if(confirmado){
      #Guardamos los puntos seleccionados, en coordenadas a escala, y también por píxeles
      listaPtsNuevos <- list()
      listaPtsNuevosPx <- list()
      for(j in 1:length(puntos$x)){
        listaPtsNuevos[[j]] <- c(factorEscala*puntos$x[j], factorEscala*(altoPixel-puntos$y[j]))
        listaPtsNuevosPx[[j]] <- c(puntos$x[j], puntos$y[j])
      }
      lista_componentes[[i]] <- listaPtsNuevos
      lista_componentes_pixeles[[i]] <- listaPtsNuevosPx
    }
    
    # Recargar imagen desde 0
    cargarImagen(img, anchoPixel, altoPixel, etiquetasX, etiquetasY)
    for(j in 1:length(lista_componentes_pixeles)){
      # Cambio de formato
      lista_componentes_pixeles[[j]] <- ordenarPts_Reloj(lista_componentes_pixeles[[j]])
      matriz_puntos_px <- do.call(rbind, lista_componentes_pixeles[[j]])
      df_puntos <- as.data.frame(matriz_puntos_px)
      colnames(df_puntos) <- c("x", "y")
      color <- colores[(j-1)%%9+1]
      lines(df_puntos$x, df_puntos$y, type = "b", col = color)
      end <- length(df_puntos$x)
      lines(df_puntos$x[c(1,end)], df_puntos$y[c(1,end)], type = "b", col = color) #Arista final
    }
  }
  
  continuar <- rstudioapi::showQuestion(
    title = "Confirmación",
    message = "Se han añadido los puntos anteriores. ¿Deseas añadir más componentes convexas?",
    ok = "Sí",
    cancel = "No"
  )
  i<- i+1
}
#Se calcula el área de ambas maneras, para comprobar que es lo mismo
areaTotal <- 0
areaComponente <- 0
areaPorPoligono <- 0
for(j in 1:length(lista_componentes)){
  areaComponente <- triangulacionAreas(lista_componentes[[j]])
  areaTotal <- areaTotal + areaComponente
  lista_componentes[[j]] <- ordenarPts_Reloj(lista_componentes[[j]])
  puntos_otraForma <- do.call(rbind,lista_componentes[[j]])
  areaPorPoligono <- abs(polyarea(puntos_otraForma[, 1], puntos_otraForma[, 2]))
  message(paste0("La componente ", j, "tiene área ", areaComponente, "por triángulos"))
  message(paste0("La componente ", j, "tiene área ", areaPorPoligono, "según R"))
}  
message(paste0("El área total por triangulación es ", areaTotal))
  
#Guardamos el archivo con los puntos escogidos
ruta_guardado <- rstudioapi::selectFile(
  caption = "Guardar Proyecto",
  label = "Guardar",
  filter = "R Data (*.rds)",
  existing = FALSE
)
if(!is.null(ruta_guardado)){
  # Extensión .rds (objetos de R)
  if(!grepl("\\.rds$", ruta_guardado, ignore.case = TRUE)){
    ruta_guardado <- paste0(ruta_guardado, ".rds")
  }
  # Guardado de imagen (me aseguro de que esté en la carpeta)
  rutaNuevaImg <- gsub("\\.rds$", ".png", ruta_guardado) 
  # Copiamos la imagen
  file.copy(from = rutaImg, to = rutaNuevaImg, overwrite = TRUE)
  # Lista de datos para guardar
  datos_proyecto <- list(
    ruta_imagen = rutaNuevaImg,
    componentes = lista_componentes,
    componentes_px = lista_componentes_pixeles,
    escala = factorEscala,         
    dimensiones = c(anchoPixel, altoPixel)
  )
  # Guardado de datos
  saveRDS(datos_proyecto, file = ruta_guardado)
  rstudioapi::showDialog("Éxito", paste("Proyecto guardado en:", ruta_guardado))
  
}else{
  message("Operación de guardado cancelada por el usuario.")
}