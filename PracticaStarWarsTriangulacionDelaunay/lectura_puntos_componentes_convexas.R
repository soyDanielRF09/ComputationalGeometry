intercambiarPts <- function(listaPts, i, j){
  aux <- listaPts[[i]]
  listaPts[[i]] <- listaPts[[j]]
  listaPts[[j]] <- aux
  return(listaPts)
}

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
areaTriangulo <- function(u,v){
  return(abs(u[1]*v[2]- u[2]*v[1])/2)
}
triangulacionAreas<- function(listaPts){
  listaPts <- ordenarPts_Reloj(listaPts)
  listaVectores <- list()
  for(i in 2:length(listaPts)){
    listaVectores[[i-1]] <- listaPts[[i]] - listaPts[[1]]
  }
  sumaArea <- 0
  for(j in 1:(length(listaPts)-2)){   #Un elemento menos que los puntos, y el último no lo tiene que verificar
    sumaArea <- sumaArea + areaTriangulo(listaVectores[[j]], listaVectores[[j+1]])
  }
  return(sumaArea)
}


########## Segunda parte ##########
cargarImagen <- function(imagen, ancho_px, alto_px, etiquetas_x, etiquetas_y){
  plot(img, axes = FALSE, ylim = c(alto_px, 0), xaxs = "i", yaxs = "i") #Quitamos los ejes para reestablecerlos en unidades reales
  
  #Ejes con etiquetas nuevas:
  axis(1, at = seq(0, ancho_px, length.out = length(etiquetas_x)), labels = etiquetas_x)
  axis(2, at = seq(alto_px, 0, length.out = length(etiquetas_y)), labels = etiquetas_y)
  
  title(main = "Modelo (Unidades Reales)", xlab = "Distancia X", ylab = "Altura Y")
}

# Tomamos el archivo RDS generado con el otro programa
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
  # Guardamos los datos en variables
  comps_leidas <- datos_cargados$componentes
  comps_px_leidas <- datos_cargados$componentes_px
  escala_leida <- datos_cargados$escala
  dimensiones_leidas <- datos_cargados$dimensiones
  ruta_img_leida <- datos_cargados$ruta_imagen
  
  # Abrimos la imagen si se puede
  if(file.exists(ruta_img_leida)){
    colores <- c("cyan", "red", "green", "blue", "violet", "yellow", "orange", "brown", "black")
    img <- load.image(ruta_img_leida)
    anchoPixel <- dimensiones_leidas[1]
    altoPixel <- dimensiones_leidas[2]
    etiquetasX <- round(seq(0, anchoPixel*escala_leida, length.out = 4), 2)
    etiquetasY <- round(seq(0, altoPixel*escala_leida, length.out = 4), 2)
    cargarImagen(img, anchoPixel, altoPixel, etiquetasX, etiquetasY)
    
    
    # Redibujo los puntos y las líneas
    for(j in 1:length(comps_px_leidas)){
      # Cambio de formato
      comps_px_leidas[[j]] <- ordenarPts_Reloj(comps_px_leidas[[j]])
      matriz_puntos_px <- do.call(rbind, comps_px_leidas[[j]])
      df_puntos <- as.data.frame(matriz_puntos_px)
      colnames(df_puntos) <- c("x", "y")
      color <- colores[(j-1)%%9+1]
      lines(df_puntos$x, df_puntos$y, type = "b", col = color)
      end <- length(df_puntos$x)
      lines(df_puntos$x[c(1,end)], df_puntos$y[c(1,end)], type = "b", col = color) #Arista final
    }
    
    rstudioapi::showDialog("Éxito", "Proyecto y datos cargados correctamente.")
    
  }else{
    rstudioapi::showDialog("Aviso", "Los datos se han cargado, pero con la imagen ha habido un error")
  }
  rstudioapi::showDialog("", "Se procede a calcular el área de la figura a través de triangulaciones...")
  areaTotal <- 0
  for(j in 1:length(comps_leidas)){
    comp <- comps_leidas[[j]]
    areaComponente <- triangulacionAreas(comp)
    areaTotal <- areaTotal + areaComponente
    comp <- ordenarPts_Reloj(comp)
    comps_leidas[[j]] <- comp
    puntos_otraForma <- do.call(rbind,comp)
    areaPorPoligono <- abs(polyarea(puntos_otraForma[, 1], puntos_otraForma[, 2]))
    message(paste0("La componente ", j, "tiene área ", areaComponente, "por triángulos"))
    message(paste0("La componente ", j, "tiene área ", areaPorPoligono, "según R"))
    
    # Dibujar triangulaciones (recuperando la escala en píxeles con los puntos ya ordenados)
    ptAlto <- comp[[1]]
    ptAlto[1] <- ptAlto[1]/escala_leida
    ptAlto[2] <- altoPixel - ptAlto[2]/escala_leida
    for(i in 2:length(comp)){
      pt <- comp[[i]]
      pt[1] <- pt[1]/escala_leida
      pt[2] <- altoPixel - pt[2]/escala_leida
      segments(ptAlto[1], ptAlto[2], pt[1], pt[2], col = "white", lty = 5)
    }
  }  
  rstudioapi::showDialog(
    "Resultado",
    paste0("Con la triangulación ofrecida en la imagen, y los puntos dados, se ha podido aproximar un área de ", areaTotal)
  )
}else{
  message("Carga cancelada. Ha habido algún error con el archivo")
}
