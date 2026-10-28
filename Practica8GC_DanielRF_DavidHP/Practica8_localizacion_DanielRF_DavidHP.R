##########################################################
# Práctica de Geometría Computacional: Localización
# Daniel Rodríguez Fernández, David Hernández Paradela
##########################################################

negativo_tolerancia <- function(x, tol = 1e-8) return(x < -tol)
positivo_tolerancia <- function(x, tol = 1e-8) return(x > tol)
nulo_tolerancia     <- function(x, tol = 1e-8) return(abs(x) < tol)

dist2 <- function(p, q) return((p[1]-q[1])^2 + (p[2]-q[2])^2)

giro <- function(p, q, r){
  A <- cbind(q-p, r-p)
  return(det(A))
}

extremosDelSegmento <- function(P, indices){
  puntos <- P[, indices]
  if(any(duplicated(P[, indices], MARGIN = 2))) stop("Puntos repetidos")
  coords <- if(all(nulo_tolerancia(puntos[1,] - puntos[1,1]))) puntos[2,] else puntos[1,]
  sol <- c(indices[which(nulo_tolerancia(coords - min(coords)))[1]],
           indices[which(nulo_tolerancia(coords - max(coords)))[1]])
  return(sol)
}


######################## Funciones para calcular el 1-centro ################################
siguiente <- function(i,n){      # Devuelve el siguiente índice de i en forma modular, teniendo en cuenta que en R los índices empiezan en 1, y no en 0
  sig <- i%%n + 1                # Restar 1 para estar en Z_n, sumar 1 para avanzar, hacer módulo n, volver a sumar 1 para estar en Z_n +1, como antes
  return(sig)
}
anterior <- function(i,n){     # Devuelve el anterior índice de i en forma modular, teniendo en cuenta que en R los índices empiezan en 1, y no en 0
  ant <- (i-2)%%n + 1
  return(ant)
}
adyacentes <- function(i,n){
  return(c(anterior(i,n), siguiente(i,n)))
}

rectangulo_obtuso <- function(P, indices){
  for(i in 1:3){
    indexV  <- indices[i]
    indexO1 <- indices[siguiente(i,3)]
    indexO2 <- indices[anterior(i,3)]
    v1 <- P[, indexO1] - P[, indexV]
    v2 <- P[, indexO2] - P[, indexV]
    if(sum(v1 * v2) <= 0){
      return(list(res = TRUE, lado_opuesto = c(indexO1, indexO2)))
    }
  }
  return(list(res = FALSE))
}

centro2p <- function(p1, p2){
  return(list(radio = sqrt(dist2(p1, p2))/2, centro = (p1 + p2)/2))
}

# El circuncentro3p (xc,yc) cumple: (x_i-xc)^2+(y_i-yc)^2 = R^2, desde i=1 hasta i=3. Igualando R^2, queda un sistema de ecc:
# x_1 ^2 + x_c ^2 - 2x_1*x_c + y_1 ^2 + y_c ^2 - 2y_1*y_c = x_2 ^2 + x_c ^2 - 2x_2*x_c + y_2 ^2 + y_c ^2 - 2y_2*y_c
# x_1 ^2 + x_c ^2 - 2x_1*x_c + y_1 ^2 + y_c ^2 - 2y_1*y_c = x_3 ^2 + x_c ^2 - 2x_3*x_c + y_3 ^2 + y_c ^2 - 2y_3*y_c
# x_2 ^2 + x_c ^2 - 2x_2*x_c + y_2 ^2 + y_c ^2 - 2y_2*y_c = x_3 ^2 + x_c ^2 - 2x_3*x_c + y_3 ^2 + y_c ^2 - 2y_3*y_c

# Del sistema, sale:
# x_1 ^2 - 2x_1*x_c + y_1 ^2 - 2y_1*y_c = x_2 ^2 - 2x_2*x_c + y_2 ^2 - 2y_2*y_c
# x_1 ^2 - 2x_1*x_c + y_1 ^2 - 2y_1*y_c = x_3 ^2 - 2x_3*x_c + y_3 ^2 - 2y_3*y_c
# x_2 ^2 - 2x_2*x_c + y_2 ^2 - 2y_2*y_c = x_3 ^2 - 2x_3*x_c + y_3 ^2 - 2y_3*y_c

# Equivalente:
#  2(x_j - x_i)*x_c + 2(y_j - y_i)*y_c  = x_j ^2 - x_i ^2 + y_j ^2 - y_i ^2 para i = 1,2 y j = 2,3 (y podemos descartar una ecuación: basta con 2)

# El anterior es un sistema de ecuaciones lineales, que R es capaz de resolver de forma eficiente

circuncentro3p <- function(p1, p2, p3){
  A <- rbind(2*(p2-p1), 2*(p3-p1))
  b <- c(sum(p2^2) - sum(p1^2), sum(p3^2) - sum(p1^2))
  centro <- tryCatch(solve(A, b), error = function(e) return(NULL))
  if(is.null(centro)){
    P <- cbind(p1, p2, p3)
    ind <- extremosDelSegmento(P, c(1, 2, 3))
    centro <- (P[, ind[1]] + P[, ind[2]]) / 2
  }
  radio <- sqrt(dist2(centro, p1))
  return(list(centro = centro, radio = radio))
}

circuncentro <- function(P){
  if(ncol(P) == 3){
    return(circuncentro3p(P[,1], P[,2], P[,3]))
  }else if(ncol(P) == 2){
    return(centro2p(P[,1], P[,2]))
  }else if(ncol(P) == 1){
    return(P)
  }else{
    return(NULL)
  }
}

elzingaHearn_1centro <- function(P){
  P <- unique(P, MARGIN = 2)
  n <- ncol(P)
  if(n < 4) return(c(circuncentro(P), indices=seq_len(n)))
  # Llamamos al caso recursivo
  return(elzingaHearn_recursivo(P, 1, 2))
}

elzingaHearn_recursivo <- function(P, i, j){
  n <- ncol(P)
  aux <- centro2p(P[,i], P[,j])
  r2 <- (aux$radio)^2
  c <- aux$centro

  k <- 1
  while(k <= n && !positivo_tolerancia(dist2(P[,k], c) - r2)){ 
    k <- k + 1
  }
  if(k == n + 1) return(list(centro = c, radio = sqrt(r2), indices= c(i,j)))   # Si no hay puntos dentro, el círulo es mínimo

  return(elzingaHearn_decision(P,i,j,k))
}
elzingaHearn_decision <- function(P, i, j, k){
  if(nulo_tolerancia(giro(P[,i], P[,j], P[,k]), tol=1e-6)){   # Si están alineados los tres (caso RARO), repetimos con los extremos
    Psub <- P[, c(i, j, k)]
    ind  <- extremosDelSegmento(Psub, c(1, 2, 3))
    # Recuperamos los índices globales
    globales <- c(i, j, k)
    return(elzingaHearn_recursivo(P, globales[ind[1]], globales[ind[2]]))
  }
  
  tri <- rectangulo_obtuso(P, c(i, j, k))
  if(tri$res){
    return(elzingaHearn_recursivo(P, tri$lado_opuesto[1], tri$lado_opuesto[2]))
  }else{
    return(elzingaHearn_recursivo2(P, i, j, k))
  }
}
elzingaHearn_recursivo2 <- function(P, i, j, k){
  n   <- ncol(P)
  aux <- circuncentro(P[, c(i, j, k)])
  r2  <- (aux$radio)^2
  c   <- aux$centro

  t <- 1
  while(t <= n && !positivo_tolerancia(dist2(P[,t], c) - r2)){
    t <- t + 1
  }
  
  if(t == n +1) return(list(centro = c, radio = sqrt(r2), indices=c(i,j,k))) # Si todos están dentro, es solución

  indices <- c(i, j, k)
  triangulo <- P[, indices]
  distancias2   <- colSums((triangulo - P[, t])^2)
   index_relativo <- which.max(distancias2)   # posición 1, 2 o 3
  indexQ <- indices[index_relativo]

  pos_opuestos <- setdiff(1:3, index_relativo)    # Coger los índices que no son los de Q
  indexO1 <- indices[pos_opuestos[1]]
  indexO2 <- indices[pos_opuestos[2]]
  
  indexR <- if(negativo_tolerancia(giro(P[,indexQ], c, P[,indexO1]) *giro(P[,indexQ], c, P[,t]))) indexO1 else indexO2

  return( elzingaHearn_decision(P, indexQ, indexR, t) )
}

graficar_1circulo <- function(P, fun_1circulo = NULL, nombre = "Elzinga-Hearn"){
  
  if(!is.null(fun_1circulo)){
    tiempo <- system.time({sol <- fun_1circulo(P)})
    c <- sol$centro
    r <- sol$radio
  }
  
  # Calcular límites con margen para que el círculo no quede cortado
  if(!is.null(fun_1circulo)){
    margen <- r * 0.1
    xlim <- c(c[1] - r - margen, c[1] + r + margen)
    ylim <- c(c[2] - r - margen, c[2] + r + margen)
  }else{
    ylim <- range(P[2,])
    xlim <- range(P[1,])
  }
  
  plot(P[1,], P[2,],
       pch = 19, col = "black",
       xlab = "x", ylab = "y",
       main = paste("1-círculo:\n", nombre),
       xlim = xlim, ylim = ylim,
       asp = 1)   # asp = 1 fuerza escala igual en X e Y
  
  if(!is.null(fun_1circulo)){
    symbols(x = c[1], y = c[2], circles = r, inches = FALSE, add = TRUE,fg = "blue", bg = NA, lwd = 2)
    
    points(x = c[1], y = c[2], pch = 3, cex = 1.5, col = "orange", lwd = 2)
    puntosLim <- P[,sol$indices]
    points(x = puntosLim[1,], y = puntosLim[2,], pch = 19, cex = 1.5, col = "yellow", lwd = 2)
    
    t <- tiempo["elapsed"]
    legend("topright",legend = c(paste("Tiempo:", round(t * 1000, 3), "ms"),
                                 paste("Radio: ", round(r, 2)), 
                                 paste("Centro (x): ", round(c[1], 2)),
                                 paste("Centro (y): ", round(c[2], 2))),
           bty = "n")
  }
}

######################## Funciones para calcular la p-mediana ################################

# Distancias
calcular_distancias <- function(nodos) {
  n <- nrow(nodos)
  D <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      D[i, j] <- floor(sqrt((nodos$x[i] - nodos$x[j])^2 + (nodos$y[i] - nodos$y[j])^2))
    }
  }
  rownames(D) <- colnames(D) <- nodos$id
  return(D)
}


# Asignación de clientes por distancias

asignar_clientes <- function(nodos, medianas_ids, capacidad, D){
  n <- nrow(nodos)
  asignacion <- integer(n)          # mediana asignada a cada cliente
  carga <- rep(0, length(medianas_ids))
  names(carga) <- medianas_ids
  no_servido <- integer(0)
  
  # Ordenar clientes por distancia mínima a cualquier mediana (los más cercanos primero)
  idx_med <- which(nodos$id %in% medianas_ids)
  dist_min <- apply(D[, idx_med, drop = FALSE], 1, min)
  orden <- order(dist_min)
  
  for(i in orden){
    id_cliente <- nodos$id[i]
    dem <- nodos$demanda[i]
    
    # Ordenar medianas por distancia al cliente
    d_a_meds <- D[i, idx_med]
    ord_meds <- idx_med[order(d_a_meds)]
    
    asignado <- FALSE
    for(j in ord_meds){
      id_med <- nodos$id[j]
      if(carga[as.character(id_med)] + dem <= capacidad){
        asignacion[i] <- id_med
        carga[as.character(id_med)] <- carga[as.character(id_med)] + dem
        asignado <- TRUE
        break
      }
    }
    if(!asignado) no_servido <- c(no_servido, id_cliente)
  }
  
  list(asignacion = asignacion, carga = carga, no_servido = no_servido)
}


# Coste total

calcular_coste <- function(nodos, asignacion, D) {
  coste <- 0
  for (i in seq_len(nrow(nodos))){
    med <- asignacion[i]
    if(med != 0){
      j <- which(nodos$id == med)
      coste <- coste + D[i, j]   # sin ponderar por demanda
    }
  }
  return(coste)
}


# Voraz 

voraz_pmediana <- function(capacidad, P, p, demandas) {
  nodos <- data.frame(id = 1:ncol(P), x = P[1,], y = P[2,], demanda = demandas)
  n <- ncol(P)
  D <- calcular_distancias(nodos)
  
  medianas_seleccionadas <- integer(0)
  
  # Asignaciones (sin valorar capacidad)
  for (iter in seq_len(p)){
    candidatos_restantes <- setdiff(nodos$id, medianas_seleccionadas)
    mejor_coste_iter <- Inf
    mejor_nodo_iter <- NA
    
    for (v in candidatos_restantes) {
      meds_prueba <- c(medianas_seleccionadas, v)
      idx_med <- which(nodos$id %in% meds_prueba)
      dist_min <- apply(D[, idx_med, drop = FALSE], 1, min)
      c_prueba <- sum(dist_min)                # coste sin capacidad
      if(c_prueba < mejor_coste_iter){
        mejor_coste_iter <- c_prueba
        mejor_nodo_iter  <- v
      }
    }
    
    medianas_seleccionadas <- c(medianas_seleccionadas, mejor_nodo_iter)
  }
  
  # Asignación final (con capacidad)
  resultado_asig <- asignar_clientes(nodos, medianas_seleccionadas, capacidad, D)
  coste_final <- calcular_coste(nodos, resultado_asig$asignacion, D)
  
  return(list(
    centros = medianas_seleccionadas,eleccionClientes = resultado_asig$asignacion,
    coste = coste_final, existeSol = length(resultado_asig$no_servido) == 0))
}


############### Graficar el problema de la p-mediana #############################

graficar_pmediana <- function(capacidad, P, nCentros, arrayDemandas, fun_pmediana = NULL){
  plot(P[1,], P[2,], pch = 20, col = "gray",
       xlab = "x", ylab = "y",
       main = paste("Problema de la p-mediana (p =", nCentros, ")"),
       asp = 1)

  if(!is.null(fun_pmediana)){
    tiempo <- system.time({ sol <- fun_pmediana(capacidad, P, nCentros, arrayDemandas) })
    centros_indices <- sol$centros
    asignaciones <- sol$eleccionClientes
    colores_centros <- rainbow(nCentros)
    mapa_colores <- setNames(colores_centros, centros_indices)

    for(i in 1:ncol(P)){
      centro_actual <- asignaciones[i]
      color_grupo   <- mapa_colores[as.character(centro_actual)]
      segments(P[1,i], P[2,i], P[1,centro_actual], P[2,centro_actual],
               col = color_grupo, lty = 1, lwd = 0.5)
    }
    points(P[1, centros_indices], P[2, centros_indices],
           col = colores_centros, pch = 19, cex = 1.5)
    points(P[1, centros_indices], P[2, centros_indices],
           pch = 3, col = "black", lwd = 2)

    # BUG 4 CORREGIDO: sol$existeSol (no sol$factibilidad)
    strBooleano <- if(sol$existeSol) "El problema tiene solución" else
                     "No se ha encontrado una solución factible"
    t <- tiempo["elapsed"]
    legend("topright",
           legend = c(paste("Coste total:", round(sol$coste, 2)),
                      paste("Tiempo:", round(t * 1000, 2), "ms"),
                      strBooleano),bty = "n", cex = 0.8)
  }
}


############# Función para cargar los datos #############

cargar_problema <- function(){
  nombre_archivo <- file.choose()
  if(!file.exists(nombre_archivo)) stop(paste("Error: El archivo '", nombre_archivo, "' no existe."))

  linea1<- scan(nombre_archivo, nlines = 1, quiet = TRUE)
  n <- linea1[1]
  p <- linea1[2]
  capacidad <- linea1[3]

  df_nodos <- read.table(nombre_archivo, skip = 1, header = FALSE)
  colnames(df_nodos) <- c("id", "x", "y", "demanda")

  P <- t(as.matrix(df_nodos[, c("x", "y")]))
  arrayDemandas <- df_nodos$demanda

  cat("Resumen de Instancia\n")
  cat("Nodos (n):", n, " Servidores (p):", p, "\n")
  cat("Capacidad:", capacidad, "\n")

  return(list(n = n, p = p, capacidad = capacidad, P = P, demandas = arrayDemandas))
}


##################### "Main" ###############################

datos <- cargar_problema()
graficar_1circulo(datos$P, elzingaHearn_1centro)
graficar_pmediana(datos$capacidad, datos$P, datos$p, datos$demandas, voraz_pmediana)
