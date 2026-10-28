##########################################################################
# Práctica de Geometría Commputacional 9: Triangulaciones y coloraciones
# Daniel Rodríguez Fernández, David Hernández Paradela
##########################################################################

# Librería para grafos:
#install.packages("igraph")

############### Funciones útiles ##################

negativo_tolerancia <- function(x, tol = 1e-8) {
  return(x < -tol)
}
positivo_tolerancia <- function(x, tol = 1e-8) {
  return(x > tol)
}
nulo_tolerancia <- function(x, tol = 1e-8) {
  return(abs(x) < tol)
}
dist2 <- function(p, q){
  return((p[1] - q[1])^2 + (p[2] - q[2])^2)
}
# Devuelve > 0 si el giro es a la izquierda (antihorario), < 0 si el giro es a la derecha, o 0 si son colineales
areaOrientada <- function(p, q, r){
  A <- cbind(q-p,r-p)
  return(det(A))
}


############## Triangulaciones ###########
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

convexo<- function(Poli,indices,i){
  # Índices consecutivos de nuestro subpolígono
  n <- length(indices)
  
  p_ant <- Poli[, indices[anterior(i,n)]]
  p_act <- Poli[, indices[i]]
  p_sig <- Poli[, indices[siguiente(i,n)]]
  
  # Un vértice es convexo si el giro (p_ant, p_act, p_sig) es antihorario
  
  a <- areaOrientada(p_ant, p_act, p_sig)
  
  # Partimos de un sentido antihorario: el determinante debe ser positivo
  return(positivo_tolerancia(a))
}


recta <- function(Poli,indices,i){
  # Índices consecutivos de nuestro subpolígono
  n <- length(indices)
  
  p_ant <- Poli[, indices[anterior(i,n)]]
  p_act <- Poli[, indices[i]]
  p_sig <- Poli[, indices[siguiente(i,n)]]
  
  # Un vértice es convexo si el giro (p_ant, p_act, p_sig) es antihorario
  
  a <- areaOrientada(p_ant, p_act, p_sig)
  
  # Partimos de un sentido antihorario: el determinante debe ser positivo
  return(nulo_tolerancia(a))
}
enTriangulo<- function(Poli,indices,i, l){   # Función para comprobar si un punto está en el interior de un triángulo
  # Índices consecutivos de nuestro subpolígono
  n <- length(indices)
  
  i_ind <- indices[i]
  j_ind <- indices[(i-2)%%n + 1]
  k_ind <- indices[i%%n + 1]
  
  p_ant <- Poli[, j_ind]
  p_act <- Poli[, i_ind]
  p_sig <- Poli[, k_ind]
  
  p_l <- Poli[,indices[l]]
  
  a <- areaOrientada(p_ant, p_act, p_l)    # Tiene que ser positivo (antihorario)
  b <- areaOrientada(p_act, p_sig, p_l)    # Tiene que ser positivo (antihorario)
  c <- areaOrientada(p_sig, p_ant, p_l)    # Tiene que ser positivo (antihorario)
  
  return(positivo_tolerancia(a) && positivo_tolerancia(b) && positivo_tolerancia(c))
}

esOreja <- function(Poli, indices, i){     # Devuelve TRUE si es oreja, si no: j, el más cercano en distancia a i por dentro del triángulo. USAR SOLO SI p_i ES CONVEXO
  if(!convexo(Poli, indices,i)) stop("Error: la función solo se aplica a vétices convexos")
  n <- length(indices)
  candidato <- -1                    # Si no es oreja, nos quedaremos con el vértice más cercano con p_i
  dCandidato <- Inf
  j <- siguiente(siguiente(i,n),n)                # El siguiente del siguiente índice, o empezamos desde el principio (modular) si nos pasamos de n
  indAdyIzq <- anterior(i,n)
  while(j != indAdyIzq){                            # Cuando llegamos al adyacente desde la izquierda, paramos
    if(enTriangulo(Poli,indices, i, j)){            # Si está dentro del triángulo formado por los adyacentes de p_i, buscamos el que tiene menor distancia con a
      d <- dist2(Poli[,indices[i]], Poli[,indices[j]])
      if(d<dCandidato){
        dCandidato <- d
        candidato <- j
      }
    }
    j <- siguiente(j,n)      
  }
  if(candidato == -1){  # Si no hemos encontrado ningún vértice dentro del triángulo, tenemos una oreja. Nueva diagonal: los adyacentes de p_i
    return(list(oreja = TRUE))
  }else{
    return(list(oreja = FALSE, verticeB = candidato))   
  }
}

buscarDiagonal <- function(Poli, indices){   # Devuelve c(a,b), índices de indices para la nueva diagonal
  n <- length(indices)
  i <- 1
  encontrado <- FALSE
  while(i <= n && !convexo(Poli, indices, i)){   # Buscamos un vértice convexo (siempre hay uno)
    i <- i+1
  }
  # En ocasiones, puede ocurrir que al eliminar orejas y separar en subpolígonos, terminemos con una recta de puntos alineados (caso peine)
  if(i == n+1){  # Si no hay ningún convexo, no es un polígono. Caso 1: todos están alineados (caso del peine). Caso 2: hay algún reflex, pero 0 convexos. No es un polígono real
    t <- 1
    while(t <= n && recta(Poli, indices, t)){   # Buscamos si algún vértice no está alineado
      t <- t+1
    }
    if(t == n+1){
      return(NULL)
    }else{
      stop(paste("Error: ¿estás seguro de que era un polígono? No se encuentra ningún vértice convexo... Quedan ", n, " vértices en esta componente"))
    }
  } 
  res <- esOreja(Poli, indices, i)
  if(res$oreja){
    diagonal <- adyacentes(i,n)
  }else{
    diagonal <- c(i,res$verticeB)
  }
  return(diagonal)
}

triangulacion_rec <- function(Poli, indices = NULL){    # Poli es matriz de puntos de un poligono ORDENADOS en sentido antihorario,
  Poli <- unique(Poli, MARGIN=2)                                                                      # triangulos es matriz de 3 filas, con los índices de triángulos seleccionados
  n <- ncol(Poli)
  if(is.null(indices)){
    indices <- seq_len(n)
  }
  
  v <- length(indices)
  if(v==3){
    return(list(indices))
  }else if(v<3){
    stop("Error: el polígono debe tener más de dos vértices")
  }
  nuevaDiag <- buscarDiagonal(Poli, indices)     # Recorremos los índices con otros índices, del 1 al n
  if(is.null(nuevaDiag)){
    return(c())   # Si no había más diagonales, es porque nos han quedado algunos puntos alineados en esta componente, y no aportan nada
  }
  # Tomamos los índices en el sentido antihorario
  # Recorremos con i, j
  i <- nuevaDiag[1]
  j <- nuevaDiag[2]
  indices1 <- c(indices[nuevaDiag[1]])
  indices2 <- c(indices[nuevaDiag[2]])
  
  while(i != nuevaDiag[2]){      # Terminará añadiendo nuevaDiag[2]
    i <- siguiente(i,v)
    indices1 <- c(indices1, indices[i])
  }
  while(j != nuevaDiag[1]){      # Terminará añadiendo nuevaDiag[1]
    j <- siguiente(j,v)
    indices2 <- c(indices2, indices[j])
  }
  
  return(c(triangulacion_rec(Poli, indices1), triangulacion_rec(Poli, indices2)))
}


##### Función para graficar ############ 
graficar_triangulacion <- function(Poli, fun_triangulacion = NULL, 
                                   mostrar_indices = FALSE, rellenar = TRUE){
  n <- ncol(Poli)
  
  # Dibujar polígono base
  plot(Poli[1, ], Poli[2, ],
       type = "n",
       xlab = "x", ylab = "y",
       main = "Triangulación de polígono",
       asp = 1)
  
  # Dibujar contorno del polígono
  polygon(Poli[1, ], Poli[2, ], border = "black", lwd = 2)
  
  # Dibujar vértices
  points(Poli[1, ], Poli[2, ], pch = 19)
  
  if(mostrar_indices){
    text(Poli[1, ], Poli[2, ], labels = 1:n, pos = 3, cex = 0.8)
  }
  
  if(!is.null(fun_triangulacion)){
    
    # Medir tiempo
    tiempo <- system.time({
      triangulos <- fun_triangulacion(Poli)
    })
    
    nTri <- length(triangulos)
    colores <- rainbow(nTri, alpha = 0.4)
    
    # Dibujar cada triángulo
    for(k in seq_along(triangulos)){
      tri <- triangulos[[k]]
      
      x <- Poli[1, tri]
      y <- Poli[2, tri]
      
      if(rellenar){
        polygon(x, y, col = colores[k], border = "black")
      } else {
        polygon(x, y, border = colores[k], lwd = 2)
      }
    }
    
    # Redibujar contorno encima (así queda más limpio)
    polygon(Poli[1, ], Poli[2, ], border = "black", lwd = 2)
    
    # Leyenda
    t <- tiempo["elapsed"]
    legend("topright",
           legend = c(paste("Nº triángulos:", nTri),
                      paste("Tiempo:", round(t * 1000, 2), "ms")),
           bty = "n", cex = 0.8)
  }
}

Poli1 <- matrix(c(0,1,2,1.5,0.5,
                 0,0,1,2,1.5), nrow = 2, byrow = TRUE)

# "Peine"
n_dientes <- 10
x <- c(0)
y <- c(10)
for(i in 1:n_dientes){
  x <- c(x, i*2 - 1, i*2, i*2 + 1)
  y <- c(y, 0, 10, 10)
}
x <- c(x, 0)
y <- c(y, 15) # Cerramos
Poli_Peine <- rbind(x, y)

Poli_Boca <- matrix(c(
  0, 0,
  10, 0,
  10, 10,
  1, 5,   # Este es el vértice traicionero (muy profundo)
  0, 10
), nrow = 2, byrow = FALSE)

Poli_Limite <- matrix(c(
  0, 0,
  5, 0.0000000001, # Casi colineal con (0,0) y (10,0)
  10, 0,
  10, 10,
  0, 10
), nrow = 2, byrow = FALSE)
Poli_Aguja <- matrix(c(
  0, 0,
  100, 0.1,
  0, 0.2
), nrow = 2, byrow = FALSE)

# Generamos 500 ángulos entre 0 y 2*pi
n_puntos <- 100
t <- seq(0, 2*pi, length.out = n_puntos + 1)[-(n_puntos + 1)]  # Quitamos el último, porque genera el mismo ánngulo que el primero
# El radio varía para crear entrantes y salientes
radio <- 10 + 2*sin(5*t) + cos(10*t)

# Convertimos a coordenadas polares (x, y)
x <- radio * cos(t)
y <- radio * sin(t)

# Por columnas
Poli_Ameba <- rbind(x, y)

graficar_triangulacion(Poli1, triangulacion_rec, mostrar_indices = TRUE)
graficar_triangulacion(Poli_Peine, triangulacion_rec, mostrar_indices = TRUE)
graficar_triangulacion(Poli_Boca, triangulacion_rec, mostrar_indices = TRUE)
graficar_triangulacion(Poli_Limite, triangulacion_rec, mostrar_indices = TRUE)
graficar_triangulacion(Poli_Aguja, triangulacion_rec, mostrar_indices = TRUE)
graficar_triangulacion(Poli_Ameba, triangulacion_rec, mostrar_indices = TRUE)


#################### SEGUNDA PARTE: coloración de grafos ################################

matriz_grafo <- function(nVertices, aristas){
  
  if(ncol(aristas) != 2){
    index <- t(aristas)
  } else {
    index <- aristas
  }
  
  A <- matrix(0, nrow = nVertices, ncol = nVertices)
  
  A[cbind(index[,1], index[,2])] <- 1
  A[cbind(index[,2], index[,1])] <- 1
  
  return(A)
}

coloracion <- function(nVertices, aristas, coloreados = NULL){
  
  matrizGrafo <- matriz_grafo(nVertices, aristas)
  grados <- colSums(matrizGrafo)
  
  if(is.null(coloreados)){
    coloreados <- rep(0, nVertices)
  }
  
  while(any(coloreados == 0)){
    # Calcular saturaciones solo para no coloreados
    saturaciones <- rep(0, nVertices)
    for(i in 1:nVertices){
      if(coloreados[i] == 0){
        saturaciones[i] <- saturacion(i, matrizGrafo, coloreados)
      }
    }
    
    # Elegir vértice:
    # Prioridad: el más saturado
    max_sat <- max(saturaciones[coloreados == 0])
    candidatos <- which(saturaciones == max_sat & coloreados == 0)
    
    # Seunda prioridad: grado
    if(length(candidatos) > 1){
      grados_cand <- grados[candidatos]
      elegido <- candidatos[which.max(grados_cand)]
    } else {
      elegido <- candidatos[1]
    }
    
    # Obtenemos colores usados por vértices vecinos
    vecinos <- which(matrizGrafo[elegido, ] == 1)
    colores_vecinos <- unique(coloreados[vecinos])
    
    # Asignar el menor color disponible
    color <- 1
    while(color %in% colores_vecinos){
      color <- color + 1
    }
    
    coloreados[elegido] <- color
  }
  
  return(coloreados)
}

saturacion <- function(i, matriz, coloreados){
  vecinos <- which(matriz[i, ] == 1)
  colores_vecinos <- unique(coloreados[vecinos])
  
  # No contar el 0 (no coloreados)
  colores_vecinos <- colores_vecinos[colores_vecinos != 0]
  
  return(length(colores_vecinos))
}


graficar_coloracion <- function(P, aristas, fun_coloracion = NULL){
  # P: matriz 2 x n (coordenadas de los vértices)
  # aristas: matriz o lista de pares (i, j)
  
  nVertices <- ncol(P)
  
  # Dibujar estructura base
  plot(P[1, ], P[2, ],
       pch = 20, col = "gray",
       xlab = "x", ylab = "y",
       main = "Coloración de grafos",
       asp = 1)
  
  # Dibujar aristas
  for(k in 1:nrow(aristas)){
    i <- aristas[k, 1]
    j <- aristas[k, 2]
    
    segments(P[1, i], P[2, i],
             P[1, j], P[2, j],
             col = "gray", lwd = 1)
  }
  
  if(!is.null(fun_coloracion)){
    
    # Medir tiempo y obtener solución
    tiempo <- system.time({
      colores_vertices <- fun_coloracion(nVertices, aristas)
    })
    
    nColores <- max(colores_vertices)
    paleta <- rainbow(nColores)
    
    # Dibujar vértices coloreados
    points(P[1, ], P[2, ],
           col = paleta[colores_vertices],
           pch = 19, cex = 1.5)
    
    # Opcional: remarcar con borde negro
    points(P[1, ], P[2, ],
           pch = 1, col = "black", lwd = 1.2)
    
    # Leyenda
    t <- tiempo["elapsed"]
    legend("topright",
           legend = c(paste("Nº colores:", nColores),
                      paste("Tiempo:", round(t * 1000, 2), "ms")),
           bty = "n", cex = 0.8)
  }
}

#### Gráficas ##########
graficar_coloracion <- function(nVertices, aristas, fun_coloracion = NULL){
  library(igraph)
  # Crear grafo
  g <- graph_from_edgelist(aristas, directed = FALSE)
  
  if(!is.null(fun_coloracion)){
    tiempo <- system.time({
      colores_vertices <- fun_coloracion(nVertices, aristas)
    })
    
    nColores <- max(colores_vertices)
    paleta <- rainbow(nColores)
    
    V(g)$color <- paleta[colores_vertices]
    
    t <- tiempo["elapsed"]
    
    par(bg = "#f0f0f0")
    plot(g,
         vertex.size = 25,
         vertex.label = 1:nVertices,
         main = paste("Coloración (", nColores, "colores,",
                      round(t * 1000, 2), "ms)"))
  }else{
    par(bg = "#f0f0f0")
    plot(g,
         vertex.size = 25,
         vertex.label = 1:nVertices,
         main = "Grafo")
  }
}


aristas1 <- matrix(c(1,2,
                    2,3,
                    3,4,
                    4,5,
                    5,1,
                    2,4), byrow = TRUE, ncol = 2)
grafo_completo <- function(n){
  t(combn(n, 2))
}
aristas2 <- grafo_completo(7)

grafo_ciclo <- function(n){
  cbind(1:n, c(2:n,1))
}

aristas3 <- grafo_ciclo(7)

grafo_bipartito <- function(m, n){
  izquierda <- 1:m
  derecha <- (m+1):(m+n)
  expand.grid(izquierda, derecha)
}

aristas4 <- as.matrix(grafo_bipartito(4,5))

grafo_rueda <- function(n){
  ciclo <- cbind(2:n, c(3:n,2))
  centro <- cbind(rep(1, n-1), 2:n)
  rbind(ciclo, centro)
}

aristas5 <- grafo_rueda(8)

aristas6 <- matrix(c(
  1,2, 2,3, 3,4, 4,5, 5,1,
  6,8, 8,10, 10,7, 7,9, 9,6,
  1,6, 2,7, 3,8, 4,9, 5,10
), byrow=TRUE, ncol=2)

graficar_coloracion(5, aristas1, coloracion)
graficar_coloracion(7, aristas2, coloracion)
graficar_coloracion(7, aristas3, coloracion)
graficar_coloracion(9, aristas4, coloracion)
graficar_coloracion(8, aristas5, coloracion)
graficar_coloracion(10, aristas6, coloracion)




# Coloración del mapa de china
#############################################################

# Descargamos datos de coordenadas geográficas de la Tierra

#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")

# Calcula los centroides de las provincias (para poner en la imagen) y también adyacencias de provincias

#install.packages("sf")

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Provincias de China
china <- ne_download(scale = 50, type = "states", category = "cultural", returnclass = "sf")
china <- china[china$admin == "China",]

# Calculamos centroides de cada provincia (coordenadas para los nodos)
centroides <- st_centroid(china)
coords <- st_coordinates(centroides)  # matriz nx2 ( X (longitud) e Y (latitud) )

n <- nrow(china)

# Detectar adyacencias de provincias
toca <- st_touches(china)  # lista: toca[[i]] guarda lista de índices de provincias que tocan a i

aristas_china <- c()    # Matriz con filas por cada arista (i,j) que hay
for(i in 1:n){
  for(j in toca[[i]]){
    if(i < j){
      aristas_china <- rbind(aristas_china, c(i, j))
    }
  }
}

# Coloreamos
colores_asignados <- coloracion(n, aristas_china)
n_colores <- max(colores_asignados)
paleta <- c("#e41a1c", "#377eb8", "#4daf4a", "#ff7f00", "#FFDE21")  # 5 colores (deben bastar 4)

# Visualizamos mapa coloreado con provincias rellenas
if(n_colores > length(paleta)){
  stop("No hay suficientes colores en la paleta")
}
china$color <- paleta[colores_asignados]

plot(st_geometry(china),
     col = china$color,
     border = "white",
     lwd = 0.5,
     main = paste("Mapa de China", n_colores, " colores (algoritmo DSatur)"))

# Superponemos el grafo de adyacencias sobre el mapa
for(k in 1:nrow(aristas_china)){
  i <- aristas_china[k, 1]
  j <- aristas_china[k, 2]
  segments(coords[i, 1], coords[i, 2],
           coords[j, 1], coords[j, 2],
           col = "grey", lwd = 0.8)
}

# Nodos (centroides) encima
points(coords, pch = 19, cex = 0.7, col = "black")

legend("bottomleft",
       legend = paste("Color", 1:n_colores),
       fill = paleta[1:n_colores],
       border = NA, bty = "n", cex = 0.8)


######## Repetimos, con el de EEUU
# Provincias de EEUU
eeuu <- ne_download(scale = 50, type = "states", category = "cultural", returnclass = "sf")
eeuu <- eeuu[eeuu$admin == "United States of America",]

# Calculamos centroides de cada provincia (coordenadas para los nodos)
centroides2 <- st_centroid(eeuu)
coords2 <- st_coordinates(centroides2)  # matriz nx2 ( X (longitud) e Y (latitud) )

n2 <- nrow(eeuu)

# Detectar adyacencias de provincias
toca2 <- st_touches(eeuu)  # lista: toca[[i]] guarda lista de índices de provincias que tocan a i

aristas_eeuu <- c()    # Matriz con filas por cada arista (i,j) que hay
for(i in 1:n2){
  for(j in toca2[[i]]){
    if(i < j){
      aristas_eeuu <- rbind(aristas_eeuu, c(i, j))
    }
  }
}

# Coloreamos
colores_asignados2 <- coloracion(n2, aristas_eeuu)
n_colores2 <- max(colores_asignados2)

# Visualizamos mapa coloreado con provincias rellenas
if(n_colores2 > length(paleta)){
  stop("No hay suficientes colores en la paleta")
}
eeuu$color <- paleta[colores_asignados2]

plot(st_geometry(eeuu),
     col = eeuu$color,
     border = "white",
     lwd = 0.5,
     main = paste("Mapa de EEUU", n_colores, " colores (algoritmo DSatur)"))

# Superponemos el grafo de adyacencias sobre el mapa
for(k in 1:nrow(aristas_eeuu)){
  i <- aristas_eeuu[k, 1]
  j <- aristas_eeuu[k, 2]
  segments(coords2[i, 1], coords2[i, 2],
           coords2[j, 1], coords2[j, 2],
           col = "grey", lwd = 0.8)
}

# Nodos (centroides) encima
points(coords2, pch = 19, cex = 0.7, col = "black")

legend("bottomleft",
       legend = paste("Color", 1:n_colores),
       fill = paleta[1:n_colores],
       border = NA, bty = "n", cex = 0.8)

