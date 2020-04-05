#' Función que calcula la traza de una matriz cuadrada
#' y muestra el resultado por pantalla utilizando la Teoría
#' Espectral de Matrices
#'
#' @export

trazaespectral <- function(A){
  
  if(nrow(A) != ncol(A)){
    cat("Es necesaria una matriz cuadrada \n")
  }
  else{
    aux <- matrix(c(round(eigen(A)$values)))
    
    sum <- 0
    
    for(i in 1:length(aux)){
      sum <- sum + aux[i]
    }
    
    cat("La matriz ")
    cat("\n")
    #prmatrix(A, rowlab=rep("",nrow(A)), collab=rep("",ncol(A)))
    colnames(A) <- rownames(A) <- rep("", ncol(A))
    print(A)
    cat("\n")
    cat("tiene como valores propios: \n")
    #prmatrix(aux, rowlab=rep("",nrow(aux)), collab=rep("",ncol(aux)))
    colnames(aux) <- ""
    rownames(aux) <- rep("", nrow(aux))
    print(aux)
    cat("\n")
    cat("y su traza mediante Teor?a Espectral se puede calcular como: ");
    for(i in 1:length(aux)){
      if(i < length(aux)){
        cat(aux[i],"+ ")
      }
      else{
        cat(aux[i])
      }
    }
    cat(" =",sum)
  }
  
}