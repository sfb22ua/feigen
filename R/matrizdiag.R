#' Función que calcula la matriz diagonal D
#' a partir de los valores propios de A
#'
#' @export

matrizdiag <- function(A){
  
  if(nrow(A) != ncol(A)){
    cat("Es necesaria una matriz cuadrada")
  }
  else{
    aux <- round(eigen(A)$values)
    
    cat("La matriz ")
    cat("\n")
    #prmatrix(A, rowlab=rep("",nrow(A)), collab=rep("",ncol(A)))
    colnames(A) <- rownames(A) <- rep("", ncol(A)) 
    print(A)
    cat("\n")
    cat("tiene ",length(aux)," valores propios de los cuales extraemos que: \n")
    cat("\n")
    
    i <- 1
    j <- 1
    k <- 1
    true <- 0
    count <- 0
    
    while(i <= length(aux)){
      
      if(i != 1){
        
        while(k <= length(cogidos) && true == 0){
          if(aux[i] == cogidos[k]){
            true <- 1
          }
          k <- k+1
        }
      }
      
      if(true != 1){
        
        a <- aux[i]
        
        while(j <= length(aux)){
          
          if(a == aux[j]){
            count <- count + 1
          }
          j <- j + 1
        }
        
        if(i == 1){
          cogidos <- matrix(c(aux[i]))
          valores <- matrix(c(count))
        }
        else{
          cogidos <- cbind(cogidos,aux[i])
          valores <- cbind(valores,count)
        }
      }
      count <- 0
      true <- 0
      i <- i + 1
      j <- 1
      k <- 1
    }
    
    k <- 1
    while (k <= length(valores)){
      cat("El valor propio ",cogidos[k]," tiene multiplicidad algebraica ",valores[k],"\n")
      k <- k + 1
    }
    cat("\n")
    cat("y su matriz diagonal queda como la siguiente: \n");
    
    D <- diag(nrow(A))
    for(q in  1:nrow(A)){
      D[q,q] <- aux[q]
    }
    
    colnames(D) <- rownames(D) <- rep("", ncol(D)) 
    print(D)
    
  }
}
