#' Funcion que calcula el Algoritmo QR de una matriz
#' cuadrada A mostrando por pantalla las matrices Q y R
#'
#' @export 
#' @import pracma

gramschmidt <- function(A) {
  
  if(qr(A)$rank != ncol(A)){
    
    cat("Los vectores de la matriz A no son linealmente independientes \n")
    cat(">> No se le puede aplicar el Algotitmo QR")
    
  }
  else{
    
    Q <- gramSchmidt(A)$Q
    R <- gramSchmidt(A)$R
    
    trueQ <- 0
    trueR <- 0
    
    for(i in 1:nrow(Q)){
      if(Q[i,1] < 0){
        trueQ <- 1
      }
    }
    
    for(j in 1:nrow(R)){
      if(R[j,1] < 0){
        trueR <- 0
      }
    }
    
    
    if(trueQ == 0 && trueR == 0){
      
      cat("La matriz Q del Algoritmo QR es: \n")
      colnames(Q) <- rep("", ncol(Q))
      rownames(Q) <- rep("", nrow(Q)) 
      print(Q)
      cat("\n")
      cat("La matriz R del Algoritmo QR es: \n")
      colnames(R) <- rownames(R) <- rep("", ncol(R)) 
      print(R)
      
    }
    else if(trueQ == 1 && trueR == 1){
      
      cat("La matriz Q del Algoritmo QR es: \n")
      colnames(Q) <- rep("", ncol(Q))
      rownames(Q) <- rep(" ", nrow(Q)) 
      print(Q)
      cat("\n")
      cat("La matriz R del Algoritmo QR es: \n")
      colnames(R) <- rownames(R) <- rep(" ", ncol(R)) 
      print(R)
      
    }
    else if(trueQ == 1 && trueR == 0){
      
      cat("La matriz Q del Algoritmo QR es: \n")
      colnames(Q) <- rep("", ncol(Q))
      rownames(Q) <- rep("", nrow(Q)) 
      print(Q)
      cat("\n")
      cat("La matriz R del Algoritmo QR es: \n")
      colnames(R) <- rownames(R) <- rep(" ", ncol(R)) 
      print(R)
      
    }
    else if(trueQ == 0 && trueR == 1){
      
      cat("La matriz Q del Algoritmo QR es: \n")
      colnames(Q) <- rep("", ncol(Q))
      rownames(Q) <- rep(" ", nrow(Q)) 
      print(Q)
      cat("\n")
      cat("La matriz R del Algoritmo QR es: \n")
      colnames(R) <- rownames(R) <- rep("", ncol(R)) 
      print(R)
      
    }
  }
}