#' Función que calcula los vectores propios de una matriz cuadrada
#' y muestra el resultado por pantalla utilizando la Teoría
#' Espectral de Matrices
#'
#' @export

vecprop <- function(A){
  
  if(nrow(A) != ncol(A)){
    cat("Es necesaria una matriz cuadrada \n")
  }
  else{
    
    NullSpace <- function (A) {
      m <- dim(A)[1]; n <- dim(A)[2]
      ## QR factorization and rank detection
      QR <- base::qr.default(A)
      r <- QR$rank
      ## cases 2 to 4
      if ((r < min(m, n)) || (m < n)) {
        R <- QR$qr[1:r, , drop = FALSE]
        P <- QR$pivot
        F <- R[, (r + 1):n, drop = FALSE]
        I <- base::diag(1, n - r)
        B <- -1.0 * base::backsolve(R, F, r)
        Y <- base::rbind(B, I)
        X <- Y[base::order(P), , drop = FALSE]
        return(X)
      }
      ## case 1
      return(base::matrix(0, n, 1))
    }
    
    aux <- round(eigen(A)$values)
    
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
    
    
    for(t in 1:length(cogidos)){
      cat("Los vectores propios asociados al valor propio ",cogidos[t]," son: \n");
      vecs <- matrix(NullSpace(A - diag(cogidos[t], nrow(A))), ncol=valores[t])
      #prmatrix(vecs, rowlab=rep("",nrow(vecs)), collab=rep("",ncol(vecs)))
      colnames(vecs) <- rep("", ncol(vecs))
      rownames(vecs) <- rep("", nrow(vecs))
      print(vecs)
      cat("\n")
    }
    
    
  }
}