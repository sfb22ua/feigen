#' Cálculo de las potencias de una matriz 
#' cuadrada mediante la propiedad de semejanza
#' de matrices P^-1*A*P = D <=> A = P*D*P^-1
#'
#' @export

calcpot <- function(A,n){
  
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
      
      if(t == 1){
        vecs <- matrix(NullSpace(A - diag(cogidos[t], nrow(A))), ncol=valores[t])
      }
      else{
        vecs2 <- matrix(NullSpace(A - diag(cogidos[t], nrow(A))), ncol=valores[t])
        vecs <- cbind(vecs,vecs2)
      }
      
      if(t == length(cogidos)){
        cat("La dimensión de la base B de vectores propios linealmente independientes de A es",ncol(vecs)," \n")
        cat("\n")
      }
    }
    
    if(ncol(vecs) == ncol(A)){
      cat("La dimensión de la matriz A coincide con la dimensión de la base B \n")
      cat(">> A ES DIAGONALIZABLE \n")
      
      P <- vecs
      D <- diag(nrow(A))
      for(q in  1:nrow(A)){
        D[q,q] <- aux[q]
      }
      
      powmat = function(A,n)
      {
        if (n==1)  return (A)
        if (n==2)  return (A%*%A)
        if (n>2) return (A%*%powmat(A,n-1))
      }
      
      Ap <- P%*%powmat(D,n)%*%solve(P)
      
      cat("\n")
      cat("A elevado a",n,"es: \n")
      colnames(Ap) <- rownames(Ap) <- rep("", ncol(Ap)) 
      print(Ap)
      cat("\n")
      
    }
    else{
      cat("La dimensión de la matriz A no coincide con la dimensión de la base B \n")
      cat(">> A NO ES DIAGONALIZABLE")
    }
  }
}