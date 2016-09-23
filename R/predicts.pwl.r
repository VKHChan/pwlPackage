#' A function that uses the piecewise equations to predict new values.
#' @param pwl The piecewise linear equation
#' @param newData The new data [x, y] that user wants to predict values with the provided pwl
#' @return The predicted values, y', using the provided pwl equation
#' @export

predicts.pwl <- function(pwl, newData){
  if(class(pwl)!="pwl") stop("Need to provide the piecewise linear equations")

  x <- as.matrix(newData)
  y <- matrix(nrow = nrow(x))

  BP <- pwl$BreakPoints
  noOfBP <- length(BP)

  i <- 1
  repeat{
    if(x[i] <= BP[1]){
      y[i] <- pwl$coeffs[1,1] + pwl$coeffs[2,1]*x[i]
    }else if(x[i] >= BP[noOfBP]){
      y[i] <- pwl$coeffs[1,noOfBP+1] + pwl$coeffs[2,noOfBP+1]*x[i]
    }else{
      j <- 1
      repeat{
        if(x[i] > BP[j] & x[i] <= BP[j+1]){
          y[i] <- pwl$coeffs[1,j+1] + pwl$coeffs[2,j+1]*x[i]
        }
        j <- j +1
        if(j >= noOfBP) break()
      }
    }
    i <- i+1
    if(i>nrow(x)) break()
  }
  y
}
