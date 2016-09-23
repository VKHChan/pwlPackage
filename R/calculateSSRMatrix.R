#' A function that calculate and returns the ssr (sum of squared residual) of all possible combinations of segments in a matrix form.
#' The index of row and column represents the starting and ending point of the segment.
#' For example, value at entry [1,100] of the matrix is the SSR of the segment
#' starting at data 1 and ending at data 100.
#' @param data The input data
#' @return SSRMatrix

calculateSSRMatrix <- function(data){
  #A square matrix with the size of n x n is required to store the MSE,
  #where n is the number of data points.
  #The row and column index represents the data entry of the start and the end of the segment.
  #i.e. [4,10] reprents the SSR of the segment starting from data entry 4 to data entry 10.
  #The matrix can be used up to look up the SSR values for any given number
  #of breakpoints for the approximation.
  #WARNING: THIS CAN BE VERY TIME CONSUMING IF THE DATASIZE IS BIG!!!

  SSRMatrix <- matrix(nrow=nrow(as.matrix(data)), ncol=nrow(as.matrix(data)))
  j <- 1
  repeat{
    #we need at least 3 points to have a line.
    i<-2+j

    repeat{

      x <- data[j:i,1]
      y <- data[j:i,2]

      line <- lm(y~x)
      SSRMatrix[j,i]<- as.numeric(sum((line$residuals)^2))

      i<-i+1
      if(i > nrow(data)) break()
    }
    j <- j+1
    #print(j)
    if(j >= nrow(data)-1) break()
  }
  SSRMatrix
}
