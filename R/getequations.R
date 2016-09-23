#' A function that gets the equations with the given the breakpoints.
#' @param data The data to be approximated
#' @param BPs The list of breakpoints
#' @return  The coefficients of the equations, fitted values, and residuals.

getequations <- function(data, BPs){
  noOfBP <- length(BPs)
  piecewise <- list(coeffs = matrix, BreakPoints = c(), fitted = matrix, residuals = matrix)
  piecewise$BreakPoints <- BPs


  if(noOfBP == 1){
    # if there is only one breakpoints
    # find both lines
    x1 <- data[data[,1] <= BPs[1], 1, drop = FALSE]
    x2 <- data[data[,1] >= BPs[1], 1, drop = FALSE]
    y1 <- data[data[,1] <= BPs[1], 2, drop = FALSE]
    y2 <- data[data[,1] >= BPs[1], 2, drop = FALSE]

    line1 <- lm(y1~x1)
    line2 <- lm(y2~x2)

    piecewise$coeffs <- cbind(line1$coefficients, line2$coefficients)
    piecewise$fitted <- rbind(line1$fitted.values, line2$fitted.values)
    piecewise$residuals <- rbind(line1$residuals, line2$residuals)

  }else{
    # otherwise, find one segment at a time
    i <- 0
    repeat{
      if(i == 0){
        # the first segment
        x1 <- data[data[,1] <= BPs[i+1], 1, drop = FALSE]
        y1 <- data[data[,1] <= BPs[i+1], 2, drop = FALSE]
      }else if(i == noOfBP){
        # finding the last segment
        x1 <- data[data[,1] >= BPs[i], 1, drop = FALSE]
        y1 <- data[data[,1] >= BPs[i], 2, drop = FALSE]
      }else{
        # finding all the other segments in between
        x1 <- data[data[,1] >= BPs[i] & data[,1] <= BPs[i+1], 1, drop = FALSE]
        y1 <- data[data[,1] >= BPs[i] & data[,1] <= BPs[i+1], 2, drop = FALSE]
      }
      line1 <- lm(y1~x1)

      if(i == 0){
        piecewise$coeffs <- as.matrix(line1$coefficients)
        piecewise$fitted <- as.matrix(line1$fitted.values)
        piecewise$residuals <- as.matrix(line1$residuals)
      }else{
        piecewise$coeffs <- cbind(piecewise$coeffs, line1$coefficients)
        piecewise$fitted <- rbind(piecewise$fitted, as.matrix(line1$fitted.values))
        piecewise$residuals <- rbind(piecewise$residuals, as.matrix(line1$residuals))
      }

      i <- i+1
      if(i>noOfBP) break()
    }
  }
  piecewise
}
