#' A function that finds the  piecewise linear approximation function for the given data and number of breakpoints (bp).
#' @param data The data that user wants to approximate
#' @param noOfBP The desired number of breakpoints
#' @param l The minumum distance between breakpoints
#' @param maxBP The user can either provide number of desired breakpoints, or the desired maximum number of breakpoints. When maxBP is given, the function returns all the PWL equations from 1 to maxBP.
#' @return a class "pwl" which includes the coefficients of the equations, loaction of the breakpoints, fitted values, residuals and mse.
#' @export

pwl <- function(data, noOfBP = NULL, l, maxBP = NULL, ...){

  # check if l is given
  if(missing(l)) stop("Please specify mininum length between break points:\n length has to be at least 1", call. = FALSE)

  # check if either no of BP or error is given
  if(missing(noOfBP)&& missing(maxBP)) stop("Please specify either number of desired breakpoints or maximum number of breakpoint. \n If both are specified, all the piecewise equation up to the specified number of breakpoint will be given.", call. = FALSE)

  # first sort the given dataset
  data <- data[sort.list(data[,1]),]

  # check if the length and no of BP given will fit into the given data
  size <- nrow(data)
  maxBP.allowed <- (size/l) -1
  if(!is.null(noOfBP)){
    if(noOfBP > maxBP.allowed) stop("The data set is not big enought to fit the number of breakpoints given.\n Either lower the number of desired break points or distance between break points", call. = FALSE)
  }
  if(!is.null(maxBP)){
    if(maxBP > maxBP.allowed) stop("The data set is not big enought to fit the number of breakpoints given.\n Either lower the number of desired break points or distance between break points", call. = FALSE)
  }


  allresult <- list()
  result <- list(minssr=0, minmse=0, BP=c())

  # If there is only one breakpoint, we don't need to calculate the MSE matrix
  if(((!is.null(noOfBP))&&(noOfBP == 1)) || ((!is.null(maxBP))&&(maxBP == 1))){
    name <- "pwl1"
    result <- findoneBP(data, l)
    result$minmse <- result$minssr/nrow(data)
    result$minmse <- result$minssr/nrow(data)

    piecewise <- getequations(data, result$BP)
    piecewise$mse <- result$minmse

    class(piecewise) <- "pwl"

    allresult[[name]] <- piecewise

  }else{
  # otherwise, calculate SSR Matrix
    ssrMatrix <- calculateSSRMatrix(data)
    print("SSRMatrix done!")

    # if maxBP is given, all the pwl equations up to the maxBP will be returned
    if(!is.null(maxBP)){
      i <- 1
      max <- maxBP
    # otherwise if only noOfBP is given, the pwl equation with the specifed BP will be returned
    }else{
      i <- noOfBP
      max <- noOfBP
    }

    repeat{
      name <- paste0("pwl", i)

      result <- findBP(ssrMatrix, i, l, 1, nrow(data))
      print(paste0("BP found, BP = ", result$BP))

      BP <- result$BP
      result$minmse <- result$minssr/nrow(data)
      result$BP <- data[BP,1]

      piecewise <- getequations(data, result$BP)
      piecewise$mse <- result$minmse

      class(piecewise) <- "pwl"

      allresult[[name]] <- piecewise

      i <- i+1
      if(i>max) break()
    }

  }
  allresult
}
