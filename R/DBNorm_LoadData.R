#-----------------------------------------------------------------
# Project : DBNorm
#-----------------------------------------------------------------

#' Loading build-in datasets
#' 
#' This function loads build-in data array for examples
#' 
#' @author Qinxue Meng
#' @param n n-th data array to load; 
#'        if n = 1, DArray1 is loaded; 
#'        if n = 2, DArray2 is loaded; 
#'        if n = 3, DArray3 is loaded; 
#'        if n = 4, DArray4 is loaded; 
#'        if n is not 1, 2, 3, 4, all four data arrays are loaded. 
#' @return None
#' @details 
#' This function loads example data arrays for user to test 
#' @seealso \code{data()}
#' @export
#' @examples 
#' # load DArray1
#' loadData(1)
#' # load all data arrays
#' loadData(5)

loadData <- function(n){
  
  if (n == 1){
    data(DArray1) 
  } else if (n == 2) {
    data(DArray2)
  } else if (n == 3) {
    data(DArray3)
  } else if (n == 4) {
    data(DArray4)
  } else {
    data(DArray1) 
    data(DArray2) 
    data(DArray3) 
    data(DArray4) 
  }
  
}

