#-----------------------------------------------------------------
# Project : DBNorm
#-----------------------------------------------------------------


#--------------------------------------------------------------
# calculate probability of a given element
#--------------------------------------------------------------
calProb <- function(elem, data){
  Fn <- ecdf(data$data)
  prob <- Fn(elem)
  return (prob)
}

#--------------------------------------------------------------
# locate mapped element in base array
#--------------------------------------------------------------
calElem <- function(prob, data){
  
  i = 1
  elem = 0
  y_prob = 0.0
  
  if (prob == 0){
    return(data$min)
  }
  
  if (prob == 1){
    return(data$max)
  }
  
  data$y_predicted = data$y_predicted / sum(data$y_predicted)
  
  while (TRUE){
    #print(i)
    #print(prob)
    #print(y_prob)
    
    if (prob > y_prob){
      y_prob = y_prob + data$y_predicted[i]
      i = i + 1
    } else {
      elem = data$x_data[i] - (y_prob - prob) * data$diff 
      i = 1
      break
    }
  }
  
  return(elem)
}

#' Normalizing a target data array to a basis array based on their distributions
#' 
#' @author Qinxue Meng, Paul Kennedy
#' @param tg a target data array
#' @param bs a basis data array
#' @return A normalized target data array with 
#' the same distribution with the basis data array
#' @details 
#' The function maps a target data array to a basis array based on their distributions 
#' and the basis data array can be an arbitary data array or a standard distribution 
#' such as normal distribution. 
#' @export
#' @examples 
#' # Normalize DArray1 to DArray3
#' # load build-in data arrays 
#' data(DArray1) 
#' data(DArray3) 
#' 
#' # Capturing distribution information 
#' DBdata1 <- genDistData(DArray1, 500)
#' DBdata3 <- genDistData(DArray3, 500)
#' 
#' # Using Gaussian function to fit DBdata3
#' DBdata3 <- gaussianFit(DBdata3)
#' 
#' # Normalize DBdata1 to the Gaussian fitting function of DBdata3
#' DArray1 = conNormalizer(DArray1, DArray3)
#' DA1toDA3DBdata <- genDistData(DA1toDA3, 500)
#' visDistData(DA1toDA3DBdata, "P", "DA1toDA3", "Range", "Probability")
#'
conNormalizer <- function(tg, bs){
  
  tg$mapped_data = tg$data
  
  for (i in 1:tg$len){
    prob = calProb(tg$data[i], tg)
    tg$mapped_data[i] = calElem(prob, bs)
  }
  
  tg$mapped_data
}

#' Normalizing a target data array to a basis array based on element positions
#' 
#' @author Qinxue Meng, Paul Kennedy
#' @param tg a target data array
#' @param bs a basis data array
#' @return A normalized target data array with 
#' the same distribution with the basis data array
#' @details 
#' The function normalize target data array to a basis array based on element 
#' positions. This method does not need to do fitting before normalization and 
#' works for discrete values as well.  
#' @export
#' @examples 
#' # Calculating the polynomial curve fitting function of DArray1's distribution
#' DArray1 = disNormalizer(DArray1, DArray3)
#'
disNormalizer <- function(tg, bs){
  
  bs_list = sort(unlist(bs))
  tg_list = sort(unlist(tg))
  bs_len  = length(bs_list)
  tg_len  = length(tg_list)
  
  tg_new  = tg
  
  for (i in 1:tg_len) {
    
    position = 0; 
    tg_value = tg[i]
    
    # find in target
    tg_positions = which(tg_list == tg_value, arr.ind = TRUE)
    if (length(tg_positions) > 1) {
      tg_position = tg_positions[length(tg_positions)]
    } else { 
      tg_position = tg_positions
    }
    
    # map to base
    tg_new[i] = bs_list[floor((tg_position-1) / tg_len * bs_len) + 1]
  }
  
  tg_new
  
}

#' Normalizing a target data array to a standard distribution
#' 
#' @author Qinxue Meng, Paul Kennedy
#' @param tg a target data array
#' @param bs a standard distribution created by defineDist(dist)
#' @return A normalized target data array with 
#' the same distribution with the standard distribution
#' @details 
#' The function normalize target data array to a standard distribution.
#' @export
#' @importFrom distr::q()
#' @examples 
#' # Normalize a given data array into a normal distribution
#' loadData(0)
#' DBdata1 <- genDistData(DArray1, 500)
#' DBdata5 <- defineDist(Norm(mean=0, sd=1))
#' DA1toDA5 <- distrNormalizer(DBdata1, DBdata5)
#' DA1toDA5DBdata <- genDistData(DA1toDA5, 500)
#' visDistData(DA1toDA5DBdata, "P", "DA1toDA5", "Range", "Probability")
#'
distrNormalizer <- function(tg, bs){
  
  tg$mapped_data = tg$data
  
  for (i in 1:tg$len){
    prob = calProb(tg$data[i], tg)
    if (prob == 1){
      tg$mapped_data[i] = bs$max
    } else if (prob == 0) {
      tg$mapped_data[i] = bs$min
    } else {
      tg$mapped_data[i] = bs$data@q(prob)
    }
    
  }
  
  tg$mapped_data
  
}



