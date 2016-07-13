#-----------------------------------------------------------------
# Project : DBNorm
#-----------------------------------------------------------------


#--------------------------------------------------------------
# calculate probability of a given element
#--------------------------------------------------------------
calProb <- function(elem, data){
  
  prob = 0.0
  
  if (elem == data$min){
    return(0)
  }
  
  if (elem == data$max){
    return(1)
  }
  
  offset = (elem - data$min) * sum(data$y_predicted[data$y_predicted<0])
  offset = offset / (data$max-data$min)
  
  #for (i in 2:data$nbin){
  #  if (i == data$nbin){
  #    prob = prob + (elem - data$x_data[i]) * data$y_predicted[i]
  #  } else if (elem > data$x_data[i]){
  #    prob = prob + data$y_predicted[i-1]
  #  } else {
  #    prob = prob + (elem - data$x_data[i-1]) * data$y_predicted[i-1]
  #    break
  #  }
  #}
  for (i in 1:data$nbin){
    if (elem > data$x_data[i]){
      prob = prob + data$y_predicted[i]
    } else {
      prob = prob + (elem - data$x_data[i-1]) * data$y_predicted[i]
      break
    }
  }
  
  #if (as.numeric(prob) > 1){
  #  print(elem)
  #}
  
  return(as.numeric(prob+offset))
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
  
  while (TRUE){
    #print(i)
    #print(prob)
    #print(y_prob)
    
    if (prob > y_prob){
      y_prob = y_prob + data$y_prob[i]
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
#' DArray1 = conNormalizer(DArray1, DArray3)
#'
conNormalizer <- function(tg, bs){
  
  tg$mapped_data = tg$data
  
  for (i in 1:tg$len){
    prob = calProb(tg$data[i], tg)
    tg$mapped_data[i] = calElem(prob, bs)
  }
  
  tg
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



