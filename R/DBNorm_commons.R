#-----------------------------------------------------------------
# Project : DBNorm
#-----------------------------------------------------------------

#' Generating distribution dataset based on input data arrays.
#' 
#' @author Qinxue Meng
#' @param data input data array 
#' @param nbin number of bins 
#' @return a distribution dataset of a given input data array
#' @details 
#' This function generates distribution dataset based on input data arrays for downstream analysis.
#' @seealso \code{list()}
#' @export
#' @examples 
#' # load DArray1
#' DData1 <- genDistData(DArray1, 500)

genDistData <- function(data, nbin) {
  
  dd <- vector("list")
  dd$data   = data
  dd$x_data = numeric(length = nbin)
  dd$y_freq = numeric(length = nbin)
  dd$y_prob = numeric(length = nbin)
  dd$len = length(data)
  dd$max = max(data)
  dd$min = min(data)
  dd$nbin = nbin
  dd$diff = (dd$max - dd$min) / dd$nbin
  
  # generate x
  for (i in 1:nbin){
    dd$x_data[i] = dd$min + (i-1) * dd$diff
  }
  
  # generate y - frequence
  for (i in 1:dd$len){
    for (j in 1:dd$nbin){
      if (dd$data[i] <= dd$x_data[j]){
        dd$y_freq[j] = dd$y_freq[j] + 1
        break
      }
      
      if (j == dd$nbin){
        dd$y_freq[j] = dd$y_freq[j] + 1
      }
    }
  }
  
  # generate y - probability
  for (i in 1:dd$nbin){
    dd$y_prob[i] = dd$y_freq[i] / dd$len
  }
  
  dd
  
}

#' Generating distribution data based on predefined distribution
#' 
#' @author Qinxue Meng
#' @param dist a predefined distribution 
#' @param min the lower bound of data range and default value is 0
#' @param max the upper bound of data range and default value is 1
#' @return a distribution dataset of the input predefined distribution
#' @details 
#' This function generates distribution data based on predefined distribution. 
#' The purpose of this function is to enable to normalize arbitrary distributions 
#' into a standard distribution. 
#' @seealso \code{list()}
#' @export
#' @importFrom("distr", d) 
#' @examples 
#' # generate distribution data of a normal distribution
#' DArray5 <- defineDist(Norm(mean=0, sd=1))
#' 
defineDist <- function(dist, min=0, max=1) {
  
  nbin = 500
  diff = (max - min) / nbin 
  dd <- vector("list")
  dd$data   = NULL
  dd$x_data = seq(min, max, diff) 
  dd$y_freq = d(dist)(dd$x_data)
  dd$y_prob = d(dist)(dd$x_data)
  dd$len = 0
  dd$max = max
  dd$min = min
  dd$nbin = nbin
  dd$diff = (dd$max - dd$min) / dd$nbin
  
  dd$fitting <- "Predefined distribution"
  dd$y_predicted <- dd$y_freq
  dd$equ <- dist
  
  dd
  
}

#' Visualising distribution dataset
#' 
#' @author Qinxue Meng
#' @param DBdata a distribution dataset 
#' @param type plot by frequence / probability
#' @param t title of plot
#' @param xl description of x-asis 
#' @param yl description of y-asis 
#' @details 
#' This function generates distribution data based on predefined distribution. 
#' The purpose of this function is to enable to normalize arbitrary distributions 
#' into a standard distribution. 
#' @export
#' @examples 
#' # visualising a distribution data
#' DBdata1 <- genDistData(DArray1, 500)
#' visDistData(DBdata1, "F", "DArray1", "Range", "Frequence")
#' visDistData(DBdata1, "P", "DArray1", "Range", "Probability")
#' 

visDistData <- function(DBdata, type, t, xl, yl) {
  
  x = DBdata$x_data
  
  if (type == "F"){
    y = DBdata$y_freq
  } else {
    y = DBdata$y_prob
  }
  
  #hist(data$data, data$nbin, main=t, xlab=xl, ylab=yl)
  plot(x, y, main=t, xlab=xl, ylab=yl, type = "h")
}

#' Visualising fitting results on the input distribution
#' 
#' @author Qinxue Meng
#' @param DBdata a distribution dataset 
#' @param t title of plot
#' @param xl description of x-asis 
#' @param yl description of y-asis 
#' @details 
#' The function visualizes data distribution and corresponding fitting 
#' function so as to provide an intutive way to evaluate the 
#' performance of fitting function.
#' @export
#' @examples 
#' # visualising fitting results on DArray1's distribution
#' visFitting(DBdata1, "DArray1", "Range", "Probability")
#' 

visFitting <- function(DBdata, t, xl, yl){
  visDistData(DBdata, "P", t, xl, yl)
  lines(DBdata$x_data, DBdata$y_predicted, col='blue', lwd=6)
}






