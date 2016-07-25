#-----------------------------------------------------------------
# Project : DBNorm
#-----------------------------------------------------------------

#' fitting a distribution by polynomial curve fitting
#' 
#' @author Qinxue Meng, Paul Kennedy
#' @param DBdata input distribution dataset 
#' @param n the degree of polynomial functions
#' @return a polynomial curve fitting function
#' @details 
#' The function fits distributions by polynomial curve fitting and
#' returns a polynomial curve fitting function.
#' @seealso \code{lm}
#' @export
#' @examples 
#' # Calculating the polynomial curve fitting function of DArray1's distribution
#' DBdata1 = polyFit(DBdata1, 3)
#' 
polyFit <- function(DBdata, n){
  
  DBdata$fitting <- "Polynomial Curve Fitting"
  
  x = DBdata$x_data
  y = DBdata$y_prob
  
  polyFit <- lm(y ~ poly(x, n))
  
  DBdata$y_predicted <- predict(polyFit, data.frame(x=x))
  
  # print expression
  len = length(polyFit$coefficients)
  DBdata$equ = paste("(", polyFit$coefficients[1], ")", sep="")
  for (i in 2:len){
    DBdata$equ = paste("(", polyFit$coefficients[i], ")*", "x^", i-1, "+", DBdata$equ, sep="")
  }
  
  DBdata$fit.results <- polyFit
  
  DBdata
  
}

#' fitting a distribution by fourier curve fitting
#' 
#' @author Qinxue Meng, Paul Kennedy
#' @param DBdata input distribution dataset 
#' @param n the degree of the fourier fitting function
#' @return a fourier curve fitting function
#' @details 
#' The function fits distributions by fourier curve fitting and
#' returns a fourier curve fitting function.
#' @seealso \code{lm}
#' @export
#' @examples 
#' # Calculating the fourier curve fitting function of DArray1's distribution
#' DBdata1 = fourierFit(DBdata1, 3)
#' 
fourierFit <- function(DBdata, n){
  
  DBdata$fitting <- "Fourier Curve Fitting"
  
  # building a fourier equation
  equ = "y ~ a + "
  for (i in 1:n){
    equ = paste(equ, "a", i, "*cos(w*", i, "*x) + b",i,"*sin(w*", i, "*x)", sep="")
    if (i < n){equ = paste(equ, "+ ")}
  }
  equ = as.formula(equ)
  
  # define initial values of parameters
  start <- vector("list")
  start$w = 0.01
  start$a = 0.1
  for (i in 1:n){
    start$tmp = 0.1
    names(start)[length(names(start))] = paste("a",i,sep="")
    start$tmp = 0.1
    names(start)[length(names(start))] = paste("b",i,sep="")
  }
  
  x = DBdata$x_data
  y = DBdata$y_prob
  df <- data.frame(x, y)
  
  fit.nlxb <- nlxb(equ, start=start, data=df)
  fit.nls  <- nls2(equ, df, start=fit.nlxb$coefficients, algorithm="brute-force")
  
  DBdata$y_predicted <- predict(fit.nls, data.frame(x=x))
  
  params <- summary(fit.nls)$parameters[,1]
  
  # print expression
  DBdata$equ = paste("y ~ ", params[2], " + ", sep="")
  for (i in 1:n){
    DBdata$equ = paste(DBdata$equ, params[i+2], "*cos(",params[1],"*", i, "*x) + ", params[i+3],
                       "*sin(", params[1],"*", i, "*x)", sep="")
    if (i < n){DBdata$equ = paste(DBdata$equ, "+ ")}
  }
  
  DBdata$fit.results <- fit.nls
  
  DBdata
  
}

#' fitting a distribution by gaussian curve fitting
#' 
#' @author Qinxue Meng, Paul Kennedy
#' @param DBdata input distribution dataset 
#' @return a gaussian curve fitting function
#' @details 
#' The function fits distributions by gaussian curve fitting and
#' returns a gaussian curve fitting function.
#' @seealso \code{optim}
#' @export
#' @examples 
#' # Calculating the gaussian curve fitting function of DArray1's distribution
#' DBdata1 = gaussianFit(DBdata1)
#' 

gaussianFit <- function(DBdata){
  
  DBdata$fitting <- "Gaussian Curve Fitting"
  
  x = DBdata$x_data
  y = DBdata$y_prob
  
  # build gaussian equations
  fitG = function(x,y){
    f = function(p){
      d = p[3]*dnorm(x,mean=p[1],sd=p[2])
      sum((d-y)^2)
    }
    optim(c(mean(x),sd(x),1),f)
  }
  
  fit1G = fitG(x,y)
  p = fit1G$par
  
  DBdata$y_predicted = p[3] * dnorm(x, p[1], p[2])
  
  DBdata$equ = paste("(", p[3],")*exp^(-(",p[2],")*(x-",p[1],")^2)", sep="")
  
  DBdata$fit.results <- fit1G
  
  DBdata
  
}

#' fitting a distribution by a customised curve function
#' 
#' @author Qinxue Meng, Paul Kennedy
#' @param DBdata input distribution dataset 
#' @param formula a customised curve function 
#' @return a customised curve fitting function
#' @details 
#' The function fits distributions by a customised curve fitting and
#' returns a customised curve fitting function.
#' @seealso \code{lm}
#' @export
#' @examples 
#' # Calculating the customised curve fitting function of DArray1's distribution
#' DBdata1 = custFit(DBdata1)
#' 


custFit <- function(DBdata, formula){
  
  DBdata$fitting <- "Customised Curve Fitting"
  
  x = DBdata$x_data
  y = DBdata$y_prob
  
  # build fourier equations
  custFit <- lm(as.formula(formula))
  DBdata$y_predicted <- predict(custFit, data.frame(x=x))
  
  # print expression
  len = length(custFit$coefficients)
  DBdata$equ = paste("(", custFit$coefficients[1], ")", sep="")
  for (i in 2:len){
    if (!is.na(custFit$coefficients[i])){
      DBdata$equ = paste(DBdata$equ, " + (", custFit$coefficients[i], ")*", 
                       names(custFit$coefficients[i]), sep="")
    }
  }
  
  DBdata$fit.results <- custFit
  
  DBdata
  
}

