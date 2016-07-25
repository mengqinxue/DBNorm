library(DBNorm)

# load example data arrays
loadData(0)

DBdata1 <- genDistData(DArray1, 500)
DBdata2 <- genDistData(DArray2, 500)
DBdata3 <- genDistData(DArray3, 500)
DBdata4 <- genDistData(DArray4, 500)

# define distribution
DBdata5 <- defineDist(Norm(mean=0, sd=1), -5, 5)

# visualising distribution datasets
visDistData(DBdata1, "F", "DArray1", "Range", "Frequency")
visDistData(DBdata1, "P", "DArray1", "Range", "Probability")

visDistData(DBdata2, "F", "DArray2", "Range", "Frequency")
visDistData(DBdata2, "P", "DArray2", "Range", "Probability")

visDistData(DBdata3, "F", "DArray3", "Range", "Frequency")
visDistData(DBdata3, "P", "DArray3", "Range", "Probability")

visDistData(DBdata4, "F", "DArray4", "Range", "Frequency")
visDistData(DBdata4, "P", "DArray4", "Range", "Probability")

visDistData(DBdata5, "F", "Normal distribution (0, 1)", "Range", "Frequency")
visDistData(DBdata5, "P", "Normal distribution (0, 1)", "Range", "Probability")

# Poly fitting
DBdata1 <- polyFit(DBdata1, 9)
DBdata2 <- polyFit(DBdata2, 9)
DBdata3 <- polyFit(DBdata3, 9)
DBdata4 <- polyFit(DBdata4, 9)

# Fourier fitting
DBdata1 <- fourierFit(DBdata1, 3)
DBdata2 <- fourierFit(DBdata2, 9)
DBdata3 <- fourierFit(DBdata3, 9)
DBdata4 <- fourierFit(DBdata4, 9)

# Gaussian fitting
DBdata1 <- gaussianFit(DBdata1)
DBdata2 <- gaussianFit(DBdata2)
DBdata3 <- gaussianFit(DBdata3)
DBdata4 <- gaussianFit(DBdata4)

# Customised fitting
DBdata1 = custFit(DBdata1, "y ~ x + cos(x)")
DBdata2 = custFit(DBdata2, "y ~ x + cos(x)")
DBdata3 = custFit(DBdata3, "y ~ x + cos(x)")
DBdata4 = custFit(DBdata4, "y ~ x + cos(x)")

# Visualising fitting results
visFitting(DBdata1, "DArray1", "Range", "Probability")
visFitting(DBdata2, "DArray2", "Range", "Probability")
visFitting(DBdata3, "DArray3", "Range", "Probability")
visFitting(DBdata4, "DArray4", "Range", "Probability")
visFitting(DBdata5, "DArray5", "Range", "Probability")

# Continuous Normalization
DA1toDA3 = conNormalizer(DBdata1, DBdata3)
DA1toDA3DBdata <- genDistData(DA1toDA3$mapped_data, 500)
visDistData(DA1toDA3DBdata, "F", "DArray1", "Range", "Frequency")

# Discrete Normalization
DA1toDA3 = disNormalizer(DBdata1$data, DBdata3$data)
DA1toDA3DBdata <- genDistData(DA1toDA3, 500)
visDistData(DA1toDA3DBdata, "F", "DArray1", "Range", "Frequency")

# Test AoV
n = 11
DBdata4$fitting <- "Fourier Curve Fitting"

x = DBdata4$x_data
y = DBdata4$y_prob

# build fourier equations
equ = "y ~ "
for (i in 1:n){
  equ = paste(equ, "cos(", i, "*x) + sin(", i, "*x)", sep="")
  if (i < n){equ = paste(equ, "+ ")}
}
equ = as.formula(equ)
fourierFit <- aov(equ, data=data.frame(x,y), singular.ok=F)

y_p <- predict(fourierFit, data.frame(x=x))

visDistData(DBdata4, "P", "DArray4", "Range", "Probability")
lines(x, predict(fourierFit),col=2)


# Test for nlxb + nls2
library(nlmrt)
library(nls2)

n = 4
equ = "y ~ a + "
for (i in 1:n){
  equ = paste(equ, "a", i, "*cos(w*", i, "*x) + b",i,"*sin(w*", i, "*x)", sep="")
  if (i < n){equ = paste(equ, "+ ")}
}

start <- vector("list")
start$w = 0.01
start$a = 0.1
for (i in 1:n){
  start$tmp = 0.1
  names(start)[length(names(start))] = paste("a",i,sep="")
  start$tmp = 0.1
  names(start)[length(names(start))] = paste("b",i,sep="")
}
equ = as.formula(equ)

x <- DBdata1$x_data
y <- DBdata1$y_prob
df <- data.frame(x, y)

fit.nlxb <- nlxb(equ, start=start, data=dflist, control=list(offset=10))
fit.nls  <- nls2(equ, df, start=fit.nlxb$coefficients, algorithm="brute-force")

visDistData(DBdata4, "P", "DArray1", "Range", "Probability")
lines(x, predict(fit.nls, data.frame(x=x)),col=2)
