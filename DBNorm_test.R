library(DBNorm)
library(distr)

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
DBdata1 <- fourierFit(DBdata1, 9)
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

# Normalization
disNormalizer(DBdata1$data, DBdata3$data)






