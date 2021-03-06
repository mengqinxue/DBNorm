# DBNorm
Distribution-based normalization

## Install in R
install.packages("devtools")

library("devtools")

install_github("mengqinxue/DBNorm")

PS: if lazy loading failed to install a required package "distr", "nls2" and "nlmrt", please install it manually before running DBNorm. 

## Example data arrays
This library provides four example data arrays for testing purpose and they are DArray1 (22,277), DArray2 (22,277), DArray3 (54,675) and DArray4 (33,297). 

These four example data arrays can be loaded via function data() or loadData(). 

## Functions

#### 1. loadData(n)
This function loads build-in data arrays. 

###### Params
**n** - n-th data array to load; if n = 1, DArray1 is loaded; if n = 2, DArray2 is loaded; if n = 3, DArray3 is loaded; if n = 4, DArray4 is loaded; if n is not 1, 2, 3, 4, all four data arrays are loaded;

#### 2. genDistData(data, nbin)
The function generates distributions data for downstream analysis, such as fitting and distribution plot. 

######Params
**data** - input data array 

**nbin** - the number of bins for distributions. The more number of bins, the more accurate distribution fitting functions is.

######Return
A distribution dataset of a given input data array

#### 3. defineDist(dist)
The function generates distribution data based on predefined distribution

######Params
**dist** - a predefined distribution;

**min** - the lower bound of data range and default value is 0;

**max** - the upper bound of data range and default value is 1;

######Return
A distribution dataset of the input predefined distribution


#### 4. visDistData(DBdata, type, t, xl, yl)
The function plots distributions of distribution data which is generated by genDistData()

######Params
**DBdata** - distribution data

**type** - plot by frequence / probability

**t** - title of plot

**xl** - description of x-asis 

**yl** - description of y-asis


#### 5. polyFit(DBdata, n)
The function fits distributions by polynomial curve fitting and returns a polynomial curve fitting function.

###### Params
**DBdata** - distribution data

**n** - the degree of polynomial functions

###### Return
A polynomial curve fitting function


#### 6. fourierFit(DBdata, n)
The function fits distributions by fourier curve fitting and returns a fourier curve fitting function.

###### Params
**DBdata** - distribution data

**n** - the degree of the fourier fitting function

###### Return
A fourier curve fitting function

#### 7. gaussianFit(DBdata)
The function fits distributions by gaussian curve fitting and returns a gaussian curve fitting function.

###### Params
**DBdata** - distribution data

###### Return 
A gaussian curve fitting function

#### 8. custFit(DBdata, formula)
The function fits distributions by a customised curve fitting and returns a customised curve fitting function.

###### Params 
**DBdata** - distribution data

**formula** - a customised fitting formula

###### Return
A customised curve fitting function

#### 9. visFitting(DBdata, t, xl, yl)
The function visualizes data distribution and corresponding fitting function so as to provide an intutive way to evaluate the performance of fitting function.

###### Params 
**DBdata** - distribution data

**t** - title of plot

**xl** - description of x-asis 

**yl** - description of y-asis


#### 10. conNormalizer(tg, bs)
The function normalizes a target data array to a basis array based on their distributions and the basis data array can be an arbitary data array or a standard distribution such as normal distribution. 

###### Params
**tg** - a target data array

**bs** - a basis data array

###### Return
A normalized target data array with the same distribution with the basis data array

#### 11. disNormalizer(tg, bs)
The function normalize target data array to a basis array based on element positions. This method does not need to do fitting before normalization and works for discrete values as well.  

###### Params 
**tg** - a target data array

**bs** - a basis data array

###### Return
A normalized target data array with the same distribution with the basis data array

#### 12. distrNormalizer(tg, bs)
The function normalize target data array to a standard distribution. 

###### Params 
**tg** - a target data array

**bs** - a standard distribution created by defineDist(dist)

###### Return
A normalized target data array with the same distribution with the basis data array
