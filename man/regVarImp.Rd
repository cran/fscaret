\name{regVarImp}
\alias{regVarImp}
\title{
regVarImp
}
\description{
The function uses the caret package advantage to perform fitting of numerous regression models.
}
\usage{
regVarImp(model, xTrain, yTrain, xTest,
	  fitControl, myTimeLimit, no.cores,
	  lk_col, supress.output, mySystem)
}
\arguments{
  \item{model}{
Chosed models as called from function fscaret(), argument Used.funcRegPred.
}
  \item{xTrain}{
Training data set, data frame of input vector
}
  \item{yTrain}{
Training data set, vector of observed outputs
}
  \item{xTest}{
Testing data set, data frame of input vector
}
  \item{fitControl}{
Fitting controls passed to caret function
}
  \item{myTimeLimit}{
Time limit in seconds for single model fitting
}
  \item{no.cores}{
Number of used cores for calculations
}
  \item{lk_col}{
Number of columns for whole data set (inputs + output)
}
  \item{supress.output}{
If TRUE output of models are supressed.
}
  \item{mySystem}{
Called from fscaret() result of function .Platform$OS.type
}
}
\references{
Kuhn M. (2008) Building Predictive Models in R Using the caret Package \emph{Journal of Statistical Software} \bold{28(5)} \url{http://www.jstatsoft.org/}.
}
\author{
Jakub Szlek and Aleksander Mendyk
}
\examples{

# Hashed to comply with new CRAN check
# 
# Load library
library(fscaret)

# Read working directory
myWD <- getwd()

# Set working directory to tmp
setwd(tempdir())

# Load dataset
data(dataset.train)
data(dataset.test)

# Make objects
trainDF <- dataset.train
testDF <- dataset.test
model <- c("lm","pls","pcr")
fitControl <- trainControl(method = "boot", returnResamp = "all") 
myTimeLimit <- 5
no.cores <- 2
supress.output <- TRUE

mySystem <- .Platform$OS.type

if(mySystem=="windows"){
no.cores <- 1
}
# Scan dimensions of trainDF [lk_row x lk_col]
lk_col = ncol(trainDF)
lk_row = nrow(trainDF)

# Read labels of trainDF
labelsFrame <- as.data.frame(colnames(trainDF))

# Create a train data set matrix
trainMatryca_nr <- matrix(data=NA,nrow=lk_row,ncol=lk_col)

row=0
col=0

for(col in 1:(lk_col)) {
   for(row in 1:(lk_row)) {
     trainMatryca_nr[row,col] <- (as.numeric(trainDF[row,col]))
    }
}

# Pointing standard data set train
xTrain <- data.frame(trainMatryca_nr[,-lk_col])
yTrain <- as.vector(trainMatryca_nr[,lk_col])


#--------Scan dimensions of trainDataFrame1 [lk_row x lk_col]
lk_col_test = ncol(testDF)
lk_row_test = nrow(testDF)

testMatryca_nr <- matrix(data=NA,nrow=lk_row,ncol=lk_col)

row=0
col=0

for(col in 1:(lk_col_test)) {
   for(row in 1:(lk_row_test)) {
     testMatryca_nr[row,col] <- (as.numeric(testDF[row,col]))
    }
}

# Pointing standard data set test
xTest <- data.frame(testMatryca_nr[,-lk_col])
yTest <- as.vector(testMatryca_nr[,lk_col])


myVarImp <- regVarImp(model, xTrain, yTrain, xTest,
	    fitControl, myTimeLimit, no.cores, lk_col,
	    supress.output, mySystem)

summary(myVarImp)

print(myVarImp)

# Get back to previous working directory
setwd(myWD)


}

\keyword{ models }
\keyword{ robust }
