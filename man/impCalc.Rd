\name{impCalc}
\alias{impCalc}
\title{
impCalc
}
\description{
impCalc function is designed to scale variable importance according to MSE and RMSE calculations.
It also stores the raw MSE and RMSE derived from models. impCalc function shouldn't be used alone unless
user has trained models from caret package in RData files.
}
\usage{
impCalc(skel_outfile, xTest, yTest, lk_col,labelsFrame,with.labels)
}
\arguments{
  \item{skel_outfile}{
Skeleton name of output file
}
  \item{xTest}{
Input vector of testing data set
}
  \item{yTest}{
Output vector of testing data set
}
  \item{lk_col}{
Number of columns of whole data set
}
\item{labelsFrame}{
Labels to sort variable importance
}
\item{with.labels}{
Pass with.labels argument. It is advised to ALWAYS use labels as in some cases VarImp returns importance in descending values. If you insist turning with.labels FALSE, then make sure data base contains pure data and you read it (read.csv) to data.frame with option header=FALSE. 
}
}
\details{
impCalc function lists RData files in working directory assuming there are only models derived by caret. In a loop function loads models and tries to get the variable importance.
}
\author{
Jakub Szlek and Aleksander Mendyk
}
\examples{
# 
# Hashed to comply with new CRAN check
# 
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
skel_outfile <- paste("_default_",sep="")
mySystem <- .Platform$OS.type
with.labels <- TRUE


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

testMatryca_nr <- matrix(data=NA,nrow=lk_row_test,ncol=lk_col_test)

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


# Calling lower function to create models to calculate on
myVarImp <- regVarImp(model, xTrain, yTrain, xTest,
	    fitControl, myTimeLimit, no.cores, lk_col,
	    supress.output, mySystem)


myImpCalc <- impCalc(skel_outfile, xTest, yTest, lk_col,labelsFrame,with.labels)

}
\keyword{ design }
\keyword{ models }
