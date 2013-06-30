### R code from vignette source 'fscaret.Rnw'

###################################################
### code chunk number 1: install (eval = FALSE)
###################################################
## install.packages("fscaret", dependencies = c("Depends", "Suggests"))


###################################################
### code chunk number 2: load_data (eval = FALSE)
###################################################
## basename_file <- "My_database"
## file_name <- paste(basename_file,".csv",sep="")


###################################################
### code chunk number 3: load_data (eval = FALSE)
###################################################
## matrixTrain <- read.csv(file_name,header=TRUE,sep="\t",
## 		strip.white = TRUE, na.strings = c("NA",""))


###################################################
### code chunk number 4: load_data (eval = FALSE)
###################################################
## matrixTrain <- as.data.frame(matrixTrain)


###################################################
### code chunk number 5: funcRegPred_all
###################################################
library(fscaret)
data(funcRegPred)
funcRegPred


###################################################
### code chunk number 6: fscaret_example (eval = FALSE)
###################################################
## library(fscaret)
## data(dataset.train)
## data(dataset.test)
## 
## trainDF <- dataset.train
## testDF <- dataset.test
## 
## myFS<-fscaret(trainDF, testDF, myTimeLimit = 15, preprocessData=TRUE,
## 	      Used.funcRegPred=c("pcr","pls"), with.labels=TRUE,
## 	      supress.output=TRUE)
## myRES_tab <- myFS$VarImp$matrixVarImp.MSE[1:10,]
## myRES_tab <- subset(myRES_tab, select=c("pcr","pls","SUM%","ImpGrad","Input_no"))
## myRES_rawMSE <- myFS$VarImp$rawMSE
## myRES_PPlabels <- myFS$PPlabels


###################################################
### code chunk number 7: fscaret_example
###################################################
library(fscaret)
data(dataset.train)
data(dataset.test)

trainDF <- dataset.train
testDF <- dataset.test

myFS<-fscaret(trainDF, testDF, myTimeLimit = 15, preprocessData=TRUE,
	      Used.funcRegPred=c("pcr","pls"), with.labels=TRUE,
	      supress.output=TRUE)
myRES_tab <- myFS$VarImp$matrixVarImp.MSE[1:10,]
myRES_tab <- subset(myRES_tab, select=c("pcr","pls","SUM%","ImpGrad","Input_no"))
myRES_rawMSE <- myFS$VarImp$rawMSE
myRES_PPlabels <- myFS$PPlabels


###################################################
### code chunk number 8: fscaret_example
###################################################
# Print out the Variable importance results for MSE scaling
print(myRES_tab)


###################################################
### code chunk number 9: fscaret_example
###################################################
# Print out the generalization error for models
print(myRES_rawMSE)


###################################################
### code chunk number 10: fscaret_example
###################################################
# Print out the reduced number of inputs after preprocessing
print(myRES_PPlabels)


###################################################
### code chunk number 11: barPlot
###################################################

# Present variable importance on barplot
a=0.9
b=0.7
c=2

lk_row.mse=nrow(myFS$VarImp$matrixVarImp.MSE)

setEPS()

barplot1 <- barplot(myFS$VarImp$matrixVarImp.MSE$"SUM%"[1:(a*lk_row.mse)],
	    cex.names=b, las = c, xlab="Variables", ylab="Importance Sum%",
	    names.arg=c(rownames(myFS$VarImp$matrixVarImp.MSE)[1:(a*lk_row.mse)]))
	    
lines(x = barplot1, y = myFS$VarImp$matrixVarImp.MSE$"SUM%"[1:(a*lk_row.mse)])
points(x = barplot1, y = myFS$VarImp$matrixVarImp.MSE$"SUM%"[1:(a*lk_row.mse)])


