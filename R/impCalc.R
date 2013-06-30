impCalc <- function(skel_outfile, xTest, yTest, lk_col){
# Get RMSE from all .RData files

filesRData <- list.files(pattern = "_default_.*.RData")
impCalcScaleRMSE<-list()
impCalcScaleMSE<-list()
impCalcRes<-list()

# Dummy
res <- NULL
rm(res)


if(length(filesRData)>0){

filesRDataCols <- gsubfn(".*Control_","",filesRData)
filesRDataCols <- gsubfn(".RData","",filesRDataCols)

varImpRDataOutfileRMSE <- paste("./",skel_outfile, "VarImpRes_RMSE.RData",sep="")
varImpTxtOutfileRMSE <- paste("./",skel_outfile, "VarImpRes_RMSE.txt",sep="")

varImpRDataOutfileMSE <- paste("./",skel_outfile, "VarImpRes_MSE.RData",sep="")
varImpTxtOutfileMSE <- paste("./",skel_outfile, "VarImpRes_MSE.txt",sep="")

cat("\n----Processing files:----\n")
print(filesRData)

for (i in 1:length(filesRData)){

load(filesRData[i])

print("")
print("Calculating error for model:")
print(filesRData[i])
print("")

nameTmp <- filesRDataCols[i]

predTmp <- try(predict(res, xTest),silent=TRUE)

if(class(predTmp)!="try-error"){

if(is.factor(predTmp)==TRUE){

predTmp <- as.character(predTmp)
predTmp <- gsubfn(",",".",predTmp)
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
predTmp <- as.numeric(predTmp)

}

impCalcScaleRMSE[[i]]<-RMSE(predTmp, yTest, length(yTest))
impCalcScaleMSE[[i]]<-MSE(predTmp, yTest, length(yTest))

} else {

impCalcScaleRMSE[[i]] <- NA
impCalcScaleMSE[[i]] <- NA

}

names(impCalcScaleRMSE)[[i]] <- nameTmp
names(impCalcScaleMSE)[[i]] <- nameTmp

}

# Set local settings back to "normal", because loading RWeka changes locale settings
Sys.setlocale(category = "LC_NUMERIC", locale = "C")

impCalcScaleRMSE <- as.data.frame(impCalcScaleRMSE)
impCalcScaleMSE <- as.data.frame(impCalcScaleMSE)

maxRmse <- try(max(impCalcScaleRMSE, na.rm=TRUE),silent=TRUE)

if(class(maxRmse)=="try-error"){
maxRmse <- 100000
}

maxMse <- try(max(impCalcScaleMSE, na.rm=TRUE),silent=TRUE)

if(class(maxMse)=="try-error"){
maxMse <- 100000
}

impCalcScaleRMSE[is.na(impCalcScaleRMSE)]<-100000*maxRmse
impCalcScaleMSE[is.na(impCalcScaleMSE)]<-100000*maxMse

rawRMSE <- as.data.frame(impCalcScaleRMSE)
rawMSE <- as.data.frame(impCalcScaleMSE)

minRmse <- min(impCalcScaleRMSE)
minMse <- min(impCalcScaleMSE)

impCalcScaleRMSE <- (impCalcScaleRMSE/minRmse)^(-1)
impCalcScaleMSE <- (impCalcScaleMSE/minMse)^(-1)


# Get all the results from txt files and put them into one dataframe

# Load list of files with _VarImp_

filesVarImp <- list.files(pattern = "_VarImp_")

matrycaVarImp.RMSE<-matrix(data=0,nrow=(lk_col-1),ncol=(length(filesVarImp)+3))
matrycaVarImp.MSE<-matrix(data=0,nrow=(lk_col-1),ncol=(length(filesVarImp)+3))


cat("\n----Processing files:----\n")
print(filesVarImp)

# Concatenate the results
for(i in 1:length(filesVarImp)){

currentFile <- filesVarImp[i]

# read file
tempDF <- read.csv(filesVarImp[i],header=TRUE,sep="\t", strip.white = TRUE, na.strings = c("NA",""))

if(is.factor(tempDF[,1])==TRUE){

tempDF[,1] <- as.character(tempDF[,1])
tempDF[,1] <- gsubfn(",",".",tempDF[,1])
Sys.setlocale(category = "LC_NUMERIC", locale = "C")
tempDF[,1] <- as.numeric(tempDF[,1])

}

# Check if there are any NA values and zero them before summing
tempDF[is.na(tempDF)]<-0

# Get absolute values of variable importance
tempDF <- abs(tempDF)

# Delete X char from rownames
rownames(tempDF)<-gsubfn("X","",rownames(tempDF))

# Scale results from 0 to 100
tempDF.RMSE<-(tempDF/sum(tempDF[,1]))*100*impCalcScaleRMSE[,i]
tempDF.MSE<-(tempDF/sum(tempDF[,1]))*100*impCalcScaleMSE[,i]

# Sort data.frame according to rownames
tempDF.RMSE<-tempDF.RMSE[order(as.numeric(rownames(tempDF.RMSE))),,drop=FALSE]
tempDF.MSE<-tempDF.MSE[order(as.numeric(rownames(tempDF.MSE))),,drop=FALSE]


cols=i
rows=0
for(rows in 1:nrow(tempDF.RMSE)){
matrycaVarImp.RMSE[rows,cols]<-tempDF.RMSE[rows,]
}

cols=i
rows=0
for(rows in 1:nrow(tempDF.MSE)){
matrycaVarImp.MSE[rows,cols]<-tempDF.MSE[rows,]
}

}

matrycaVarImp.RMSE[is.na(matrycaVarImp.RMSE)]<-0
matrycaVarImp.MSE[is.na(matrycaVarImp.MSE)]<-0

# Sum row-by-row
for (rows in 1:nrow(matrycaVarImp.RMSE)){
matrycaVarImp.RMSE[rows,(ncol(matrycaVarImp.RMSE)-2)]<-sum(matrycaVarImp.RMSE[rows,])
}

for (rows in 1:nrow(matrycaVarImp.MSE)){
matrycaVarImp.MSE[rows,(ncol(matrycaVarImp.MSE)-2)]<-sum(matrycaVarImp.MSE[rows,])
}

# Calculate percentages
maks.sum.rmse <- max(matrycaVarImp.RMSE[,(ncol(matrycaVarImp.RMSE)-2)])
maks.sum.mse <- max(matrycaVarImp.MSE[,(ncol(matrycaVarImp.MSE)-2)])

for (rows in 1:nrow(matrycaVarImp.RMSE)){
matrycaVarImp.RMSE[rows,(ncol(matrycaVarImp.RMSE)-1)]<-(matrycaVarImp.RMSE[rows,(ncol(matrycaVarImp.RMSE)-2)])/maks.sum.rmse*100
}

for (rows in 1:nrow(matrycaVarImp.MSE)){
matrycaVarImp.MSE[rows,(ncol(matrycaVarImp.MSE)-1)]<-(matrycaVarImp.MSE[rows,(ncol(matrycaVarImp.MSE)-2)])/maks.sum.mse*100
}

matrycaVarImp.RMSE <- as.data.frame(matrycaVarImp.RMSE)
matrycaVarImp.MSE <- as.data.frame(matrycaVarImp.MSE)

filesVarImpCols <- gsubfn(".*_VarImp_","",filesVarImp)
filesVarImpCols <- gsubfn(".txt","",filesVarImpCols)

colnames(matrycaVarImp.RMSE)<- filesVarImpCols
colnames(matrycaVarImp.MSE)<- filesVarImpCols

names(matrycaVarImp.RMSE)[length(matrycaVarImp.RMSE)-2]<-"SUM"
names(matrycaVarImp.MSE)[length(matrycaVarImp.MSE)-2]<-"SUM"

names(matrycaVarImp.RMSE)[length(matrycaVarImp.RMSE)-1]<-"SUM%"
names(matrycaVarImp.MSE)[length(matrycaVarImp.MSE)-1]<-"SUM%"

names(matrycaVarImp.RMSE)[length(matrycaVarImp.RMSE)]<-"ImpGrad"
names(matrycaVarImp.MSE)[length(matrycaVarImp.MSE)]<-"ImpGrad"


# Sort data.frame according to SUM variable importance
matrycaVarImp.RMSE<-matrycaVarImp.RMSE[order(matrycaVarImp.RMSE[,length(matrycaVarImp.RMSE)-2],decreasing=TRUE),,drop=FALSE]
matrycaVarImp.MSE<-matrycaVarImp.MSE[order(matrycaVarImp.MSE[,length(matrycaVarImp.MSE)-2],decreasing=TRUE),,drop=FALSE]


# # Importance gradient
for (rows in 2:nrow(matrycaVarImp.RMSE)){

imp1 <- matrycaVarImp.RMSE[(rows-1),(ncol(matrycaVarImp.RMSE)-2)]
imp2 <- matrycaVarImp.RMSE[(rows),(ncol(matrycaVarImp.RMSE)-2)]

matrycaVarImp.RMSE[rows,ncol(matrycaVarImp.RMSE)] <- (imp1 - imp2)/imp1*100

}
 
for (rows in 2:nrow(matrycaVarImp.MSE)){

imp1 <- matrycaVarImp.MSE[(rows-1),(ncol(matrycaVarImp.MSE)-2)]
imp2 <- matrycaVarImp.MSE[(rows),(ncol(matrycaVarImp.MSE)-2)]

matrycaVarImp.MSE[rows,ncol(matrycaVarImp.MSE)] <- (imp1 - imp2)/imp1*100

}

# Add importance variables names as last column
matrycaVarImp.MSE$Input_no <- rownames(matrycaVarImp.MSE)
matrycaVarImp.RMSE$Input_no <- rownames(matrycaVarImp.RMSE)



impCalcRes <- list("rawMSE"=rawMSE, "rawRMSE"=rawRMSE, "matrixVarImp.RMSE"=matrycaVarImp.RMSE, "matrixVarImp.MSE"=matrycaVarImp.MSE)

}

return(impCalcRes)

}