regVarImp <- function(model, xTrain, yTrain, xTest, fitControl, myTimeLimit, no.cores, lk_col, supress.output){

resultVarImpListCombREG <- NULL
resultVarImpListCombREG <- list()

myTimeLimitSet <- myTimeLimit
fitControlSet <- fitControl
lk_col <- lk_col
supress.output <- supress.output



regVarPred <- function(funcRegPred) {

#Print out function names
cat("----------------------------------------\n")
cat("Calculating: ", funcRegPred,"\n")
cat("----------------------------------------\n")


outfile<-paste("./",(lk_col-1),"in_default_REGControl_", paste(funcRegPred),".RData",sep="")

outfileImp<-paste("./",(lk_col-1),"in_default_REGControl_VarImp_", paste(funcRegPred),".txt",sep="")


#start feature selection method
timer1 <- proc.time()

if(supress.output==TRUE){

# Supress output
sink("/dev/null")
res <- invisible(try(timeout(train(xTrain,yTrain, method=funcRegPred, trControl=fitControlSet),seconds=myTimeLimitSet,my.pid=Sys.getpid()),silent=TRUE))
sink()

} else {

res <- try(timeout(train(xTrain,yTrain, method=funcRegPred, trControl=fitControlSet),seconds=myTimeLimitSet,my.pid=Sys.getpid()),silent=TRUE)

}

timer2 <- proc.time() - timer1 

variableImportanceRes <- try(varImp(res$finalModel),silent=TRUE)

if(class(res) != "try-error"){

resultVarImpListCombREG[funcRegPred] <- try(list(variableImportanceRes),silent=TRUE)

if(class(variableImportanceRes) != "try-error"){

resultVarImpListCombREG[funcRegPred] <- try(list(variableImportanceRes),silent=TRUE)

# save results
try(save(res, file=outfile),silent=TRUE)

try(write.table(variableImportanceRes, col.names=TRUE, row.names=TRUE, quote=FALSE, sep="\t", file=outfileImp),silent=TRUE)

cat("----------------------------------------\n")
cat("",funcRegPred,"\n")
cat("----------------------------------------\n")
cat("Elapsed time: ",timer2,"\n")
cat("Variable importance: \n")
try(print(variableImportanceRes),silent=TRUE)

} else {

yPred <- try(predict(res,xTest),silent=TRUE)

if(class(yPred)!="try-error"){

variableImportanceRes <- try(filterVarImp(xTest,yPred),silent=TRUE)

if(class(variableImportanceRes)!="try-error") {

resultVarImpListCombREG[funcRegPred] <- try(list(variableImportanceRes),silent=TRUE)

} else {

variableImportanceRes <- try(filterVarImp(xTest,yPred,nonpara=TRUE),silent=TRUE)

resultVarImpListCombREG[funcRegPred] <- try(list(variableImportanceRes),silent=TRUE)

}

# save results
try(save(res, file=outfile),silent=TRUE)

try(write.table(variableImportanceRes,col.names=TRUE, row.names=TRUE, quote=FALSE, sep="\t", file=outfileImp))

cat("----------------------------------------\n")
cat("",funcRegPred,"\n")
cat("----------------------------------------\n")
cat("Elapsed time: ",timer2,"\n")
cat("Variable importance: \n")
try(print(variableImportanceRes),silent=TRUE)

} else {

print("Predicting variable importance has failed!")
resultVarImpListCombREG[funcRegPred] <- try(list(cat("Predicting variable importance has failed!")),silent=TRUE)

}

}

} else {
 
print("Building model has failed or reached timelimit!")
resultVarImpListCombREG[funcRegPred] <- try(list(cat("Building model has failed or reached timelimit!")),silent=TRUE)

}

}

resultVarImpListCombREG[model] <- mclapply(model,regVarPred, mc.preschedule=FALSE, mc.cores=no.cores)

return(resultVarImpListCombREG)

}

# # test function
# 
# model <- c("rf", "rpart", "lm")
# myTimeLimit<-60
# method <- c("boot")
# returnResamp <- c("all")
# no.cores<-2
# 
# 
# skel_plik <- "MPS_PLGA_298in_base_with_labels" # Training data - it will give (xTrain and yTrain)
# skel_plik1 <- "MPS_PLGA_298in_base_with_labels" # Testing data - it will give (xTest and yTest)
# skel_outfile<-paste("./",skel_plik,"_caretPckg_",sep="")
# 
# plik <- paste(skel_plik,".txt",sep="")
# plik1 <- paste(skel_plik1,".txt",sep="")
# 
# trainDataFrame1 <- read.csv(plik,header=TRUE,sep="\t", strip.white = TRUE, na.strings = c("NA",""))
# testDataFrame1 <- read.csv(plik1,header=TRUE,sep="\t", strip.white = TRUE, na.strings = c("NA",""))
# 
# with.labels=TRUE
# 
# 
# #--------Scan dimensions of trainDataFrame1 [lk_row x lk_col]
# lk_col = ncol(trainDataFrame1)
# lk_row = nrow(trainDataFrame1)
# 
# #--------Read labels of trainDataFrame1
# labelsFrameTest <- as.data.frame(colnames(trainDataFrame1))
# 
# trainMatryca_nr <- matrix(data=NA,nrow=lk_row,ncol=lk_col)
# trainMatryca_nr_backup<- matrix(data=NA,nrow=lk_row,ncol=lk_col)
# 
# row=0
# col=0
# 
# for(col in 1:(lk_col)) {
#    for(row in 1:(lk_row)) {
#      trainMatryca_nr[row,col] <- (as.numeric(trainDataFrame1[row,col]))
#     }
# }
# 
# 
# #--------Scan dimensions of trainDataFrame1 [lk_row x lk_col]
# lk_col_test = ncol(testDataFrame1)
# lk_row_test = nrow(testDataFrame1)
# 
# testMatryca_nr <- matrix(data=NA,nrow=lk_row,ncol=lk_col)
# 
# row=0
# col=0
# 
# for(col in 1:(lk_col_test)) {
#    for(row in 1:(lk_row_test)) {
#      testMatryca_nr[row,col] <- (as.numeric(testDataFrame1[row,col]))
#     }
# }
# 
# 
# labelsFrameTest <- as.data.frame(colnames(trainDataFrame1))
# 
# fitControl <- trainControl(method = method, returnResamp = returnResamp)
# 
# # definition of input and output vector
# xTrain <- data.frame(trainMatryca_nr[,-lk_col])
# yTrain <- as.vector(trainMatryca_nr[,lk_col])
# 
# xTest <- data.frame(testMatryca_nr[,-lk_col])
# yTest <- as.vector(testMatryca_nr[,lk_col])
# 
# testRegVarImpRES <- regVarImp(model, xTrain, yTrain, fitControl, myTimeLimit, lk_col)