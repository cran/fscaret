regVarImp <- function(model, xTrain, yTrain, xTest, fitControl, myTimeLimit, no.cores, lk_col, supress.output, mySystem){

resultVarImpListCombREG <- NULL
resultVarImpListCombREG <- list()

myTimeLimitSet <- myTimeLimit
fitControlSet <- fitControl
lk_col <- lk_col
supress.output <- supress.output
mySystem <- mySystem
no.cores <- no.cores


regVarPred <- function(funcRegPred) {

#Print out function names
cat("----------------------------------------\n")
cat("Calculating: ", funcRegPred,"\n")
cat("----------------------------------------\n")


outfile<-paste("./",(lk_col-1),"in_default_REGControl_", paste(funcRegPred),".RData",sep="")

outfileImp<-paste("./",(lk_col-1),"in_default_REGControl_VarImp_", paste(funcRegPred),".txt",sep="")


#start feature selection method
timer1 <- proc.time()

if(mySystem!="windows"){
  if(supress.output==TRUE){

    # Supress output
    sink("/dev/null")
    res <- invisible(try(timeout(train(xTrain,yTrain, method=funcRegPred, trControl=fitControlSet),seconds=myTimeLimitSet,my.pid=Sys.getpid()),silent=TRUE))
    sink()

    } else {

    res <- try(timeout(train(xTrain,yTrain, method=funcRegPred, trControl=fitControlSet),seconds=myTimeLimitSet,my.pid=Sys.getpid()),silent=TRUE)

    }
    
} else {

    if(supress.output==TRUE){

    # Supress output
    sink("NUL")
    res <- try(train(xTrain,yTrain, method=funcRegPred, trControl=fitControlSet),silent=TRUE)
    sink()

    } else {

    res <- try(train(xTrain,yTrain, method=funcRegPred, trControl=fitControlSet),silent=TRUE)

    }

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
resultVarImpListCombREG[funcRegPred] <- try(list(NA),silent=TRUE)

}

}

} else {
 
print("Building model has failed or reached timelimit!")
resultVarImpListCombREG[funcRegPred] <- try(list(NA),silent=TRUE)

}

}

resultVarImpListCombREG[model] <- mclapply(model,regVarPred, mc.preschedule=FALSE, mc.cores=no.cores)

return(resultVarImpListCombREG)

}
