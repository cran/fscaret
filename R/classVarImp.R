# 
# Code block prepared to impement Class variable importance
# 
# pre-ALPHA VERSION!
# 
# Code block prepared to impement Class variable importance
# 



classVarImp <- function(model, xTrain, yTrain, xTest, fitControl, myTimeLimit, no.cores, lk_col, supress.output, mySystem){

resultVarImpListCombCLASS <- NULL
resultVarImpListCombCLASS <- list()

myTimeLimitSet <- myTimeLimit
fitControlSet <- fitControl
lk_col <- lk_col
supress.output <- supress.output
mySystem <- mySystem
no.cores <- no.cores


regVarPred <- function(funcClassPred) {

#Print out function names
cat("----------------------------------------\n")
cat("Calculating: ", funcClassPred,"\n")
cat("----------------------------------------\n")


outfile<-paste("./",(lk_col-1),"in_default_CLASSControl_", paste(funcClassPred),".RData",sep="")

outfileImp<-paste("./",(lk_col-1),"in_default_CLASSControl_VarImp_", paste(funcClassPred),".txt",sep="")


#start feature selection method
timer1 <- proc.time()

if(mySystem!="windows"){
  if(supress.output==TRUE){

    # Supress output
    sink("/dev/null")
    res <- invisible(try(timeout(train(xTrain,yTrain, method=funcClassPred, trControl=fitControlSet),seconds=myTimeLimitSet,my.pid=Sys.getpid()),silent=TRUE))
    sink()

    } else {

    res <- try(timeout(train(xTrain,yTrain, method=funcClassPred, trControl=fitControlSet),seconds=myTimeLimitSet,my.pid=Sys.getpid()),silent=TRUE)

    }
    
} else {

    if(supress.output==TRUE){

    # Supress output
    sink("NUL")
    res <- try(train(xTrain,yTrain, method=funcClassPred, trControl=fitControlSet),silent=TRUE)
    sink()

    } else {

    res <- try(train(xTrain,yTrain, method=funcClassPred, trControl=fitControlSet),silent=TRUE)

    }

}

timer2 <- proc.time() - timer1 

variableImportanceRes <- try(varImp(res$finalModel),silent=TRUE)

resultVarImpListCombCLASS[funcClassPred] <- try(list(variableImportanceRes),silent=TRUE)

if(class(variableImportanceRes) == "try-error"){

  yPred <- try(predict(res,xTest),silent=TRUE)

    if(class(yPred)!="try-error"){

      variableImportanceRes <- try(filterVarImp(xTest,yPred),silent=TRUE)

	if(class(variableImportanceRes)!="try-error") {

	  resultVarImpListCombCLASS[funcClassPred] <- try(list(variableImportanceRes),silent=TRUE)

	}

    }
}

if((class(res) != "try-error")&&(class(variableImportanceRes) != "try-error")){


# save results
try(save(res, file=outfile),silent=TRUE)

try(write.table(variableImportanceRes, col.names=TRUE, row.names=TRUE, quote=FALSE, sep="\t", file=outfileImp),silent=TRUE)

cat("----------------------------------------\n")
cat("",funcClassPred,"\n")
cat("----------------------------------------\n")
cat("Elapsed time: ",timer2,"\n")
cat("Variable importance: \n")
try(print(variableImportanceRes),silent=TRUE)

  } else if(class(res) != "try-error"){

variableImportanceRes <- try(varImp(res),silent=TRUE)

try(write.table(variableImportanceRes$importance, col.names=TRUE, row.names=TRUE, quote=FALSE, sep="\t", file=outfileImp),silent=TRUE)

resultVarImpListCombCLASS[funcClassPred] <- try(list(variableImportanceRes),silent=TRUE)

variableImportanceRes <- try(filterVarImp(xTest,yPred,nonpara=TRUE),silent=TRUE)

resultVarImpListCombCLASS[funcClassPred] <- try(list(variableImportanceRes),silent=TRUE)

# save results
try(save(res, file=outfile),silent=TRUE)

try(write.table(variableImportanceRes,col.names=TRUE, row.names=TRUE, quote=FALSE, sep="\t", file=outfileImp))

cat("----------------------------------------\n")
cat("",funcClassPred,"\n")
cat("----------------------------------------\n")
cat("Elapsed time: ",timer2,"\n")
cat("Variable importance: \n")
try(print(variableImportanceRes),silent=TRUE)

  } else if (class(variableImportanceRes) == "try-error"){

print("Predicting variable importance has failed!")
resultVarImpListCombCLASS[funcClassPred] <- try(list(NA),silent=TRUE)

  } else if (class(res) == "try-error"){
 
print("Building model has failed or reached timelimit!")
resultVarImpListCombCLASS[funcClassPred] <- try(list(NA),silent=TRUE)

  }

}

resultVarImpListCombCLASS[model] <- mclapply(model,regVarPred, mc.preschedule=FALSE, mc.cores=no.cores)

return(resultVarImpListCombCLASS)

}

# 
# Code block prepared to impement Class variable importance
# 
# pre-ALPHA VERSION!
# 
# Code block prepared to impement Class variable importance
# 