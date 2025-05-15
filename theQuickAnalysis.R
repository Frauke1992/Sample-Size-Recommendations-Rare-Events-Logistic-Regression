
######## Load packages ########
library(parallel)
source("theQuickAnalysisHelpers.R")

######## Read all data ########

allFiles <- list.files(pattern = "evaluation_data.*.rda")

condition_numbers <- sub(".*_condition (\\d+)\\.rda", "\\1", allFiles)
condition_numbers <- as.numeric(condition_numbers)
allFiles <- allFiles[order(condition_numbers)]

myCL <- makeCluster(10)

allRes <- parLapply(myCL, allFiles, function(iFile){
  currentRes <- readRDS(iFile)
})

stopCluster(myCL)

######## Extract Performance metrics ########

predictivePerformance <- 
  sapply(allRes, function(iCondition){
    sapply(iCondition, function(iSample){
      sapply(iSample, function(x) {
        tmp <- x$performanceMetrics["validation",] 
        if (!is.null(tmp)){
         if (length(tmp) > 3){
           tmp <- tmp[c(1:2,4)]
         }
         tryCatch({names(tmp)[3] <- "logLoss"}, error = function(e) browser())
         tmp
       } else{
          c(auc = NA, misclassification = NA, logLoss = NA)
        }
      }
      )    
    }, simplify = "array")
  }, simplify = "array")

dim(predictivePerformance)
meanPerf <- apply(predictivePerformance, MARGIN = c(1,2,4), mean, na.rm = TRUE)
allNA <- apply(predictivePerformance, MARGIN = c(1,2,3,4), is.na)
meanNA <- apply(allNA, MARGIN = c(1,2,4), mean)

troublePerf <- t(meanNA["auc",,])
colnames(troublePerf) <- paste0("trouble_", colnames(meanPerf))

aucPerf <- t(meanPerf["auc",,])
colnames(aucPerf) <- paste0("auc_", colnames(meanPerf))

misclassificationPerf <- t(meanPerf["misclassification",,])
colnames(misclassificationPerf) <- paste0("misclassification_", colnames(meanPerf))

logLossPerf <- t(meanPerf["logLoss",,])
colnames(logLossPerf) <- paste0("logLoss_", colnames(meanPerf))

resTable <- read.csv("Conditions.csv")

resTable <- cbind(resTable, troublePerf, aucPerf, misclassificationPerf, logLossPerf)

write.csv2(resTable, "resTable.csv", row.names = FALSE)


######## Extract Coefficients ########

allResCoef <- 
  lapply(allRes, function(iCondition){
    lapply(iCondition, function(iSample){
      lapply(names(iSample), function(iModel) {
        tryCatch({
          process_model(iModel, model = iSample[[iModel]])
        }, error = function(e) {
          print(iModel)
          print(e)
          browser()
        })
      }
      )    
    })
  })


# Apply the function to your nested list
allResCoef_methods <- reverse_nested_list(allResCoef)
names(allResCoef_methods) <- names(allRes[[1]][[1]])



selection <- lapply(names(allResCoef_methods), function(iMethod){
  tmp <- allResCoef_methods[[iMethod]]

  sapply(tmp, function(iCondition){
    sapply(iCondition, function(iSample){
      # Return NA if iSample is NULL
      if (is.null(iSample)) {
        return(NA)
      }
      if (grepl("ElasticNet", iMethod)) {
        return(iSample > 0)
      } else if (grepl("LogReg", iMethod)) {
        iSample[,"Pr(>|z|)"] <- iSample[,"Pr(>|z|)"] < 0.05
        return(iSample[,"Pr(>|z|)"])
      } else if (grepl("GBM", iMethod)) {
        return(iSample > 1)
      } else {
        stop(paste("Unknown model type:", iMethod))
      }
          })
  })
})




selection_rates <- lapply(selection, function(iMethod){
  
 tmp <- sapply(iMethod, function(iCondition){
   # Convert the result explicitly into an array if needed
   if(is.list(iCondition)){
     iCondition <- do.call(cbind, iCondition)
     # browser()
   }
   tryCatch({
       apply(iCondition, MARGIN = 1, mean, na.rm = TRUE)
   }, error = function(e) {
     browser()
   })
  }, simplify = "array")
 if(is.list(tmp)) tmp <- cbind_fill_na(tmp)
 return(tmp)
})

names(selection_rates) <- names(allResCoef_methods)

selection_rates$LogReg
selection_rates$ElasticNetRoc
selection_rates$GBMRoc

save(selection_rates, file = "selection_rates.Rdata", compress = TRUE, compression_level = 6)


######### Debugging Spielwiese

selection <- lapply(names(allResCoef_methods), function(iMethod){
  tmp <- allResCoef_methods[[iMethod]]
  
  sapply(tmp, function(iCondition){
    sapply(iCondition, function(iSample){
      # Return NA if iSample is NULL
      if (is.null(iSample)) {
        return(NA)
      }
      if (grepl("ElasticNet", iMethod)) {
        return(iSample > 0)
      } else if (grepl("LogReg", iMethod)) {
        return(iSample[, "Estimate"])
      } else if (grepl("GBM", iMethod)) {
        return(iSample > 1)
      } else {
        stop(paste("Unknown model type:", iMethod))
      }
    })
  })
})


dim(selection[[1]][[1]])

apply(selection[[1]][[1]], 1, mean, na.rm = TRUE)
