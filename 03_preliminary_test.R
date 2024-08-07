allFiles <- list.files(pattern = "evaluation")

# FILES NACH CONDITION SORTIEREN
conditionNr <- sapply(allFiles, FUN = function(iFile){
  # Extrahiere die Zahl vor ".rda"
  extracted_number <- sub(".*_condition (\\d+)\\.rda", "\\1", iFile)
  # Konvertiere den extrahierten String in eine Zahl
  extracted_number <- as.numeric(extracted_number)
})
allFiles <- allFiles[order(conditionNr)]


issues <- sapply(allFiles, function(tmp_file){
  # tmp_file <- allFiles[1]
  # tmp_sample <- tmp[[1]]
  
  tmp <- readRDS(tmp_file)
  
  tmp_issues <- sapply(tmp, function(tmp_sample){
    c(logReg_Sep = tmp_sample$LogReg$separation,
      logReg_error = !is.na(tmp_sample$LogReg$errmessage),
      UPlogReg_Sep = tmp_sample$UpsamplingLogReg$separation,
      UPlogReg_error = !is.na(tmp_sample$UpsamplingLogReg$errmessage),
      enet_logloss_error = !is.null(tmp_sample$ElasticNetLogloss$problem),
      enet_roc_error = !is.null(tmp_sample$ElasticNetRoc$problem),
      enet_UPlogloss_error = !is.null(tmp_sample$UpsamplingElasticNetLogloss$problem),
      enet_UProc_error = !is.null(tmp_sample$UpsamplingElasticNetRoc$problem),
      gbm_logloss_error = !is.null(tmp_sample$GBMLogloss$problem),
      gbm_roc_error = !is.null(tmp_sample$GBMRoc$problem),
      gbm_UPlogloss_error = !is.null(tmp_sample$UpsamplingGBMLogloss$problem),
      gbm_UProc_error = !is.null(tmp_sample$UpsamplingGBMRoc$problem)
      )
  }, simplify = "array")
  
  apply(tmp_issues, 1, mean) 
  
  
}, simplify = "array")

dimnames(issues)[[2]] <- 1:dim(issues)[2] 
issues

# write_csv2(as.data.frame(t(issues)), file = "estimation_issues.csv")



  tmp_file <- allFiles[13]
  tmp <- readRDS(tmp_file)
  tmp_sample <- tmp[[1]]
  
  tmp_issues <- sapply(tmp, function(tmp_sample){
  # print(tmp_sample$LogReg$errmessage)
  # print(tmp_sample$UpsamplingLogReg$errmessage)
  print(tmp_sample$GBMLogloss$errmessage)
  }, simplify = "array")
  

