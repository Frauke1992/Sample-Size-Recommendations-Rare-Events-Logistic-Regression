#### Requires the script "Functions Sample Evaluation" ####

##### Load neccessary packages #####
library(parallel)

# set directory to folder containing necessary scripts
directory_script <- getwd()

# Load R-script containing setup for data visualization
source(paste0(directory_script, "/02a_Functions Sample Evaluation.R"))
source(paste0(directory_script, "/02b_MetaFun Sample Evaluation.R"))



####### Sample evaluation #######
#load the samples
# Read in table containing different combinations of simulation factors
condition_table <- as.data.frame(read.csv("Conditions.csv", header = TRUE))

condition_evaluation <- lapply(1:nrow(all_conditions), FUN = function(condition_counter){
  # condition_samples <- all_conditions[[condition_counter]]
  load(paste0("./data/samples_condition_", condition_counter, ".rdata"))
  
  logFolder <- getwd()
  # Initiate cluster; type = "FORK" only on Linux/MacOS: contains all 
  # environment variables automatically
  clust <- makeCluster(20, 
                       type = "FORK", 
                       outfile = paste0(logFolder, "evaluationDataStatus", 
                                        Sys.Date(),".txt"))

  # set seed that works for parallel processing
  set.seed(342890)
  s <- .Random.seed

  clusterSetRNGStream(cl = clust, iseed = s)
  # loop to evaluate the samples with the different methods
  evaluation_samples = parLapply(clust, condition_samples, fun = function(current_sample){
 
    # evaluate samples analyzed with caret without upsampling
    output_caret <- results.caret(train_data = current_sample$train, 
                                  validation_data = current_sample$validation,
                                  samplingtype = NULL, 
                                  oracle_model = current_sample$oracle_model)

    # evaluate samples analyzed with caret with upsampling
    output_caret_upsampling <- results.caret(train_data = current_sample$train, 
                                             validation_data = current_sample$validation, 
                                             samplingtype = "up", 
                                             oracle_model = current_sample$oracle_model)
 

    # save the outputs
    output_results<-c(output_caret, 
                      output_caret_upsampling)
    # change the names for the output
    names(output_results) <- c("LogReg","ElasticNetRoc","ElasticNetLogloss", "GBMRoc", "GBMLogloss", 
                               "UpsamplingLogReg", "UpsamplingElasticNetRoc", 
                               "UpsamplingElasticNetLogloss", "UpsamplingGBMRoc", "UpsamplingGBMLogloss")

    print(paste0(condition_counter, " iterations done", "       Time: ", Sys.time()))
    return(output_results)

  })

  stopCluster(clust)

  print(paste0("Condition ", condition_counter, " done",
               "        Time: ", Sys.time()))
  saveRDS(evaluation_samples, file = paste0("evaluation_data",
                                            Sys.time(), "_condition ",
                                            condition_counter, ".rda"))
  return(condition_counter)
})

