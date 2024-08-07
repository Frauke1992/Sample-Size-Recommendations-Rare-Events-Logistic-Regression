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
load("samples_total.rdata")
loop_counter <- 0

condition_evaluation <- lapply(1:length(all_conditions), FUN = function(condition_counter){
  generated_samples <- all_conditions[[condition_counter]]
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
  evaluation_samples = parLapply(clust, generated_samples, fun = function(current_sample){
  # evaluation_samples = lapply(generated_samples, FUN = function(current_sample){
    # save the first sample of the list as the training sample
    train_sample <- current_sample$train 
    # save the second sample of the list as the validation sample
    validation_sample <- current_sample$validation 
    # evaluate samples analyzed with caret without upsampling
    output_caret <- results.caret(train_data = train_sample, validation_data = validation_sample, 
                                  samplingtype = NULL, oracle_model = current_sample$oracle_model)

    # evaluate samples analyzed with caret with upsampling
    output_caret_upsampling <- results.caret(train_data = train_sample, validation_data = validation_sample, 
                                             samplingtype = "up", oracle_model = current_sample$oracle_model)
 

    # save the outputs
    output_results<-c(output_caret, 
                      output_caret_upsampling)
    # change the names for the output
    names(output_results) <- c("LogReg","ElasticNetRoc","ElasticNetLogloss", "GBMRoc", "GBMLogloss", 
                               "UpsamplingLogReg", "UpsamplingElasticNetRoc", 
                               "UpsamplingElasticNetLogloss", "UpsamplingGBMRoc", "UpsamplingGBMLogloss")
    loop_counter <<- loop_counter + 1
    print(paste0(loop_counter, " iterations done", "       Time: ", Sys.time()))
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

