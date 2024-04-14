#### Requires the script "Functions Sample Evaluation" ####

##### Load neccessary packages #####
library(parallel)

# set directory to folder containing necessary scripts
directory_script <- "C:/Users/Frauke/Desktop/Masterarbeit/Code"

# Load R-script containing setup for data visualization
source(paste0(directory_script, "/Functions Sample Evaluation.R"))



####### Sample evaluation #######
#load the samples
total_samples <- as.list(readRDS("samples_nnoise_30.rdata"))
condition_counter <- 0
loop_counter <- 0

condition_evaluation <- lapply(total_samples, FUN = function(generated_samples){
  gc()
  logFolder <- getwd()
  # Initiate cluster; type = "FORK" only on Linux/MacOS: contains all 
  # environment variables automatically
  clust <- makeCluster(20, 
                       type = "FORK", 
                       outfile = paste0(logFolder, "/Dateien/evaluationDataStatus", 
                                        Sys.Date(),".txt"))

  # set seed that works for parallel processing
  set.seed(342890)
  s <- .Random.seed

  clusterSetRNGStream(cl = clust, iseed = s)
  loop_counter <<- 0
  # loop to evaluate the samples with the different methods
  evaluation_samples = parLapply(clust, generated_samples, fun = function(current_sample){
    loop_counter <<- loop_counter + 1
    gc()
    # save the first sample of the list as the training sample
    train_sample <- current_sample[[1]] 
    # save the second sample of the list as the validation sample
    validation_sample <- current_sample[[2]] 
    # evaluate samples analyzed with caret without upsampling
    output_caret <- results.caret(train_sample, validation_sample, 
                                  samplingtype = NULL)

    # evaluate samples analyzed with caret with upsampling
    output_caret_upsampling <- results.caret(train_sample, validation_sample, 
                                             samplingtype = "up")
 

    # save the outputs
    output_results<-c(output_caret, 
                      output_caret_upsampling)
    # change the names for the output
    names(output_results) <- c("LogReg","ElasticNetRoc","ElasticNetLogloss",
                               "UpsamplingLogReg", "UpsamplingElasticNetRoc", 
                               "UpsamplingElasticNetLogloss")

    print(paste0(loop_counter, " iterations done", "       Time: ", Sys.time()))
    gc()
    return(output_results)

  })
  gc()
  stopCluster(clust)

  condition_counter <<- condition_counter +1
  print(paste0("Condition ", condition_counter, " done",
               "        Time: ", Sys.time()))
  saveRDS(evaluation_samples, file = paste0("evaluation_data",
                                            Sys.time(), "_condition ",
                                            condition_counter, ".rda"))
  gc()
  return(evaluation_samples)
})


