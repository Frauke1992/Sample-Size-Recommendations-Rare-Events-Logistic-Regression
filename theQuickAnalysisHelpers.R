process_model <- function(name, model, verbose = FALSE) {
  if (grepl("ElasticNet", name)) {
    if (verbose) print(paste("Processing ElasticNet model:", name))
    return(model$coefficients)
  } else if (grepl("LogReg", name)) {
    if (verbose) print(paste("Processing Logistic Regression model:", name))
    return(model$coefficients)
  } else if (grepl("GBM", name)) {
    if (verbose) print(paste("Processing GBM model:", name))
    return(model$variableImportances)
  } else {
    stop(paste("Unknown model type:", name))
  }
}

reverse_nested_list <- function(nested_list) {
  # Ensure the input is a list
  if (!is.list(nested_list)) stop("Input must be a list")
  
  # Get the dimensions of the nested list
  inner_list_length <- length(nested_list[[1]][[1]])
  
  # Reorganize the structure
  result <- vector("list", inner_list_length)
  
  for (i in seq_len(inner_list_length)) {
    result[[i]] <- lapply(nested_list, function(outer_item) {
      lapply(outer_item, function(inner_item) {
        inner_item[[i]]
      })
    })
  }
  
  return(result)
}



# Define the function to rbind with NA-padding
cbind_fill_na <- function(lst) {
  max_length <- max(sapply(lst, length))  # Find the maximum length
  do.call(cbind, lapply(lst, function(vec) {
    length(vec) <- max_length  # Extend each vector to the max length with NAs
    vec
  }))
}
