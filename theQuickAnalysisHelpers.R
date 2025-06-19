extract_validation_metrics <- function(perf) {
  # 1) Feste Validierungs-Schwellen
  wanted <- c("validation_0.01", "validation_0.05",
              "validation_0.1",  "validation_0.5")
  
  # 2) Alle Spaltennamen sammeln, aber
  #    - „logloss“, „logLossCalculated“ und evtl. „logLossModel“ entfernen
  base_mets <- setdiff(colnames(perf),
                       c("logloss", "logLossCalculated", "logLossModel"))
  #    und dann „logLoss“ anfügen als einzige Log-Loss-Zeile
  mets <- c(base_mets, "logLoss")
  
  # 3) Matrix initialisieren (Zeilen=Metriken, Spalten=wanted), mit NA vorbelegt
  out <- matrix(
    NA_real_,
    nrow = length(mets),
    ncol = length(wanted),
    dimnames = list(mets, wanted)
  )
  
  # 4) Für jede vorhandene Threshold-Zeile Werte übernehmen
  present <- intersect(wanted, rownames(perf))
  for (v in present) {
    rd <- perf[v, , drop = TRUE]
    
    # a) alle „Basis“-Metriken direkt
    for (m in base_mets) {
      if (m %in% names(rd)) {
        out[m, v] <- rd[[m]]
      }
    }
    
    # b) eine einzige logLoss-Zeile
    if ("logLossCalculated" %in% names(rd)) {
      out["logLoss", v] <- rd[["logLossCalculated"]]
    } else if ("logloss" %in% names(rd)) {
      out["logLoss", v] <- rd[["logloss"]]
    }
  }
  
  out
}

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
