
######## Load packages ########
library(parallel)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)
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


# Mapping von Intercept zu Event-Fraction
event_fracs <- c(0.5, 0.1, 0.05, 0.01)

# Intercepts absteigend sortieren: höchster zuerst, niedrigster zuletzt
unique_int <- sort(unique(resTable$intercept), decreasing = TRUE)
mapping <- setNames(event_fracs, unique_int)

resTable <- resTable%>%
  mutate(event_fraction = factor(mapping[as.character(intercept)], levels = event_fracs))

resTable_long <- resTable %>%
  pivot_longer(
    cols = matches("^(trouble|auc|logloss|logLoss|misclassification)_", ignore.case = TRUE),
    names_to = c("metric", "model_type"),
    names_pattern = "^([a-zA-Z]+)_(.*)$",
    values_to = "value"
  )



model_map <- c(
  "LogReg" = "LogReg",
  "ElasticNetRoc" = "ENet_ROC",
  "ElasticNetLogloss" = "ENet_LogLoss",
  "GBMRoc" = "GBM_ROC",
  "GBMLogloss" = "GBM_LogLoss",
  "UpsamplingLogReg" = "LogReg_Up",
  "UpsamplingElasticNetRoc" = "ENet_Up_ROC",
  "UpsamplingElasticNetLogloss" = "ENet_Up_LogLoss",
  "UpsamplingGBMRoc" = "GBM_Up_ROC",
  "UpsamplingGBMLogloss" = "GBM_Up_LogLoss"
)

resTable_long <- resTable_long %>%
  mutate(model_type_short = recode(model_type, !!!model_map))

for (m in unique(resTable_long$metric)) {
  df_metric <- resTable_long %>% filter(metric == m)
  
  ylabel <- case_when(
    m == "trouble" ~ "Trouble Value",
    m == "auc" ~ "AUC",
    m == "misclassification" ~ "Misclassification Rate",
    m == "logLoss" ~ "Log Loss",
    TRUE ~ m
  )
  
  title <- case_when(
    m == "trouble" ~ "Troubles in Abhängigkeit von Event Fraction, Sample Size, n_noise_variables und Modell",
    m == "auc" ~ "AUC in Abhängigkeit von Event Fraction, Sample Size, n_noise_variables und Modell",
    m == "misclassification" ~ "Misclassification in Abhängigkeit von Event Fraction, Sample Size, n_noise_variables und Modell",
    m == "logLoss" ~ "Log Loss in Abhängigkeit von Event Fraction, Sample Size, n_noise_variables und Modell",
    TRUE ~ m
  )
  
  p <- ggplot(df_metric, aes(x = sample_size, y = value,
                             color = as.factor(n_noise_variables),
                             group = interaction(n_noise_variables, event_fraction, model_type_short))) +
    geom_line() +
    geom_point() +
    facet_grid(model_type_short ~ event_fraction, scales = "fixed") +
    labs(x = "Sample Size",
         y = ylabel,
         color = "Noise Variables",
         title = title) +
    theme_minimal() +
    theme(
      strip.text.y = element_text(size = 8),
      plot.title = element_text(face = "bold")
    )
  
  pdf(paste0("plot_", m, ".pdf"), width = 11.7, height = 8.3)
  print(p)
  dev.off()
}

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
  
  lapply(tmp, function(iCondition){
    lapply(iCondition, function(iSample){
      # Falls iSample NULL, gib einen leeren benannten Vektor zurück
      if (is.null(iSample)) {
        return(setNames(numeric(0), character(0)))
      }
      if (grepl("ElasticNet", iMethod)) {
        res <- iSample > 0
        names(res) <- rownames(iSample)
        return(res)
      } else if (grepl("LogReg", iMethod)) {
        iSample[,"Pr(>|z|)"] <- iSample[,"Pr(>|z|)"] < 0.05
        res <- iSample[,"Pr(>|z|)"]
        names(res) <- rownames(iSample)
        return(res)
      } else if (grepl("GBM", iMethod)) {
        res <- iSample > 1
        names(res) <- rownames(iSample)
        return(res)
      } else {
        stop(paste("Unknown model type:", iMethod))
      }
    })  # Kein simplify, damit Listenstruktur + Namen bleiben
  })
})

# Schritt 1: Globales Prädiktor-Superset über alle Methoden & Bedingungen sammeln
all_pred_names <- unique(unlist(
  lapply(selection, function(iMethod)
    lapply(iMethod, function(iCondition)
      lapply(iCondition, names)
    )
  )
))
all_pred_names <- all_pred_names[all_pred_names != "" & !is.na(all_pred_names)]

# Hilfsfunktion zur Bereinigung der Namen
clean_names <- function(x) {
  x <- gsub("`", "", x)     # Entfernt Backticks
  x <- gsub(":", ".", x)    # Vereinheitlicht Interaktions-Trenner
  return(x)
}
all_pred_names <- clean_names(all_pred_names)

# Hilfsfunktion: Bringt jeden Ergebnisvektor auf das Superset-Format mit bereinigten Namen
make_full_vec <- function(x, all_names) {
  if (!is.null(x) && length(x) > 0) {
    names(x) <- clean_names(names(x))
  }
  out <- rep(NA, length(all_names))
  names(out) <- all_names
  if (!is.null(x) && length(x) > 0) {
    out[names(x)] <- x
  }
  return(out)
}

# Schritt 2: Erzeuge für jedes Modell die vollständigen Selection-Rates-Matrizen (mit sauberen Namen)
selection_rates <- lapply(selection, function(iMethod){
  sapply(iMethod, function(iCondition){
    mat <- do.call(cbind, lapply(iCondition, function(x) make_full_vec(x, all_pred_names)))
    rownames(mat) <- all_pred_names
    apply(mat, 1, mean, na.rm = TRUE)
  }, simplify = "array")
})

names(selection_rates) <- names(allResCoef_methods)
selection_rates$LogReg
selection_rates$ElasticNetRoc
selection_rates$GBMRoc


save(selection_rates, file = "selection_rates.Rdata", compress = TRUE, compression_level = 6)

#### -------- plotting selection rates -------- ####


signal_vars <- c("X1", "X2", "X3")
signal_inters <- c("X1.X2", "X1.X3", "X2.X3")
true_effects <- c(signal_vars, signal_inters)

model_list <- list(
  LogReg = selection_rates$LogReg,
  UpsamplingLogReg = selection_rates$UpsamplingLogReg,
  ElasticNetLogloss = selection_rates$ElasticNetLogloss,
  UpsamplingElasticNetLogloss = selection_rates$UpsamplingElasticNetLogloss,
  ElasticNetRoc = selection_rates$ElasticNetRoc,
  UpsamplingElasticNetRoc = selection_rates$UpsamplingElasticNetRoc
)

for (model_name in names(model_list)) {
  mat <- model_list[[model_name]]
  n_conditions <- ncol(mat)
  sel_df <- as.data.frame(mat)
  sel_df$predictor <- rownames(sel_df)
  
  # Long-Format
  sel_long <- tidyr::pivot_longer(
    sel_df,
    cols = all_of(colnames(sel_df)[1:n_conditions]),
    names_to = "col_idx_chr",
    values_to = "selection_rate"
  ) %>%
    mutate(col_idx = as.integer(factor(col_idx_chr, levels = colnames(sel_df)[1:n_conditions])))
  
  # Bedingungen zuordnen
  resTable_aug <- resTable %>% mutate(col_idx = row_number())
  sel_long <- dplyr::left_join(
    sel_long, 
    resTable_aug %>% select(col_idx, sample_size, n_noise_variables, event_fraction),
    by = "col_idx"
  )
  
  # Prädiktor-Typ klassifizieren (Intercept korrekt behandeln)
  # Prüfe jede Zeile per Hand:
  my_classify <- function(pred) {
    # clean
    pred <- gsub("`", "", pred)
    if (pred %in% c("(Intercept)", "X.Intercept.")) return(NA_character_)
    if (pred %in% signal_vars) return(pred)
    if (pred %in% signal_inters) return(pred)
    if (grepl("^X\\d+$", pred)) return("Noise main")
    if (grepl("\\.", pred)) {
      vars <- unlist(strsplit(pred, "\\."))
      cat("Predictor:", pred, "vars:", paste(vars, collapse = ", "), 
          "in_signal:", paste(vars %in% signal_vars, collapse = ", "), "\n")
      if (all(!vars %in% signal_vars)) {
        return("Noise inter (no signal)")
      } else {
        return("Noise inter (with signal)")
      }
    }
    return(NA_character_)
  }
  
  # Jetzt direkt auf deinen Dataframe anwenden:
  sel_long$effect_type <- vapply(sel_long$predictor, my_classify, character(1))
  
  # Filtere raus, was nicht klassifizierbar ist:
  sel_long <- sel_long %>% filter(!is.na(effect_type))
  
  # Mittelwerte für Noise-Effekte berechnen
  summarized <- sel_long %>%
    dplyr::filter(stringr::str_detect(effect_type, "Noise")) %>%
    dplyr::group_by(effect_type, sample_size, n_noise_variables, event_fraction) %>%
    dplyr::summarise(selection_rate = mean(selection_rate, na.rm = TRUE), .groups = "drop")
  
  # Die echten Effekte aus dem Original übernehmen
  selected_signals <- sel_long %>%
    dplyr::filter(effect_type %in% true_effects)
  
  # Alles zusammenführen
  final_plotdf <- dplyr::bind_rows(
    selected_signals,
    summarized
  )
  
  # Plot
  p <- ggplot(final_plotdf,
              aes(x = as.factor(sample_size), y = selection_rate,
                  color = effect_type,
                  linetype = as.factor(n_noise_variables),
                  group = interaction(effect_type, n_noise_variables))) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    ylim(0, 1) +
    facet_wrap(~ event_fraction, nrow = 1) +
    labs(x = "Sample Size",
         y = "Selection Rate",
         color = "Effekt",
         linetype = "Noise Vars",
         title = paste0("Selection Rates (", model_name, ") nach Effekt-Typ")) +
    theme_minimal()
  
  print(p)
  ggsave(
    filename = paste0("selection_rates_", model_name, "_plot.pdf"),
    plot = p, width = 11.7, height = 8.3
  )
}

