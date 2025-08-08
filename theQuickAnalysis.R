
######## Load packages ########
library(parallel)
library(ggplot2)
library(tidyr)
library(dtplyr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyverse)
library(future)
library(future.apply)
library(qs2)
source("theQuickAnalysisHelpers.R")

######## Read all data ########
# CAREFUL! The overall RAM burden of all files is about 120 GB. 
# Make sure you have enough RAM available. I have read this in and
# reduced the object size to about 30 GB to be able to continue on a normal computer.

allFiles <- list.files(path = "./", pattern = "evaluation_data.*.rda", full.names = TRUE)

condition_numbers <- sub(".*_condition (\\d+)\\.rda", "\\1", allFiles)
condition_numbers <- as.numeric(condition_numbers)
allFiles <- allFiles[order(condition_numbers)]

myCL <- parallel::makeCluster(20, type = "PSOCK")

allRes <- parallel::parLapply(myCL, allFiles, function(iFile) {
  readRDS(iFile)
})

parallel::stopCluster(myCL)

# Remove predicted probabilities to decrease RAM burden
for (i in seq_along(allRes)) {
  for (j in seq_along(allRes[[i]])) {
    for (k in seq_along(allRes[[i]][[j]])) {
      if ("predictedProbabilities" %in% names(allRes[[i]][[j]][[k]])) {
        allRes[[i]][[j]][[k]][["predictedProbabilities"]] <- NULL
      }
    }
  }
}

# Save reduced allRes to work locally 
qs2::qs_save(allRes, "allRes_reduced.qs2", nthreads = 10)

# Load allRes from file to work locally
allRes <- qs2::qs_read("allRes_reduced.qs2", nthreads = 10)

######## Extract Performance metrics ########
predictivePerformance <- 
  sapply(allRes, function(iCondition){
   tmp <- sapply(iCondition, function(iSample){
      sapply(iSample, function(x) {
        perf <- x$performanceMetrics
        if (!is.null(perf)) {
          extract_validation_metrics(perf)
        } else {
          matrix(NA, nrow=6, ncol=6,
                 dimnames = list(
                   c("auc", "misclassification", "logLoss", "balanced_accuracy", "sensitivity", "specificity"),
                   c("threshold_0.015625", "threshold_0.03125", "threshold_0.0625","threshold_0.125",
                     "threshold_0.25",  "threshold_0.5")
                 )
          )
        }
      }, simplify = "array")    
    }, simplify = "array")
   if (!is.array(tmp)) browser()
   tmp
  }, simplify = "array")

dimnames(predictivePerformance)[[2]] <- c("threshold_0.015625", "threshold_0.03125", "threshold_0.0625","threshold_0.125",
                                          "threshold_0.25",  "threshold_0.5")

# array in langen df umwandeln
df_long <- as.data.frame.table(
  predictivePerformance,
  responseName = "value"
) %>%
  lazy_dt() %>%
  rename(
    metric    = Var1,
    threshold = Var2,
    model     = Var3,
    sample    = Var4,
    condition = Var5
  ) %>%
  # # hier alle rows mit NA in value wegfiltern
  filter(!is.na(value)) %>%
  mutate(
    threshold = as.character(threshold),
    model     = as.character(model),
    sample    = as.integer(sample),
    condition = as.integer(condition)
  ) %>%
  as_tibble()


# Die relevanten Threshold-Namen
keep <- c("threshold_0.015625", "threshold_0.03125",
          "threshold_0.0625",   "threshold_0.125",
          "threshold_0.25",     "threshold_0.5")

df_wide <- df_long %>%
  lazy_dt() %>%
  filter(threshold %in% keep) %>%
  group_by(model, sample, condition, metric) %>%
  summarise(
    # 1) Wert bei 0.5
    value_0.5 = value[threshold == "threshold_0.5"][1],
    
    # 2) Wert bei nächstkleinerer Schwelle oder Duplikat
    value_prev = {
      # lokalen Kopien aller relevanten Werte anlegen:
      val05  <- value[threshold == "threshold_0.5"][1]
      others <- threshold[threshold != "threshold_0.5" & !is.na(value)]
      if (length(others) == 0) {
        # kein anderer Threshold ⇒ Duplikat von value_0.5
        val05
      } else {
        # kleinsten Namen (lexikographisch = numerisch kleinsten Wert) nehmen:
        th_min <- min(others)
        value[threshold == th_min][1]
      }
    },
    .groups = "drop"
  ) %>%
  as_tibble()


# Umbenennen
names(df_wide)[names(df_wide) == "value_0.5"]  <- "threshold_0.5"
names(df_wide)[names(df_wide) == "value_prev"] <- "threshold_prev"

# Neu sortieren
df_wide <- df_wide[ , c("metric","model","sample","condition",
                        "threshold_prev","threshold_0.5")]

# 3. Mittelwerte und NA-Anteile **pro Modell, Condition und Metrik**:
meanPerf <- df_wide %>%
  group_by(model, condition, metric) %>%
  summarise(
    mean_0.5    = mean(threshold_0.5, na.rm = TRUE),
    mean_prev   = if ("threshold_prev" %in% names(df_wide)) {
      # existiert die Spalte, normal aggregieren …
      mean(threshold_prev, na.rm = TRUE)
    } else {
      # sonst immer NA
      NA_real_
    },
    na_frac_0.5 = mean(is.na(threshold_0.5)),
    na_frac_prev = if ("threshold_prev" %in% names(df_wide)) {
      mean(is.na(threshold_prev))
    } else {
      NA_real_
    },
    .groups = "drop"
  )

# 4. Breit machen: jede Kennzahl eine Spalte (optional nach Bedarf)
meanPerf_wide <- meanPerf %>%
  pivot_wider(
    id_cols = c(model, condition),
    names_from = metric,
    values_from = c(mean_0.5, mean_prev, na_frac_0.5, na_frac_prev),
    names_glue = "{.value}_{metric}"
  )


# 4. Merge mit finalTable (angenommen, `sample` ist der Schlüssel)
condTable <- read.csv("Conditions.csv")
condTable$condition <- 1:nrow(condTable)
finalTable <- left_join(condTable, meanPerf, by = "condition")

write.csv2(finalTable, "finalTable.csv", row.names = FALSE)

# 3. Modellmapping: Falls die Modellbezeichnung in einer Spalte steht, z. B. 'model' oder 'model_type'
# Passe ggf. die Spaltennamen an!
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

finalTable_long <- finalTable %>%
  mutate(model_type_short = recode(model.y, !!!model_map)) %>%
  pivot_longer(
    cols = c(mean_0.5, mean_prev),
    names_to = "threshold",
    names_prefix = "mean_",
    values_to = "value"
  )

finalTable_long <- finalTable_long %>%
  bind_rows(
    finalTable %>%
      mutate(model_type_short = recode(model.y, !!!model_map)) %>%
      pivot_longer(
        cols = c(na_frac_0.5, na_frac_prev),
        names_to = "threshold",
        names_prefix = "na_frac_",
        values_to = "value"
      ) %>%
      mutate(metric = "trouble")
  ) %>%
  dplyr::select(-mean_0.5, -mean_prev, -na_frac_0.5, -na_frac_prev)

# 4. Plot-Schleife wie zuvor, ggf. Variablennamen anpassen
for (thresh in c("0.5","prev")) {
  for (m in unique(finalTable_long$metric)) {
    df_metric <- finalTable_long %>%
      filter(metric == m, threshold == thresh) # oder "mean_prev" für die andere Validierung
    
    df_metric$reliability <- factor(
      df_metric$reliability,
      levels = c(1, 0.8),
      labels = c("Reliability 1", "Reliability 0.8")
    )
    
    ylabel <- case_when(
      m == "trouble"              ~ "Trouble Value",
      m == "auc"                  ~ "AUC",
      m == "misclassification"    ~ "Misclassification Rate",
      m == "logLoss"              ~ "Log Loss",
      m == "balanced_accuracy"    ~ "Balanced Accuracy",
      m == "sensitivity"          ~ "Sensitivität",
      m == "specificity"          ~ "Spezifität",
      TRUE                        ~ m
    )
    
    title <- case_when(
      m == "trouble"              ~ "Troubles in Abhängigkeit von Event Fraction, sample Size, n_noise_variables und Modell",
      m == "auc"                  ~ "AUC in Abhängigkeit von Event Fraction, sample Size, n_noise_variables und Modell",
      m == "misclassification"    ~ "Misclassification in Abhängigkeit von Event Fraction, sample Size, n_noise_variables und Modell",
      m == "logLoss"              ~ "Log Loss in Abhängigkeit von Event Fraction, sample Size, n_noise_variables und Modell",
      m == "balanced_accuracy"    ~ "Balanced Accuracy in Abhängigkeit von Event Fraction, sample Size, n_noise_variables und Modell",
      m == "sensitivity"          ~ "Sensitivität in Abhängigkeit von Event Fraction, sample Size, n_noise_variables und Modell",
      m == "specificity"          ~ "Spezifität in Abhängigkeit von Event Fraction, sample Size, n_noise_variables und Modell",
      TRUE                        ~ m
    )
    
    p <- ggplot(df_metric, aes(
      x = sample_size, y = value,
      color = as.factor(target_auc),
      shape = as.factor(n_noise_variables),
      linetype = as.factor(reliability),
      group = interaction(n_noise_variables, target_frac, model_type_short, reliability, target_auc)
    )) +
      geom_line() +
      geom_point() +
      facet_grid(model_type_short ~ target_frac, scales = "fixed") +
      labs(
        x = "sample Size",
        y = ylabel,
        color = "AUC",
        shape = "Noise Variables",
        linetype = "Reliability",
        title = title
      ) +
      theme_minimal() +
      theme(
        strip.text.y = element_text(size = 8),
        plot.title = element_text(face = "bold")
      )
    
    pdf(paste0("plot_", m,"_thresh_", thresh,".pdf"), height = 11.7, width = 8.3)
    print(p)
    dev.off()
    
    
  }
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
    lapply(iMethod, function(isample)
      lapply(isample, names)
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
  sapply(iMethod, function(isample){
    mat <- do.call(cbind, lapply(isample, function(x) make_full_vec(x, all_pred_names)))
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
    mutate(condition = as.integer(factor(col_idx_chr, levels = colnames(sel_df)[1:n_conditions])))
  
  # Bedingungen zuordnen
  sel_long <- dplyr::left_join(
    sel_long, 
    finalTable_long %>%
      filter(threshold == "0.5") %>%
      select(condition, sample_size, n_noise_variables, target_auc, target_frac, reliability)%>%
      distinct(),
    by = "condition"
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
      # cat("Predictor:", pred, "vars:", paste(vars, collapse = ", "), 
      #     "in_signal:", paste(vars %in% signal_vars, collapse = ", "), "\n")
      if (all(!vars %in% signal_vars)) {
        return("Noise inter (no signal)")
      } else {
        return("Noise inter (with signal)")
      }
    }
    return(NA_character_)
  }
  
  # Jetzt direkt auf deinen Dataframe anwenden:
  plan(multisession, workers = 10)
  sel_long$effect_type <- future_vapply(sel_long$predictor, my_classify, character(1))
  plan(sequential)
  
  # Filtere raus, was nicht klassifizierbar ist:
  sel_long <- sel_long %>% filter(!is.na(effect_type))
  
  # Mittelwerte für Noise-Effekte berechnen
  summarized <- sel_long %>%
    dplyr::filter(stringr::str_detect(effect_type, "Noise")) %>%
    dplyr::group_by(effect_type, sample_size, n_noise_variables, target_auc, target_frac, reliability) %>%
    dplyr::summarise(selection_rate = mean(selection_rate, na.rm = TRUE), .groups = "drop")
  
  # Die echten Effekte aus dem Original übernehmen
  selected_signals <- sel_long %>%
    dplyr::filter(effect_type %in% true_effects)
  
  # Alles zusammenführen
  final_plotdf <- dplyr::bind_rows(
    selected_signals,
    summarized
  )
  
  final_plotdf$reliability <- factor(
    final_plotdf$reliability,
    levels = c(1, 0.8),
    labels = c("Reliability 1", "Reliability 0.8")
  )
  
  # Plot
  p <- ggplot(final_plotdf,
              aes(x = as.factor(sample_size), y = selection_rate,
                  color = effect_type,
                  shape = as.factor(n_noise_variables),
                  linetype = as.factor(target_auc),
                  group = interaction(effect_type, reliability, target_auc, n_noise_variables, reliability))) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    ylim(0, 1) +
    facet_wrap(reliability + target_auc ~ target_frac, nrow = 4) +
    labs(x = "sample Size",
         y = "Selection Rate",
         color = "Effekt",
         linetype = "AUC",
         shape = "Noise Variables",
         title = paste0("Selection Rates (", model_name, ") nach Effekt-Typ")) +
    theme_minimal()
  
  print(p)
  ggsave(
    filename = paste0("selection_rates_", model_name, "_plot.pdf"),
    plot = p, width = 11.7, height = 8.3
  )
}

########### Debugging Spielwiese ########

df_test <- df_wide %>%
  filter(
    metric    == "logLoss",
    model      == "GBMRoc"
  )

aggregate(threshold_0.5 ~ condition, data = df_test, FUN = mean, na.rm = TRUE, trim = 0.1)




