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
source("03_Analysis_Helpers.R")

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
  filter(!is.na(value) & metric != "logLoss" & metric != "misclassification") %>%   # NAs & logLoss entfernen
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
    threshold_0.5 = value[threshold == "threshold_0.5"][1],
    
    # 2) Wert bei nächstkleinerer Schwelle oder Duplikat
    threshold_prev = {
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



# Neu sortieren
df_wide <- df_wide[ , c("metric","model","sample","condition",
                        "threshold_prev","threshold_0.5")]

# 3. Mittelwerte und NA-Anteile **pro Modell, Condition und Metrik**:
meanPerf <- df_wide %>%
  group_by(model, condition, metric) %>%
  summarise(
    mean_0.5    = mean(threshold_0.5, na.rm = TRUE),
    mean_prev   = mean(threshold_prev, na.rm = TRUE))


# 4. Breit machen: jede Kennzahl eine Spalte (optional nach Bedarf)
meanPerf_wide <- meanPerf %>%
  pivot_wider(
    id_cols = c(model, condition),
    names_from = metric,
    values_from = c(mean_0.5, mean_prev),
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

# focus on models of interest with logloss calibration (more common)
finalTable_long <- finalTable_long[!grepl("ROC", finalTable_long$model.y, ignore.case = TRUE), ]

# separate model and upsampling information
finalTable_long <- finalTable_long %>%
  mutate(
    upsampling = ifelse(str_detect(model.y, "(?i)upsampling"), "upsampled", "none"),
    model = str_remove_all(model.y, "(?i)upsampling|logloss|roc") %>%
      str_trim()
  ) %>%  # create a new combined variable for model variatiom
  mutate(
    model_variation = paste0("thresh_", threshold, "_", upsampling)
  ) %>%
  filter(!(threshold == "prev" & upsampling != "none"))


# 4. Plot-Schleife wie zuvor, ggf. Variablennamen anpassen
for (m in unique(finalTable_long$metric)) {
  for (auc_level in c(0.7, 0.9)) {
    
    df_metric <- finalTable_long %>%
      # filter out noise variables -> focus on 15 noise variables 
      filter(n_noise_variables == 15) %>%
      filter(metric == m, target_auc == auc_level)
    
    df_metric$reliability <- factor(
      df_metric$reliability,
      levels = c(1, 0.8),
      labels = c("Reliability 1", "Reliability 0.8")
    )
    
    ylabel <- case_when(
      m == "auc"                  ~ "AUC",
      m == "balanced_accuracy"    ~ "Balanced Accuracy",
      m == "sensitivity"          ~ "Sensitivität",
      m == "specificity"          ~ "Spezifität",
      TRUE                        ~ m
    )
    
    title <- case_when(
      m == "auc"                  ~ "AUC in Abhängigkeit von Event Fraction, Sample Size und Modell",
      m == "balanced_accuracy"    ~ "Balanced Accuracy in Abhängigkeit von Event Fraction, Sample Size und Modell",
      m == "sensitivity"          ~ "Sensitivität in Abhängigkeit von Event Fraction, Sample Size und Modell",
      m == "specificity"          ~ "Spezifität in Abhängigkeit von Event Fraction, Sample Size und Modell",
      TRUE                        ~ m
    )
    
    p <- ggplot(df_metric, aes(
      x = sample_size, y = value,
      color = reliability,
      shape = as.factor(threshold),
      linetype = as.factor(model_variation),
      group = interaction(target_frac, model, model_variation, reliability)
    )) +
      geom_line() +
      geom_point() +
      facet_grid(model ~ target_frac, scales = "fixed") +
      labs(
        x = "Sample Size",
        y = ylabel,
        color = "Reliability",
        shape = "Threshold",
        linetype = "Analysis Variation",
        title = paste0(title, "\n(AUC Target = ", auc_level, ")")
      ) +
      scale_color_manual(values = c("Reliability 1" = "black", "Reliability 0.8" = "grey50")) +
      scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
      theme_minimal() +
      theme(
        strip.text.y = element_text(size = 8),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    pdf(paste0("plot_", m, "_auc", auc_level * 100, ".pdf"), height = 10, width = 10)
    print(p)
    dev.off()
  }
}

######## The Plot of Kristin's dreams ########

finalTable_long$target_nEvent <- finalTable_long$target_frac*finalTable_long$sample_size

tmp_tab <- finalTable_long %>%
  filter(metric == "balanced_accuracy", 
         target_auc == 0.7, 
         reliability == 1, 
         n_noise_variables == 15,
         model == "ElasticNet", 
         threshold == 0.5,
         upsampling == "none") 


p <- ggplot(tmp_tab, aes(
  x = target_nEvent,
  y = value,
  group = as.factor(sample_size),
  color = as.factor(sample_size)
)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Anzahl Events",
    y = "Balanced Accuracy",
    color = "Sample Size",
    title = "BACC in Abhängigkeit von Anzahl Events (nach Event Fraction gruppiert)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave("plot_event_count_thresh_prev.pdf", plot = p, width = 8, height = 6)
############ Analysis of Selected Predictors ############

# extract selected predictors

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

# Rearrange levels of nested list such that "Method" is the outermost loop
allResCoef_methods <- reverse_nested_list(allResCoef)
names(allResCoef_methods) <- names(allRes[[1]][[1]])


# Compute the selection of predictors based on the coefficients
# for each method
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

# --- Signal-Benennung ---------------------------------------------------------
main_map  <- c("X1" = "Signal main (small)",
               "X2" = "Signal main (medium)",
               "X3" = "Signal main (large)")
inter_map <- c("X1.X2" = "Signal inter (small)",
               "X1.X3" = "Signal inter (medium)",
               "X2.X3" = "Signal inter (large)")

SIGNAL_LEVELS <- c(unname(main_map), unname(inter_map))
NOISE_LEVELS  <- c("Noise main",
                   "Noise inter (with signal)",
                   "Noise inter (no signal)")
EFFECT_LEVELS <- c(SIGNAL_LEVELS, NOISE_LEVELS)

# --- Farbpaletten -------------------------------------------------------------
# ursprüngliche Palette
COLOR_MAP_DEFAULT <- c(
  "Signal main (small)"   = "#fca5a5",
  "Signal main (medium)"  = "#ef4444",
  "Signal main (large)"   = "#991b1b",
  "Signal inter (small)"  = "#93c5fd",
  "Signal inter (medium)" = "#3b82f6",
  "Signal inter (large)"  = "#1e3a8a",
  "Noise main"                    = "#9ca3af",
  "Noise inter (with signal)"     = "#6b7280",
  "Noise inter (no signal)"       = "#374151"
)

# farbenblinde Alternative (Okabe–Ito inspiriert)
COLOR_MAP_CB <- c(
  "Signal main (small)"   = "#F4C2A8",  # helles Vermilion
  "Signal main (medium)"  = "#D55E00",  # Vermilion
  "Signal main (large)"   = "#7F3300",  # dunkles Vermilion
  "Signal inter (small)"  = "#A7D3F0",  # helles Blau
  "Signal inter (medium)" = "#0072B2",  # Blau
  "Signal inter (large)"  = "#003F5C",  # dunkles Blau
  "Noise main"                    = "#B9B9B9",
  "Noise inter (with signal)"     = "#8F8F8F",
  "Noise inter (no signal)"       = "#595959"
)

# Umschalter
USE_COLORBLIND_PALETTE <- TRUE
COLOR_MAP_ACTIVE <- if (USE_COLORBLIND_PALETTE) COLOR_MAP_CB else COLOR_MAP_DEFAULT

# --- Modelle ------------------------------------------------------------------
model_list <- list(
  LogReg = selection_rates$LogReg,
  UpsamplingLogReg = selection_rates$UpsamplingLogReg,
  ElasticNetLogloss = selection_rates$ElasticNetLogloss,
  UpsamplingElasticNetLogloss = selection_rates$UpsamplingElasticNetLogloss
)

# --- Klassifikation (Skalar) --------------------------------------------------
my_classify <- function(pred) {
  pred <- gsub("`", "", pred)
  if (pred %in% c("(Intercept)", "X.Intercept.")) return(NA_character_)
  if (pred %in% names(main_map))  return(unname(main_map[pred]))
  pred_dot <- gsub(":", ".", pred)
  if (pred_dot %in% names(inter_map)) return(unname(inter_map[pred_dot]))
  if (grepl("^X\\d+$", pred)) return("Noise main")
  if (grepl("[.:]", pred)) {
    vars <- unlist(strsplit(pred_dot, "\\."))
    if (all(!vars %in% names(main_map))) "Noise inter (no signal)" else "Noise inter (with signal)"
  } else {
    NA_character_
  }
}

# --- Modellfamilie & Sampling -------------------------------------------------
parse_model <- function(model_name) {
  family <- dplyr::case_when(
    grepl("LogReg", model_name)     ~ "LogReg",
    grepl("ElasticNet", model_name) ~ "ElasticNet",
    TRUE ~ model_name
  )
  sampling <- ifelse(grepl("^Upsampling", model_name), "Upsampling", "Baseline")
  list(family = family, sampling = sampling)
}

# --- Verarbeitung eines Modells ----------------------------------------------
process_model <- function(mat, model_name) {
  n_conditions <- ncol(mat)
  sel_df <- as.data.frame(mat)
  sel_df$predictor <- rownames(sel_df)
  
  sel_long <- tidyr::pivot_longer(
    sel_df,
    cols = all_of(colnames(sel_df)[1:n_conditions]),
    names_to = "col_idx_chr",
    values_to = "selection_rate"
  ) %>%
    mutate(condition = match(col_idx_chr, colnames(sel_df)[1:n_conditions]))
  
  meta <- finalTable_long %>%
    dplyr::filter(threshold %in% c("0.5", 0.5)) %>%
    dplyr::select(condition, sample_size, n_noise_variables,
                  target_auc, target_frac, reliability) %>%
    dplyr::distinct()
  
  sel_long <- dplyr::left_join(sel_long, meta, by = "condition")
  
  sel_long$effect_type <- vapply(sel_long$predictor, my_classify, character(1))
  sel_long <- dplyr::filter(sel_long, !is.na(effect_type))
  
  info <- parse_model(model_name)
  sel_long$model_family <- info$family
  sel_long$sampling     <- info$sampling
  
  sel_long
}

# --- Alle Modelle sequenziell verarbeiten ------------------------------------
all_long <- do.call(dplyr::bind_rows, lapply(
  names(model_list),
  function(mn) process_model(model_list[[mn]], mn)
))

# Faktoren
all_long$reliability  <- factor(all_long$reliability,
                                levels = c(1, 0.8),
                                labels = c("Reliability 1", "Reliability 0.8"))
all_long$model_family <- factor(all_long$model_family, levels = c("LogReg", "ElasticNet"))
all_long$sampling     <- factor(all_long$sampling, levels = c("Baseline", "Upsampling"))

# Noise mitteln, Signale beibehalten
noise_summary <- all_long %>%
  dplyr::filter(stringr::str_detect(effect_type, "^Noise")) %>%
  dplyr::group_by(effect_type, sample_size, n_noise_variables, target_auc, target_frac,
                  reliability, model_family, sampling) %>%
  dplyr::summarise(selection_rate = mean(selection_rate, na.rm = TRUE), .groups = "drop")

signal_keep <- all_long %>%
  dplyr::filter(effect_type %in% SIGNAL_LEVELS) %>%
  dplyr::select(effect_type, sample_size, n_noise_variables, target_auc, target_frac,
                reliability, model_family, sampling, selection_rate)

final_all <- dplyr::bind_rows(signal_keep, noise_summary)
final_all$effect_type  <- factor(final_all$effect_type, levels = EFFECT_LEVELS)
final_all$effect_group <- factor(ifelse(grepl("^Signal", final_all$effect_type), "Signal", "Noise"),
                                 levels = c("Signal", "Noise"))

# --- Plot-Funktion ------------------------------------------------------------
# Zeilen = Sampling + Reliability; Linetype: Signal = solid, Noise = dotted
# Getrennte Grafiken pro Familie; vier Kombinationen (AUC 0.7/0.9 × Noise 5/15)
make_plot <- function(df_sub, auc_val, nnoise_val, family_label) {
  ggplot(df_sub,
         aes(x = as.factor(sample_size), y = selection_rate,
             color = effect_type,
             linetype = effect_group,
             shape = effect_group,
             group = interaction(effect_type, effect_group, reliability, target_frac))) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = COLOR_MAP_ACTIVE,
                       breaks = EFFECT_LEVELS, drop = FALSE) +
    scale_linetype_manual(values = c("Signal" = "solid", "Noise" = "dotted")) +
    facet_grid(reliability +sampling ~ target_frac) +
    labs(x = "Sample Size",
         y = "Selection Rate",
         color = "Effect",
         linetype = "Type",
         title = paste0("Selection Rates — ", family_label,
                        " | AUC = ", auc_val,
                        " | Noise Variables = ", nnoise_val)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
}

# --- Schleifen: getrennte Grafiken für LogReg und ElasticNet ------------------
for (family in c("LogReg", "ElasticNet")) {
  for (auc_val in c(0.7, 0.9)) {
    for (nnoise_val in c(5, 15)) {
      df_sub <- final_all %>%
        dplyr::filter(model_family == family,
                      target_auc == auc_val,
                      n_noise_variables == nnoise_val)
      
      p <- make_plot(df_sub, auc_val, nnoise_val, family)
      print(p)
      ggsave(
        filename = sprintf("selection_rates_%s_auc%.1f_noise%d_%s.pdf",
                           tolower(family), auc_val, nnoise_val,
                           if (USE_COLORBLIND_PALETTE) "cb" else "default"),
        plot = p, width = 11.7, height = 8.3
      )
    }
  }
}
