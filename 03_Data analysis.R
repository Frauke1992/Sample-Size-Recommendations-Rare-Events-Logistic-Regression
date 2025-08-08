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
  ) %>% # filter out noise variables -> focus on 15 noise variables 
  filter(n_noise_variables == 15) %>% # create a new combined variable for model variatiom
  mutate(
    model_variation = paste0("thresh_", threshold, "_", upsampling)
  ) %>%
  filter(!(threshold == "prev" & upsampling != "none"))


# 4. Plot-Schleife wie zuvor, ggf. Variablennamen anpassen
for (m in unique(finalTable_long$metric)) {
  for (auc_level in c(0.7, 0.9)) {
    
    df_metric <- finalTable_long %>%
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
        shape = "Noise Variables",
        linetype = "Reliability",
        title = paste0(title, "\n(AUC Target = ", auc_level, ")")
      ) +
      scale_color_manual(values = c("Reliability 1" = "black", "Reliability 0.8" = "grey50")) +
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


ggplot(tmp_tab, aes(
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


############ Analysis of Selected Predictors ############