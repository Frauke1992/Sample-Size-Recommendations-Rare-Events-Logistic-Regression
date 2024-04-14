#### Requires the script "Data visualization setup" to be downloaded ####

##### Load neccessary packages #####

library(str2str)
library(ggplot2)
library(tidyr)
library(devEMF)

# set directory to folder containing necessary scripts
directory_script <- "C:/Users/Frauke/Desktop/Masterarbeit/Code"

# Load R-script containing setup for data visualization
source(paste0(directory_script, "/Data visualization setup.R"))

######################################################
############# Graphics balanced accuracy #############
######################################################

# Choose only the balanced accuracy values for the validation samples
balanced_accuracy_results_graph <- balanced_accuracy_result[2, , , , ]

# Adjust order of array
balanced_accuracy_results_graph <- aperm(balanced_accuracy_results_graph, 
                                         c(3, 2, 1, 4))
# Convert the array to a data frame
graph_data_bacc <- as.data.frame(balanced_accuracy_results_graph)

graph_data_bacc$prev <- c(rep(prev_levels, c(4, 4, 4, 4)))
graph_data_bacc$samplesize <- c(rep(samplesize_levels, 4))

# Set factor levels for rows (names of the weights) in order to keep the order
# when plotting
graph_data_bacc$prev <- factor(graph_data_bacc$prev, levels = prev_levels)
graph_data_bacc$samplesize <- factor(graph_data_bacc$samplesize, 
                                     levels = samplesize_levels)
# Reshape to long format for ggplot
graph_data_long_bacc <- gather(graph_data_bacc, key = sample, value, 
                               -samplesize, -prev)



reshaped_data_bacc <- separate(graph_data_long_bacc, sample, 
                               into = c("statistic", "method", "noise"), 
                               sep = "\\.")

reshaped_data_bacc$upsampling <- ifelse(grepl("Upsampling", reshaped_data_bacc$method), 
                                        "Upsampling", "No Upsampling")
reshaped_data_bacc$method <- gsub("Upsampling", "", reshaped_data_bacc$method)

# Use mutate to reorder method and upsampling variable in order of original matrix
# (needed to keep the order in ggplot)
reshaped_data_bacc$method <-factor(reshaped_data_bacc$method, 
                                   levels = method_levels)

reshaped_data_bacc$upsampling <-factor(reshaped_data_bacc$upsampling, 
                                       levels = upsampling_levels)
reshaped_data_bacc$noise <-factor(reshaped_data_bacc$noise, 
                                  levels = unique(reshaped_data_bacc$noise))

wide_data_bacc <- pivot_wider(reshaped_data_bacc,
                              names_from = statistic,
                              values_from = value)

final_data_bacc <- wide_data_bacc[wide_data_bacc$method %in% c("EnetRoc",
                                                              "EnetLogloss"), ]

# Create plot
bacc_plot <- ggplot(final_data_bacc, 
                    aes(x = noise, 
                        y = mean, 
                        group = interaction(samplesize, prev), 
                        color = prev,
                        shape = samplesize)) +
  geom_hline(yintercept = 1, linetype = 1, color = "darkgrey", linewidth = 0.5)  +
  
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                
                position = position_dodge(width = 0.88),
                width = 0.75,
                linetype ="dashed", linewidth = 0.15, alpha = 0.7
  ) +
  
  
  geom_point(position = position_dodge(width = 0.88),
             size = 3
  ) +
  
  
  labs(
    x = "Number of Noise Variables",
    y = "Relative BACC") + 
  

  
  facet_grid(upsampling ~ method, 
             scales = "free_x",
             labeller = labeller(method = method_labels,
                                 upsampling = upsampling_labels)
  )+
  scale_colour_brewer(name = "Events Fraction", palette = "Set2", 
                      guide = guide_legend(label.text.align=0.5))+
  
  scale_shape_manual(name = "Sample Size", values = c(18, 17, 16, 15)) +
  
  plot_theme+
  
  scale_y_continuous(breaks = seq(0.6,1.1,0.2))+
  coord_cartesian(ylim = c(0.6, 1.15)) 

bacc_plot

emf(file = "Balanced Accuracy_full_errorbars.emf", emfPlus = TRUE, family = "sans", 
    coordDPI = 800, width = 25, height = 11.4, units = "cm")

bacc_plot

 # Saves the file
dev.off()

#######################################
############# Graphics AUC ############
#######################################
performance_auc <- performance_metrics_results[1, 2, , , , ]
auc_matrix <- aperm(performance_auc, c(3, 2, 1, 4))

# Convert the matrix to a data frame
graph_data_auc <- as.data.frame(auc_matrix)

graph_data_auc$prev <- c(rep(prev_levels, c(4, 4, 4, 4)))
graph_data_auc$samplesize <- c(rep(samplesize_levels, 4))

# Set factor levels for rows (names of the weights) in order to keep the order
# when plotting
graph_data_auc$samplesize <- factor(graph_data_auc$samplesize,
                                    levels = samplesize_levels)
graph_data_auc$prev <- factor(graph_data_auc$prev, levels = prev_levels)

# Reshape to long format for ggplot
graph_data_long_auc <- gather(graph_data_auc, key = sample, value, 
                              -samplesize, -prev)



reshaped_data_auc <- separate(graph_data_long_auc, sample, 
                              into = c("statistic", "method", "noise"), 
                              sep = "\\.")

reshaped_data_auc$upsampling <- ifelse(grepl("Upsampling", reshaped_data_auc$method), 
                                       "Upsampling", "No Upsampling")
reshaped_data_auc$method <- gsub("Upsampling", "", reshaped_data_auc$method)

# Use mutate to reorder method variable in order of original matrix
# (needed to keep the order in ggplot)
reshaped_data_auc$method <-factor(reshaped_data_auc$method, 
                                  levels = method_levels)

reshaped_data_auc$upsampling <-factor(reshaped_data_auc$upsampling, 
                                      levels = upsampling_levels)

reshaped_data_auc$noise <-factor(reshaped_data_auc$noise, 
                                 levels = unique(reshaped_data_auc$noise))


wide_data_auc <- pivot_wider(reshaped_data_auc, 
                             names_from = statistic,
                             values_from = value)


final_data_auc <- wide_data_auc[wide_data_auc$method %in% c("EnetRoc",
                                                            "EnetLogloss"), ]


# Create plot
auc_plot <- ggplot(final_data_auc, 
                   aes(x = noise, 
                       y = mean, 
                       group = interaction(samplesize, prev), 
                       color = prev,
                       shape = samplesize)) +

  geom_hline(yintercept = 1, linetype = 1, color = "darkgrey", linewidth = 0.5)  +
  
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                
                position = position_dodge(width = 0.88),
                width = 0.75,
                linetype ="dashed", linewidth = 0.15, alpha = 0.7
  ) +
  geom_point(position = position_dodge(width = 0.88), 
             size = 3) + 
  
  labs(
    x = "Number of Noise Variables", 
    y = "Relative AUC") + 
  
  

  
  facet_grid(upsampling ~ method, 
             scales = "free_x",
             labeller = labeller(method = method_labels,
                                 upsampling = upsampling_labels)
  )+
  
  
  scale_colour_brewer(name = "Events Fraction", palette = "Set2")+
  scale_shape_manual(name = "Sample Size", values = c(18, 17, 16, 15)) +
  plot_theme +
  scale_y_continuous(breaks = seq(0.6,1.1,0.2))+
  coord_cartesian(ylim = c(0.6, 1.15))


auc_plot

emf(file = "AUC ratio_total_errrorbars.emf", emfPlus = TRUE, family = "sans", 
    coordDPI = 800, width = 25, height = 11.4, units = "cm")

auc_plot

# Saves the file
dev.off()


##############################################
############# Graphics True Model ############
##############################################
number_complete_model <- number_true_model[1,,,,]
sorted_array_model <- aperm(number_complete_model, c(3, 2, 1, 4))
graph_data_model <- as.data.frame(sorted_array_model)

graph_data_model$prev <- c(rep(prev_levels, c(4, 4, 4, 4)))
graph_data_model$samplesize <- c(rep(samplesize_levels, 4))

# Set factor levels for rows (names of the weights) in order to keep the order
# when plotting
graph_data_model$samplesize <- factor(graph_data_model$samplesize,
                                      levels = samplesize_levels)
graph_data_model$prev <- factor(graph_data_model$prev, levels = prev_levels)

# Reshape to long format for ggplot
graph_data_long_model <- gather(graph_data_model, key = sample, value, 
                                -samplesize, -prev)

reshaped_data_model <- separate(graph_data_long_model, sample, 
                                into = c("method", "type", "noise"), sep = "\\.")

reshaped_data_model$upsampling <- ifelse(grepl("Upsampling", 
                                               reshaped_data_model$method), 
                                         "Upsampling", "No Upsampling")
reshaped_data_model$method <- gsub("Upsampling", "", reshaped_data_model$method)
reshaped_data_model$type <- gsub("True Model Included", 
                                 "All True Predictors Included",
                                 reshaped_data_model$type)

reshaped_data_model$type <- gsub("True Model Only", 
                                 "Data Generating Model Recovered",
                                 reshaped_data_model$type)

# Use mutate to reorder method variable in order of original matrix
# (needed to keep the order in ggplot)
reshaped_data_model$method <-factor(reshaped_data_model$method, 
                                    levels = method_levels)

reshaped_data_model$upsampling <-factor(reshaped_data_model$upsampling, 
                                        levels = upsampling_levels)

reshaped_data_model$noise <-factor(reshaped_data_model$noise, 
                                   levels = unique(reshaped_data_model$noise))

reshaped_data_model$type <-factor(reshaped_data_model$type, 
                                  levels = c("Data Generating Model Recovered", 
                                             "All True Predictors Included"))

complete_data_model <- reshaped_data_model[reshaped_data_model$upsampling 
                                           == "Upsampling", ]

final_data_model <- complete_data_model[complete_data_model$method %in%
                                          c("EnetRoc", "EnetLogloss"), ]

model_plot <- ggplot(final_data_model,
                     aes(x = noise, 
                         y = value, 
                         shape = samplesize, 
                         color = prev,
                         group = interaction(samplesize, prev))) +
  
  geom_hline(yintercept = 1, linetype = 1, color = "darkgray")  +
  geom_hline(yintercept =0, linetype = 1, color = "darkgray")  +
  
  geom_point(position = position_dodge(width = 0.88), 
             size = 3, 
             fill = NA) + 

  labs(
    x = "Number of Noise Variables",
    y = "Relative Frequency"
  ) + 
  
 
  facet_grid(type ~ method, 
             scales = "free_x",
             labeller = labeller(method = method_labels,
                                 type = label_wrap_gen(width = 20, 
                                                       multi_line = TRUE))
  )+
  
  scale_color_brewer(name = "Events Fraction", palette = "Set2") + 
  scale_shape_manual(name = "Sample Size",values = c(18, 17, 16, 15))+
  plot_theme +
  coord_cartesian(ylim = c(0, 1.0))

model_plot
emf(file = "Model Evaluation_Complete_no upsampling.emf", emfPlus = TRUE, 
    family = "sans", 
    coordDPI = 800, width = 25, height = 10.6, units = "cm")

model_plot


# Saves the file
dev.off()

########################################################
############# Graphics predictor frequency #############
########################################################

sorted_array_frequency <- aperm(predictor_frequency, c(4, 3, 2, 1, 5))

# Convert the matrix to a data frame
graph_data_frequency <- as.data.frame(sorted_array_frequency)


graph_data_frequency$prev <- c(rep(prev_levels, c(4, 4, 4, 4)))
graph_data_frequency$samplesize <- c(rep(samplesize_levels, 4))



# Set factor levels for rows (names of the weights) in order to keep the order
# when plotting
graph_data_frequency$samplesize <- factor(graph_data_frequency$samplesize,
                                          levels = samplesize_levels)
graph_data_frequency$prev <- factor(graph_data_frequency$prev, 
                                    levels = prev_levels)

# Reshape to long format for ggplot
graph_data_long_frequency <- gather(graph_data_frequency, key = sample, value, 
                                    -samplesize, -prev)



reshaped_data_frequency <- separate(graph_data_long_frequency, sample, 
                                    into = c("statistic", "method", 
                                             "weight", "noise"), 
                                    sep = "\\.")

reshaped_data_frequency$upsampling <- ifelse(grepl("Upsampling", 
                                                   reshaped_data_frequency$method), 
                                             "Upsampling", "No Upsampling")
reshaped_data_frequency$method <- gsub("Upsampling", "", 
                                       reshaped_data_frequency$method)


# Use mutate to reorder method variable in order of original matrix
# (needed to keep the order in ggplot)
reshaped_data_frequency$method <-factor(reshaped_data_frequency$method, 
                                        levels = method_levels)

reshaped_data_frequency$upsampling <-factor(reshaped_data_frequency$upsampling, 
                                            levels = upsampling_levels)

reshaped_data_frequency$noise <-factor(reshaped_data_frequency$noise, 
                                       levels = unique(reshaped_data_frequency$noise))

wide_data_frequency <- pivot_wider(reshaped_data_frequency, 
                             names_from = statistic,
                             values_from = value)


 final_data_frequency <- wide_data_frequency[wide_data_frequency$method %in%
                                                   c("EnetRoc", "EnetLogloss"), ]

############# True Parameters ############

final_data_true <- final_data_frequency[final_data_frequency$weight 
                                        %in% c("X1", "X1:X2"),]

final_data_true <-final_data_true[final_data_true$upsampling=="No Upsampling", ]


frequency_plot_true <- ggplot(final_data_true,
                              aes(x = noise, 
                                  y = mean, 
                                  shape = samplesize, 
                                  group = interaction(samplesize, prev))) +
  
  geom_hline(yintercept = 1, linetype = 1, color = "darkgray")  +
  geom_hline(yintercept =0, linetype = 1, color = "darkgray")  +
  
  geom_point(aes(color = prev),
             position = position_dodge(width = 0.8), 
             size = 3, 
             fill = NA) + 

  labs(
    x = "Number of Noise Variables",
    y = "Relative Frequency"
  ) + 
  
  facet_grid(weight ~ method, 
             scales = "free_x",
             labeller = labeller(weight = c("X1" = "Small Simple Effect", 
                                            "X1:X2" = "Small Interaction"),
                                 method = method_labels)
  )+
  
  scale_colour_brewer(name = "Events Fraction", palette = "Set2")+
  scale_shape_manual(name = "Sample Size", values = c(18, 17, 16, 15)) +

  plot_theme+
  coord_cartesian(ylim = c(0, 1))

 frequency_plot_true
 
 emf(file = "Frequency True Effects_small_total.emf", 
     emfPlus = TRUE, 
     family = "sans", 
     coordDPI = 800, 
     width = 25, 
     height = 11.4, 
     units = "cm")
 
 frequency_plot_true
 
 
 # Saves the file
 dev.off()
 
############# Noise Interactions ############

final_data_noise_inter <- final_data_frequency[final_data_frequency$weight 
                                               %in% c("Noise Interactions"),]
frequency_plot_noise_inter <- ggplot(final_data_noise_inter,
                                     aes(x = noise, 
                                         y = mean, 
                                         shape = samplesize, 
                                         color = prev,
                                         group = interaction(samplesize, prev))) +
  
  
  geom_hline(yintercept = 0, linetype = 1, color = "darkgrey", linewidth = 0.5)  +
  
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                
                position = position_dodge(width = 0.88),
                width = 0.75,
                linetype ="dashed", linewidth = 0.15, alpha = 0.7
  ) +
  
  geom_point(position = position_dodge(width = 0.88), 
             size = 3, 
             fill = NA) + 
  

  labs(
    x = "Number of Noise Variables",
    y = "Number of Noise Interactions"
  ) + 
  
  facet_grid(upsampling ~ method, 
             scales = "free_x",
             labeller = labeller(method = method_labels)
  )+
  scale_colour_brewer(name = "Events Fraction", palette = "Set2")+
  scale_shape_manual(name = "Sample Size", values = c(18, 17, 16, 15)) +
  plot_theme+
  coord_cartesian(ylim = c(0,110))

frequency_plot_noise_inter


emf(file = "Frequency Noise Interactions_total.emf", 
    emfPlus = TRUE, 
    family = "sans", 
    coordDPI = 800, 
    width = 25, 
    height = 11.4, 
    units = "cm")

frequency_plot_noise_inter

# Saves the file
dev.off()

#########################################
############# Graphics Bias #############
#########################################
bias_array <- results_bias_total[, , , , ]
graph_data_bias <- as.data.frame(bias_array)

# Set factor levels for rows (names of the weights) in order to keep the order
# when plotting

graph_data_bias$weight <- factor(rownames(graph_data_bias),
                                 levels = rownames(graph_data_bias))
# Reshape to long format for ggplot
graph_data_long_bias <- gather(graph_data_bias, key = sample, value, -weight)



reshaped_data_bias <- separate(graph_data_long_bias, sample, 
                               into = c("statistic", "method", "condition", "noise"), 
                               sep = "\\.")

reshaped_data_bias$upsampling <- ifelse(grepl("Upsampling", reshaped_data_bias$method), 
                                        "Upsampling", "No Upsampling")
reshaped_data_bias$method <- gsub("Upsampling", "", reshaped_data_bias$method)

reshaped_data_bias$condition <- as.numeric(gsub("condition ", "", 
                                                reshaped_data_bias$condition))

reshaped_data_bias$samplesize <- ifelse(reshaped_data_bias$condition 
                                        %in% 1:4, 100, 
                                        ifelse(reshaped_data_bias$condition 
                                               %in% 5:8, 500, 
                                               ifelse(reshaped_data_bias$condition 
                                                      %in% 9:12, 
                                                      "1,000", "5,000")))
reshaped_data_bias$prev <- ifelse(reshaped_data_bias$condition 
                                  %in% c(1, 5, 9, 13), ".05",
                                  ifelse(reshaped_data_bias$condition 
                                         %in% c(2, 6, 10, 14), ".10",
                                         ifelse(reshaped_data_bias$condition 
                                                %in% c(3, 7, 11, 15), ".30", ".50")))
reshaped_data_bias <- subset(reshaped_data_bias, select = -c(condition))

reshaped_data_bias$samplesize <- factor(reshaped_data_bias$samplesize, 
                                        levels = samplesize_levels)
reshaped_data_bias$prev <- factor(reshaped_data_bias$prev, levels = prev_levels)




# Use mutate to reorder method and upsampling variable in order of original matrix
# (needed to keep the order in ggplot)
reshaped_data_bias$method <-factor(reshaped_data_bias$method, 
                                   levels = method_levels)

reshaped_data_bias$upsampling <-factor(reshaped_data_bias$upsampling, 
                                       levels = upsampling_levels)


reshaped_data_bias$noise <-factor(reshaped_data_bias$noise, 
                                  levels = unique(reshaped_data_bias$noise))


wide_data_bias <- pivot_wider(reshaped_data_bias,
                              names_from = statistic,
                              values_from = value)

final_data_bias <- wide_data_bias[wide_data_bias$method %in%
                                         c("EnetRoc", "EnetLogloss"), ]


############# Interactions Plot ############
data_interactions <-final_data_bias[final_data_bias$weight %in% 
                                      c("X1:X2","X2:X3"),]

data_interactions <- data_interactions[data_interactions$upsampling == "No Upsampling",]

# Create plot
interactions_plot <- ggplot(data_interactions, 
                            aes(x = noise, y = mean, 
                                group = interaction(samplesize, prev),
                                color = prev
                            )) +
  
  geom_hline(yintercept = 0, linetype = 1, color = "darkgrey", linewidth = 0.5)  +
  
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                
                position = position_dodge(width = 0.88),
                width = 0.75,
                linetype ="dashed", linewidth = 0.15, alpha = 0.7
  ) +
  
  geom_point(aes(shape = samplesize),
             position = position_dodge(width = 0.88), 
             size = 3) + 
  
  
  labs(x = "Number of Noise Variables",
       y = "Bias in %"
  ) + 
  
  
  facet_grid(weight ~ method,
             scales = "free_x",
             labeller = labeller(
                                 weight = c("X1:X2" = "Small True Weight", 
                                            "X2:X3" = "Large True Weight"))
  ) +
  
  scale_colour_brewer(name = "Events Fraction", palette = "Set2")+
  scale_shape_manual(name = "Sample Size", values = c(18,17,16,15)) +
  
  plot_theme +
  
  coord_cartesian(ylim = c(-100,40))
interactions_plot



emf(file = "Bias_Interactions_No Upsampling_total_errorbars.emf", 
    emfPlus = TRUE, 
    family = "sans", 
    coordDPI = 800, 
    width = 25, 
    height = 10.6, 
    units = "cm")

interactions_plot


# Saves the file
dev.off()


############# Noise Plot ############
data_noise <-final_data_bias[final_data_bias$weight == c("Noise Predictors", 
                                                         "Noise Interactions"), ]

data_noise <- data_noise[data_noise$upsampling == "Upsampling",]

# Create plot
noise_plot <- ggplot(data_noise, 
                     aes(x = noise, 
                         y = mean, 
                         group = interaction(samplesize, prev),
                         color = prev)) +
  geom_hline(yintercept = 0, linetype = 1, color = "darkgrey", linewidth = 0.5)  +
  
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                
                position = position_dodge(width = 0.88),
                width = 0.75,
                linetype ="dashed", linewidth = 0.15, alpha = 0.7
  ) +
 
  geom_point(aes(shape = samplesize), 
             position = position_dodge(width = 0.88), 
             size = 3) + 
  
  labs(x = "Number of Noise Variables",
       y = "Average Absolute Regression Weight"
  ) + 

  
  facet_grid(weight ~ method,
              scales = "free_x",
              labeller = labeller(method = method_labels)
  ) +
  
  scale_colour_brewer(name = "Events Fraction", palette = "Set2")+
  scale_shape_manual(name = "Sample Size", values = c(18,17,16,15)) +
 
  plot_theme +
  
  coord_cartesian(ylim = c(0,0.45))
noise_plot

emf(file = "Average Regression Weights_Upsampling_Noise_total_errorbars.emf", 
    emfPlus = TRUE, 
    family = "sans", 
    coordDPI = 800, 
    width = 25, 
    height = 11.4, 
    units = "cm")

noise_plot

# Saves the file
dev.off()
