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

# Add events fractions
graph_data_bacc$prev <- c(rep(prev_levels, c(4, 4, 4, 4)))
# Add sample size
graph_data_bacc$samplesize <- c(rep(samplesize_levels, 4))

# Set factor levels for sample size and events fraction in order to keep the order
# when plotting
graph_data_bacc$prev <- factor(graph_data_bacc$prev, levels = prev_levels)
graph_data_bacc$samplesize <- factor(graph_data_bacc$samplesize, 
                                     levels = samplesize_levels)
# Reshape to long format for ggplot
graph_data_long_bacc <- gather(graph_data_bacc, key = sample, value, 
                               -samplesize, -prev)

# Separate the column names into separate columns
reshaped_data_bacc <- separate(graph_data_long_bacc, sample, 
                               into = c("statistic", "method", "noise"), 
                               sep = "\\.")

# Split upsampling information from method names, adding a new column with
# upsampling or no upsampling
reshaped_data_bacc$upsampling <- ifelse(grepl("Upsampling", reshaped_data_bacc$method), 
                                        "Upsampling", "No Upsampling")
# Remove "upsampling" from method names
reshaped_data_bacc$method <- gsub("Upsampling", "", reshaped_data_bacc$method)

# Set factor levels for method of analysis, upsampling and NV in order to 
# keep the order when plotting
reshaped_data_bacc$method <-factor(reshaped_data_bacc$method, 
                                   levels = method_levels)
reshaped_data_bacc$upsampling <-factor(reshaped_data_bacc$upsampling, 
                                       levels = upsampling_levels)
reshaped_data_bacc$noise <-factor(reshaped_data_bacc$noise, 
                                  levels = unique(reshaped_data_bacc$noise))

# split Statistic-column into multiple columns
# Resulting in separate columns for mean, lower_bound and upper_bound
wide_data_bacc <- pivot_wider(reshaped_data_bacc,
                              names_from = statistic,
                              values_from = value)

# Select only the two methods using ElasticNet
final_data_bacc <- wide_data_bacc[wide_data_bacc$method %in% c("EnetRoc",
                                                              "EnetLogloss"), ]

# Create plot
bacc_plot <- ggplot(final_data_bacc, 
                    aes(x = noise, # Use NV along x-axis
                        y = mean, # use mean values as general y-value
                        # group by both sample size and events fraction:
                        group = interaction(samplesize, prev), 
                        color = prev, # Make events fractions different colors
                        shape = samplesize)) +# make sample sizes different shapes
  # and horizontal line at Y = 1
  geom_hline(yintercept = 1, linetype = 1, color = "darkgrey", linewidth = 0.5)  +
  
  # Add errorbars with the lower and upper bounds for 95%-quantiles
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                position = position_dodge(width = 0.88),
                width = 0.75,
                linetype ="dashed", linewidth = 0.15, alpha = 0.7
  ) +
  
  # Add shapes for mean values
  geom_point(position = position_dodge(width = 0.88),
             size = 3
  ) +
  
  # Set names for axis labels
  labs(
    x = "Number of Noise Variables",
    y = "Relative BACC") + 
  

  # Split plot into multiple plots, using upsampling for top and bottom split
  # and method for left and right split
  facet_grid(upsampling ~ method, 
             scales = "free_x",
             # set label names for the facets
             labeller = labeller(method = method_labels,
                                 upsampling = upsampling_labels)
  )+
  # Set color palette
  scale_colour_brewer(name = "Events Fraction", palette = "Set2", 
                      guide = guide_legend(label.text.align=0.5))+
  # set shapes
  scale_shape_manual(name = "Sample Size", values = c(18, 17, 16, 15)) +
  
  # use plot theme set in script "Data visualization setup"
  plot_theme+
  
  # Set breaks on y-axis
  scale_y_continuous(breaks = seq(0.6,1.1,0.2))+
  # Set y-axis limits for graph
  coord_cartesian(ylim = c(0.6, 1.15)) 

# Execute plot
bacc_plot

# Set up parameters for the EMF file
emf(file = "Balanced Accuracy_full_errorbars.emf", 
    emfPlus = TRUE, 
    family = "sans", 
    coordDPI = 800, 
    width = 25, 
    height = 11.4, 
    units = "cm")
# Plot object to be saved
bacc_plot

# Close the graphics device and save the plot as an EMF file
dev.off()

#######################################
############# Graphics AUC ############
#######################################
# Choose only the AUC values for the validation samples
performance_auc <- performance_metrics_results[1, 2, , , , ]

# Adjust order of array
auc_matrix <- aperm(performance_auc, c(3, 2, 1, 4))

# Convert the array to a data frame
graph_data_auc <- as.data.frame(auc_matrix)

# Add events fractions
graph_data_auc$prev <- c(rep(prev_levels, c(4, 4, 4, 4)))
# Add sample size
graph_data_auc$samplesize <- c(rep(samplesize_levels, 4))

# Set factor levels for sample size and events fraction in order to keep the order
# when plotting
graph_data_auc$samplesize <- factor(graph_data_auc$samplesize,
                                    levels = samplesize_levels)
graph_data_auc$prev <- factor(graph_data_auc$prev, levels = prev_levels)

# Reshape to long format for ggplot
graph_data_long_auc <- gather(graph_data_auc, key = sample, value, 
                              -samplesize, -prev)


# Separate the column names into separate columns
reshaped_data_auc <- separate(graph_data_long_auc, sample, 
                              into = c("statistic", "method", "noise"), 
                              sep = "\\.")
# Split upsampling information from method names, adding a new column with
# upsampling or no upsampling
reshaped_data_auc$upsampling <- ifelse(grepl("Upsampling", reshaped_data_auc$method), 
                                       "Upsampling", "No Upsampling")
# Remove "upsampling" from method names
reshaped_data_auc$method <- gsub("Upsampling", "", reshaped_data_auc$method)

# Set factor levels for method of analysis, upsampling and NV in order to 
# keep the order when plotting
reshaped_data_auc$method <-factor(reshaped_data_auc$method, 
                                  levels = method_levels)
reshaped_data_auc$upsampling <-factor(reshaped_data_auc$upsampling, 
                                      levels = upsampling_levels)
reshaped_data_auc$noise <-factor(reshaped_data_auc$noise, 
                                 levels = unique(reshaped_data_auc$noise))

# split Statistic-column into multiple columns
# Resulting in separate columns for mean, lower_bound and upper_bound
wide_data_auc <- pivot_wider(reshaped_data_auc, 
                             names_from = statistic,
                             values_from = value)

# Select only the two methods using ElasticNet
final_data_auc <- wide_data_auc[wide_data_auc$method %in% c("EnetRoc",
                                                            "EnetLogloss"), ]


# Create plot
auc_plot <- ggplot(final_data_auc, 
                   aes(x = noise, # Use NV along x-axis
                       y = mean, # use mean values as general y-value
                       # group by both sample size and events fraction:
                       group = interaction(samplesize, prev), 
                       color = prev, # Make events fractions different colors
                       shape = samplesize)) + # make sample sizes different shapes
  # add horizontal line at Y = 1
  geom_hline(yintercept = 1, linetype = 1, color = "darkgrey", linewidth = 0.5)  +
  # Add errorbars with the lower and upper bounds for 95%-quantiles
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                
                position = position_dodge(width = 0.88),
                width = 0.75,
                linetype ="dashed", linewidth = 0.15, alpha = 0.7
  ) +
  # Add shapes for mean values to plot
  geom_point(position = position_dodge(width = 0.88), 
             size = 3) + 
  # Set names for axis labels
  labs(
    x = "Number of Noise Variables", 
    y = "Relative AUC") + 
  
  # Split plot into multiple plots, using upsampling for top and bottom split
  # and method for left and right split
  facet_grid(upsampling ~ method, 
             scales = "free_x",
             # set label names for the facets
             labeller = labeller(method = method_labels,
                                 upsampling = upsampling_labels)
  )+
  
  # Set color palette
  scale_colour_brewer(name = "Events Fraction", palette = "Set2")+
  # Set shapes
  scale_shape_manual(name = "Sample Size", values = c(18, 17, 16, 15)) +
  # use plot theme set in script "Data visualization setup"
  plot_theme +
  # set breaks on y-axis
  scale_y_continuous(breaks = seq(0.6,1.1,0.2))+
  # Set y-axis limits for graph
  coord_cartesian(ylim = c(0.6, 1.15))

# Execute plot
auc_plot

# Set up parameters for the EMF file
emf(file = "AUC ratio_total_errrorbars.emf", emfPlus = TRUE, family = "sans", 
    coordDPI = 800, width = 25, height = 11.4, units = "cm")
# Plot object to be saved
auc_plot
# Close the graphics device and save the plot as an EMF file
dev.off()


##############################################
############# Graphics True Model ############
##############################################
# Choose only the values for the data generating model
number_complete_model <- number_true_model[1,,,,]

# Adjust order of array so that conditions come first
sorted_array_model <- aperm(number_complete_model, c(3, 2, 1, 4))
graph_data_model <- as.data.frame(sorted_array_model)

# Add events fractions
graph_data_model$prev <- c(rep(prev_levels, c(4, 4, 4, 4)))
# Add samplesizes
graph_data_model$samplesize <- c(rep(samplesize_levels, 4))

# Set factor levels for sample size and events fraction in order to keep the order
# when plotting
graph_data_model$samplesize <- factor(graph_data_model$samplesize,
                                      levels = samplesize_levels)
graph_data_model$prev <- factor(graph_data_model$prev, levels = prev_levels)

# Reshape to long format for ggplot
graph_data_long_model <- gather(graph_data_model, key = sample, value, 
                                -samplesize, -prev)

# Separate the column names into separate columns
reshaped_data_model <- separate(graph_data_long_model, sample, 
                                into = c("method", "type", "noise"), sep = "\\.")
# Split upsampling information from method names, adding a new column with
# upsampling or no upsampling
reshaped_data_model$upsampling <- ifelse(grepl("Upsampling", 
                                               reshaped_data_model$method), 
                                         "Upsampling", "No Upsampling")
# Remove "upsampling" from method names
reshaped_data_model$method <- gsub("Upsampling", "", reshaped_data_model$method)

# Change all instances of "True Model Included" to "All True Parameters Included"
reshaped_data_model$type <- gsub("True Model Included", 
                                 "All True Parameters Included",
                                 reshaped_data_model$type)
# Change all instances of "True Model Only" to "Data Generating Model Recovered"
reshaped_data_model$type <- gsub("True Model Only", 
                                 "Data Generating Model Recovered",
                                 reshaped_data_model$type)

# Set factor levels for method of analysis, upsampling and NV in order to 
# keep the order when plotting
reshaped_data_model$method <-factor(reshaped_data_model$method, 
                                    levels = method_levels)
reshaped_data_model$upsampling <-factor(reshaped_data_model$upsampling, 
                                        levels = upsampling_levels)
reshaped_data_model$noise <-factor(reshaped_data_model$noise, 
                                   levels = unique(reshaped_data_model$noise))
reshaped_data_model$type <-factor(reshaped_data_model$type, 
                                  levels = c("Data Generating Model Recovered", 
                                             "All True Parameters Included"))

# Select only values for analysis with upsampling
complete_data_model <- reshaped_data_model[reshaped_data_model$upsampling 
                                           == "Upsampling", ]
# Select only values for the two methods using ElasticNet
final_data_model <- complete_data_model[complete_data_model$method %in%
                                          c("EnetRoc", "EnetLogloss"), ]

model_plot <- ggplot(final_data_model,
                     aes(x = noise, # Use NV along x-axis
                         y = value, # use value as y-value
                         shape = samplesize, # make sample sizes different shapes
                         color = prev, # make events fractions different colors
                         # group by both sample sizes and events fractions:
                         group = interaction(samplesize, prev))) +
  # and horizontal line at Y = 1
  geom_hline(yintercept = 1, linetype = 1, color = "darkgray")  +
  # and horizontal line at Y = 0
  geom_hline(yintercept =0, linetype = 1, color = "darkgray")  +
  
  # Add shapes in plot
  geom_point(position = position_dodge(width = 0.88), 
             size = 3, 
             fill = NA) + 

  # Set names for axis labels
  labs(
    x = "Number of Noise Variables",
    y = "Relative Frequency"
  ) + 
  
  # Split plot into multiple plots, using the model type analyzed for top and 
  # bottom split and method for left and right split
  facet_grid(type ~ method, 
             scales = "free_x",
             # set labels for the facets
             labeller = labeller(method = method_labels,
                                 type = label_wrap_gen(width = 20, 
                                                       multi_line = TRUE))
  )+
  
  # Set color palette
  scale_color_brewer(name = "Events Fraction", palette = "Set2") + 
  # Set shapes
  scale_shape_manual(name = "Sample Size",values = c(18, 17, 16, 15))+
  # Use plot theme set in script "Data visualization setup"
  plot_theme +
  # Set y-axis limits for plot
  coord_cartesian(ylim = c(0, 1.0))

# Plot object
model_plot

# Set up parameters for the EMF file
emf(file = "Model Evaluation_Complete_no upsampling.emf", emfPlus = TRUE, 
    family = "sans", 
    coordDPI = 800, width = 25, height = 11.1, units = "cm")
# Plot object to be saved
model_plot
# Close the graphics device and save the plot as an EMF file
dev.off()

########################################################
############# Graphics predictor frequency #############
########################################################
#
sorted_array_frequency <- aperm(predictor_frequency, c(4, 3, 2, 1, 5))

# Convert the matrix to a data frame
graph_data_frequency <- as.data.frame(sorted_array_frequency)


graph_data_frequency$prev <- c(rep(prev_levels, c(4, 4, 4, 4)))
graph_data_frequency$samplesize <- c(rep(samplesize_levels, 4))



# Set factor levels for sample size and events fraction in order to keep the order
# when plotting
graph_data_frequency$samplesize <- factor(graph_data_frequency$samplesize,
                                          levels = samplesize_levels)
graph_data_frequency$prev <- factor(graph_data_frequency$prev, 
                                    levels = prev_levels)

# Reshape to long format for ggplot
graph_data_long_frequency <- gather(graph_data_frequency, key = sample, value, 
                                    -samplesize, -prev)


# Separate the column names into separate columns
reshaped_data_frequency <- separate(graph_data_long_frequency, sample, 
                                    into = c("statistic", "method", 
                                             "weight", "noise"), 
                                    sep = "\\.")

# Split upsampling information from method names, adding a new column with
# upsampling or no upsampling
reshaped_data_frequency$upsampling <- ifelse(grepl("Upsampling", 
                                                   reshaped_data_frequency$method), 
                                             "Upsampling", "No Upsampling")
# Remove "upsampling" from method names
reshaped_data_frequency$method <- gsub("Upsampling", "", 
                                       reshaped_data_frequency$method)


# Set factor levels for method of analysis, upsampling and NV in order to 
# keep the order when plotting
reshaped_data_frequency$method <-factor(reshaped_data_frequency$method, 
                                        levels = method_levels)
reshaped_data_frequency$upsampling <-factor(reshaped_data_frequency$upsampling, 
                                            levels = upsampling_levels)
reshaped_data_frequency$noise <-factor(reshaped_data_frequency$noise, 
                                       levels = unique(reshaped_data_frequency$noise))

# split Statistic-column into multiple columns
# Resulting in separate columns for mean, lower_bound and upper_bound
wide_data_frequency <- pivot_wider(reshaped_data_frequency, 
                             names_from = statistic,
                             values_from = value)

# Select only the two methods using ElasticNet
 final_data_frequency <- wide_data_frequency[wide_data_frequency$method %in%
                                                   c("EnetRoc", "EnetLogloss"), ]

############# True Parameters ############
# Select only values for small true parameters
final_data_true <- final_data_frequency[final_data_frequency$weight 
                                        %in% c("X1", "X1:X2"),]
# Select only values without upsampling
final_data_true <-final_data_true[final_data_true$upsampling=="No Upsampling", ]


frequency_plot_true <- ggplot(final_data_true,
                              aes(x = noise, # Use NV along x-axis
                                  y = mean, # use mean values as general y-values
                                  shape = samplesize, # make sample sizes 
                                                      # different shapes
                                  color = prev, # make events sizes different colors
                                  # group by both sample size and events fraction:
                                  group = interaction(samplesize, prev))) +
  # Add horizontal line at Y = 1
  geom_hline(yintercept = 1, linetype = 1, color = "darkgray")  +
  # Add horizontal line at Y = 0
  geom_hline(yintercept =0, linetype = 1, color = "darkgray")  +
  
  # Add shapes for mean values to plot
  geom_point(position = position_dodge(width = 0.8), 
             size = 3, 
             fill = NA) + 
  # set names of axis labels
  labs(
    x = "Number of Noise Variables",
    y = "Relative Frequency"
  ) + 
  
  # Split plot into multiple plots, using regression weight for top and bottom 
  # split and method for left and right split
  facet_grid(weight ~ method, 
             scales = "free_x",
             # Set label names for facets
             labeller = labeller(weight = c("X1" = "Small Simple Effect", 
                                            "X1:X2" = "Small Interaction"),
                                 method = method_labels)
  )+
  # Set color palette
  scale_colour_brewer(name = "Events Fraction", palette = "Set2")+
  # Set shapes
  scale_shape_manual(name = "Sample Size", values = c(18, 17, 16, 15)) +
  # use plot theme set in script "Data visualization setup"
  plot_theme+
  # Set y-axis limits for graph
  coord_cartesian(ylim = c(0, 1))
  # Plot object
 frequency_plot_true
 
 # Set up parameters for the EMF file
 emf(file = "Frequency True Effects_small_total.emf", 
     emfPlus = TRUE, 
     family = "sans", 
     coordDPI = 800, 
     width = 25, 
     height = 11.4, 
     units = "cm")
 # Plot object to be saved
 frequency_plot_true
 # Close the graphics device and save the plot as an EMF file
 dev.off()
 
############# Noise Interactions ############
# Select only values for noise interactions
final_data_noise_inter <- final_data_frequency[final_data_frequency$weight 
                                               %in% c("Noise Interactions"),]
 
frequency_plot_noise_inter <- ggplot(final_data_noise_inter,
                                     aes(x = noise, # use NV along x-axis
                                         y = mean, # use mean as general y-value
                                         shape = samplesize, # make sample sizes
                                                             # different shapes
                                         color = prev, # make events fractions
                                                       # different colors
                                         # group by both sample size and 
                                         # events fraction:
                                         group = interaction(samplesize, prev))) +
  
  # and horizontal line at Y = 0
  geom_hline(yintercept = 0, linetype = 1, color = "darkgrey", linewidth = 0.5)  +
  
  # Add errorbars with the lower and upper bounds for 95%-quantiles
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                
                position = position_dodge(width = 0.88),
                width = 0.75,
                linetype ="dashed", linewidth = 0.15, alpha = 0.7
  ) +
  # Add shapes for mean values to plot
  geom_point(position = position_dodge(width = 0.88), 
             size = 3, 
             fill = NA) + 
  
  # set names for axis labels
  labs(
    x = "Number of Noise Variables",
    y = "Number of Noise Interactions"
  ) + 
  
  # Split plot into multiple plots, using upsampling for top and bottom split
  # and method for left and right split
  facet_grid(upsampling ~ method, 
             scales = "free_x",
             # Set label names for facets
             labeller = labeller(method = method_labels)
  )+
  # Set color palette
  scale_colour_brewer(name = "Events Fraction", palette = "Set2")+
  # Set shapes
  scale_shape_manual(name = "Sample Size", values = c(18, 17, 16, 15)) +
  # use plot theme set in script "Data visualization setup"
  plot_theme+
  # Set y-axis limits for graph
  coord_cartesian(ylim = c(0,110))

frequency_plot_noise_inter

# Set up parameters for the EMF file
emf(file = "Frequency Noise Interactions_total.emf", 
    emfPlus = TRUE, 
    family = "sans", 
    coordDPI = 800, 
    width = 25, 
    height = 11.4, 
    units = "cm")
# Plot object to be saved
frequency_plot_noise_inter

# Close the graphics device and save the plot as an EMF file
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


# Separate the column names into separate columns
reshaped_data_bias <- separate(graph_data_long_bias, sample, 
                               into = c("statistic", "method", "condition", "noise"), 
                               sep = "\\.")

# Split upsampling information from method names, adding a new column with
# upsampling or no upsampling
reshaped_data_bias$upsampling <- ifelse(grepl("Upsampling", reshaped_data_bias$method), 
                                        "Upsampling", "No Upsampling")
# Remove "upsampling" from method names
reshaped_data_bias$method <- gsub("Upsampling", "", reshaped_data_bias$method)

# Remove "condition" from the condition column, leaving only the number of the 
# condition
reshaped_data_bias$condition <- as.numeric(gsub("condition ", "", 
                                                reshaped_data_bias$condition))

# Add sample sizes by checking condition name
reshaped_data_bias$samplesize <- ifelse(reshaped_data_bias$condition 
                                        %in% 1:4, 100, 
                                        ifelse(reshaped_data_bias$condition 
                                               %in% 5:8, 500, 
                                               ifelse(reshaped_data_bias$condition 
                                                      %in% 9:12, 
                                                      "1,000", "5,000")))
# Add events fraction by checking condition name
reshaped_data_bias$prev <- ifelse(reshaped_data_bias$condition 
                                  %in% c(1, 5, 9, 13), ".05",
                                  ifelse(reshaped_data_bias$condition 
                                         %in% c(2, 6, 10, 14), ".10",
                                         ifelse(reshaped_data_bias$condition 
                                                %in% c(3, 7, 11, 15), ".30", ".50")))
# Remove column with condition names
reshaped_data_bias <- subset(reshaped_data_bias, select = -c(condition))

# Set factor levels for sample size and events fraction in order to keep the order
# when plotting
reshaped_data_bias$samplesize <- factor(reshaped_data_bias$samplesize, 
                                        levels = samplesize_levels)
reshaped_data_bias$prev <- factor(reshaped_data_bias$prev, levels = prev_levels)



# Set factor levels for method of analysis, upsampling and NV in order to 
# keep the order when plotting
reshaped_data_bias$method <-factor(reshaped_data_bias$method, 
                                   levels = method_levels)
reshaped_data_bias$upsampling <-factor(reshaped_data_bias$upsampling, 
                                       levels = upsampling_levels)
reshaped_data_bias$noise <-factor(reshaped_data_bias$noise, 
                                  levels = unique(reshaped_data_bias$noise))

# split Statistic-column into multiple columns
# Resulting in separate columns for mean, lower_bound and upper_bound
wide_data_bias <- pivot_wider(reshaped_data_bias,
                              names_from = statistic,
                              values_from = value)

# Select only the two methods using ElasticNet
final_data_bias <- wide_data_bias[wide_data_bias$method %in%
                                         c("EnetRoc", "EnetLogloss"), ]


############# Interactions Plot ############
# Select only values for small and large true interactions
data_interactions <-final_data_bias[final_data_bias$weight %in% 
                                      c("X1:X2","X2:X3"),]
# Select only values without upsampling
data_interactions <- data_interactions[data_interactions$upsampling == "No Upsampling",]

# Create plot
interactions_plot <- ggplot(data_interactions, 
                            aes(x = noise, # Use NV along x-axis
                                y = mean, # use mean as general y-value
                                # group by both sample size and events fraction:
                                group = interaction(samplesize, prev),
                                color = prev, # Make events fractions different 
                                              # colors
                                shape = samplesize # make sample sizes different 
                                                   # shapes
                            )) +
  # Add horizontal line at Y = 1
  geom_hline(yintercept = 0, linetype = 1, color = "darkgrey", linewidth = 0.5)  +
  # Add errorbars with the lower and upper bounds for 95%-quantiles
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                
                position = position_dodge(width = 0.88),
                width = 0.75,
                linetype ="dashed", linewidth = 0.15, alpha = 0.7
  ) +
  # Add shapes for mean values to plot
  geom_point(position = position_dodge(width = 0.88), 
             size = 3) + 
  
  # Set names for axis labels
  labs(x = "Number of Noise Variables",
       y = "Bias in %"
  ) + 
  
  
  # Split plot into multiple plots, using weight for top and bottom split
  # and method for left and right split
  facet_grid(weight ~ method,
             scales = "free_x",
             # Set label names for facets
             labeller = labeller(
                                 weight = c("X1:X2" = "Small True Weight", 
                                            "X2:X3" = "Large True Weight"))
  ) +
  # Set color palette
  scale_colour_brewer(name = "Events Fraction", palette = "Set2")+
  # Set shapes
  scale_shape_manual(name = "Sample Size", values = c(18,17,16,15)) +
  # use plot theme set in script "Data visualization setup"
  plot_theme +
  # Set y-axis limits for graph
  coord_cartesian(ylim = c(-100,40))
interactions_plot


# Set up parameters for the EMF file
emf(file = "Bias_Interactions_No Upsampling_total_errorbars.emf", 
    emfPlus = TRUE, 
    family = "sans", 
    coordDPI = 800, 
    width = 25, 
    height = 10.6, 
    units = "cm")
# Plot object to be saved
interactions_plot


# Close the graphics device and save the plot as an EMF file
dev.off()


############# Noise Plot ############
# Select only values for noise parameters
data_noise <-final_data_bias[final_data_bias$weight == c("Noise Predictors", 
                                                         "Noise Interactions"), ]

# Select only values for analysis with upsampling 
data_noise <- data_noise[data_noise$upsampling == "Upsampling",]

# Create plot
noise_plot <- ggplot(data_noise, 
                     aes(x = noise, # Use NV along x-axis
                         y = mean, # use mean values as general y-value
                         # use mean values as general y-value
                         group = interaction(samplesize, prev),
                         color = prev, # Make events fractions different colors
                         shape = samplesize  # make sample sizes different shape
                         )) +
  # and horizontal line at Y = 0
  geom_hline(yintercept = 0, linetype = 1, color = "darkgrey", linewidth = 0.5)  +
  # Add errorbars with the lower and upper bounds for 95%-quantiles
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                
                position = position_dodge(width = 0.88),
                width = 0.75,
                linetype ="dashed", linewidth = 0.15, alpha = 0.7
  ) +
  # Add shapes for mean values to plot
  geom_point(position = position_dodge(width = 0.88), 
             size = 3) + 
  # Set names for axis labels
  labs(x = "Number of Noise Variables",
       y = "Average Absolute Regression Weight"
  ) + 

  # Split plot into multiple plots, using weight for top and bottom split
  # and method for left and right split
  facet_grid(weight ~ method,
              scales = "free_x",
              labeller = labeller(method = method_labels)
  ) +
  # Set color palette
  scale_colour_brewer(name = "Events Fraction", palette = "Set2")+
  # Set shapes
  scale_shape_manual(name = "Sample Size", values = c(18,17,16,15)) +
  # use plot theme set in script "Data visualization setup"
  plot_theme +
  # Set y-axis limits for graph
  coord_cartesian(ylim = c(0,0.45))
# Execute plot
noise_plot

# Set up parameters for the EMF file
emf(file = "Average Regression Weights_Upsampling_Noise_total_errorbars.emf", 
    emfPlus = TRUE, 
    family = "sans", 
    coordDPI = 800, 
    width = 25, 
    height = 11.4, 
    units = "cm")
# Plot object to be saved
noise_plot

# Close the graphics device and save the plot as an EMF file
dev.off()
