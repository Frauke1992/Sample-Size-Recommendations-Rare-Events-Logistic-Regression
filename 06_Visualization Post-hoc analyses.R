#### Requires the script "Data visualization setup" to be downloaded ####
#### Requires the script "Analysis Difference Noise True Compare" to be executed first ####
##### Load neccessary packages #####

library(str2str)
library(ggplot2)
library(tidyr)
library(devEMF)

# set directory to folder containing necessary scripts
directory_script <- "C:/Users/Frauke/Desktop/Masterarbeit/Code"

# Load R-script containing setup for data visualization
source(paste0(directory_script, "/Data visualization setup.R"))


##############################################
############# Relative Frequency ############# 
##############################################
# Adjust order of array
sorted_array_compare <- aperm(compare_sum, c(4, 3, 2, 1, 5))

# Convert the array to a data frame
graph_data_compare <- as.data.frame(sorted_array_compare)

# Add events fractions
graph_data_compare$prev <- c(rep(prev_levels, c(4, 4, 4, 4)))
# Add sample size
graph_data_compare$samplesize <- c(rep(samplesize_levels, 4))

# Set factor levels for sample size and events fraction in order to keep the order
# when plotting
graph_data_compare$samplesize <- factor(graph_data_compare$samplesize,
                                        levels = samplesize_levels)
graph_data_compare$prev <- factor(graph_data_compare$prev, 
                                  levels = prev_levels)

# Reshape to long format for ggplot
graph_data_long_compare <- gather(graph_data_compare, key = sample, value, 
                                  -samplesize, -prev)


# Separate the column names into separate columns
reshaped_data_compare <- separate(graph_data_long_compare, sample, 
                                  into = c("method", 
                                           "effectsize",
                                           "weight", 
                                           "noise"), 
                                  sep = "\\.")

# Split upsampling information from method names, adding a new column with
# upsampling or no upsampling
reshaped_data_compare$upsampling <- ifelse(grepl("Upsampling", 
                                                 reshaped_data_compare$method), 
                                           "Upsampling", "No Upsampling")
# Remove "upsampling" from method names
reshaped_data_compare$method <- gsub("Upsampling", "", 
                                     reshaped_data_compare$method)

# Set factor levels for method of analysis, upsampling and NV in order to 
# keep the order when plotting
reshaped_data_compare$method <-factor(reshaped_data_compare$method, 
                                      levels = method_levels)
reshaped_data_compare$upsampling <-factor(reshaped_data_compare$upsampling, 
                                          levels = upsampling_levels)
reshaped_data_compare$noise <-factor(reshaped_data_compare$noise, 
                                     levels = unique(reshaped_data_compare$noise))
#########################
####### Filtering #######
#########################
# save data into new object
final_data_compare <- reshaped_data_compare
# Select only values for noise interactions
final_data_compare <-final_data_compare[final_data_compare$weight
                                        == "noise_interaction",]
# select only values for small true interaction effect
final_data_compare <-final_data_compare[final_data_compare$effectsize %in%
                                          c("X1:X2"),]


##########################
########## Plot ##########
##########################

compare_plot <- ggplot(final_data_compare,
                       aes(x = noise, # Use NV along x-axis
                           y = value, # Use value as y-values
                           shape = samplesize, # make sample sizes different shapes
                           color = prev, # make events fractions different colors
                           # group by both sample size and events fraction:
                           group = interaction(samplesize, prev))) +
  # add horizontal line at Y = 1
  geom_hline(yintercept = 1, linetype = 1, color = "darkgray")  +
  # add horizontal line at Y = 0
  geom_hline(yintercept = 0, linetype = 1, color = "darkgray")  +
  
  # Add shapes to plot
  geom_point(aes(),
             position = position_dodge(width = 0.8), 
             size = 3, 
             fill = NA) + 
  
  # Set names for axis labels
  labs(
    x = "Number of Noise Variables",
    y = "Relative Frequency"
  ) + 
  
  # Split plot into multiple plots, using effectsize for top and bottom split
  # and method for left and right split
  facet_grid(effectsize ~ method, 
             scales = "free_x",
             # Set label names for facets
             labeller = labeller(method = method_labels,
                                 upsampling = upsampling_labels)
  )+
  # Set color palette
  scale_colour_brewer(name = "Events Fraction", palette = "Set2")+
  # Set shapes
  scale_shape_manual(name = "Sample Size", values = c(18, 17, 16, 15)) +
  # use plot theme set in script "Data visualization setup"
  plot_theme+
  # set y-axis limits for graph
  coord_cartesian(ylim = c(0, 1))
compare_plot

##########################
######### Saving #########
##########################

file_name <-"Relative frequency_noise interaction smaller than small interaction.emf"
# Set up parameters for the EMF file
emf(file = file_name,
    emfPlus = TRUE,
    family = "sans",
    coordDPI = 800,
    width = 25,
    height = 11.4,
    units = "cm")
# Plot object to be saved
compare_plot


# Close the graphics device and save the plot as an EMF file
dev.off()

###############################################
###### Regression Weight Size Difference ###### 
###############################################
# Adjust order of array
sorted_array_compare_diff <- aperm(compare_diff_sorted_new, c(4, 1, 3, 2, 7, 5, 6))

# Convert the array to a data frame
graph_data_compare_diff <- as.data.frame(sorted_array_compare_diff)

# Add events fractions
graph_data_compare_diff$prev <- c(rep(prev_levels, c(4, 4, 4, 4)))
# Add sample size
graph_data_compare_diff$samplesize <- c(rep(samplesize_levels, 4))



# Set factor levels for sample size and events fraction in order to keep the order
# when plotting
graph_data_compare_diff$samplesize <- factor(graph_data_compare_diff$samplesize,
                                             levels = samplesize_levels)
graph_data_compare_diff$prev <- factor(graph_data_compare_diff$prev, 
                                       levels = prev_levels)

# Reshape to long format for ggplot
graph_data_long_compare_diff <- gather(graph_data_compare_diff, key = sample, value, 
                                       -samplesize, -prev)


# Separate the column names into separate columns
reshaped_data_compare_diff <- separate(graph_data_long_compare_diff, sample, 
                                       into = c("statistic",
                                                "method", 
                                                "effectsize",
                                                "weight", 
                                                "noise",
                                                "comparison_size"), 
                                       sep = "\\.")
# Split upsampling information from method names, adding a new column with
# upsampling or no upsampling
reshaped_data_compare_diff$upsampling <- ifelse(grepl("Upsampling", 
                                                      reshaped_data_compare_diff$
                                                        method), 
                                                "Upsampling", "No Upsampling")
# Remove "upsampling" from method names
reshaped_data_compare_diff$method <- gsub("Upsampling", "", 
                                          reshaped_data_compare_diff$method)


# Set factor levels for method of analysis, upsampling and NV in order to 
# keep the order when plotting
reshaped_data_compare_diff$method <-factor(reshaped_data_compare_diff$method, 
                                           levels = method_levels)
reshaped_data_compare_diff$upsampling <-factor(reshaped_data_compare_diff$upsampling, 
                                               levels = upsampling_levels)
reshaped_data_compare_diff$noise <-factor(reshaped_data_compare_diff$noise, 
                                          levels = unique(reshaped_data_compare_diff$
                                                            noise))

# split Statistic-column into multiple columns
# Resulting in separate columns for mean, lower_bound and upper_bound
wide_data_compare_diff <- pivot_wider(reshaped_data_compare_diff,
                                      names_from = statistic,
                                      values_from = value)




#########################
####### Filtering #######
#########################
# Save data into new object
final_data_compare_diff <- wide_data_compare_diff

# Select only values for analysis with upsampling
final_data_compare_diff <- final_data_compare_diff[final_data_compare_diff$upsampling 
                                                   =="Upsampling", ]

# Select only values for "noise smaller
final_data_compare_diff <- final_data_compare_diff[final_data_compare_diff$
                                                     comparison_size
                                                   =="noise_smaller", ]
# Select only values for small true parameters
final_data_compare_diff <-final_data_compare_diff[final_data_compare_diff$
                                                    effectsize %in%
                                                    c("X1","X1:X2"),]
# Select only values for noise interactions
final_data_compare_diff <-final_data_compare_diff[final_data_compare_diff$weight 
                                                  == "noise_interaction",]


##########################
########## Plot ##########
##########################

compare_diff_plot <- ggplot(final_data_compare_diff,
                            aes(x = noise, # Use NV along x-axis
                                y = mean, # use mean values as general y-values
                                shape = prev, # make events fractions different
                                              # colors 
                                color = samplesize, # make samplesizes different
                                                    # shapes
                                # group by both sample size and events fraction:
                                group = interaction(samplesize, prev))) +
  
  # add horizontal line at Y = 1
  geom_hline(yintercept =0, linetype = 1, color = "darkgray")  +
  
  # Add errorbars with the lower and upper bounds for 95%-quantiles
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                
                position = position_dodge(width = 0.88),
                width = 0.75,
                linetype ="dashed", linewidth = 0.15, alpha = 0.7
  ) +
  
  # Add shapes for mean values to plot
  geom_point(
    position = position_dodge(width = 0.88), 
    size = 3, 
    fill = NA) + 
  
  # set names for axis labels
  labs(
    x = "Number of Noise Variables",
    y = "Difference in Regression Weight"
  ) + 
  
  # Split plot into multiple plots, using effectsize for top and bottom split
  # and method for left and right split
  facet_grid(effectsize ~ method, 
             scales = "free_x",
             # Set labels for facets
             labeller = labeller(method = method_labels,
                                 effectsize = c("X1" = "Small Simple Effect",
                                                "X1:X2" = "Small Interaction"))
  )+
  # Set color palette
  scale_colour_brewer(name = "Events Fraction", palette = "Set2")+
  # Set shapes
  scale_shape_manual(name = "Sample Size", values = c(18, 17, 16, 15)) +
  # use plot theme set in script "Data visualization setup"
  plot_theme+
  # Set y-axis limits for graph
  coord_cartesian(ylim = c(0, 1))
compare_diff_plot

##########################
######### Saving #########
##########################

file_name <-"TBF.emf"
# Set up parameters for the EMF file
emf(file = file_name,
    emfPlus = TRUE,
    family = "sans",
    coordDPI = 800,
    width = 25,
    height = 11.4,
    units = "cm")
# Plot object to be saved
compare_diff_plot_true

# Close the graphics device and save the plot as an EMF file
dev.off()


