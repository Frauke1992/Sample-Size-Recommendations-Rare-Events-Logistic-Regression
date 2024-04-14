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

sorted_array_compare <- aperm(compare_sum, c(4, 3, 2, 1, 5))

# Convert the matrix to a data frame
graph_data_compare <- as.data.frame(sorted_array_compare)


graph_data_compare$prev <- c(rep(prev_levels, c(4, 4, 4, 4)))
graph_data_compare$samplesize <- c(rep(samplesize_levels, 4))



# Set factor levels for rows (names of the weights) in order to keep the order
# when plotting
graph_data_compare$samplesize <- factor(graph_data_compare$samplesize,
                                        levels = samplesize_levels)
graph_data_compare$prev <- factor(graph_data_compare$prev, 
                                  levels = prev_levels)

# Reshape to long format for ggplot
graph_data_long_compare <- gather(graph_data_compare, key = sample, value, 
                                  -samplesize, -prev)



reshaped_data_compare <- separate(graph_data_long_compare, sample, 
                                  into = c("method", 
                                           "effectsize",
                                           "weight", 
                                           "noise"), 
                                  sep = "\\.")

reshaped_data_compare$upsampling <- ifelse(grepl("Upsampling", 
                                                 reshaped_data_compare$method), 
                                           "Upsampling", "No Upsampling")
reshaped_data_compare$method <- gsub("Upsampling", "", 
                                     reshaped_data_compare$method)


# Use mutate to reorder method variable in order of original matrix
# (needed to keep the order in ggplot)
reshaped_data_compare$method <-factor(reshaped_data_compare$method, 
                                      levels = method_levels)

reshaped_data_compare$upsampling <-factor(reshaped_data_compare$upsampling, 
                                          levels = upsampling_levels)

reshaped_data_compare$noise <-factor(reshaped_data_compare$noise, 
                                     levels = unique(reshaped_data_compare$noise))
#########################
####### Filtering #######
#########################
final_data_compare <- reshaped_data_compare

final_data_compare <- final_data_compare[final_data_compare$upsampling
                                         =="No Upsampling", ]


final_data_compare <-final_data_compare[final_data_compare$weight
                                        == "noise_predictor",]

final_data_compare <-final_data_compare[final_data_compare$effectsize %in%
                                          c("X1", "X1:X2"),]


##########################
########## Plot ##########
##########################

compare_plot <- ggplot(final_data_compare,
                       aes(x = noise, 
                           y = value, 
                           shape = samplesize, 
                           group = samplesize)) +
  
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
  
  
  
  facet_grid(effectsize ~ method, 
             scales = "free_x",
             labeller = labeller(method = method_labels,
                                 upsampling = upsampling_labels)
  )+
  
  scale_colour_brewer(name = "Events Fraction", palette = "Set2")+
  scale_shape_manual(name = "Sample Size", values = c(18, 17, 16, 15)) +
  
  plot_theme+
  coord_cartesian(ylim = c(0, 1))
compare_plot

##########################
######### Saving #########
##########################

file_name <-"Relative frequency_noise interaction smaller than small interaction.emf"

emf(file = file_name,
    emfPlus = TRUE,
    family = "sans",
    coordDPI = 800,
    width = 25,
    height = 11.4,
    units = "cm")

compare_plot


# Saves the file
dev.off()

###############################################
###### Regression Weight Size Difference ###### 
###############################################

sorted_array_compare_diff <- aperm(compare_diff_sorted_new, c(4, 1, 3, 2, 7, 5, 6))


dimnames(sorted_array_compare_diff)
# Convert the matrix to a data frame
graph_data_compare_diff <- as.data.frame(sorted_array_compare_diff)


graph_data_compare_diff$prev <- c(rep(prev_levels, c(4, 4, 4, 4)))
graph_data_compare_diff$samplesize <- c(rep(samplesize_levels, 4))



# Set factor levels for rows (names of the weights) in order to keep the order
# when plotting
graph_data_compare_diff$samplesize <- factor(graph_data_compare_diff$samplesize,
                                             levels = samplesize_levels)
graph_data_compare_diff$prev <- factor(graph_data_compare_diff$prev, 
                                       levels = prev_levels)

# Reshape to long format for ggplot
graph_data_long_compare_diff <- gather(graph_data_compare_diff, key = sample, value, 
                                       -samplesize, -prev)



reshaped_data_compare_diff <- separate(graph_data_long_compare_diff, sample, 
                                       into = c("statistic",
                                                "method", 
                                                "effectsize",
                                                "weight", 
                                                "noise",
                                                "comparison_size"), 
                                       sep = "\\.")

reshaped_data_compare_diff$upsampling <- ifelse(grepl("Upsampling", 
                                                      reshaped_data_compare_diff$method), 
                                                "Upsampling", "No Upsampling")
reshaped_data_compare_diff$method <- gsub("Upsampling", "", 
                                          reshaped_data_compare_diff$method)


# Use mutate to reorder method variable in order of original matrix
# (needed to keep the order in ggplot)
reshaped_data_compare_diff$method <-factor(reshaped_data_compare_diff$method, 
                                           levels = method_levels)

reshaped_data_compare_diff$upsampling <-factor(reshaped_data_compare_diff$upsampling, 
                                               levels = upsampling_levels)

reshaped_data_compare_diff$noise <-factor(reshaped_data_compare_diff$noise, 
                                          levels = unique(reshaped_data_compare_diff$noise))
wide_data_compare_diff <- pivot_wider(reshaped_data_compare_diff,
                                      names_from = statistic,
                                      values_from = value)




#########################
####### Filtering #######
#########################
final_data_compare_diff <- wide_data_compare_diff

final_data_compare_diff <- final_data_compare_diff[final_data_compare_diff$upsampling 
                                                   =="Upsampling", ]


final_data_compare_diff <- final_data_compare_diff[final_data_compare_diff$comparison_size
                                                   =="noise_smaller", ]

final_data_compare_diff <-final_data_compare_diff[final_data_compare_diff$effectsize %in%
                                                    c("X1","X2"),]

# final_data_compare_diff <-final_data_compare_diff[final_data_compare_diff$effectsize %in%
#                                                     c("X2","X1:X3"),]


final_data_compare_diff <-final_data_compare_diff[final_data_compare_diff$weight 
                                                  == "noise_predictor",]


##########################
########## Plot ##########
##########################

compare_diff_plot <- ggplot(final_data_compare_diff,
                            aes(x = noise, 
                                y = mean, 
                                shape = prev, 
                                color = effectsize,
                                group = prev)) +
  
  
  geom_hline(yintercept =0, linetype = 1, color = "darkgray")  +
  
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                
                position = position_dodge(width = 0.88),
                width = 0.75,
                linetype ="dashed", linewidth = 0.15, alpha = 0.7
  ) +
  
  
  geom_point(
    position = position_dodge(width = 0.88), 
    size = 3, 
    fill = NA) + 
  
  labs(
    x = "Number of Noise Variables",
    y = "Difference in Regression Weight"
  ) + 
  
  
  
  facet_grid(samplesize ~ method, 
             scales = "free_x",
             labeller = labeller(method = method_labels,
                                 effectsize = c("X1" = "Small Simple Effect",
                                                "X1:X2" = "Small Interaction"))
  )+
  
  scale_colour_brewer(name = "Events Fraction", palette = "Set2")+
  scale_shape_manual(name = "Sample Size", values = c(18, 17, 16, 15)) +
  
  plot_theme+
  coord_cartesian(ylim = c(0, 1))
compare_diff_plot

##########################
######### Saving #########
##########################

file_name <-"TBF.emf"
emf(file = file_name,
    emfPlus = TRUE,
    family = "sans",
    coordDPI = 800,
    width = 25,
    height = 11.4,
    units = "cm")

compare_diff_plot_true


# Saves the file
dev.off()


