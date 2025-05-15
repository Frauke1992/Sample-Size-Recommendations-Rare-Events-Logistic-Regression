


############################################################## 
################## Data visualization setup ################## 
############################################################## 
#### Set up theme for all plots ####
plot_theme <- theme(
  # Set font family for text elements
  text = element_text(family = "sans"),
  # Place legend at the bottom of the plot
  legend.position = "bottom",
  # Set legend title size and margin
  legend.title = element_text(size = 10.25, 
                              margin = margin(r = 15, l = 15, unit = "pt")),
  
  # Remove boxes around legend key elements
  legend.key = element_blank(),
  # Set legend box background color and border
  legend.box.background = element_rect(color = "black", linewidth = 0.5),
  # Set legend text size and margin
  legend.text = element_text(size = 9, margin = margin(r = 15, unit = "pt")),
  # Set legend box layout
  legend.box = "vertical",
  # Remove legend background
  legend.background = element_blank(),
  # Align legend text and title
  legend.text.align = 0,
  legend.title.align = 1,
  # Reduce vertical distance between legend items
  legend.spacing.y = unit(-0.2, "cm"),
  # Justify legend box
  legend.box.just = "left",
  # Set space between legend box and plot
  legend.box.spacing = unit (0.5, "cm"),
  
  # Remove background color of panels
  panel.background = element_rect(fill = NA, color = "black", linewidth = 0.5),  
  # Increase spacing between facets
  panel.spacing = unit(0.5, "lines"),  
  # Remove grid lines
  panel.grid = element_blank(),
  
  # Remove background of plot
  plot.background = element_rect(fill = NA, color = NA),
  # Add axis ticks
  axis.ticks = element_line(color = "black", linewidth = 0.5) , 
  # Set size of axis title and text
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 9, color = "black"),
  # Set position of axis titles
  axis.title.y = element_text(vjust = 2.5, size = 12, face="bold"),
  axis.title.x = element_text(vjust = -0.8, face="bold"),
  
  # Set size of facet levels
  strip.text = element_text(size = 10, angle = 0),  
  # Adjust facet label background
  strip.background = element_rect(fill = NA , color = "black", linewidth = 0.5), 
  # Place facet labels outside the plot
  strip.placement = "outside"  
)


#### Labels and levels for the simulation and evaluation factors #### 
# Set labels for method factor
method_labels <- as_labeller(c("EnetRoc" ="ElasticNet[AUC]", 
                               "EnetLogloss" ="ElasticNet[LL]"), 
                             default = label_parsed)

# Set labels for upsampling factor
upsampling_labels <- as_labeller(c("No Upsampling" = "Without Upsampling",
                                   "Upsampling" = "With Upsampling"))

# Set factor levels of events fraction
prev_levels <- c(".05", ".10", ".30", ".50")

# Set factor levels of sample size
samplesize_levels <- c("100", "500", "1,000", "5,000")

# Set factor levels of method
method_levels <- c("EnetRoc", "EnetLogloss", "LogReg")

# Set factor levels of upsampling
upsampling_levels <- c("No Upsampling", "Upsampling")