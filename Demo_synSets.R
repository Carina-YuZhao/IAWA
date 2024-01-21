library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork)
library(cowplot)
source("fc_alpha_cuts.R")
source("fc_visualisation.R")

# Define the data sets (i.e., excel sheets, from 1 to 10) to process
sheets_to_process <- c(1, 2)

#Examples
#sheets_to_process <- c(3, 4)
#sheets_to_process <- c(5, 6)
#sheets_to_process <- c(7, 8)
#sheets_to_process <- c(9, 10)

#sheets_to_process <- c(1, 7, 8)
#sheets_to_process <- c(2, 9, 10)

plots_list <- list()

# Loop through each sheet
for (sheet in sheets_to_process) {
  # Load the data from the current sheet
  data <- read_excel('Demo_sets.xlsx', sheet = sheet)
  
  # Visualisation of IV data with the result of Interval Weighted Average (equal weight).
  p1 <- IWAe(data)
  
  # Visualisation and IV data modeling using IAA
  p2 <- IAA(data)
  
  # Alpha levels (customize as needed)
  AlphaLevels <- c(0.2, 0.4, 0.6, 0.8, 1)
  #Examples
  #AlphaLevels <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  #AlphaLevels <- c(1/3, 2/3, 1)
  
  # Calculate Interval Agreement Weighted Average (IAWA)
  union_result <- IAWA(data, AlphaLevels)
  #print(union_result)
  
  # Visualise IAWA result together with the IAA
  p2 <- IAWA_visualisation(p2, union_result)
 
  
  # Adjust margins of p1 and p2
  p1 <- p1 + theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
  p2 <- p2 + theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
  
  
  # Combine p1 and p2 vertically for each sheet
  combined_plot <- p2 / p1
  combined_plot <- combined_plot + plot_layout(heights = c(1, 0.4)) # Adjust the heights as needed
  
  # Add title to the combined plot
  plot_title <- paste("Set -", sheet)
  
  combined_plot <- combined_plot + ggtitle(plot_title)+
    theme(plot.title = element_text(size = 9))
  
  # Store the plots in the list
  plots_list[[length(plots_list) + 1]] <- combined_plot
  
  
  # Optionally, save the combined plot to a file
  # ggsave(paste0("IAWA_", sheet, ".png"), combined_plot, width = 11, height = 8.5)
}

# Combine all plots into a single plot layout
final_plot <- plot_grid(plotlist = plots_list, nrow = 1)

# Display the final combined plot
final_plot
