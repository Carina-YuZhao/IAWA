#####IV data visualisation########
####This also produce result for Interval (equal) Weighted Average (IWAe)

IWAe <- function(data){
  # Calculate mean of Min, Max (i.e., IWAe) of IV data
  mean_min <- mean(data$Min)
  mean_max <- mean(data$Max)
  mean_min <- round(mean_min, 2)
  mean_max <- round(mean_max, 2)
  IvAverage <- paste("IWAe: [", mean_min, ", ", mean_max, "]", sep = "")
  #print(union_result_text)
  p1 <- ggplot(data, aes(x = Min, y = 1:nrow(data), xend = Max, yend = 1:nrow(data))) +
    geom_segment(color = 'darkblue', alpha = 1) +
    geom_segment(aes(x = mean_min, y = 0, xend = mean_max, yend = 0), color = "red", size = 1) +
    labs(title = '', x = 'rating (x)', y = 'response') +
    scale_x_continuous(limits = c(0, 20))+ # Domain (0,20)
    scale_y_continuous(breaks = 0:nrow(data), minor_breaks = NULL) + # Set y-axis breaks to integers and remove non-integer grid lines
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = rel(0.9)),  # Smaller x-axis title
      axis.title.y = element_text(size = rel(0.9)),  # Smaller y-axis title
      axis.text.x = element_text(size = rel(0.9)),   # Smaller x-axis tick labels
      axis.text.y = element_text(size = rel(0.9))    # Smaller y-axis tick labels
    )
  
  p1 <- p1 + annotate("text", x = 0.1, y = 0.3, label = IvAverage, 
                      hjust = 0.1, vjust = -0.3, size = 2.8, colour = "black")

  p1
}
  
#####IAA######
  
IAA <- function(data){
  # Define the step size for the calculation
  step <- 0.01
  
  # Calculate the minimum and maximum values for the responses
  min_value <- min(data$Min)
  max_value <- max(data$Max)
  
  # Define the boundaries
  rating <- seq(from = min_value-0.01, to = max_value+0.01, by = step)
  
  # Initialize an empty vector to store the agreement values
  agreements <- numeric(length(rating))
  
  # Calculate the agreement for each data point
  for (i in 1:(length(rating))) {
    count <- sum((data$Min <= rating[i] & 
                    data$Max >= rating[i]))
    agreements[i] <- count
  }
  
  # Calculate x and y values
  x_values <- rating
  y_values <- agreements / length(data[[1]])
  
  
  # Find the maximum agreement
  max_agreement <- max(agreements)
  
  # Find the rating with maximum agreement
  max_agreement_rating <- rating[agreements == max_agreement]
  
  # Calculate the mean of maximum agreement rating
  mean_max_agreement <- mean(max_agreement_rating)
  
  # Calculate the centroid
  centroid <- sum(rating * agreements) / sum(agreements)
  
  # Create a dataframe for plotting
  plot_data <- data.frame(x = x_values, y = y_values)
  
  # Create the line chart
  p2 <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line(color = "darkblue") +  # Line connecting the points
    geom_area(fill = "lightblue", alpha = 0.5) +
    labs(title = '', x = '', y = 'agreement (μ(x))') +
    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(breaks = seq(0.2, 1, by = 0.2)) + 
    scale_x_continuous(limits = c(0, 20)) +
    #geom_vline(aes(xintercept = mean_max_agreement), color = 'purple', linetype = 'dashed', size = 0.1) +
    geom_vline(aes(xintercept = centroid), color = 'brown', linetype = 'dashed', size = 0.1) +
    geom_hline(yintercept = 0.2, color = "black", linetype = "dashed", size = 0.4) +
    geom_hline(yintercept = 0.4, color = "black", linetype = "dashed", size = 0.4) +
    geom_hline(yintercept = 0.6, color = "black", linetype = "dashed", size = 0.4) +
    geom_hline(yintercept = 0.8, color = "black", linetype = "dashed", size = 0.4) +
    geom_hline(yintercept = 1.0, color = "black", linetype = "dashed", size = 0.4) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = rel(0.9)),  # Smaller x-axis title
      axis.title.y = element_text(size = rel(0.9)),  # Smaller y-axis title
      axis.text.x = element_text(size = rel(0.9)),   # Smaller x-axis tick labels
      axis.text.y = element_text(size = rel(0.9))    # Smaller y-axis tick labels
    )
  
  p2
}

#######IAWA visualisation#######

IAWA_visualisation <- function(p, union_result) {
  # Parse the intervals and create a data frame
  intervals_df <- do.call(rbind, lapply(union_result, function(interval_str) {
    nums <- as.numeric(unlist(strsplit(gsub("[()]", "", interval_str), ",")))
    data.frame(start = nums[1], end = nums[2], y = 0)
  }))
  # Add the intervals as horizontal lines on the plot
  p <- p + geom_segment(data = intervals_df, aes(x = start, y = y, xend = end, yend = y),
                        color = "red", size = 1)
  
  # Add label
  union_result_text <- paste("IAWA:", toString(union_result))
  union_result_text <- gsub("\\(", "[", union_result_text)
  union_result_text <- gsub("\\)", "]", union_result_text)
  # Add union_result as text to the p2 plot
  p <- p + annotate("text", x = 0, y = 0.05, label = union_result_text, 
                      hjust = 0.05, vjust = -0.1, size = 2.8, colour = "black")
  
  p
}

# Example usage
# Assuming 'p' is the existing ggplot object and 'union_result' is from the IAWA function
#p_updated <- IAWA_visualisation(p, union_result)
#p_updated


MaxAgreement<-function(data){
  # Define the step size for the interval calculation
  step <- 0.01
  
  # Calculate the minimum and maximum values for the responses
  min_value <- min(data$Min)
  max_value <- max(data$Max)
  
  # Define the boundaries
  rating <- seq(from = min_value-0.01, to = max_value+0.01, by = step)
  
  
  # Initialize an empty vector to store the agreement values
  agreements <- numeric(length(rating))
  
  
  # Calculate the agreement for each data point
  for (i in 1:(length(rating))) {
    count <- sum((data$Min <= rating[i] & 
                    data$Max >= rating[i]))
    agreements[i] <- count
  }
  
  # Calculate x and y values
  x_values <- rating
  y_values <- agreements / length(data[[1]])
  max(y_values)
}


