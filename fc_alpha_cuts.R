#####Alpha-Cuts at select levels#####

AlphaCuts <- function(x, y, AlphaLevels) {
  AlphaCut <- function(AlphaLevel) {
    Y <- ifelse(y >= AlphaLevel, AlphaLevel, 0)
    transitions <- which(diff(c(0, Y, 0)) != 0)
    starts <- transitions[c(TRUE, FALSE)]
    ends <- transitions[c(FALSE, TRUE)] - 1
    if (length(starts) == 0 || length(ends) == 0) return(NULL)
    pairs <- mapply(c, x[starts], x[ends], SIMPLIFY = FALSE)
    pairs
  }
  
  results <- lapply(AlphaLevels, AlphaCut)
  results <- results[!sapply(results, is.null)]
  names(results) <- as.character(AlphaLevels[!sapply(results, is.null)])
  results
}


# Example usage
x <- seq(0.1, 1, length.out = 10)
y <- c(0.1, 0.2, 0.3, 0.5, 0.25, 0.05, 0.4, 0.5, 0.22, 0.12)
AlphaLevels <- c(0.2, 0.3, 0.4)
result <- AlphaCuts(x, y, AlphaLevels)
result


######calculate IWA for all combinations and listed in a table########
alpha_results_T <- function(result) {
  # Function to extract and format pairs
  format_pairs <- function(pairs) {
    lapply(pairs, function(pair) paste("(", paste(pair, collapse = ","), ")", sep = ""))
  }
  
  # Prepare the data by formatting pairs for each alpha level
  formatted_pairs <- lapply(result, format_pairs)
  
  # Check if there are any alpha levels with pairs
  if(length(formatted_pairs) == 0 || any(sapply(formatted_pairs, length) == 0)) {
    return(data.frame(ID = integer(0)))  # Return an empty dataframe if no pairs are present
  }
  
  # Generate all combinations
  combination_indices <- expand.grid(formatted_pairs)
  
  # Rename columns to reflect alpha levels
  colnames(combination_indices) <- names(result)
  
  # Compute the weighted average for each combination
  compute_weighted_average <- function(row) {
    alpha_levels <- as.numeric(names(result))
    starts <- ends <- numeric(length(alpha_levels))
    
    for (i in seq_along(alpha_levels)) {
      pair <- gsub("[()]", "", row[i])
      pair_values <- as.numeric(unlist(strsplit(pair, ",")))
      starts[i] <- pair_values[1]
      ends[i] <- pair_values[2]
    }
    
    weight_sum <- sum(alpha_levels)
    weighted_avg_start <- round(sum(starts * alpha_levels) / weight_sum, 2)
    weighted_avg_end <- round(sum(ends * alpha_levels) / weight_sum, 2)
    
    c(weighted_avg_start, weighted_avg_end)
  }
  
  # Apply the weighted average computation to each row
  weighted_averages <- t(apply(combination_indices, 1, compute_weighted_average))
  
  # Add the weighted averages to the table
  combination_table <- cbind(combination_indices, 
                             "Weighted Average" = I(apply(weighted_averages, 1, function(x) paste("(", paste(x, collapse = ","), ")", sep = ""))))
  
  # Add ID column
  combination_table$ID <- seq_len(nrow(combination_table))
  
  # Reorder columns to have ID first
  combination_table <- combination_table[, c("ID", names(combination_indices), "Weighted Average")]
  
  combination_table
}

# Example usage with your provided result
result <- AlphaCuts(x, y, AlphaLevels)
table <- alpha_results_T(result)
table


####### calculate the union of all IWA combinations ######## 
calculate_intervals_union <- function(weighted_avg_column) {
  # Check if there is only one row
  if (length(weighted_avg_column) == 1) {
    return(weighted_avg_column)
  }
  
  # Function to parse intervals from the weighted average column
  parse_interval <- function(weighted_avg) {
    # Remove parentheses and split by comma
    interval <- gsub("[()]", "", weighted_avg)
    as.numeric(unlist(strsplit(interval, ",")))
  }
  
  # Parse all intervals
  intervals <- t(sapply(weighted_avg_column, parse_interval))
  #print(intervals)
  # Function to compute the union of intervals
  union_intervals <- function(intervals) {
    # Sort intervals by start time
    sorted_intervals <- intervals[order(intervals[, 1]),]
    # Initialize the union of intervals
    union <- matrix(numeric(0), ncol = 2)
    current <- sorted_intervals[1,]
    
    for (i in 2:nrow(sorted_intervals)) {
      if (sorted_intervals[i, 1] <= current[2]) {
        # Extend the current interval if overlapping
        current[2] <- max(current[2], sorted_intervals[i, 2])
      } else {
        # Add the current interval to the union and start a new one
        union <- rbind(union, current)
        current <- sorted_intervals[i,]
      }
    }
    # Add the last interval
    rbind(union, current)
  }
  
  # Compute the union of intervals
  union_result <- union_intervals(intervals)
  
  # Format the result
  apply(union_result, 1, function(interval) paste("(", paste(interval, collapse = ","), ")", sep = ""))
}

# Example usage
# Assuming 'table' is the result table from the previous function
weighted_avg_column <- table$`Weighted Average`
union_result <- calculate_intervals_union(weighted_avg_column)
union_result


########Interval Agreement Weighted Average (IAWA)##########
# model interval-valued data to Fuzzy Set (FS) by IAA, and transform FS to interval by alpha-cut and IWA. 
IAWA <- function(data, AlphaLevels) {
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
  
  # Filter AlphaLevels to include only values <= max(y_values)
  AlphaLevels <- AlphaLevels[AlphaLevels <= max(y_values)]
  
  # Run AlphaCuts
  result <- AlphaCuts(x_values, y_values, AlphaLevels)
  
  # Generate the combination table
  table <- alpha_results_T(result)
  
  # In case the result table is empty or doesn't have the 'Weighted Average' column
  if (nrow(table) == 0 || !"Weighted Average" %in% colnames(table)) {
    return("No valid combinations found.")
  }
  
  # Extract the 'Weighted Average' column
  weighted_avg_column <- table$`Weighted Average`
  
  # Calculate the union of intervals
  union_result <- calculate_intervals_union(weighted_avg_column)
  
  # Return the union result
  union_result
}





