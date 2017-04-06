
runTopsis = function(df) {
  
  # Some hard coded values for a specific data set
  col_price = 3
  col_non_factor_begin = col_price
  col_mtr = 13
  
  # Each function is a step of Topsis process
  
  norm_df = getNormalisedData(df, col_price, 
                              col_non_factor_begin, col_mtr)
  
  weighted_norm_df = getWeightedData(norm_df, col_non_factor_begin)
  
  max_critical_values = getMaxCriticalValues(weighted_norm_df, col_non_factor_begin)
  min_critical_values = getMinCriticalValues(weighted_norm_df, col_non_factor_begin)
  
  pos_ideal_solutions = getPositiveIdealSols(weighted_norm_df, 
                                             max_critical_values, col_non_factor_begin)
  neg_ideal_solutions = getNegativeIdealSols(weighted_norm_df, 
                                             min_critical_values, col_non_factor_begin)
  
  rel_closeness = getRelativeCloseness(pos_ideal_solutions, neg_ideal_solutions)
  
  print(rel_closeness)
  
}


getRelativeCloseness = function(pos_ideal_solutions, neg_ideal_solutions) {
  rel_closeness = c()
  for (i in 1:length(pos_ideal_solutions)) {
    val = (neg_ideal_solutions[i] / (neg_ideal_solutions[i] + pos_ideal_solutions[i]))
    rel_closeness = c(rel_closeness, val)
  }
  return(rel_closeness)
}


getNegativeIdealSols = function(df, min_critical_values, col_non_factor_begin) {
  
  solutions = c()
  
  for (row in 1:nrow(df)) {
    square_sum = 0
    for (col in col_non_factor_begin:ncol(df)) {
      val = (min_critical_values[col - 2] - df[row, col]) ^ 2
      square_sum = square_sum + val
    }
    solutions = c(solutions, square_sum ^ (1/2))
  }
  
  return(solutions)
  
}


getPositiveIdealSols = function(df, max_critical_values, col_non_factor_begin) {
  
  solutions = c()
  
  for (row in 1:nrow(df)) {
    square_sum = 0
    for (col in col_non_factor_begin:ncol(df)) {
      val = (max_critical_values[col - 2] - df[row, col]) ^ 2
      square_sum = square_sum + val
    }
    solutions = c(solutions, square_sum ^ (1/2))
  }
  
  return(solutions)
  
}


getMaxCriticalValues = function(df, col_non_factor_begin) {
  
  max_values = c()
  
  for (col in col_non_factor_begin:ncol(df)) {
    max_value = max(df[[col]])
    max_values = c(max_values, max_value)
  }
  
  return(max_values)
  
}


getMinCriticalValues = function(df, col_non_factor_begin) {
  
  min_values = c()
  
  for (col in col_non_factor_begin:ncol(df)) {
    min_value = min(df[[col]])
    min_values = c(min_values, min_value)
  }
  
  return(min_values)
  
}


getWeightedData = function(df, col_non_factor_begin) {

  # Taking all weights = 0.1
  # Can be changed according to need
  weights = c(0, 0, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
              0.1, 0.1, 0.1, 0.1, 0.1, 0.1)

  for (i in col_non_factor_begin:ncol(df)) {
    weight = weights[i]
    for (j in 1:nrow(df)) {
      df[j, i] = df[j, i] * weight
    }
  }
  
  return(df)
}


getNormalisedData = function(df, col_price, 
                             col_non_factor_begin, col_mtr) {
  
  # Normalize price first
  price_min = min(df[[col_price]])
  for (i in 1:nrow(df)) {
    df[i, col_price] = price_min  / df[i, col_price]
  }
  
  # Normalize minimum turning radius
  mtr_min = min(df[[col_mtr]])
  for (i in 1:nrow(df)) {
    df[i, col_mtr] = mtr_min / df[i, col_mtr]
  }
  
  # Normalise the rest of the data
  for (i in col_non_factor_begin:ncol(df)) {
    if (i != col_mtr || i != col_price) {
      value_max = max(df[[i]])
      for (j in 1:nrow(df)) {
        df[j, i] = df[j, i] / value_max
      }
    }
  }
  
  return(df)
}

