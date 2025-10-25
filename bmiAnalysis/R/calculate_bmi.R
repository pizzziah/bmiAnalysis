calculate_bmi <- function(weights, heights) {
  library(ggplot2)
  
  bmi = weights / (heights^2)
  category <- cut(
    bmi,
    breaks = c(-Inf, 18.5, 25, 30, Inf),
    labels = c("Underweight", "Normal", "Overweight", "Obese")
  )
  
  data <- data.frame(Weight = weights, Height = heights, BMI = bmi, Category = category)
  write.csv(data, "bmi_results.csv", row.names = FALSE)
  
  plot <- ggplot(data, aes(x = Category, y = BMI, fill = Category)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "BMI Distribution by Category")
  
  print(plot)
  return(data)
}

run_bmi_many_times <- function(n = 10000) {
  weights <- runif(n, 40, 120)
  heights <- runif(n, 1.4, 2.0)
  results <- vector("list", n)
  
  for (i in seq_len(n)) {
    results[[i]] <- calculate_bmi(weights[i], heights[i])
  }
  return(results)
}


