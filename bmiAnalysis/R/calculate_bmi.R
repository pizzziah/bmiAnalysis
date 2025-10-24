calculate_bmi <- function(weight, height) {
  bmi <- weight / (height^2)

  if (bmi < 18.5) {
    category <- "Underweight"
  } else if (bmi < 25) {
    category <- "Normal"
  } else if (bmi < 30) {
    category <- "Overweight"
  } else {
    category <- "Obese"
  }
  return(list(BMI = bmi, Category = category))
}
