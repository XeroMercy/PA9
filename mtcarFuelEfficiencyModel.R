# mtcarFuelEfficiencyModel.R
# Wesley Sutton
# 11/08/2026
# Models fuel efficiency in mtcars using vehicle weight.

# Plotted fuel efficiency against vehicle weight.
plot(
  mpg ~ wt,
  data = mtcars,
  main = "Fuel Efficiency by Vehicle Weight",
  xlab = "Vehicle weight (thousands of pounds)",
  ylab = "Fuel efficiency (MPG)",
  pch = 19,
  col = "steelblue4"
)

# Fit a linear model to predict MPG from weight.
mpg_model <- lm(mpg ~ wt, data = mtcars)

# Added the fitted regression line to the scatter plot.
abline(mpg_model, col = "firebrick", lwd = 2)

# Found the slope, its p-value, and its 95% confidence interval.
slope <- coef(mpg_model)[["wt"]]
p_value <- summary(mpg_model)$coefficients["wt", "Pr(>|t|)"]
slope_ci <- confint(mpg_model, "wt", level = 0.95)

# Reported the change in MPG for each additional 1,000 pounds.
cat("Average MPG change per additional 1,000 lb:", round(slope, 2), "\n")
cat("95% confidence interval for the slope:", round(slope_ci[1], 2), "to",
    round(slope_ci[2], 2), "\n")

if (p_value < 0.05) {
  cat("Significant at 0.05. p-value:", signif(p_value, 3), "\n")
}

# Converted 6,000 pounds to the units used by wt.
weight_lb <- 6000
new_car <- data.frame(wt = weight_lb / 1000)

# Predicted MPG and found a 95% prediction interval for one car.
mpg_prediction <- predict(
  mpg_model,
  newdata = new_car,
  interval = "prediction",
  level = 0.95
)

# Rounded the prediction for display.
mpg_result <- round(mpg_prediction, 2)
cat("\nPredicted MPG at 6,000 lb:", mpg_result[1, "fit"], "\n")
cat("95% prediction interval:", mpg_result[1, "lwr"], "to",
    mpg_result[1, "upr"], "MPG\n")

# Checked whether the weight exceeds the heaviest car in the data.
cat("Use caution at 6,000 lb. Sample maximum:", max(mtcars$wt) * 1000, "lb.\n")