rm(list=ls())
library(RSADBE)
data(Gasoline)
gas <- Gasoline
y <- as.vector(gas[,1])
x1 <- as.vector(gas[,2])
x6 <- as.vector(gas[,7])
x1_square <- as.matrix(x1%*%t(x1))
u<- as.matrix(eigen(x1_square)$vector)
d <- as.matrix(diag(25)*eigen(x1_square)$value)


#(a) Multiple Linear Regression
aw1 <- lm(formula = y~x1+x6,data = gas)
b0 <- aw1$coefficients[1]
b1 <- aw1$coefficients[2]
b6 <- aw1$coefficients[3]

s <- (sum((y-aw1$coefficients[1]-aw1$coefficients[2]*x1-aw1$coefficients[3]*x6)^2))/(length(y)-2)


#(b) ANOVA
aw2 <- anova(aw1)

#r_square
r_square <- (aw2$`Sum Sq`[1]+aw2$`Sum Sq`[2])/(aw2$`Sum Sq`[1]+aw2$`Sum Sq`[2]+aw2$`Sum Sq`[3])
r_square_adjust <- summary(aw1)$adj.r.squared

aw3 <- data.frame(
  r_square,
  r_square_adjust
)


# Residual standard deviation
mse <- aw2$`Mean Sq`[3]
s <- sqrt(mse)


# Beta1 confidence interval
X <- model.matrix(aw1)

beta_hat <- coef(aw1)
MSE <- deviance(aw1) / df.residual(aw1)
vcov_beta <- MSE * solve(t(X) %*% X)
SE_beta1 <- sqrt(vcov_beta[2, 2])
t_critical <- qt(0.975, df.residual(aw1))

lower_bound <- beta_hat[2] - t_critical * SE_beta1
upper_bound <- beta_hat[2] + t_critical * SE_beta1
c(lower_bound, upper_bound)


#t-statistic for testing beta

summary(aw1)$coefficients
#t-statistic for testing H0:beta1=0
t_beta1 <- summary(aw1)$coefficients["x1", "t value"]

#t-statistic for testing H0:beta6=0
t_beta6 <- summary(aw1)$coefficients["x6", "t value"]

t_beta1
t_beta6

# 0.95 CI on the mean gasoline mileage

X <- model.matrix(aw1)
x0 <- matrix(c(1, 275, 2), nrow = 3)
beta_hat <- coef(aw1)
mse <- deviance(aw1) / df.residual(aw1)
y_hat <- t(x0) %*% beta_hat
se_mean <- sqrt(
  mse * t(x0) %*% solve(t(X) %*% X) %*% x0
)
t_critical <- qt(0.975, df.residual(aw1))

lower_bound2 <- y_hat - t_critical * se_mean
upper_bound2 <- y_hat + t_critical * se_mean

y_ci <- c(lower_bound2, upper_bound2)
y_ci

#0.95 prediction interval on the mean gasoline mileage
X <- model.matrix(aw1)
x0_pred <- matrix(c(1,275,2),nrow=3)
beta_hat <- coef(aw1)
mse <- deviance(aw1) / df.residual(aw1)

y_hat <- t(x0_pred) %*% beta_hat
se_pred <- sqrt(
  mse * (1 + t(x0_pred) %*% solve(t(X) %*% X) %*% x0_pred)
)
t_critical <- qt(0.975, df.residual(aw1))

lower_bound3 <- y_hat - t_critical * se_pred
upper_bound3 <- y_hat + t_critical * se_pred

y_pred <- c(lower_bound3, upper_bound3)

y_pred

# Visualization of Regression Model
# Fix x6 = 2 and vary x1

library(ggplot2)

# Create values of x1 for prediction
newdata <- data.frame(
  x1 = seq(min(x1), max(x1), length.out = 100),
  x6 = 2
)

# 95% Confidence Interval
ci <- predict(
  aw1,
  newdata = newdata,
  interval = "confidence",
  level = 0.95
)

# 95% Prediction Interval
pi <- predict(
  aw1,
  newdata = newdata,
  interval = "prediction",
  level = 0.95
)

# Combine results
plot_data <- data.frame(
  x1 = newdata$x1,
  fit = ci[, "fit"],
  ci_lower = ci[, "lwr"],
  ci_upper = ci[, "upr"],
  pi_lower = pi[, "lwr"],
  pi_upper = pi[, "upr"]
)

# Plot
ggplot() +
  
  # 95% Prediction Interval
  geom_ribbon(
    data = plot_data,
    aes(
      x = x1,
      ymin = pi_lower,
      ymax = pi_upper
    ),
    alpha = 0.15
  ) +
  
  # 95% Confidence Interval
  geom_ribbon(
    data = plot_data,
    aes(
      x = x1,
      ymin = ci_lower,
      ymax = ci_upper
    ),
    alpha = 0.30
  ) +
  
  # Observed data
  geom_point(
    data = gas,
    aes(
      x = x1,
      y = y
    ),
    size = 2
  ) +
  
  # Regression line
  geom_line(
    data = plot_data,
    aes(
      x = x1,
      y = fit
    ),
    linewidth = 1
  ) +
  
  labs(
    title = "Multiple Linear Regression",
    subtitle = "95% Confidence Interval and Prediction Interval (x6 = 2)",
    x = "x1",
    y = "Response (y)"
  ) +
  
  theme_minimal()



