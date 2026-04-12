library(ggplot2)

#scatterplot + separate regression lines by sex
ggplot(monkey_subject, aes(x = tac, y = median_fgc, color = sex)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Tactility score",
    y = "Median fGC concentration",
    color = "Sex"
  ) +
  theme_classic()

ggplot(monkey_clean, aes(x = tac, y = fgc, color = sex)) +
  geom_point(size = 1, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Raw Tactility score",
    y = "Median fecal glucocorticoid concentration",
    color = "Sex"
  ) +
  theme_classic()


#Boxplot of FGC by sex
ggplot(monkey_clean, aes(x = sex, y = fgc, fill = sex)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    x = "Sex",
    y = "fGC concentration"
  ) +
  theme_classic() +
  guides(fill = "none")

#Boxplot of tactility score by sex
ggplot(monkey_clean, aes(x = sex, y = tac, fill = sex)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    x = "Sex",
    y = "Tactility Score"
  ) +
  theme_classic() +
  guides(fill = "none")

#Boxplot of sociability score by sex
ggplot(monkey_clean, aes(x = sex, y = soc, fill = sex)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    x = "Sex",
    y = "Sociability Score"
  ) +
  theme_classic() +
  guides(fill = "none")

#Boxplot of excitability score by sex
ggplot(monkey_clean, aes(x = sex, y = exc, fill = sex)) +
  geom_boxplot(alpha = 0.6) +
  labs(
    x = "Sex",
    y = "Excitability Score"
  ) +
  theme_classic() +
  guides(fill = "none")


#ggpredict plot
ggplot(pred_df, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  labs(
    x = "Tactility Score (z-score)",
    y = "Predicted median fGC",
    color = "Sex",
    fill = "Sex"
  ) +
  theme_classic()


#assumptions
par(mfrow = c(2,2))
plot(model_tac)

shapiro.test(residuals(model_tac))


