library(ggplot2)

#scatterplot + separate regression lines by sex
ggplot(monkey_subject, aes(x = tac, y = median_fgc, color = sex)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Tactility score",
    y = "Median fecal glucocorticoid concentration",
    color = "Sex"
  ) +
  theme_classic()


#Boxplot of median FGC by sex
ggplot(monkey_subject, aes(x = sex, y = median_fgc, fill = sex)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.8) +
  labs(
    x = "Sex",
    y = "Median fecal glucocorticoid concentration"
  ) +
  theme_classic() +
  guides(fill = "none")

#Boxplot of tactility score by sex
ggplot(monkey_subject, aes(x = sex, y = tac, fill = sex)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.8) +
  labs(
    x = "Sex",
    y = "Tactility Score"
  ) +
  theme_classic() +
  guides(fill = "none")

#Boxplot of sociability score by sex
ggplot(monkey_subject, aes(x = sex, y = soc, fill = sex)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.8) +
  labs(
    x = "Sex",
    y = "Sociability Score"
  ) +
  theme_classic() +
  guides(fill = "none")

#Boxplot of excitability score by sex
ggplot(monkey_subject, aes(x = sex, y = exc, fill = sex)) +
  geom_boxplot(alpha = 0.6) +
  geom_jitter(width = 0.1, size = 2, alpha = 0.8) +
  labs(
    x = "Sex",
    y = "Excitability Score"
  ) +
  theme_classic() +
  guides(fill = "none")

#assumptions
par(mfrow = c(2,2))
plot(model_tac_z)

shapiro.test(residuals(model_tac_z))
