library(ggplot2)

#scatterplot + separate regression lines by sex
ggplot(monkey_subject, aes(x = tac, y = median_fgc, color = sex)) +
  scale_color_manual(values = c("#D29AE3", "#9ADEE3")) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Tactility Z-Score",
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




#assumptions
par(mfrow = c(2,2))
plot(model_tac)

shapiro.test(residuals(model_tac))


