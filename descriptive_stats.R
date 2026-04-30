library(ggplot2)

#figure 2 in paper
par(mfrow = c(2, 4))
par(mfrow = c(1,1))

hist(monkey_clean$fgc[monkey_clean$subject == "WAN"], main = "WAN (F)", xlab = "fGC", ylab = "Frequency", col = "#D29AE3")
hist(monkey_clean$fgc[monkey_clean$subject == "KRI"], main = "KRI (F)", xlab = "fGC", ylab = "Frequency", col = "#D29AE3")
hist(monkey_clean$fgc[monkey_clean$subject == "JOA"], main = "JOA (F)", xlab = "fGC", ylab = "Frequency", col = "#D29AE3")
hist(monkey_clean$fgc[monkey_clean$subject == "ANN"], main = "ANN (F)", xlab = "fGC", ylab = "Frequency", col = "#D29AE3")
hist(monkey_clean$fgc[monkey_clean$subject == "GEO"], main = "GEO (M)", xlab = "fGC", ylab = "Frequency", col = "#9ADEE3")
hist(monkey_clean$fgc[monkey_clean$subject == "ISA"], main = "ISA (M)", xlab = "fGC", ylab = "Frequency", col = "#9ADEE3")
hist(monkey_clean$fgc[monkey_clean$subject == "TIM"], main = "TIM (M)", xlab = "fGC", ylab = "Frequency", col = "#9ADEE3")
hist(monkey_clean$fgc[monkey_clean$subject == "GUL"], main = "GUL (M)", xlab = "fGC", ylab = "Frequency", col = "#9ADEE3")



hist(monkey_clean$tac[monkey_clean$sex == "Female"])
hist(monkey_clean$tac[monkey_clean$sex == "Male"])

#figure 1 in paper

fgc_mean = mean(monkey_clean$fgc)
fgc_median = median(monkey_clean$fgc)

ggplot(monkey_clean, aes(x = fgc)) +
  geom_histogram(fill = "#e7f8fd", color = "#002d54", bins = 28) +
  geom_vline(xintercept = fgc_mean, color = "#26ac95", linewidth = 1.7, linetype = "dotted") +
  geom_vline(xintercept = fgc_median, color = "#894df8", linewidth = 1.2, linetype = "dashed") +
  theme_minimal() +
  labs(
    x = "fGC Concentration",
    y = "Frequency"
    ) +
  annotate("text", x = fgc_mean + 820, y = 98, label = paste("Mean:", round(fgc_mean, 2)), color = "#26ac95") +
  annotate("text", x = fgc_median + 970, y = 93, label = paste("Median:", round(fgc_median, 2)), color = "#894df8")
