library(ggplot2)
library(RColorBrewer)
library(tidyverse)

ggplot(monkey_subject, aes(x=sex, y=tac, fill=sex)) + 
  geom_violin(trim = FALSE) +
  stat_summary(fun = "mean",
               geom = "crossbar", 
               width = 0.8,
               colour = "black") +
  theme_minimal()


# Mean +/- 1 Standard Deviation

ggplot(monkey_subject, aes(x=sex, y=tac, fill=sex)) + 
  geom_violin() +
  stat_summary(fun.data = "mean_sdl", 
               fun.args = list(mult = 1), 
               geom = "pointrange", color = "blue")

ggplot(monkey_subject, aes(x=sex, y=tac, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_brewer(palette = "Pastel1") +
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 21, 
               size = 2, 
               colour = 'darkblue') +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(monkey_subject, aes(x=sex, y=exc, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_brewer(palette = "Pastel1") +
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 21, 
               size = 2, 
               colour = 'darkblue') +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(monkey_subject, aes(x=sex, y=soc, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_brewer(palette = "Pastel1") +
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 21, 
               size = 2, 
               colour = 'darkblue') +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(monkey_subject, aes(x=sex, y=median_fgc, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_brewer(palette = "Pastel1") +
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 21, 
               size = 2, 
               colour = 'darkblue') +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")

### raw data

ggplot(monkey_clean, aes(x=sex, y=tac, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_brewer(palette = "Pastel1") +
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 21, 
               size = 2, 
               colour = 'darkblue') +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(monkey_clean, aes(x=sex, y=soc, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_brewer(palette = "Pastel1") +
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 21, 
               size = 2, 
               colour = 'darkblue') +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(monkey_clean, aes(x=sex, y=exc, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_brewer(palette = "Pastel1") +
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 21, 
               size = 2, 
               colour = 'darkblue') +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(monkey_clean, aes(x=sex, y=fgc, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_brewer(palette = "Pastel1") +
  stat_summary(fun = "mean", 
               geom = "point", 
               shape = 21, 
               size = 2, 
               colour = 'darkblue') +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")



df_long = monkey_clean %>%
  pivot_longer(
    cols = c(exc, soc, tac),
    names_to = "syndrome",
    values_to = "score" 
  )

ggplot(df_long, aes(x = syndrome, y = score, fill = sex)) +
  geom_violin(trim = FALSE, position = position_dodge(width = 0.8)) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.8)) +
  theme_minimal()
