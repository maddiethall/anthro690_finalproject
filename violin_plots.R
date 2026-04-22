library(ggplot2)
library(ggsignif)
library(RColorBrewer)
library(tidyverse)


ggplot(monkey_subject, aes(x=sex, y=tac_z, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("#D29AE3", "#9ADEE3")) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(monkey_subject, aes(x=sex, y=exc_z, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("#D29AE3", "#9ADEE3")) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(monkey_subject, aes(x=sex, y=soc_z, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("#D29AE3", "#9ADEE3")) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(monkey_subject, aes(x=sex, y=median_fgc, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("#D29AE3", "#9ADEE3")) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")


### raw data 

ggplot(monkey_clean, aes(x=sex, y=tac, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("#D29AE3", "#9ADEE3")) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(monkey_clean, aes(x=sex, y=exc, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("#D29AE3", "#9ADEE3")) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(monkey_clean, aes(x=sex, y=soc, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("#D29AE3", "#9ADEE3")) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(monkey_clean, aes(x=sex, y=fgc, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("#D29AE3", "#9ADEE3")) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  theme_minimal() +
  theme(legend.position = "none")


### behavioral syndromes together

df_long_cols = monkey_subject %>%
  rename(
    "Tactility" = tac_z,
    "Sociability" = soc_z,
    "Excitability" = exc_z,
    "Sex" = sex
  )

df_long = df_long_cols %>%
  pivot_longer(
    cols = c(Tactility, Sociability, Excitability),
    names_to = "syndrome",
    values_to = "score"
  )


ggplot(df_long, aes(x = syndrome, y = score, fill = Sex)) +
  geom_violin(trim = FALSE, position = position_dodge(width = 0.6)) +
  geom_boxplot(width = 0.15, outlier.shape = NA,
               position = position_dodge(width = 0.6)) +
  scale_fill_manual(values = c("#D29AE3", "#9ADEE3")) +
  labs(
    x = "Behavioral Syndrome",
    y = "Z-Score"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 10)),
    axis.title = element_text(size = 13),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) +
  geom_signif(
    xmin = 0.8, xmax = 1.1,   
    annotations = "*",        
    y_position = max(df_long$score) + .7,
    textsize = 5
  ) +
  geom_signif(
    xmin = 2.9, xmax = 3.2,
    annotations = "*",
    y_position = max(df_long$score) + 0.7,
    textsize = 5
  )
