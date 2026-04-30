library(ggplot2)
library(ggsignif)
library(RColorBrewer)
library(tidyverse)


## Violin plots of median syndrome scores, figure 5 in paper

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

## Violin plot of fgc and sex, figure 4 in paper

ggplot(monkey_subject, aes(x=sex, y=median_fgc, fill=sex)) + 
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("#D29AE3", "#9ADEE3")) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  labs(y = "Median fGC Concentration"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 13, margin = margin(r = 12)),
    axis.title = element_text(size = 13),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) +
  geom_signif(
    xmin = 1, xmax = 2,   
    annotations = "**",        
    y_position = max(monkey_subject$median_fgc) + 700,
    textsize = 5
  )


#scatterplot + separate regression lines by sex, figure 7 in paper

ggplot(monkey_subject, aes(x = tac, y = median_fgc, color = sex)) +
  scale_color_manual(values = c("#D29AE3", "#9ADEE3")) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Median Tactility Score",
    y = "Median fGC concentration",
    color = "Sex"
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 13, margin = margin(t = 10)),
    axis.title.y = element_text(size = 13, margin = margin(r = 12)),
    axis.title = element_text(size = 13),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  )
