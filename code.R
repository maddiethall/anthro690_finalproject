library(dplyr)
library(lme4)
library(lmerTest)

monkey_df = read.csv('/Users/maddiethall/R Repos/anthro690_finalproject/all_activity.csv')
monkey_clean = monkey_df %>%
  select(subject, sex, fgc, exc, soc, tac, excs, socs, tacs, rank, ranks, rankf)

monkey_subject = monkey_clean %>%
  group_by(subject) %>%
  summarise(
    sex = first(sex),
    exc = mean(exc, na.rm = TRUE),
    soc = mean(soc, na.rm = TRUE),
    tac = mean(tac, na.rm = TRUE),
    rank = mean(rank, na.rm = TRUE),
    mean_fgc = mean(fgc, na.rm = TRUE),
    median_fgc = median(fgc, na.rm = TRUE),
    sd_fgc = sd(fgc, na.rm = TRUE),
    n_samples = n()
  )

#scale predictors
monkey_subject$tac_z = scale(monkey_subject$tac)
monkey_subject$soc_z = scale(monkey_subject$soc)
monkey_subject$exc_z = scale(monkey_subject$exc)


model_exc_z = lm(median_fgc ~ exc_z * sex + rank, data = monkey_subject)
summary(model_exc_z)

model_soc_z = lm(median_fgc ~ soc * sex + rank, data = monkey_subject)
summary(model_soc_z)

#best model
model_tac_z = lm(median_fgc ~ tac_z * sex + rank, data = monkey_subject)
summary(model_tac_z)

model_full = lm(median_fgc ~ exc + soc + tac * sex, data = monkey_subject)
summary(model_full)


cor.test(monkey_subject$exc, monkey_subject$median_fgc, method = "spearman")
cor.test(monkey_subject$soc, monkey_subject$median_fgc, method = "spearman")
cor.test(monkey_subject$tac, monkey_subject$median_fgc, method = "spearman")

wilcox.test(mean_fgc ~ sex, data = monkey_subject)
wilcox.test(exc ~ sex, data = monkey_subject)
wilcox.test(soc ~ sex, data = monkey_subject)
wilcox.test(tac ~ sex, data = monkey_subject)

