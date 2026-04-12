library(dplyr)
library(ggeffects)
library(knitr)
library(kableExtra)
library(broom)


monkey_df = read.csv('/Users/maddiethall/R Repos/anthro690_finalproject/all_activity.csv')
monkey_clean = monkey_df %>%
  select(subject, sex, fgc, exc, soc, tac, excs, socs, tacs, rank, ranks, rankf)

monkey_subject = monkey_clean %>%
  group_by(subject) %>%
  summarise(
    sex = first(sex),
    exc = median(exc, na.rm = TRUE),
    soc = median(soc, na.rm = TRUE),
    tac = median(tac, na.rm = TRUE),
    rank = median(rank, na.rm = TRUE),
    median_fgc = median(fgc, na.rm = TRUE),
    n_samples = n()
  )

monkey_subject$sex = factor(monkey_subject$sex)
monkey_subject$exc_z = as.numeric(scale(monkey_subject$exc))
monkey_subject$soc_z = as.numeric(scale(monkey_subject$soc))
monkey_subject$tac_z = as.numeric(scale(monkey_subject$tac))
monkey_subject$rank_z = as.numeric(scale(monkey_subject$rank))

model_exc = lm(median_fgc ~ exc_z * sex + rank_z, data = monkey_subject)
summary(model_exc)

model_soc = lm(median_fgc ~ soc_z * sex + rank_z, data = monkey_subject)
summary(model_soc)

model_full = lm(median_fgc ~ exc_z + soc_z + tac_z * sex, data = monkey_subject)
summary(model_full)

model_null = lm(median_fgc ~ sex + rank_z, data = monkey_subject)

#best model
model_tac = lm(median_fgc ~ tac_z * sex + rank_z, data = monkey_subject)
summary(model_tac)

aic_df_all = AIC(model_exc, model_soc, model_tac, model_full, model_null) |>
  tibble::rownames_to_column("Model") |>
  rename(AIC = `AIC`) |>
  arrange(AIC) |>
  mutate(deltaAIC = AIC - min(AIC))

kable(aic_df_all, digits = 1, caption = "AIC Comparison of Candidate Models") %>%
  kable_classic(full_width = FALSE)





preds = ggpredict(model_tac, terms = c("tac_z", "sex"))
pred_df = as.data.frame(preds)




cor.test(monkey_subject$exc_z, monkey_subject$median_fgc, method = "spearman")
cor.test(monkey_subject$soc_z, monkey_subject$median_fgc, method = "spearman")
cor.test(monkey_subject$tac_z, monkey_subject$median_fgc, method = "spearman")

cor.test(monkey_subject$tac_z, monkey_subject$soc_z, method = "spearman")
cor.test(monkey_subject$exc_z, monkey_subject$tac_z, method = "spearman")
cor.test(monkey_subject$exc_z, monkey_subject$soc_z, method = "spearman")

wilcox.test(median_fgc ~ sex, data = monkey_subject)
wilcox.test(exc_z ~ sex, data = monkey_subject)
wilcox.test(soc_z ~ sex, data = monkey_subject)
wilcox.test(tac_z ~ sex, data = monkey_subject)


coef_table <- tidy(model_tac) %>%
  mutate(
    stars = case_when(
      p.value < 0.001 ~ "&#42;&#42;&#42;",
      p.value < 0.01  ~ "&#42;&#42;",
      p.value < 0.05  ~ "&#42;",
      TRUE            ~ ""
    ),
    
    estimate  = formatC(estimate,  format = "f", digits = 3),
    std.error = formatC(std.error, format = "f", digits = 3),
    statistic = formatC(statistic, format = "f", digits = 3),
    
    pval_text = ifelse(
      p.value < 0.001,
      "&lt;0.001",
      formatC(p.value, format = "f", digits = 3)
    ),
    
    raw_term  = paste0(term, stars),
    raw_pval  = paste0(pval_text, stars),
    
    term = ifelse(stars != "",
                  paste0("<strong>", raw_term, "</strong>"),
                  raw_term),
    
    p.value = ifelse(stars != "",
                     paste0("<strong>", raw_pval, "</strong>"),
                     raw_pval)
  ) %>%
  select(term, estimate, std.error, statistic, p.value)

kable(coef_table,
      format = "html",
      escape = FALSE,
      caption = "Coefficient Estimates for Best-Fitting Linear Regression Model",
      col.names = c("Term","Estimate","Std. Error","z","p")) %>%
  kable_classic(full_width = FALSE)


wilcox.test(fgc ~ sex, data = monkey_clean)
wilcox.test(exc ~ sex, data = monkey_clean)
wilcox.test(soc ~ sex, data = monkey_clean)
wilcox.test(tac ~ sex, data = monkey_clean)
