# ============================================================
# Analysis for last10k (departure) and Period10k (duration)
# ============================================================
library(dplyr)
library(tidyr)
library(boot)
library(car)
library(MuMIn)
setwd("/***")
df <- read.csv2("res_3.2.csv", header = TRUE, check.names = FALSE,
                fileEncoding = "UTF-8-BOM", sep = ";", 
                na.strings = c("", "NA", " "), stringsAsFactors = FALSE)
df <- df %>% mutate(across(-year, ~ suppressWarnings(as.numeric(as.character(.)))))
# ============================================================
# last10k
# ============================================================
cat("\n--- 1. LAST10K TREND ---\n")
m_last <- lm(last10k ~ year, data = df)
cat(sprintf("Slope: %.2f days/decade (95%% CI: %.2f; %.2f), p = %.3f\n",
            coef(m_last)["year"]*10, confint(m_last)["year",1]*10, 
            confint(m_last)["year",2]*10, summary(m_last)$coefficients["year",4]))
cat("\n--- 2. LAST10K CORRELATIONS ---\n")
ct_last_hunt <- cor.test(df$last10k, df$hunt_marsh_bal, method="spearman", exact=FALSE)
ct_last_mayT <- cor.test(df$last10k, df$may_T, method="spearman", exact=FALSE)
cat(sprintf("Last10k vs Hunt: rho = %.2f, p = %.3f, n = %d\n", ct_last_hunt$estimate, ct_last_hunt$p.value, sum(!is.na(df$last10k) & !is.na(df$hunt_marsh_bal))))
cat(sprintf("Last10k vs May_T: rho = %.2f, p = %.3f, n = %d\n", ct_last_mayT$estimate, ct_last_mayT$p.value, sum(!is.na(df$last10k) & !is.na(df$may_T))))
cat("\n--- 3. LAST10K MULTIPLE REGRESSION ---\n")
m_last_full <- lm(last10k ~ may_T + hunt_marsh_bal, data = df)
print(summary(m_last_full)$coefficients)
print(confint(m_last_full))
# ============================================================
# Period10k
# ============================================================
cat("\n--- 4. PERIOD10K DESCRIPTIVE STATS ---\n")
mean_period <- mean(df$Period10k, na.rm = TRUE)
ci_period <- t.test(df$Period10k)$conf.int[1:2]
range_period <- range(df$Period10k, na.rm = TRUE)
n_period <- sum(!is.na(df$Period10k))
cat(sprintf("Mean: %.1f days (95%% CI: %.1f; %.1f), Range: %d-%d, n = %d\n",
            mean_period, ci_period[1], ci_period[2], range_period[1], range_period[2], n_period))
cat("\n--- 5. PERIOD10K TREND ---\n")
m_period <- lm(Period10k ~ year, data = df)
cat(sprintf("Slope: %.2f days/decade (95%% CI: %.2f; %.2f), p = %.3f\n",
            coef(m_period)["year"]*10, confint(m_period)["year",1]*10, 
            confint(m_period)["year",2]*10, summary(m_period)$coefficients["year",4]))
cat("\n--- 6. PERIOD10K MULTIPLE REGRESSION ---\n")
# Проверяем влияние погоды и антропогенных факторов
m_period_full <- lm(Period10k ~ melt_april + may_T + hunt_marsh_bal + fire_proc, data = df)
cat("VIF:\n")
print(vif(m_period_full, type = "predictor"))
df_period_clean <- df %>% filter(!is.na(Period10k))
options(na.action = "na.fail")
dredge_res <- dredge(lm(Period10k ~ melt_april + may_T + hunt_marsh_bal, data = df_period_clean))
cat("\nTop models:\n")
print(head(dredge_res, 3))
options(na.action = "na.omit")
# --- 1. LAST10K TREND ---
m_last <- lm(last10k ~ year, data = df)
cat(sprintf("Last10k Trend: slope = %.2f days/decade (95%% CI: %.2f; %.2f), p = %.3f\n",
            coef(m_last)["year"]*10, confint(m_last)["year",1]*10, 
            confint(m_last)["year",2]*10, summary(m_last)$coefficients["year",4]))
# --- 2. LAST10K MULTIPLE REGRESSION ---
m_last_full <- lm(last10k ~ may_T + hunt_marsh_bal, data = df)
cat("\nLast10k Multiple Regression:\n")
print(summary(m_last_full)$coefficients)
print(confint(m_last_full))
# --- 3. PERIOD10K TREND ---
m_period <- lm(Period10k ~ year, data = df)
cat(sprintf("\nPeriod10k Trend: slope = %.2f days/decade (95%% CI: %.2f; %.2f), p = %.3f\n",
            coef(m_period)["year"]*10, confint(m_period)["year",1]*10, 
            confint(m_period)["year",2]*10, summary(m_period)$coefficients["year",4]))