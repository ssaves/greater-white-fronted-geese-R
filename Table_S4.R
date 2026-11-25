# ============================================================
# FIXED: Table S4 - Period comparison (Before vs After 2014)
# ============================================================
library(dplyr)
library(broom)
setwd("/***")
df_32 <- read.csv2("res_3.2.csv", header = TRUE, check.names = FALSE,
                   fileEncoding = "UTF-8-BOM", sep = ";", 
                   na.strings = c("", "NA"), stringsAsFactors = FALSE)
names(df_32)[1] <- "Year"
df_32 <- df_32 %>% mutate(across(-Year, ~ suppressWarnings(as.numeric(as.character(.)))))
df_period <- df_32 %>%
  filter(!is.na(may_bird_max)) %>%
  mutate(Period = factor(ifelse(Year <= 2014, "Before 2014", "After 2014"),
                         levels = c("Before 2014", "After 2014")))
y_before <- df_period$may_bird_max[df_period$Period == "Before 2014"]
y_after  <- df_period$may_bird_max[df_period$Period == "After 2014"]
n_before <- length(y_before)
n_after  <- length(y_after)
t_res     <- t.test(y_after, y_before)   # CI for After - Before
diff_mean <- t_res$estimate[1] - t_res$estimate[2]
ci_mean   <- t_res$conf.int
p_mean    <- t_res$p.value
m_trend <- lm(may_bird_max ~ Year * Period, data = df_period)
cf_t    <- summary(m_trend)$coefficients
ci_t    <- confint(m_trend)
int_name <- "Year:PeriodAfter 2014"
slope_before <- cf_t["Year", "Estimate"]
if (int_name %in% rownames(cf_t)) {
  int_est_trend  <- cf_t[int_name, "Estimate"]
  int_p_trend    <- cf_t[int_name, "Pr(>|t|)"]
  int_ci_trend   <- ci_t[int_name, ]
  slope_after    <- slope_before + int_est_trend
} else {
  int_est_trend <- NA; int_p_trend <- NA; int_ci_trend <- c(NA, NA); slope_after <- NA
}
df_temp <- df_period %>% filter(!is.na(may_T))
n_tb <- sum(df_temp$Period == "Before 2014")
n_ta <- sum(df_temp$Period == "After 2014")
m_temp <- lm(may_bird_max ~ may_T * Period, data = df_temp)
cf_m   <- summary(m_temp)$coefficients
ci_m   <- confint(m_temp)
int_name_t <- "may_T:PeriodAfter 2014"
coef_temp_before <- cf_m["may_T", "Estimate"]
if (int_name_t %in% rownames(cf_m)) {
  int_est_temp <- cf_m[int_name_t, "Estimate"]
  int_p_temp   <- cf_m[int_name_t, "Pr(>|t|)"]
  int_ci_temp  <- ci_m[int_name_t, ]
  coef_temp_after <- coef_temp_before + int_est_temp
} else {
  int_est_temp <- NA; int_p_temp <- NA; int_ci_temp <- c(NA, NA); coef_temp_after <- NA
}
fmt_p <- function(p) ifelse(is.na(p), "NA", ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p)))
table_s4 <- tibble(
  Comparison = c(
    "Mean date of peak abundance (DOY)",
    "Linear trend in peak date (days/year)",
    "Effect of May temperature on peak date (days/\u00B0C)"
  ),
  Before_2014 = c(
    sprintf("%.1f (n=%d)", mean(y_before), n_before),
    sprintf("%.3f", slope_before),
    sprintf("%.2f (n=%d)", coef_temp_before, n_tb)
  ),
  After_2014 = c(
    sprintf("%.1f (n=%d)", mean(y_after), n_after),
    sprintf("%.3f", slope_after),
    sprintf("%.2f (n=%d)", coef_temp_after, n_ta)
  ),
  Difference = c(
    sprintf("%.1f", diff_mean),
    sprintf("%.3f", int_est_trend),
    sprintf("%.2f", int_est_temp)
  ),
  CI_95 = c(
    sprintf("[%.1f; %.1f]", ci_mean[1], ci_mean[2]),
    sprintf("[%.3f; %.3f]", int_ci_trend[1], int_ci_trend[2]),
    sprintf("[%.2f; %.2f]", int_ci_temp[1], int_ci_temp[2])
  ),
  p_value = c(
    fmt_p(p_mean), fmt_p(int_p_trend), fmt_p(int_p_temp)
  )
)
table_s4 <- table_s4 %>%
  mutate(across(-Comparison, ~ gsub("\\.", ",", .)))
cat("=== TABLE S4 ===\n")
print(as.data.frame(table_s4))
write.csv(table_s4, "Table_S4_Period_Comparison.csv", row.names = FALSE)