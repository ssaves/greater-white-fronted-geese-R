# ============================================================
# Supplementary Table S1: Spearman Correlations (with Bootstrap CI)
# ============================================================
library(dplyr)
library(tidyr)
library(broom)
library(boot)
setwd("/***")
df <- read.csv2("res_3.2.csv", header = TRUE, check.names = FALSE,
                fileEncoding = "UTF-8-BOM", sep = ";", 
                na.strings = c("", "NA", " "), stringsAsFactors = FALSE)

df <- df %>% mutate(across(-year, ~ suppressWarnings(as.numeric(as.character(.)))))
vars <- c("melt_april", "aprilT", "TartuTapril", "TartuT5aprilDay")
get_spearman_boot <- function(var_name, data) {
  test_data <- data %>% filter(!is.na(first10K) & !is.na(.data[[var_name]]))
  ct <- cor.test(test_data$first10K, test_data[[var_name]], 
                 method = "spearman", exact = FALSE) 
  boot_func <- function(d, i) {
    cor(d$first10K[i], d[[var_name]][i], method = "spearman")
  }
  set.seed(123)
  b <- boot(test_data, boot_func, R = 2000)
  ci <- tryCatch(boot.ci(b, type = "perc")$percent[4:5], 
                 error = function(e) c(NA, NA))
  tibble(
    Variable = var_name,
    rho = ct$estimate,
    CI_low = ci[1],
    CI_high = ci[2],
    p.value = ct$p.value,
    N = nrow(test_data)
  )
}
table_s1 <- bind_rows(lapply(vars, get_spearman_boot, data = df))
table_s1_formatted <- table_s1 %>%
  mutate(
    rho = round(rho, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p_stars = case_when(
      is.na(p.value) ~ "NA",
      p.value < 0.001 ~ "< 0.001 ***",
      p.value < 0.01  ~ paste0(sprintf("%.3f", p.value), " **"),
      p.value < 0.05  ~ paste0(sprintf("%.3f", p.value), " *"),
      TRUE            ~ sprintf("%.3f", p.value)
    ),
    Predictor = case_when(
      Variable == "melt_april" ~ "Snowmelt date",
      Variable == "aprilT" ~ "April temperature (Olonec)",
      Variable == "TartuTapril" ~ "April temperature (Tartu)",
      Variable == "TartuT5aprilDay" ~ "Date of T > 5 C (Tartu)"
    )
  ) %>%
  select(Predictor, rho, CI_low, CI_high, p_stars, N)
cat("=== TABLE S1: SPEARMAN CORRELATIONS ===\n")
print(as.data.frame(table_s1_formatted))
write.csv(table_s1_formatted, "Table_S1_Spearman.csv", row.names = FALSE)