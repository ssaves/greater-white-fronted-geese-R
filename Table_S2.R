# ============================================================
# Supplementary Table S2: Pairwise Spearman Correlations
# ============================================================
library(dplyr)
library(tidyr)
library(boot)
setwd("/***")
df <- read.csv2("res_3.2.csv", header = TRUE, check.names = FALSE,
                fileEncoding = "UTF-8-BOM", sep = ";", 
                na.strings = c("", "NA", " "), stringsAsFactors = FALSE)
df <- df %>% mutate(across(-year, ~ suppressWarnings(as.numeric(as.character(.)))))
get_pair_corr <- function(data, var1, var2, label1, label2) {
  test_data <- data %>% filter(!is.na(.data[[var1]]) & !is.na(.data[[var2]]))
  ct <- cor.test(test_data[[var1]], test_data[[var2]], 
                 method = "spearman", exact = FALSE)
  boot_func <- function(d, i) {
    cor(d[[var1]][i], d[[var2]][i], method = "spearman")
  }
  set.seed(123)
  b <- boot(test_data, boot_func, R = 2000)
  ci <- tryCatch(boot.ci(b, type = "perc")$percent[4:5], 
                 error = function(e) c(NA, NA))
  tibble(
    Pair = paste0(label1, " \u00D7 ", label2),
    rho = ct$estimate,
    CI_low = ci[1],
    CI_high = ci[2],
    p.value = ct$p.value,
    N = nrow(test_data)
  )
}
s2_row1 <- df %>% filter(year <= 2014) %>%
  get_pair_corr("may_bird_max", "fire_proc", 
                "Peak abundance date", "Burned area (%)")
s2_row2 <- df %>%
  get_pair_corr("may_bird_max", "hunt_marsh_bal", 
                "Peak abundance date", "Marsh hunting intensity")
s2_row3 <- df %>%
  get_pair_corr("fire_proc", "hunt_marsh_bal", 
                "Burned area (%)", "Marsh hunting intensity")
table_s2 <- bind_rows(s2_row1, s2_row2, s2_row3)
table_s2_formatted <- table_s2 %>%
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
    )
  ) %>%
  select(Pair, rho, CI_low, CI_high, p_stars, N)
cat("=== TABLE S2: PAIRWISE CORRELATIONS ===\n")
print(as.data.frame(table_s2_formatted))

write.csv(table_s2_formatted, "Table_S2_Pairwise_Correlations.csv", row.names = FALSE)