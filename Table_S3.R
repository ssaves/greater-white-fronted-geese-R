# ============================================================
# Supplementary Table S3: Correlations with Abundance
# ============================================================
library(dplyr)
library(tidyr)
library(boot)
setwd("/***")
df_ans <- read.csv2("Ansealbi.csv", header = TRUE, check.names = FALSE, fileEncoding = "UTF-8-BOM", sep = ";", na.strings = c("", "NA"), stringsAsFactors = FALSE)
names(df_ans)[1] <- "Year"
df_31 <- read.csv2("res_3.1.csv", header = TRUE, check.names = FALSE, fileEncoding = "UTF-8-BOM", sep = ";", na.strings = c("", "NA"), stringsAsFactors = FALSE)
names(df_31)[1] <- "Year"
df_32 <- read.csv2("res_3.2.csv", header = TRUE, check.names = FALSE, fileEncoding = "UTF-8-BOM", sep = ";", na.strings = c("", "NA"), stringsAsFactors = FALSE)
names(df_32)[1] <- "Year"
df <- df_ans %>%
  left_join(df_31 %>% select(Year, hunt_bag, crops_proc, fire_proc), by = "Year") %>%
  left_join(df_32 %>% select(Year, hunt_marsh_bal), by = "Year") %>%
  mutate(across(-Year, ~ suppressWarnings(as.numeric(as.character(.)))))
get_s3_corr <- function(data, response_var, predictor_var, response_label, predictor_label) {
  test_data <- data %>% filter(!is.na(.data[[response_var]]) & !is.na(.data[[predictor_var]])) 
  if(nrow(test_data) < 5) return(NULL) 
  ct <- cor.test(test_data[[response_var]], test_data[[predictor_var]], method = "spearman", exact = FALSE)  
  boot_func <- function(d, i) cor(d[[response_var]][i], d[[predictor_var]][i], method = "spearman")
  set.seed(123)
  b <- boot(test_data, boot_func, R = 2000)
  ci <- tryCatch(boot.ci(b, type = "perc")$percent[4:5], error = function(e) c(NA, NA)) 
  tibble(
    Response = response_label,
    Predictor = predictor_label,
    rho = ct$estimate,
    CI_low = ci[1],
    CI_high = ci[2],
    p.value = ct$p.value,
    N = nrow(test_data)
  )
}
weather_vars <- c("melt_april", "aprilT", "may_T", "TartuTapril", "TartuTmay")
anthro_vars <- c("fire_proc", "crops_proc", "hunt_marsh_bal", "hunt_bag")
var_labels <- c(
  melt_april = "Snowmelt date", aprilT = "April T (Olonec)", may_T = "May T (Olonec)",
  TartuTapril = "April T (Tartu)", TartuTmay = "May T (Tartu)",
  fire_proc = "Burned area (%)", crops_proc = "Crops area (%)",
  hunt_marsh_bal = "Marsh hunting intensity", hunt_bag = "Harvested birds"
)
results_max <- bind_rows(lapply(c(weather_vars, anthro_vars), function(v) {
  get_s3_corr(df, "Maximum", v, "Maximum abundance", var_labels[v])
}))
results_avg <- bind_rows(lapply(c(weather_vars, anthro_vars), function(v) {
  get_s3_corr(df, "Average", v, "Average abundance", var_labels[v])
}))
table_s3 <- bind_rows(results_max, results_avg)
table_s3_formatted <- table_s3 %>%
  mutate(
    rho = round(rho, 2), CI_low = round(CI_low, 2), CI_high = round(CI_high, 2),
    p_stars = case_when(
      is.na(p.value) ~ "NA",
      p.value < 0.001 ~ "< 0.001 ***",
      p.value < 0.01  ~ paste0(sprintf("%.3f", p.value), " **"),
      p.value < 0.05  ~ paste0(sprintf("%.3f", p.value), " *"),
      TRUE            ~ sprintf("%.3f", p.value)
    )
  ) %>%
  select(Response, Predictor, rho, CI_low, CI_high, p_stars, N)
cat("=== TABLE S3: CORRELATIONS WITH ABUNDANCE ===\n")
print(as.data.frame(table_s3_formatted))
write.csv(table_s3_formatted, "Table_S3_Abundance_Correlations.csv", row.names = FALSE)