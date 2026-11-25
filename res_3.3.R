# ============================================================
# Comprehensive analysis of goose abundance (Ansealbi + other data)
# ============================================================
library(dplyr)
library(tidyr)
library(broom)
library(car)
library(MuMIn)
setwd("/***")
df_ans <- read.csv2("Ansealbi.csv", header = TRUE, check.names = FALSE,
                    fileEncoding = "UTF-8-BOM", sep = ";", na.strings = c("", "NA"), stringsAsFactors = FALSE)
names(df_ans)[1] <- "Year"
df_31 <- read.csv2("res_3.1.csv", header = TRUE, check.names = FALSE,
                   fileEncoding = "UTF-8-BOM", sep = ";", na.strings = c("", "NA"), stringsAsFactors = FALSE)
names(df_31)[1] <- "Year"
df_32 <- read.csv2("res_3.2.csv", header = TRUE, check.names = FALSE,
                   fileEncoding = "UTF-8-BOM", sep = ";", na.strings = c("", "NA"), stringsAsFactors = FALSE)
names(df_32)[1] <- "Year"
df <- df_ans %>%
  left_join(df_31 %>% select(Year, hunt_bag, crops_proc, fire_proc), by = "Year") %>%
  left_join(df_32 %>% select(Year, hunt_marsh_bal), by = "Year") %>%
  mutate(across(-Year, ~ suppressWarnings(as.numeric(as.character(.))))) %>%
  filter(!is.na(Maximum)) 
cat("\n=== CORRELATIONS ===\n")
vars_weather <- c("melt_april", "aprilT", "may_T", "TartuTapril", "TartuTmay")
vars_anthro <- c("fire_proc", "crops_proc", "hunt_marsh_bal", "hunt_bag")
for (v in c(vars_weather, vars_anthro)) {
  # Для Maximum
  ct_max <- cor.test(df$Maximum, df[[v]], method = "spearman", exact = FALSE)
  # Для Average
  ct_avg <- cor.test(df$Average, df[[v]], method = "spearman", exact = FALSE)
  
  cat(sprintf("%s | Max: rho = %.2f, p = %.3f | Avg: rho = %.2f, p = %.3f\n", 
              v, ct_max$estimate, ct_max$p.value, ct_avg$estimate, ct_avg$p.value))
}
cat("\n=== REGRESSION MODELS (Weather only) ===\n")
m_max_full <- lm(Maximum ~ melt_april + aprilT + may_T + TartuTapril + TartuTmay, data = df)
m_avg_full <- lm(Average ~ melt_april + aprilT + may_T + TartuTapril + TartuTmay, data = df)
options(na.action = "na.fail")
cat("\n-- Maximum Model Selection --\n")
dredge_max <- dredge(m_max_full, extra = "adjR^2")
print(head(dredge_max, 3))
best_max <- get.models(dredge_max, 1)[[1]]
print(tidy(best_max, conf.int = TRUE))
cat("Adj R2 Max:", summary(best_max)$adj.r.squared, "\n")
cat("\n-- Average Model Selection --\n")
dredge_avg <- dredge(m_avg_full, extra = "adjR^2")
print(head(dredge_avg, 3))
best_avg <- get.models(dredge_avg, 1)[[1]]
print(tidy(best_avg, conf.int = TRUE))
cat("Adj R2 Avg:", summary(best_avg)$adj.r.squared, "\n")
options(na.action = "na.omit")