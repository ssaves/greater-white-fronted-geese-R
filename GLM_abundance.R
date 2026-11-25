# ============================================================
# Negative Binomial GLM for goose abundance
# ============================================================
library(dplyr)
library(MASS)       # glm.nb
library(DHARMa)     # residual diagnostics
library(MuMIn)      # dredge, pseudo-R2
library(car)        # vif
library(broom)      # tidy output
setwd("/***")
df_ans <- read.csv2("Ansealbi.csv", header = TRUE, check.names = FALSE,
                    fileEncoding = "UTF-8-BOM", sep = ";",
                    na.strings = c("", "NA"), stringsAsFactors = FALSE)
names(df_ans)[1] <- "Year"
df <- df_ans %>%
  mutate(across(-Year, ~ suppressWarnings(as.numeric(as.character(.))))) %>%
  filter(!is.na(Maximum), !is.na(Average))
nrow(df)   # 26
cat("\n=== OVERDISPERSION CHECK ===\n")
pois_max <- glm(Maximum ~ aprilT + melt_april, family = poisson, data = df)
cat("Poisson (Maximum): dispersion =",
    round(summary(pois_max)$deviance / summary(pois_max)$df.residual, 2), "\n")
cat("\n=== VIF ===\n")
full_lm_max <- lm(Maximum ~ aprilT + may_T + melt_april + TartuTapril + TartuTmay, data = df)
print(vif(full_lm_max))   # все < 5 - OK
full_nb_max <- glm.nb(Maximum ~ aprilT + may_T + melt_april + TartuTapril + TartuTmay,
                      data = df)
full_nb_avg <- glm.nb(Average ~ aprilT + may_T + melt_april + TartuTapril + TartuTmay,
                      data = df)
cat("\n=== THETA ===\n")
cat("theta (Maximum):", round(full_nb_max$theta, 2), "\n")
cat("theta (Average):", round(full_nb_avg$theta, 2), "\n")
options(na.action = "na.fail")
dredge_max <- dredge(full_nb_max, extra = c("AICc", "adjR^2"))
dredge_avg <- dredge(full_nb_avg, extra = c("AICc", "adjR^2"))
cat("\n=== TOP 3 MODELS: Maximum ===\n")
print(head(dredge_max, 3))
cat("\n=== TOP 3 MODELS: Average ===\n")
print(head(dredge_avg, 3))
best_nb_max <- get.models(dredge_max, 1)[[1]]
best_nb_avg <- get.models(dredge_avg, 1)[[1]]
cat("\n=== BEST NB MODEL: Maximum ===\n")
print(tidy(best_nb_max, conf.int = TRUE, exponentiate = FALSE))
cat("\n-- IRR (exp(beta)) --\n")
print(tidy(best_nb_max, conf.int = TRUE, exponentiate = TRUE))
cat("\n=== BEST NB MODEL: Average ===\n")
print(tidy(best_nb_avg, conf.int = TRUE, exponentiate = FALSE))
cat("\n-- IRR (exp(beta)) --\n")
print(tidy(best_nb_avg, conf.int = TRUE, exponentiate = TRUE))
cat("\n=== PSEUDO-R2 ===\n")
cat("Nagelkerke R2 (Maximum):", round(r.squaredGLMM(best_nb_max)[1, "R2N"], 3), "\n")
cat("Nagelkerke R2 (Average):", round(r.squaredGLMM(best_nb_avg)[1, "R2N"], 3), "\n")
set.seed(123)
sim_max <- simulateResiduals(best_nb_max, n = 1000)
sim_avg <- simulateResiduals(best_nb_avg, n = 1000)
png("dharma_max.png", width = 1200, height = 800, res = 150)
plot(sim_max)
dev.off()
png("dharma_avg.png", width = 1200, height = 800, res = 150)
plot(sim_avg)
dev.off()
cat("\n=== DHARMa TESTS: Maximum ===\n")
testDispersion(sim_max)     
testZeroInflation(sim_max)   
testUniformity(sim_max)      
cat("\n=== DHARMa TESTS: Average ===\n")
testDispersion(sim_avg)
testZeroInflation(sim_avg)
testUniformity(sim_avg)
cat("\n=== COMPARISON: lm vs NB ===\n")
lm_max <- lm(Maximum ~ aprilT + melt_april, data = df)
lm_avg <- lm(Average ~ aprilT + may_T + melt_april, data = df)
cat("lm p-values (Maximum):\n");  print(tidy(lm_max) %>% filter(term != "(Intercept)") %>% select(term, p.value))
cat("NB p-values (Maximum):\n");  print(tidy(best_nb_max) %>% filter(term != "(Intercept)") %>% select(term, p.value))
cat("lm p-values (Average):\n");  print(tidy(lm_avg) %>% filter(term != "(Intercept)") %>% select(term, p.value))
cat("NB p-values (Average):\n");  print(tidy(best_nb_avg) %>% filter(term != "(Intercept)") %>% select(term, p.value))
library(performance)
library(dplyr)
cat("Nagelkerke R2 (Maximum):", round(r2_nagelkerke(best_nb_max), 3), "\n")
cat("Nagelkerke R2 (Average):", round(r2_nagelkerke(best_nb_avg), 3), "\n")
cat("\nlm p-values (Maximum):\n")
print(tidy(lm_max) %>% filter(term != "(Intercept)") %>% dplyr::select(term, p.value))
cat("\nNB p-values (Maximum):\n")
print(tidy(best_nb_max) %>% filter(term != "(Intercept)") %>% dplyr::select(term, p.value))