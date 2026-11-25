library(dplyr)
library(broom)
make_s5 <- function(model, response_name) {
  tidy(model, conf.int = TRUE, exponentiate = FALSE) %>%
    mutate(
      IRR = exp(estimate),
      IRR_low = exp(conf.low),
      IRR_high = exp(conf.high),
      Response = response_name
    ) %>%
    dplyr::select(Response, term, estimate, std.error, IRR, IRR_low, IRR_high, p.value)
}
table_s5 <- bind_rows(
  make_s5(best_nb_max, "Maximum"),
  make_s5(best_nb_avg, "Average")
) %>%
  mutate(across(c(estimate, std.error, IRR, IRR_low, IRR_high, p.value), ~ round(., 4)))
write.csv(table_s5, "Table_S5_NB_GLM.csv", row.names = FALSE)
print(as.data.frame(table_s5))