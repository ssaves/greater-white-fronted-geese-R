library(dplyr)
library(broom)
make_s6_row <- function(model, model_name, n) {
  tidy(model, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(Model = model_name, N = n) %>%
    dplyr::select(Model, N, term, estimate, std.error, conf.low, conf.high, p.value)
}
s6 <- bind_rows(
  make_s6_row(m1, "M1: Weather + Hunting (all years)", nrow(df_peak)),
  make_s6_row(m2, "M2: Weather + Hunting (before 2014)", nrow(df_before)),
  make_s6_row(m3, "M3: Weather + Fire (before 2014)", nrow(df_fire)),
  make_s6_row(m4, "M4: Weather + Fire + Hunting (before 2014)", nrow(df_full))
) %>%
  mutate(across(c(estimate, std.error, conf.low, conf.high, p.value), ~ round(., 3)))
write.csv(s6, "Table_S6_Peak_Models.csv", row.names = FALSE, dec = ".")
print(as.data.frame(s6))