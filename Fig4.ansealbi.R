# ============================================================
# Goose abundance 1997-2025 (Temp-plot style)
# ============================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales) # for comma formatting on Y-axis
setwd("/***")
df <- read.csv2("Ansealbi.csv", header = TRUE, check.names = FALSE,
                fileEncoding = "UTF-8-BOM", dec = ",", sep = ";",
                na.strings = c("", "NA", " "),
                strip.white = TRUE,
                stringsAsFactors = FALSE)
names(df) <- c("Year", "Maximum", "Average")
df <- df %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.)))))
df_long <- df %>%
  pivot_longer(-Year, names_to = "Variable", values_to = "Value") %>%
  filter(!is.na(Value)) # Removes the NA rows for 2020-2022
pal <- c("Maximum" = "#D55E00",   # vermillion
         "Average" = "#56B4E9")   # sky blue
stats_tbl <- df_long %>%
  group_by(Variable) %>%
  summarise(
    slope = coef(lm(Value ~ Year))["Year"] * 10,        # birds per decade
    ci_lo = confint(lm(Value ~ Year))["Year", 1] * 10,
    ci_hi = confint(lm(Value ~ Year))["Year", 2] * 10,
    p     = summary(lm(Value ~ Year))$coefficients["Year", "Pr(>|t|)"],
    .groups = "drop"
  ) %>%
  mutate(
    p_txt = ifelse(p < 0.001, "< 0.001", sprintf("= %.3f", p)),
    # Using %.0f (no decimals) since abundance numbers are large
    label = sprintf("%s: slope = %.0f birds/decade, 95%% CI [%.0f; %.0f], p %s",
                    Variable, slope, ci_lo, ci_hi, p_txt)
  )
ann_df <- stats_tbl %>%
  mutate(
    y      = 37500 - (row_number() - 1) * 3500,
    x_icon = 1996.5,
    x_text = 1997.5
  )
p <- ggplot(df_long, aes(Year, Value, colour = Variable, fill = Variable)) +
  # Main data lines and points
  geom_line(aes(group = Variable), linewidth = 0.6) +
  geom_point(aes(shape = Variable), size = 2.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              alpha = 0.15, linewidth = 0.9, linetype = "dashed",
              show.legend = FALSE) +
 annotate("rect",
           xmin = 1995.5, xmax = 2024,
           ymin = min(ann_df$y) - 1800,
           ymax = max(ann_df$y) + 1800,
           fill = "white", colour = "grey60", linewidth = 0.3) +
  geom_point(data = ann_df,
             aes(x = x_icon, y = y, colour = Variable, shape = Variable),
             size = 3.8, inherit.aes = FALSE) +
  geom_text(data = ann_df,
            aes(x = x_text, y = y, label = label),
            hjust = 0, vjust = 0.5, size = 4,
            colour = "black", inherit.aes = FALSE) +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_shape_manual(values = c("Maximum" = 17,   # triangle
                                "Average" = 16)) + # circle
  coord_cartesian(xlim = c(1995, 2027), ylim = c(0, 40000)) +
  scale_y_continuous(labels = comma, breaks = seq(0, 30000, 10000)) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  labs(
    x = "Year",
    y = "Number of birds",
    colour = NULL, shape = NULL,
    ) +
  theme_bw(base_size = 15) +
  theme(
    legend.position = "none",                # legend is inside the block
    panel.grid.minor = element_blank(),
   )
print(p)
ggsave("abundance_trends.tiff", plot = p,
       device = "tiff", width = 10, height = 6.5, units = "in",
       dpi = 600, compression = "lzw")