# ============================================================
# Goose congregations 1997-2025 (DOY scale, temp-plot style)
# ============================================================
library(ggplot2)
library(dplyr)
library(tidyr)
setwd("/***")
df <- read.csv2("Congregations.csv", header = TRUE, check.names = FALSE,
                fileEncoding = "UTF-8-BOM", dec = ",", sep = ";",
                na.strings = c("", "NA", " "),
                strip.white = TRUE,
                stringsAsFactors = FALSE)
names(df) <- c("Year", "First", "Max", "Last")
df <- df %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.)))))
df_long <- df %>%
  pivot_longer(-Year, names_to = "Stage", values_to = "Day") %>%
  filter(!is.na(Day)) %>%
  mutate(Stage = factor(Stage,
                        levels = c("First", "Max", "Last"),
                        labels = c("First >10000", "Maximum", "Last >10000")))
pal <- c("First >10000" = "#009E73",   # bluish green
         "Maximum"     = "#E69F00",   # orange
         "Last >10000" = "#000000")   # black
stats_tbl <- df_long %>%
  group_by(Stage) %>%
  summarise(
    slope = coef(lm(Day ~ Year))["Year"] * 10,        # days per decade
    ci_lo = confint(lm(Day ~ Year))["Year", 1] * 10,
    ci_hi = confint(lm(Day ~ Year))["Year", 2] * 10,
    p     = summary(lm(Day ~ Year))$coefficients["Year", "Pr(>|t|)"],
    .groups = "drop"
  ) %>%
  mutate(
    p_txt = ifelse(p < 0.001, "< 0.001", sprintf("= %.3f", p)),
    label = sprintf("%s: slope = %.2f days/decade, 95%% CI [%.2f; %.2f], p %s",
                    Stage, slope, ci_lo, ci_hi, p_txt)
  )
ann_df <- stats_tbl %>%
  mutate(
    y      = 152 - (row_number() - 1) * 2.8,
    x_icon = 1996.5,
    x_text = 1997.5
  )
p <- ggplot(df_long, aes(Year, Day, colour = Stage, fill = Stage)) +
  # Main data lines and points
  geom_line(aes(group = Stage), linewidth = 0.6) +
  geom_point(aes(shape = Stage), size = 2.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              alpha = 0.15, linewidth = 0.9, linetype = "dashed",
              show.legend = FALSE) +
  annotate("rect",
           xmin = 1995.5, xmax = 2024,
           ymin = min(ann_df$y) - 1.2,
           ymax = max(ann_df$y) + 1.2,
           fill = "white", colour = "grey60", linewidth = 0.3) +
  geom_point(data = ann_df,
             aes(x = x_icon, y = y, colour = Stage, shape = Stage),
             size = 3.8, inherit.aes = FALSE) +
  geom_text(data = ann_df,
            aes(x = x_text, y = y, label = label),
            hjust = 0, vjust = 0.5, size = 4,
            colour = "black", inherit.aes = FALSE) +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_shape_manual(values = c("First >10000" = 16,   # circle
                                "Maximum"     = 17,   # triangle
                                "Last >10000" = 15)) + # square
  coord_cartesian(xlim = c(1995, 2027), ylim = c(105, 155)) +
  scale_y_continuous(breaks = seq(110, 150, 10)) +
  scale_x_continuous(breaks = seq(1995, 2025, 5)) +
  labs(
    x = "Year",
    y = "Day of year (DOY)",
    colour = NULL, shape = NULL,
    ) +
 theme_bw(base_size = 15) +
  theme(
    legend.position = "none",                # legend is now inside the block
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, colour = "grey30")
  )
print(p)
ggsave("congregations_trends.tiff", plot = p,
       device = "tiff", width = 10, height = 6.5, units = "in",
       dpi = 600, compression = "lzw")