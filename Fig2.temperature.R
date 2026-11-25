# ============================================================
# Average monthly temperatures 1997-2025
# ============================================================
library(ggplot2)
library(dplyr)
library(tidyr)
setwd("/***")
df <- read.csv2("averageT.csv", header = TRUE, check.names = FALSE,
                fileEncoding = "UTF-8-BOM", dec = ",", sep = ";")
names(df)[1] <- "Year"
df_long <- df %>%
  pivot_longer(-Year, names_to = "Month", values_to = "Temp") %>%
  mutate(Month = factor(Month, levels = c("March", "April", "May")))
deg <- "\u00B0C"   # degree Celsius, encoding-safe
stats_tbl <- df_long %>%
  group_by(Month) %>%
  summarise(
    slope = coef(lm(Temp ~ Year))["Year"] * 10,        # deg C per decade
    ci_lo = confint(lm(Temp ~ Year))["Year", 1] * 10,
    ci_hi = confint(lm(Temp ~ Year))["Year", 2] * 10,
    p     = summary(lm(Temp ~ Year))$coefficients["Year", "Pr(>|t|)"],
    .groups = "drop"
  ) %>%
  mutate(
    p_txt = ifelse(p < 0.001, "< 0.001", sprintf("= %.3f", p)),
    label = sprintf("%s: slope = %.2f%s/decade, 95%% CI [%.2f; %.2f], p %s",
                    Month, slope, deg, ci_lo, ci_hi, p_txt)
  )
ann_df <- stats_tbl %>%
  mutate(
    y      = 17.2 - (row_number() - 1) * 1.3,   # 17.2, 15.9, 14.6
    x_icon = 1996.7,                            # icon column
    x_text = 1997.6                             # text column
  )
pal <- c(March = "#E69F00",   # orange
         April = "#009E73",   # bluish green
         May   = "#0072B2")   # blue
p <- ggplot(df_long, aes(Year, Temp, colour = Month, fill = Month)) +
  geom_line(aes(group = Month), linewidth = 0.6) +
  geom_point(aes(shape = Month), size = 2.2) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              alpha = 0.15, linewidth = 0.9, linetype = "dashed",
              show.legend = FALSE) +
  # background rectangle for the stats block
  annotate("rect",
           xmin = 1996.0, xmax = 2023,
           ymin = min(ann_df$y) - 0.7,
           ymax = max(ann_df$y) + 0.7,
           fill = "white", colour = "grey60", linewidth = 0.3) +
  # month icons (same shape/colour as in the plot)
  geom_point(data = ann_df,
             aes(x = x_icon, y = y, colour = Month, shape = Month),
             size = 3.8, inherit.aes = FALSE) +
  # text next to icons
  geom_text(data = ann_df,
            aes(x = x_text, y = y, label = label),
            hjust = 0, vjust = 0.5, size = 4,
            colour = "black", inherit.aes = FALSE) +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_shape_manual(values = c(March = 15, April = 16, May = 17)) +
  coord_cartesian(ylim = c(-12, 18)) +
  labs(
    x = "Year",
    y = paste0("Average temperature (", deg, ")"),
    colour = "Month", fill = "Month", shape = "Month"
  ) +
  theme_bw(base_size = 15) +
  theme(legend.position = "none",              # <-- legend folded into the block
        panel.grid.minor = element_blank())
print(p)
ggsave("averageT_trends.tiff", plot = p,
       device = "tiff", width = 9, height = 6, units = "in",
       dpi = 600, compression = "lzw")