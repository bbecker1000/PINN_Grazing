# ============================================================
# RAW DATA PLOT: BARE GROUND BY TREATMENT AND YEAR
# ============================================================

# Summary statistics for overlay
BG_summary <- TargetData %>%
  filter(!is.na(BG_pct)) %>%
  group_by(Year.f, Treatment) %>%
  summarise(
    mean_bg = mean(BG_pct, na.rm = TRUE),
    se_bg   = sd(BG_pct, na.rm = TRUE) / sqrt(n()),
    n       = n(),
    .groups = "drop"
  )

# Raw data plot with mean ± SE overlay
p.BG_raw <- TargetData %>%
  filter(!is.na(BG_pct)) %>%
  ggplot(aes(x = Year.f, y = BG_pct, color = Treatment)) +
  # Raw jittered points
  geom_jitter(width = 0.2, alpha = 0.3, size = 1.5) +
  # Mean line
  geom_line(data = BG_summary,
            aes(x = Year.f, y = mean_bg,
                color = Treatment, group = Treatment),
            linewidth = 1) +
  # Mean point
  geom_point(data = BG_summary,
             aes(x = Year.f, y = mean_bg, color = Treatment),
             size = 3) +
  # SE ribbon
  geom_ribbon(data = BG_summary,
              aes(x    = Year.f,
                  y    = mean_bg,
                  ymin = mean_bg - se_bg,
                  ymax = mean_bg + se_bg,
                  fill = Treatment,
                  group = Treatment),
              alpha = 0.15, color = NA) +
  # Pre/post treatment line
  geom_vline(xintercept = 1.5, linetype = 4, color = "gray40") +
  scale_color_manual(values = subtreatment_colors) +
  scale_fill_manual(values  = subtreatment_colors, guide = "none") +
  theme_pinn +
  legend_bottom +
  ylab("Bare Ground Cover (%)") +
  xlab("Year") +
  annotate("text", x = 1.2, y = max(TargetData$BG_pct, na.rm = TRUE) * 0.95,
           label = "Pre", color = "gray40", size = 4, hjust = 1) +
  annotate("text", x = 1.8, y = max(TargetData$BG_pct, na.rm = TRUE) * 0.95,
           label = "Post", color = "gray40", size = 4, hjust = 0) +
  ggtitle("Bare Ground Cover by Treatment and Year (raw data)")

p.BG_raw
ggsave("Output/BareGround_raw.png", p.BG_raw,
       width = 20, height = 15, units = "cm", dpi = 300)
rm(p.BG_raw, BG_summary)
gc()