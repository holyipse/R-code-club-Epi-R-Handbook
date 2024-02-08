battleground.data <- tibble(
  state = factor(
    rep(1:6, each = 2),
    levels = 1:6,
    labels = c("Nevada", "Georgia", "Arizona", "Michigan", "Pennsylvania", "Wisconsin")
  ),
  candidate = factor(
    rep(1:2, 6),
    levels = 1:2,
    labels = c("Biden", "Trump")
  ),
  percent = c(
    41, 52,
    43, 49,
    44, 49,
    43, 48,
    44, 48,
    47, 44
  ),
  label2 = ifelse(candidate == "Biden", paste0(round(percent), "%"), paste0(round(percent))),
  label1x = -20,
  label2x = -8
) %>%
  group_by(
    state
  ) %>%
  mutate(
    higher = factor(
      ifelse(percent == max(percent), 1, 2),
      levels = 1:2,
      labels = c("higher", "lower")
      ),
    fface = ifelse(percent == max(percent), "bold", "plain")
  ) %>%
  ungroup()

battleground.data %>%
  ggplot(
    aes(
      y = fct_rev(candidate),
      x = percent,
      fill = candidate
    )
  ) +
  geom_col() +
  geom_text(
    aes(
      x = label1x,
      label = candidate,
      hjust = 0,
      color = higher,
      fontface = fface
    )
  ) +
  geom_text(
    aes(
      x = label2x,
      label = label2,
      hjust = 0,
      color = higher,
      fontface = fface
    )
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(
    values = c(
      "Biden" = "#0080c3",
      "Trump" = "#da1e37"
    )
  ) +
  scale_color_manual(
    values = c(
      "lower" = "#969696",
      "higher" = "#000000"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.spacing.y = unit(2, "lines"),
    strip.text = element_text(
      hjust = 0,
      face = "bold",
      size = 16,
      margin = margin(0, 0, 5, 0),
      color = "#717171"
    )
  ) +
  facet_wrap(
    ~state,
    ncol = 2)
