library(tidyverse)
library(ggtext)

poll.data <- tibble(
  statement = factor(
    rep(1:3, each = 4),
    levels = 1:3,
    labels = c(
      "... is too old to be president",
      "... does not have the mental sharpness to be president",
      "... does not have the temperament to be president"
    )
  ),
  year = rep(c(2020, 2023), 6),
  candidate = factor(
    rep(c(rep(1, 2), rep(2, 2)), 3),
    levels = 1:2,
    labels = c("Biden", "Trump")
  ),
  share = c(
    34, 71,
    18, 39,
    45, 62,
    48, 44,
    39, 51,
    58, 55
  )
) %>%
  # Create new variable to bold "Biden" and keep "Trump" plain
  mutate(
    fface = ifelse(candidate == "Biden", "bold", "plain")
  )

poll.data %>%
  
  # Call ggplot ----------------------------------------------------------------
  ggplot(
    
    # Define aesthetics --------------------------------------------------------
    aes(
      x = year,
      y = share,
      
      # Reversing the factor ensures that the lines for Biden will be on top
      color = fct_rev(candidate) 
    )
  ) +
  
  # Plot lines -----------------------------------------------------------------
  # Note that aesthetics were defined previously
  geom_line(
    linewidth = 1.2
  ) +
  
  # Add blank background layer for labels --------------------------------------
  # Note that we are adding a new aesthetic but that the others carry forward
  # We are not adding the text here because some of the labels are too close;
  # We'll add them as a separate text layer next
  geom_label(
    aes(
      label = share
    ),
    fill = "#f7f6f4",
    color = NA,
    label.size = NA
  ) +
  
  # Add text labels for shares -------------------------------------------------
  geom_text(
    aes(
      label = share,
      fontface = fface
    ),
  ) +
  
  # Add text labels for candidates ---------------------------------------------
  geom_text(
    
    ## Create new summary data for candidate labels -----------------------------
    data = poll.data %>%
      group_by(
        statement, candidate, fface
      ) %>%
      summarize(
        year = sum(year) / 2,
        share_diff = abs(diff(share)),
        share = sum(share) / 2
      ) %>%
      group_by(
        statement
      ) %>%
      mutate(
        vjust = ifelse(share == max(share), -1 - (share_diff * 0.05), 2 + (share_diff * 0.05))
      ) %>%
      ungroup(),
    aes(
      label = candidate,
      fontface = fface,
      vjust = vjust
    )
  ) +
  
  # Specify limits for y axis --------------------------------------------------
  # Define a lower limit for the y axis and allow natural maximum
  scale_y_continuous(
    limits = c(0, NA)
    ) +
  
  # Specify values for x axis and expand ---------------------------------------
  # Display only the relevant years and add expansion to allow room for labels
  scale_x_continuous(
    breaks = c(2020, 2023), 
    expand = expansion(mult = 0.1)
    ) +

  # Define colors --------------------------------------------------------------
  scale_color_manual(
    values = c(
      "Biden" = "#2e7eaf",
      "Trump" = "#b9b9b9"
      )
  ) +

  # Add title and caption ------------------------------------------------------
  labs(
    title = "Share Who Think Each Candidate ...",
    caption = "Based on New York times/Siena College polls of registered voters in battleground states conducted Oct. 22 to Nov. 3 and Times/Siena polls in 2020."
  ) +
  
  # Start with a minimal theme -------------------------------------------------
  theme_minimal() +
  
  # Customize theme ------------------------------------------------------------
  theme(
    
    # Remove legend
    legend.position = "none",
    
    # Remove y axis labeling
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    # Remove x axis title
    axis.title.x = element_blank(),
    
    # Add x axis line
    axis.line.x = element_line(
      color = "#5b5b59",
      linewidth = 1
      ),
    
    # Remove panel grid lines
    panel.grid = element_blank(),
    
    # Shade panel and remove border
    panel.background = element_rect(
      fill = "#f7f6f4", 
      color = NA
      ),
    
    # Add space between panels
    panel.spacing = unit(2, "lines"),
    
    # Allow word wrapping in panel title
    strip.text = element_textbox_simple(
      vjust = 0,
      margin = margin(20, 0, 10, 0)
      ),
    
    # Define caption text color and add padding
    plot.caption = element_textbox_simple(
      color = "#848d94",
      margin = margin(20, 15, 5, 15)
      )
  ) +
  
  # Create separate plots by statement
  facet_wrap(~statement)
