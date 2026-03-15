# ============================================================
# Research Landscape Map
# AV Passenger Experience Literature Review
# ============================================================

required_packages <- c(
  "tidyverse",
  "janitor",
  "viridis",
  "gridExtra",
  "cowplot",
  "ggtext"
)

missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# ============================================================
# 1 Load dataset
# ============================================================

data <- read.csv("Data/TableLiterature.csv")
data <- clean_names(data)

# ============================================================
# 2 Clean platform names
# ============================================================

data$platform <- tolower(as.character(data$platform))

data$platform <- case_when(
  str_detect(data$platform, "sim") ~ "Simulator",
  str_detect(data$platform, "real") ~ "Real-world",
  TRUE ~ "Other"
)

# ============================================================
# 3 Count study combinations
# ============================================================

landscape_data <- data %>%
  filter(!is.na(scenario), !is.na(measurements), !is.na(platform)) %>%
  count(scenario, measurements, platform)

# ============================================================
# 4 Plot Research Landscape
# ============================================================
landscape_plot <- ggplot(
  landscape_data %>%
    mutate(
      measurements = stringr::str_wrap(
        as.character(measurements),
        width = 10,
        whitespace_only = FALSE
      ),
      scenario = stringr::str_wrap(
        as.character(scenario),
        width = 10,
        whitespace_only = FALSE
      )
    ),
  aes(
    x = measurements,
    y = scenario,
    size = n,
    color = platform
  )
) +
  geom_point(alpha = 0.8) +
  scale_size(range = c(3, 12)) +
  scale_color_manual(
    values = c(
      "Simulator" = "#4E79A7",
      "Real-world" = "#F28E2B",
      "Other" = "grey60"
    )
  ) +
  labs(
    title = NULL,
    x = "Measurement Type",
    y = "Study Scenario",
    size = "Number of Studies",
    color = "Platform"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(
      color = "black",
      size = 14,
      angle = 90,
      hjust = 0,
      vjust = 0.5,
      margin = margin(t = 2)
    ),
    
    axis.text.y = element_text(
      color = "black",
      size = 14,
      hjust = 0,
      vjust = 0.5,
      margin = margin(r = -2)
    ),
    
    axis.title.x = element_text(
      color = "black",
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(t = 12)
    ),
    
    axis.title.y = element_text(
      color = "black",
      face = "bold",
      size = 16,
      hjust = 0.5,
      margin = margin(r = 12)
    ),
    
    legend.position = "bottom",
    legend.direction = "vertical",
    legend.box = "vertical",
    
    legend.title = element_text(
      color = "black",
      face = "bold",
      size = 14
    ),
    
    legend.text = element_text(
      color = "black",
      size = 12
    ),
    
    panel.border = element_rect(
      color = "grey40",
      fill = NA,
      linewidth = 0.8
    ),
    
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
  ) +
  guides(
    size = guide_legend(
      title.position = "left",
      title.hjust = 0.5,
      nrow = 1,
      byrow = TRUE,
      order = 1
    ),
    color = guide_legend(
      title.position = "left",
      title.hjust = 0.5,
      nrow = 1,
      byrow = TRUE,
      order = 2,
    )
  )

landscape_plot

landscape_plot

# ============================================================
# 5 Save figure
# ============================================================

ggsave(
  "figures/research_landscape_map.png",
  landscape_plot,
  width = 8,
  height = 10,
  dpi = 300
)

ggsave(
  "figures/research_landscape_map.pdf",
  landscape_plot,
  width = 12,
  height = 6
)
