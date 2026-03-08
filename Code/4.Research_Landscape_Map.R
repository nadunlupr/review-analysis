# ============================================================
# Research Landscape Map
# AV Passenger Experience Literature Review
# ============================================================

library(tidyverse)
library(janitor)

# ============================================================
# 1 Load dataset
# ============================================================

data <- read.csv("Data/TableLiterature.csv")
data <- clean_names(data)

# ============================================================
# 2 Clean platform names (optional but recommended)
# ============================================================

data$platform <- tolower(data$platform)

data$platform <- case_when(
  str_detect(data$platform, "sim") ~ "Simulator",
  str_detect(data$platform, "real") ~ "Real-world",
  TRUE ~ "Other"
)

# ============================================================
# 3 Count study combinations
# ============================================================

landscape_data <- data %>%
  count(
    scenario,
    measurements,
    platform
  )

# ============================================================
# 4 Plot Research Landscape
# ============================================================

landscape_plot <- ggplot(
  landscape_data,
  aes(
    x = scenario,
    y = measurements,
    size = n,
    color = platform
  )
) +
  
  geom_point(alpha = 0.8) +
  
  scale_size(range = c(3,12)) +
  
  scale_color_manual(
    values = c(
      "Simulator" = "#4E79A7",
      "Real-world" = "#F28E2B",
      "Other" = "grey60"
    )
  ) +
  coord_flip() +
  labs(
    title = "Research Landscape of AV Passenger Experience Studies",
    x = "Study Scenario",
    y = "Measurement Type",
    size = "Number of Studies",
    color = "Platform"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    legend.position = "bottom"
  )

landscape_plot

# ============================================================
# 5 Save figure
# ============================================================

ggsave(
  "figures/research_landscape_map.pdf",
  landscape_plot,
  width = 10,
  height = 6
)