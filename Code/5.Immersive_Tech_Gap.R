# ============================================================
# Immersive Technology Gap Map
# ============================================================

library(tidyverse)
library(janitor)

# ============================================================
# 1 Load dataset
# ============================================================

data <- read.csv("Data/TableLiterature.csv")
data <- clean_names(data)

# ============================================================
# 2 Clean platform labels
# ============================================================

data$platform <- tolower(data$platform)

data$platform <- case_when(
  str_detect(data$platform,"sim") ~ "Simulator",
  str_detect(data$platform,"real") ~ "Real-world",
  TRUE ~ "Other"
)

# ============================================================
# 3 Count combinations
# ============================================================

immersive_map <- data %>%
  count(
    platform,
    display,
    measurement_domain
  )

# ============================================================
# 4 Plot Immersive Technology Landscape
# ============================================================

immersive_plot <- ggplot(
  immersive_map,
  aes(
    x = display,
    y = platform,
    size = n,
    color = platform
  )
) +
  
  geom_point(alpha = 0.8) +
  
  facet_wrap(~measurement_domain) +
  
  scale_size(range = c(3,12)) +
  
  scale_color_manual(
    values = c(
      "Simulator" = "#4E79A7",
      "Real-world" = "#F28E2B",
      "Other" = "grey60"
    )
  ) +
  
  labs(
    title = "Immersive Technology Research Landscape in AV Passenger Studies",
    x = "Display / Immersive Technology",
    y = "Experimental Platform",
    size = "Number of Studies",
    color = "Platform"
  ) +
  theme_minimal(base_size = 14) +
  
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "bottom"
  )

immersive_plot

# ============================================================
# 5 Save figure
# ============================================================

ggsave(
  "figures/immersive_research_landscape.pdf",
  immersive_plot,
  width = 10,
  height = 6
)