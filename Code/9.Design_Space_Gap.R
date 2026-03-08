# ============================================================
# Design Space Gap Map
# ============================================================

library(tidyverse)
library(janitor)
library(viridis)

# ============================================================
# Load dataset
# ============================================================

data <- read.csv("Data/TableLiterature.csv")
data <- clean_names(data)

# ============================================================
# Select key dimensions
# ============================================================

design_space <- data %>%
  select(
    platform,
    scenario,
    measurements
  )

# ============================================================
# Count observed combinations
# ============================================================

coverage <- design_space %>%
  count(platform, scenario, measurements)

# ============================================================
# Create full design space
# ============================================================

full_space <- coverage %>%
  complete(
    platform,
    scenario,
    measurements,
    fill = list(n = 0)
  )

# ============================================================
# Create Gap Map
# ============================================================

gap_plot <- ggplot(
  full_space,
  aes(
    x = scenario,
    y = platform,
    fill = n
  )
) +
  geom_tile(color = "white") +
  
  geom_text(
    aes(label = n),
    size = 4
  ) +
  
  facet_wrap(~ measurements) +
  
  scale_fill_viridis(
    option = "cividis"
  ) +
  
  labs(
    title = "Design Space Coverage of AV Passenger Experience Research",
    x = "Scenario",
    y = "Experimental Platform",
    fill = "Number of Studies"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

gap_plot

# ============================================================
# Save figure
# ============================================================

ggsave(
  "figures/design_space_gap_map.pdf",
  gap_plot,
  width = 12,
  height = 8
)

