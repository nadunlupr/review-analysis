# ============================================================
# Design Space Gap Grid
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
# Define Design Space Dimensions
# ============================================================

experimental_dims <- c(
  "platform",
  "driving_mode",
  "cabin_structure",
  "cabin_visibility",
  "motion_dof",
  "display"
)

focus_dims <- c(
  "focus_group",
  "participant_seating_position",
  "scenario",
  "controlled_sensory_focus",
  "driver_presence_visibility"
)

evaluation_dims <- c(
  "comfort_factor",
  "measurements",
  "measurement_domain",
  "focus_condition"
)

# ============================================================
# Convert to long format
# ============================================================

design_space_long <- data %>%
  pivot_longer(
    cols = c(
      experimental_dims,
      focus_dims,
      evaluation_dims
    ),
    names_to = "dimension",
    values_to = "value"
  )

# ============================================================
# Assign category labels
# ============================================================

design_space_long <- design_space_long %>%
  mutate(
    category = case_when(
      dimension %in% experimental_dims ~ "Experimental Setup",
      dimension %in% focus_dims ~ "Study Focus",
      dimension %in% evaluation_dims ~ "Evaluation Measures"
    )
  )

# ============================================================
# Count studies per dimension value
# ============================================================

gap_grid <- design_space_long %>%
  count(category, dimension, value)

# ============================================================
# Plot Gap Grid
# ============================================================

gap_grid_plot <- ggplot(
  gap_grid,
  aes(
    x = category,
    y = value,
    fill = n
  )
) +
  
  geom_tile(color = "white") +
  
  geom_text(
    aes(label = n),
    size = 4
  ) +
  
  facet_wrap(~dimension, scales = "free_y") +
  
  scale_fill_viridis(option = "cividis") +
  
  labs(
    title = "Coverage of Design Space Dimensions in AV Passenger Experience Studies",
    x = "Design Space Category",
    y = "Dimension Value",
    fill = "Number of Studies"
  ) +
  
  theme_minimal(base_size = 7) +
  
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(face = "bold")
  )

gap_grid_plot

# ============================================================
# Save figure
# ============================================================

ggsave(
  "figures/design_space_gap_grid.pdf",
  gap_grid_plot,
  width = 12,
  height = 10
)