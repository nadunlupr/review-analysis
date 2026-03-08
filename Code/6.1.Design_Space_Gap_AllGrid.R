# ============================================================
# Design Space Research Opportunity Map (Final Clean Version)
# ============================================================

library(tidyverse)
library(janitor)
library(viridis)

# ============================================================
# 1 Load dataset
# ============================================================

data <- read.csv("Data/TableLiterature.csv")
data <- clean_names(data)

# ============================================================
# 2 Define Design Space Dimensions
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

all_dims <- c(experimental_dims, focus_dims, evaluation_dims)

# ============================================================
# 3 Convert to long format
# ============================================================

design_space_long <- data %>%
  pivot_longer(
    cols = all_dims,
    names_to = "dimension",
    values_to = "value"
  )

# ============================================================
# 4 Assign design space categories
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
# 5 Capitalize labels (consistent with other plots)
# ============================================================

design_space_long <- design_space_long %>%
  mutate(
    dimension = str_to_title(gsub("_", " ", dimension)),
    value = str_to_title(gsub("_", " ", value))
  )

# ============================================================
# 6 Count studies
# ============================================================

gap_grid <- design_space_long %>%
  count(category, dimension, value)

# ============================================================
# 7 Ensure missing combinations appear
# ============================================================

gap_grid <- gap_grid %>%
  complete(category, dimension, value, fill = list(n = 0))

# ============================================================
# 8 Sort values by frequency
# ============================================================

gap_grid <- gap_grid %>%
  group_by(dimension) %>%
  mutate(value = fct_reorder(value, n)) %>%
  ungroup()

# ============================================================
# 9 Plot Design Space Gap Map
# ============================================================

gap_plot <- ggplot(
  gap_grid,
  aes(
    x = value,
    y = dimension,
    fill = n
  )
) +
  
  geom_tile(color = "white") +
  
  geom_text(
    aes(label = n),
    size = 3,color = "white"
  ) +
  
  facet_grid(
    category ~ .,
    scales = "free_y",
    space = "free_y"
  ) +
  
  scale_fill_viridis() +
  
  labs(
    title = "Coverage of the Autonomous Vehicle Passenger Experience Design Space",
    x = "Dimension Value",
    y = "Design Space Dimension",
    fill = "Number of Studies"
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    strip.text.y = element_text(
      angle = 0,
      face = "bold",
      size = 12
    ),
    axis.text.x = element_text(
      angle = 90,
      hjust = 1
    ),
    panel.grid = element_blank()
  )

gap_plot

# ============================================================
# 10 Save figure
# ============================================================

dir.create("figures", showWarnings = FALSE)

ggsave(
  "figures/design_space_gap_map_final.pdf",
  gap_plot,
  width = 20,
  height = 18
)
