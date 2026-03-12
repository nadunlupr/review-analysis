# ============================================================
# Design Space Coverage Matrix
# Autonomous Vehicle Passenger Experience Literature Review
# ============================================================
required_packages <- c(
  "tidyverse",
  "janitor",
  "viridis",
  "gridExtra",
  "cowplot",
  "grid"
)

missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

library(tidyverse)
library(janitor)
library(viridis)
library(gridExtra)
library(cowplot)
library(grid)

# ============================================================
# 1 Load dataset
# ============================================================

data <- read.csv("Data/TableLiterature.csv")

data <- clean_names(data)

# ============================================================
# 2 Convert variables to factors
# ============================================================

data <- data %>%
  mutate(
    platform = as.factor(platform),
    display = as.factor(display),
    scenario = as.factor(scenario),
    measurements = as.factor(measurements),
    measurement_domain = as.factor(measurement_domain)
  )

# ============================================================
# 3 Platform × Display Technology Matrix
# ============================================================

matrix_platform_display <- data %>%
  count(platform, display) %>%
  complete(platform, display, fill = list(n = 0))

plot_platform_display <- ggplot(
  matrix_platform_display %>%
    mutate(
      platform = stringr::str_wrap(
        as.character(platform),
        width = 12,
        whitespace_only = FALSE
      ),
      display = stringr::str_wrap(
        as.character(display),
        width = 10,
        whitespace_only = FALSE
      )
    ),
  aes(x = display, y = platform, fill = n)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 6, color = "white") +
  scale_fill_viridis() +
  labs(
    title = NULL,
    x = "Display Technology",
    y = "Experimental Platform",
    fill = "Number of Studies"
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
    legend.direction = "horizontal",
    legend.title.position = "left",
    legend.text.position = "bottom",
    
    legend.title = element_text(
      color = "black",
      face = "bold",
      size = 14,
      vjust = 0,
      margin = margin(r = 10, b = 21)
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
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = grid::unit(4, "cm"),
      barheight = grid::unit(0.45, "cm")
    )
  )
plot_platform_display
# ============================================================
# 4 Scenario × Measurement Type Matrix
# ============================================================

matrix_scenario_measurement <- data %>%
  count(scenario, measurements) %>%
  complete(scenario, measurements, fill = list(n = 0))

plot_scenario_measurement <- ggplot(
  matrix_scenario_measurement %>%
    mutate(
      scenario = stringr::str_wrap(
        as.character(scenario),
        width = 12,
        whitespace_only = FALSE
      ),
      measurements = stringr::str_wrap(
        as.character(measurements),
        width = 12,
        whitespace_only = FALSE
      )
    ),
  aes(x = measurements, y = scenario, fill = n)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 6, color = "white") +
  scale_fill_viridis() +
  labs(
    title = NULL,
    x = "Measurement Type",
    y = "Scenario",
    fill = "Number of Studies"
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
    legend.direction = "horizontal",
    legend.title.position = "left",
    legend.text.position = "bottom",
    
    legend.title = element_text(
      color = "black",
      face = "bold",
      size = 14,
      vjust = 0,
      margin = margin(r = 15, b = 21)
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
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = grid::unit(4, "cm"),
      barheight = grid::unit(0.35, "cm")
    )
  )

 plot_scenario_measurement

# ============================================================
# 5 Platform × Measurement Domain Matrix
# ============================================================

matrix_platform_measurement <- data %>%
  count(platform, measurement_domain) %>%
  complete(platform, measurement_domain, fill = list(n = 0))

plot_platform_measurement <- ggplot(
  matrix_platform_measurement,
  aes(x = measurement_domain, y = platform, fill = n)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 3, color = "white") +
  scale_fill_viridis() +
  labs(
    title = "Platform vs Measurement Domain",
    x = "Measurement Domain",
    y = "Experimental Platform",
    fill = "Number of Studies"
  ) +
  theme_minimal(base_size = 10)

# plot_platform_measurement
# ============================================================
# 6 Participant Seating Position × Cabin Visibility Matrix
# ============================================================

matrix_seating_position_cabin_visibility <- data %>%
  count(participant_seating_position, cabin_visibility) %>%
  complete(participant_seating_position, cabin_visibility, fill = list(n = 0))

plot_seating_position_cabin_visibility <- ggplot(
  matrix_platform_measurement %>%
    mutate(
      participant_seating_position = stringr::str_wrap(
        as.character(participant_seating_position),
        width = 12,
        whitespace_only = FALSE
      ),
      cabin_visibility = stringr::str_wrap(
        as.character(cabin_visibility),
        width = 10,
        whitespace_only = FALSE
      )
    ),
  aes(x = cabin_visibility, y = participant_seating_position, fill = n)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 6, color = "white") +
  scale_fill_viridis() +
  labs(
    title = NULL,
    x = "Cabin Visibility",
    y = "Participant Seating Position",
    fill = "Number of Studies"
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
    legend.direction = "horizontal",
    legend.title.position = "left",
    legend.text.position = "bottom",
    
    legend.title = element_text(
      color = "black",
      face = "bold",
      size = 14,
      vjust = 0,
      margin = margin(r = 10, b = 21)
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
    fill = guide_colorbar(
      direction = "horizontal",
      barwidth = grid::unit(4, "cm"),
      barheight = grid::unit(0.45, "cm")
    )
  )

plot_seating_position_cabin_visibility
# ============================================================
# 7 Combine all matrices
# ============================================================

coverage_matrix_plot1 <- grid.arrange(
  plot_platform_display,
  plot_scenario_measurement,
  plot_platform_measurement,
  ncol = 1
)

# coverage_matrix_plot3 <- plot_grid(
#   plot_platform_display,
#   plot_scenario_measurement,
#   plot_platform_measurement,
#   labels = c("A", "B", "C"),
#   ncol = 1
# )
# coverage_matrix_plot3
# ============================================================
# 7 Save figure
# ============================================================

ggsave(
  "figures/design_space_coverage_matrix.pdf",
  coverage_matrix_plot,
  width = 10,
  height = 12
)

plot_platform_display
ggsave(
  "figures/platform_display_coverage.png",
  plot_platform_display,
  width = 10,
  height = 12,
  dpi = 300
)

plot_scenario_measurement
ggsave(
  "figures/scenario_measurements_coverage.png",
  plot_scenario_measurement,
  width = 10,
  height = 12,
  dpi = 300
)

plot_seating_position_cabin_visibility
ggsave(
  "figures/seating_position_cabin_visibility_coverage.png",
  plot_seating_position_cabin_visibility,
  width = 10,
  height = 12,
  dpi = 300
)

