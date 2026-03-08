# ============================================================
# Design Space Coverage Matrix
# Autonomous Vehicle Passenger Experience Literature Review
# ============================================================

library(tidyverse)
library(janitor)
library(viridis)
library(gridExtra)
library(cowplot)

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
  matrix_platform_display,
  aes(x = display, y = platform, fill = n)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 3, color = "white") +
  scale_fill_viridis() +
  labs(
    title = "Platform vs Display Technology",
    x = "Display Technology",
    y = "Experimental Platform",
    fill = "Number of Studies"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1)
  )

# ============================================================
# 4 Scenario × Measurement Type Matrix
# ============================================================

matrix_scenario_measurement <- data %>%
  count(scenario, measurements) %>%
  complete(scenario, measurements, fill = list(n = 0))

plot_scenario_measurement <- ggplot(
  matrix_scenario_measurement,
  aes(x = measurements, y = scenario, fill = n)
) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 3, color = "white") +
  scale_fill_viridis() +
  labs(
    title = "Scenario vs Measurement Type",
    x = "Measurement Type",
    y = "Scenario",
    fill = "Number of Studies"
  ) +
  theme_minimal(base_size = 10)

# plot_scenario_measurement

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
# 6 Combine all matrices
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

