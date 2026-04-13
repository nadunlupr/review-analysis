# ======================================
# Sankey Diagram Analysis
# ======================================

packages <- c("tidyverse", "ggalluvial", "janitor")

missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) install.packages(missing_packages)

library(tidyverse)
library(ggalluvial)
library(janitor)

# ======================================
# 1 Load dataset
# ======================================

data <- read.csv("Data/TableLiterature.csv")

data <- clean_names(data)


# ======================================
# 3 Clean platform names
# ======================================

data$platform <- tolower(data$platform)

data$platform <- case_when(
  str_detect(data$platform, "sim") ~ "Simulator",
  str_detect(data$platform, "real") ~ "Real-world"
)

# ======================================
# 4 Convert all variables to factors
# ======================================

data <- data %>%
  mutate(across(c(
    platform,
    driving_mode,
    cabin_structure,
    cabin_visibility,
    motion_dof,
    display,
    focus_group,
    participant_seating_position,
    scenario,
    controlled_sensory_focus,
    driver_presence_visibility,
    comfort_factor,
    measurements,
    measurement_domain,
    focus_condition
  ), as.factor))

# ======================================
# 5 Platform colour scheme
# ======================================

platform_colors <- c(
  "Simulator" = "#4E79A7",
  "Real-world" = "#F28E2B"
)

# ======================================
# 6 Create output folder
# ======================================

dir.create("figures_sankey", showWarnings = FALSE)

# ======================================
# 7 Experimental Setup Sankey
# ======================================

exp_setup_plot <- ggplot(
  data,
  aes(
    axis1 = platform,
    axis2 = driving_mode,
    axis3 = cabin_structure,
    axis4 = cabin_visibility,
    axis5 = motion_dof,
    axis6 = display
  )
) +
  
  geom_alluvium(aes(fill = platform), width = 0.25, alpha = 0.85) +
  
  geom_stratum(width = 0.1, fill = "grey90", color = "black") +
  
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3
  ) +
  
  scale_x_discrete(
    limits = c("1","2","3","4","5","6"),
    labels = c(
      "Platform",
      "Driving Mode",
      "Cabin Structure",
      "Cabin Visibility",
      "Motion DOF",
      "Display"
    )
  ) +
  
  scale_fill_manual(values = platform_colors, drop = FALSE) +
  
  labs(
    x = NULL,
    y = "Number of Reviewed Studies",
    fill = "Platform"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

exp_setup_plot

ggsave(
  "figures_sankey/sankey_experimental_setup.pdf",
  exp_setup_plot,
  width = 11,
  height = 6
)

# ======================================
# 8 Study Focus Sankey
# ======================================

focus_plot <- ggplot(
  data,
  aes(
    axis1 = focus_group,
    axis2 = participant_seating_position,
    axis3 = scenario,
    axis4 = controlled_sensory_focus,
    axis5 = driver_presence_visibility
  )
) +
  
  geom_alluvium(aes(fill = platform), width = 0.25, alpha = 0.85) +
  
  geom_stratum(width = 0.3, fill = "grey90", color = "black") +
  
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3
  ) +
  
  scale_x_discrete(
    limits = c("1","2","3","4","5"),
    labels = c(
      "Focus Group",
      "Seating Position",
      "Scenario",
      "Sensory Focus",
      "Driver Presence"
    )
  ) +
  
  scale_fill_manual(values = platform_colors, drop = FALSE) +
  
  labs(
    x = NULL,
    y = "Number of Reviewed Studies",
    fill = "Platform"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

focus_plot

ggsave(
  "figures_sankey/sankey_study_focus.pdf",
  focus_plot,
  width = 11,
  height = 6
)

# ======================================
# 9 Evaluation Measures Sankey
# ======================================

evaluation_plot <- ggplot(
  data,
  aes(
    axis1 = comfort_factor,
    axis2 = measurements,
    axis3 = measurement_domain,
    axis4 = focus_condition
  )
) +
  
  geom_alluvium(aes(fill = platform), width = 0.25, alpha = 0.85) +
  
  geom_stratum(width = 0.3, fill = "grey90", color = "black") +
  
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3
  ) +
  
  scale_x_discrete(
    limits = c("1","2","3","4"),
    labels = c(
      "Comfort Factor",
      "Measurements",
      "Measurement Domain",
      "Focus Condition"
    )
  ) +
  
  scale_fill_manual(values = platform_colors, drop = FALSE) +
  
  labs(
    x = NULL,
    y = "Number of Reviewed Studies",
    fill = "Platform"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

evaluation_plot

ggsave(
  "figures_sankey/sankey_evaluation_measures.pdf",
  evaluation_plot,
  width = 10,
  height = 6
)


# ======================================
# 10 Sankey – FULL DESIGN SPACE
# ======================================

full_plot <- ggplot(
  data,
  aes(
    axis1 = platform,
    axis2 = driving_mode,
    axis3 = cabin_structure,
    axis4 = scenario,
    axis5 = measurements,
    axis6 = focus_condition
  )
) +
  
  geom_alluvium(aes(fill = platform), width = 0.25, alpha = 0.85) +
  
  geom_stratum(width = 0.3, fill = "grey90", color = "black") +
  
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 3) +
  
  scale_x_discrete(
    limits = c(
      "Platform",
      "Driving Mode",
      "Cabin Structure",
      "Scenario",
      "Measurement Type",
      "Focus Condition"
    ),
    expand = c(.05, .05)
  ) +
  
  scale_fill_manual(values = platform_colors) +
  
  labs(
    y = "Number of Reviewed Studies",
    fill = "Platform"
  ) +
  
  theme_minimal(base_size = 14)+
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank()
  )

full_plot

ggsave(
  "figures_sankey/sankey_full_design_space.pdf",
  full_plot,
  width = 12,
  height = 6
)
