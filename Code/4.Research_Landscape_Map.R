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

data <- read.csv("Data/TableLiterature.csv", stringsAsFactors = FALSE)
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
# 3 Clean and expand scenario + measurement_domain
# ============================================================

data$scenario <- as.character(data$scenario)
data$measurement_domain <- as.character(data$measurement_domain)

data <- data %>%
  mutate(
    scenario = scenario %>%
      stringr::str_trim() %>%
      stringr::str_replace_all("\\s*\\+\\s*", "+"),
    measurement_domain = measurement_domain %>%
      stringr::str_trim() %>%
      stringr::str_replace_all("\\s*\\+\\s*", "+")
  ) %>%
  tidyr::separate_rows(scenario, sep = "\\+") %>%
  tidyr::separate_rows(measurement_domain, sep = "\\+") %>%
  mutate(
    scenario = case_when(
      scenario == "NDRT" ~ "NDRT",
      scenario == "Vigilance" ~ "Vigilance",
      TRUE ~ scenario
    ),
    measurement_domain = case_when(
      str_to_lower(measurement_domain) == "subjective" ~ "Subjective",
      str_to_lower(measurement_domain) == "behaviour" ~ "Behaviour",
      str_to_lower(measurement_domain) == "physiology" ~ "Physiology",
      TRUE ~ measurement_domain
    )
  )

# ============================================================
# 4 Count study combinations
# ============================================================

landscape_data <- data %>%
  filter(!is.na(scenario), scenario != "",
         !is.na(measurement_domain), measurement_domain != "",
         !is.na(platform), platform != "") %>%
  count(scenario, measurement_domain, platform)

# ============================================================
# 5 Plot Research Landscape
# ============================================================

landscape_plot <- ggplot(
  landscape_data %>%
    mutate(
      measurement_domain = stringr::str_wrap(
        as.character(measurement_domain),
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
    x = measurement_domain,
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
    x = "Measurement Domain",
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
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.justification = "center",
    
    legend.title = element_text(
      face = "bold",
      color = "black"
    ),
    
    legend.text = element_text(
      color = "black",
      size = 12
    ),
    
    legend.key.height = unit(0.9, "lines"),
    legend.key.width = unit(1.2, "lines"),
    
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
      title.hjust = 0,
      title.vjust = 0.5,
      direction = "horizontal",
      nrow = 1,
      byrow = TRUE,
      order = 1
    ),
    color = guide_legend(
      title.position = "left",
      title.hjust = 0,
      title.vjust = 1,
      direction = "horizontal",
      nrow = 1,
      byrow = TRUE,
      order = 2
    )
  )

landscape_plot

# ============================================================
# 6 Save figure
# ============================================================

ggsave(
  "figures/research_landscape_map.png",
  landscape_plot,
  width = 8,
  height =  10,
  dpi = 600
)

ggsave(
  "figures/research_landscape_map.pdf",
  landscape_plot,
  width = 8,
  height = 10,
  dpi = 600
)
