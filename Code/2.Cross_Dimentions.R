# ======================================
# Cross-Dimension Analysis
# ======================================

library(tidyverse)
library(janitor)
library(grid)

# ======================================
# 1 Load dataset
# ======================================

data <- read.csv("Data/TableLiterature.csv")
data <- clean_names(data)

# Inspect
glimpse(data)

# ======================================
# 2 Variables used in design space
# ======================================

vars <- c(
  "platform",
  "driving_mode",
  "display_technology",
  "scenario",
  "measurement_domain",
  "controlled_sensory_focus",
  "participant_seating_position",
  "driver_presence_visibility",
  "cabin_structure_visibility",
  "motion_dof",
  "comfort_factor",
  "focus_condition"
)

# ======================================
# 3 Create output folder
# ======================================

dir.create("figures_cross", showWarnings = FALSE)

# ======================================
# 4 Variables where "+" means combined categories
# ======================================

split_plus_vars <- c("display_technology", "scenario", "measurement_domain", "focus_condition")

# ======================================
# 5 Professional variable-family color system
# ======================================

family_gradients <- list(
  tech = c("#DCEAF4", "#7FA6C9", "#2F4B7C"),
  immersive = c("#F3E8FF", "#B999E5", "#5B3C88"),
  context = c("#FDEBE2", "#F2A97E", "#C65D2E"),
  human = c("#E3F3EC", "#8FC9AE", "#2F6F4F"),
  attention = c("#FBE4EC", "#E78FB3", "#A63D6E"),
  structure = c("#ECEFF4", "#97A6BA", "#4A5A70"),
  motion = c("#E6F4F1", "#7FC8BE", "#2F7F77"),
  default = c("#E5ECF6", "#8FA9D6", "#345E9A")
)

variable_family <- c(
  platform = "tech",
  driving_mode = "tech",
  display_technology = "immersive",
  scenario = "context",
  measurement_domain = "attention",
  controlled_sensory_focus = "attention",
  participant_seating_position = "human",
  driver_presence_visibility = "human",
  cabin_structure_visibility = "structure",
  motion_dof = "motion",
  comfort_factor = "human",
  focus_condition = "attention"
)

get_variable_palette <- function(variable, n) {
  family_name <- variable_family[[variable]]
  
  if (is.null(family_name) || !(family_name %in% names(family_gradients))) {
    family_name <- "default"
  }
  
  base_cols <- family_gradients[[family_name]]
  grDevices::colorRampPalette(base_cols)(max(n, 2))
}

# ======================================
# 6 Helper formatting functions
# ======================================

pretty_var_name <- function(x) {
  stringr::str_to_title(gsub("_", " ", x))
}

pretty_value <- function(x, width = 12) {
  stringr::str_wrap(as.character(x), width = width, whitespace_only = FALSE)
}

get_text_color <- function(hex_color) {
  rgb <- grDevices::col2rgb(hex_color) / 255
  
  channel_luminance <- function(c) {
    ifelse(c <= 0.03928, c / 12.92, ((c + 0.055) / 1.055)^2.4)
  }
  
  r <- channel_luminance(rgb[1])
  g <- channel_luminance(rgb[2])
  b <- channel_luminance(rgb[3])
  
  luminance <- 0.2126 * r + 0.7152 * g + 0.0722 * b
  
  if (luminance > 0.5) "black" else "white"
}

# ======================================
# 6 Focus condition grouping helper
# ======================================

group_focus_condition <- function(x) {
  x_clean <- x %>%
    as.character() %>%
    stringr::str_trim() %>%
    stringr::str_to_lower()
  
  dplyr::case_when(
    x_clean %in% c("anxiety", "affectivity", "discomfort") ~ "Affective",
    x_clean %in% c("motion sickness") ~ "Physiology",
    x_clean %in% c("attention", "decision making", "mind wandering", "mind wondering") ~ "Cognitive",
    x_clean %in% c("communication", "sense of agency", "taking over control", "driving style") ~ "Interaction/Performance",
    TRUE ~ NA_character_
  )
}

# ======================================
# 7 Helper: prepare cross-dimension data
# ======================================

prepare_cross_data <- function(var1, var2) {
  
  df <- data %>%
    select(all_of(c(var1, var2))) %>%
    filter(
      !is.na(.data[[var1]]),
      !is.na(.data[[var2]]),
      .data[[var1]] != "",
      .data[[var2]] != ""
    )
  
  if (var1 %in% split_plus_vars) {
    df <- df %>%
      separate_rows(all_of(var1), sep = "\\s*\\+\\s*") %>%
      mutate(!!var1 := str_trim(.data[[var1]]))
  }
  
  if (var2 %in% split_plus_vars) {
    df <- df %>%
      separate_rows(all_of(var2), sep = "\\s*\\+\\s*") %>%
      mutate(!!var2 := str_trim(.data[[var2]]))
  }
  
  # Apply grouping to focus_condition after splitting
  if (var1 == "focus_condition") {
    df <- df %>%
      mutate(focus_condition = group_focus_condition(focus_condition)) %>%
      filter(!is.na(focus_condition))
  }
  
  if (var2 == "focus_condition") {
    df <- df %>%
      mutate(focus_condition = group_focus_condition(focus_condition)) %>%
      filter(!is.na(focus_condition))
  }
  
  df
}

# ======================================
# 9 Cross-dimension percentage plot
#    vertical_padding adds internal space
# ======================================

plot_cross_dimension_precent <- function(var1, var2, vertical_padding = NULL) {
  
  df <- prepare_cross_data(var1, var2)
  
  fill_levels <- df %>%
    distinct(.data[[var2]]) %>%
    pull(1) %>%
    as.character() %>%
    sort()
  
  fill_palette <- get_variable_palette(var2, length(fill_levels))
  names(fill_palette) <- fill_levels
  
  plot_df <- df %>%
    count(.data[[var1]], .data[[var2]]) %>%
    group_by(.data[[var1]]) %>%
    mutate(percent = n / sum(n)) %>%
    ungroup() %>%
    mutate(
      y_label = pretty_value(.data[[var1]], width = 20),
      fill_hex = fill_palette[as.character(.data[[var2]])],
      text_color = vapply(fill_hex, get_text_color, character(1))
    )
  
  legend_ncol <- 3
  legend_nrow <- ceiling(length(fill_levels) / legend_ncol)
  title_vjust_value <- if (legend_nrow > 1) 0.87 else 0.56
  
  p <- ggplot(
    plot_df,
    aes(
      x = percent,
      y = factor(y_label, levels = unique(y_label)),
      fill = .data[[var2]]
    )
  ) +
    geom_col(color = NA, key_glyph = draw_key_point) +
    geom_text(
      aes(
        label = scales::percent(percent, accuracy = 1),
        color = I(text_color)
      ),
      position = position_stack(vjust = 0.5),
      size = 5
    ) +
    scale_fill_manual(
      values = fill_palette,
      labels = pretty_value(fill_levels, width = 12)
    ) +
    scale_x_continuous(
      labels = scales::percent_format(),
      expand = expansion(mult = c(0.04, 0.04))
    ) +
    labs(
      x = "Percentage of Studies",
      y = pretty_var_name(var1),
      fill = ifelse(var2 == "measurement_domain", "Measurement", pretty_var_name(var2))
    ) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.4),
      axis.text.x = element_text(
        color = "black",
        size = 14,
        hjust = 0.5,
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
        color = "black",
        face = "bold",
        size = 14,
        vjust = title_vjust_value
      ),
      legend.text = element_text(
        color = "black",
        size = 12
      ),
      legend.key.width = unit(0.9, "lines"),
      legend.key.height = unit(0.9, "lines"),
      panel.border = element_rect(
        color = "grey40",
        fill = NA,
        linewidth = 0.8
      ),
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
    ) +
    guides(
      fill = guide_legend(
        title.position = "left",
        title.hjust = 0,
        title.vjust = title_vjust_value,
        direction = "horizontal",
        ncol = legend_ncol,
        byrow = TRUE,
        override.aes = list(
          shape = 22,
          size = 8,
          colour = NA,
          stroke = 0
        )
      )
    )
  
  if (!is.null(vertical_padding)) {
    p <- p + scale_y_discrete(
      drop = FALSE,
      expand = expansion(add = c(vertical_padding, vertical_padding))
    )
  }
  
  return(p)
}

# ======================================
# 10 Example plots in R viewer
# ======================================

plot_cross_dimension_precent("platform", "display_technology", vertical_padding = 0.8)
plot_cross_dimension_precent("scenario", "measurement_domain")
plot_cross_dimension_precent("driver_presence_visibility", "measurement_domain")
plot_cross_dimension_precent("focus_condition", "scenario")

# ======================================
# 13 Selected exports
# ======================================

ggsave(
  "figures_cross/Platform_V_Display.png",
  plot_cross_dimension_precent("platform", "display_technology", vertical_padding = 0.8),
  width = 7,
  height = 5.458,
  dpi = 600
)

ggsave(
  "figures_cross/Platform_V_Display.pdf",
  plot_cross_dimension_precent("platform", "display_technology", vertical_padding = 0.8),
  width = 7,
  height = 5.458,
  dpi = 600
)

ggsave(
  "figures_cross/Scenario_V_Measurements.png",
  plot_cross_dimension_precent("scenario", "measurement_domain"),
  width = 7,
  height = 5,
  dpi = 600
)

ggsave(
  "figures_cross/Scenario_V_Measurements.pdf",
  plot_cross_dimension_precent("scenario", "measurement_domain"),
  width = 7,
  height = 5,
  dpi = 600
)

ggsave(
  "figures_cross/Driver_Presence_V_Measurements.png",
  plot_cross_dimension_precent("driver_presence_visibility", "measurement_domain"),
  width = 7,
  height = 5,
  dpi = 600
)

ggsave(
  "figures_cross/driver_presence_v_measurements.pdf",
  plot_cross_dimension_precent("driver_presence_visibility", "measurement_domain"),
  width = 7,
  height = 5,
  dpi = 600
)

ggsave(
  "figures_cross/Focus_Condition_V_Scenario.png",
  plot_cross_dimension_precent("focus_condition", "scenario"),
  width = 7,
  height = 5.5,
  dpi = 600
)

ggsave(
  "figures_cross/Focus_Condition_V_Scenario.pdf",
  plot_cross_dimension_precent("focus_condition", "scenario"),
  width = 7,
  height = 6,
  dpi = 600
)
