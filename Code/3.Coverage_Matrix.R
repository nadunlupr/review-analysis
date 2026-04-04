# ============================================================
# Design Space Coverage Matrix
# Autonomous Vehicle Passenger Experience Literature Review
# ============================================================

required_packages <- c(
  "tidyverse",
  "janitor",
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
library(gridExtra)
library(cowplot)
library(grid)

# ============================================================
# 1 Load dataset
# ============================================================

data <- read.csv("Data/TableLiterature.csv", stringsAsFactors = FALSE)
data <- clean_names(data)

# ============================================================
# 2 Helpers
# ============================================================

wrap_value <- function(x, width = 12) {
  stringr::str_wrap(as.character(x), width = width, whitespace_only = FALSE)
}

get_text_color_from_fill <- function(fill_hex) {
  rgb <- grDevices::col2rgb(fill_hex) / 255
  
  channel_luminance <- function(c) {
    ifelse(c <= 0.03928, c / 12.92, ((c + 0.055) / 1.055)^2.4)
  }
  
  r <- channel_luminance(rgb[1])
  g <- channel_luminance(rgb[2])
  b <- channel_luminance(rgb[3])
  
  luminance <- 0.2126 * r + 0.7152 * g + 0.0722 * b
  
  if (luminance > 0.5) "black" else "white"
}

split_plus_values <- function(x, protected_values = character()) {
  x <- as.character(x)
  
  if (length(x) == 0 || is.na(x) || stringr::str_squish(x) == "") {
    return(NA_character_)
  }
  
  x_clean <- stringr::str_squish(x)
  x_clean <- stringr::str_replace_all(x_clean, "\\s*\\+\\s*", " + ")
  
  protected_clean <- stringr::str_squish(protected_values)
  protected_clean <- stringr::str_replace_all(protected_clean, "\\s*\\+\\s*", " + ")
  
  if (x_clean %in% protected_clean) {
    return(x_clean)
  }
  
  split_vals <- stringr::str_split(
    stringr::str_replace_all(x_clean, "\\s*\\+\\s*", "+"),
    "\\+"
  )[[1]]
  
  stringr::str_squish(split_vals)
}

expand_plus_column <- function(df, col_name, protected_values = character()) {
  df[[col_name]] <- purrr::map(df[[col_name]], split_plus_values, protected_values = protected_values)
  tidyr::unnest_longer(df, all_of(col_name))
}

prepare_matrix_data <- function(df, cols, protected_map = list()) {
  out <- df %>% select(all_of(cols))
  
  for (col_name in cols) {
    protected_values <- protected_map[[col_name]]
    if (is.null(protected_values)) {
      protected_values <- character()
    }
    out <- expand_plus_column(out, col_name, protected_values = protected_values)
  }
  
  out %>%
    filter(if_all(everything(), ~ !is.na(.) & stringr::str_squish(as.character(.)) != ""))
}

set_panel_height <- function(p, panel_height = NULL, unit_type = "cm") {
  if (is.null(panel_height)) {
    return(p)
  }
  
  g <- if (inherits(p, "gtable")) p else ggplotGrob(p)
  
  panel_rows <- unique(g$layout$t[g$layout$name == "panel"])
  g$heights[panel_rows] <- grid::unit(panel_height, unit_type)
  
  g
}

# ============================================================
# 3 Professional muted gradients
# ============================================================

heatmap_gradients <- list(
  platform_display = c("#F3E8FF", "#B999E5", "#5B3C88"),
  scenario_measurements = c("#FBE4EC", "#E78FB3", "#A63D6E"),
  platform_measurement = c("#DCEAF4", "#7FA6C9", "#2F4B7C"),
  seating_cabin = c("#ECEFF4", "#97A6BA", "#4A5A70")
)

# ============================================================
# 4 Shared heatmap builder
# ============================================================

make_heatmap <- function(df, x_var, y_var, fill_title, gradient_cols,
                         x_label, y_label,
                         x_wrap = 10, y_wrap = 12,
                         tile_text_size = 6,
                         legend_barheight = 0.45,
                         panel_height = NULL,
                         panel_height_unit = "cm",
                         x_title_gap = NULL,
                         legend_top_gap = NULL,
                         bottom_plot_margin = NULL) {
  
  df_full <- df %>%
    count(.data[[y_var]], .data[[x_var]], name = "n") %>%
    tidyr::complete(.data[[y_var]], .data[[x_var]], fill = list(n = 0))
  
  max_n <- max(df_full$n, na.rm = TRUE)
  fill_fun <- grDevices::colorRampPalette(gradient_cols)
  fill_lookup <- fill_fun(max(max_n, 1) + 1)
  
  df_plot <- df_full %>%
    mutate(
      x_label = wrap_value(.data[[x_var]], width = x_wrap),
      y_label = wrap_value(.data[[y_var]], width = y_wrap),
      fill_hex = fill_lookup[n + 1],
      text_color = vapply(fill_hex, get_text_color_from_fill, character(1))
    )
  
  if (is.null(x_title_gap)) {
    x_title_gap <- 12
  }
  
  if (is.null(legend_top_gap)) {
    legend_top_gap <- 0
  }
  
  if (is.null(bottom_plot_margin)) {
    bottom_plot_margin <- 10
  }
  
  p <- ggplot(
    df_plot,
    aes(
      x = x_label,
      y = y_label,
      fill = n
    )
  ) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(
      aes(
        label = n,
        color = I(text_color)
      ),
      size = tile_text_size,
    ) +
    scale_fill_gradientn(
      colours = gradient_cols,
      limits = c(0, max_n),
      breaks = pretty(c(0, max_n), n = 5)
    ) +
    labs(
      title = NULL,
      x = x_label,
      y = y_label,
      fill = fill_title
    ) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      
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
        margin = margin(t = x_title_gap)
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
      legend.title.position = "left",
      
      legend.title = element_text(
        color = "black",
        face = "bold",
        size = 14,
        vjust = 0.7,
        margin = margin(r = 12, b = 8)
      ),
      
      legend.text = element_text(
        color = "black",
        size = 12
      ),
      
      legend.margin = margin(t = legend_top_gap),
      
      panel.border = element_rect(
        color = "grey40",
        fill = NA,
        linewidth = 0.8
      ),
      
      plot.margin = margin(t = 10, r = 20, b = bottom_plot_margin, l = 10)
    ) +
    guides(
      fill = guide_colorbar(
        title.position = "left",
        title.hjust = 0,
        title.vjust = 0.7,
        direction = "horizontal",
        barwidth = grid::unit(4, "cm"),
        barheight = grid::unit(legend_barheight, "cm")
      )
    )
  
  p <- set_panel_height(
    p,
    panel_height = panel_height,
    unit_type = panel_height_unit
  )
  
  return(p)
}

# ============================================================
# 5 Build expanded datasets
#    Split all "+" combos except "Subjective + Objective"
# ============================================================

protected_map <- list(
  measurements = "Subjective + Objective"
)

data_platform_display <- prepare_matrix_data(
  data,
  cols = c("platform", "display"),
  protected_map = protected_map
)

data_scenario_measurements <- prepare_matrix_data(
  data,
  cols = c("scenario", "measurement_domain"),
  protected_map = protected_map
)

data_platform_measurement <- prepare_matrix_data(
  data,
  cols = c("platform", "measurement_domain"),
  protected_map = protected_map
)

data_seating_cabin <- prepare_matrix_data(
  data,
  cols = c("participant_seating_position", "cabin_structure_visibility"),
  protected_map = protected_map
)

# ============================================================
# 6 Optional shared layout controls
# ============================================================

common_panel_height <- 18
common_x_title_gap <- NULL
common_legend_top_gap <- NULL
common_bottom_plot_margin <- NULL

# ============================================================
# 7 Create plots
# ============================================================

plot_platform_display <- make_heatmap(
  df = data_platform_display,
  x_var = "display",
  y_var = "platform",
  fill_title = "Number of Studies",
  gradient_cols = heatmap_gradients$platform_display,
  x_label = "Display Technology",
  y_label = "Experimental Platform",
  x_wrap = 12,
  y_wrap = 10,
  tile_text_size = 6,
  legend_barheight = 0.45,
  panel_height = common_panel_height,
  x_title_gap = 12.6,
  legend_top_gap = common_legend_top_gap,
  bottom_plot_margin = common_bottom_plot_margin
)

plot_scenario_measurement <- make_heatmap(
  df = data_scenario_measurements,
  x_var = "measurement_domain",
  y_var = "scenario",
  fill_title = "Number of Studies",
  gradient_cols = heatmap_gradients$scenario_measurements,
  x_label = "Measurement Domain",
  y_label = "Scenario",
  x_wrap = 12,
  y_wrap = 12,
  tile_text_size = 6,
  legend_barheight = 0.45,
  panel_height = common_panel_height,
  x_title_gap = common_x_title_gap,
  legend_top_gap = common_legend_top_gap,
  bottom_plot_margin = common_bottom_plot_margin
)

plot_platform_measurement <- make_heatmap(
  df = data_platform_measurement,
  x_var = "measurement_domain",
  y_var = "platform",
  fill_title = "Number of Studies",
  gradient_cols = heatmap_gradients$platform_measurement,
  x_label = "Measurement Domain",
  y_label = "Experimental Platform",
  x_wrap = 12,
  y_wrap = 12,
  tile_text_size = 6,
  legend_barheight = 0.45,
  panel_height = common_panel_height,
  x_title_gap = common_x_title_gap,
  legend_top_gap = common_legend_top_gap,
  bottom_plot_margin = common_bottom_plot_margin
)

plot_seating_position_cabin_visibility <- make_heatmap(
  df = data_seating_cabin,
  x_var = "cabin_structure_visibility",
  y_var = "participant_seating_position",
  fill_title = "Number of Studies",
  gradient_cols = heatmap_gradients$seating_cabin,
  x_label = "Cabin Structure Visibility",
  y_label = "Participant Seating Position",
  x_wrap = 10,
  y_wrap = 12,
  tile_text_size = 6,
  legend_barheight = 0.45,
  panel_height = common_panel_height,
  x_title_gap = 22,
  legend_top_gap = common_legend_top_gap,
  bottom_plot_margin = common_bottom_plot_margin
)

# ============================================================
# 8 Combine selected matrices
# ============================================================

coverage_matrix_plot <- gridExtra::grid.arrange(
  grobs = list(
    plot_platform_display,
    plot_scenario_measurement,
    plot_platform_measurement
  ),
  ncol = 1
)

# ============================================================
# 9 Show plots in R viewer
# ============================================================

plot_platform_display
plot_scenario_measurement
plot_platform_measurement
plot_seating_position_cabin_visibility

# ============================================================
# 10 Save figures
# ============================================================

dir.create("figures", showWarnings = FALSE)

ggsave(
  "figures/design_space_coverage_matrix.pdf",
  coverage_matrix_plot,
  width = 7,
  height = 11.5
)

ggsave(
  "figures/platform_display_coverage.png",
  plot_platform_display,
  width = 7,
  height = 9.4,
  dpi = 600
)

ggsave(
  "figures/scenario_measurements_coverage.png",
  plot_scenario_measurement,
  width = 7,
  height = 9.4,
  dpi = 600
)

ggsave(
  "figures/platform_measurement_domain_coverage.png",
  plot_platform_measurement,
  width = 7,
  height = 9.4,
  dpi = 600
)

ggsave(
  "figures/seating_position_cabin_visibility_coverage.png",
  plot_seating_position_cabin_visibility,
  width = 7,
  height = 9.4,
  dpi = 600
)

