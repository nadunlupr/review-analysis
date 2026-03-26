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
  "display",
  "scenario",
  "measurements",
  "controlled_sensory_focus",
  "focus_group",
  "participant_seating_position",
  "driver_presence_visibility",
  "cabin_structure",
  "cabin_visibility",
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

split_plus_vars <- c("display", "scenario")

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
  display = "immersive",
  scenario = "context",
  measurements = "attention",
  controlled_sensory_focus = "attention",
  focus_group = "human",
  participant_seating_position = "human",
  driver_presence_visibility = "human",
  cabin_structure = "structure",
  cabin_visibility = "structure",
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
  
  df
}

# ======================================
# 8 Cross-dimension count plot
# ======================================

plot_cross_dimension <- function(var1, var2) {
  
  df <- prepare_cross_data(var1, var2)
  
  fill_levels <- df %>%
    distinct(.data[[var2]]) %>%
    pull(1) %>%
    as.character() %>%
    sort()
  
  fill_palette <- get_variable_palette(var2, length(fill_levels))
  names(fill_palette) <- fill_levels
  
  plot_df <- df %>%
    mutate(
      x_label = pretty_value(.data[[var1]], width = 12)
    )
  
  p <- ggplot(
    plot_df,
    aes(
      x = x_label,
      fill = .data[[var2]]
    )
  ) +
    geom_bar(position = "stack", color = NA) +
    coord_flip() +
    scale_fill_manual(
      values = fill_palette,
      labels = pretty_value(fill_levels, width = 12)
    ) +
    labs(
      x = pretty_var_name(var1),
      y = "Number of Reviewed Studies",
      fill = pretty_var_name(var2)
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
      legend.title = element_text(
        color = "black",
        face = "bold",
        size = 14,
        vjust = 1
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
    guides(fill = guide_legend(ncol = 3, byrow = TRUE))
  
  return(p)
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
      y_label = pretty_value(.data[[var1]], width = 12),
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
      fill = pretty_var_name(var2)
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

plot_cross_dimension("platform", "driving_mode")
plot_cross_dimension_precent("platform", "display", vertical_padding = 0.8)
plot_cross_dimension_precent("scenario", "measurements")
plot_cross_dimension_precent("driver_presence_visibility", "measurements")

# ======================================
# 11 Generate ALL cross-dimension count plots
# ======================================

for (i in seq_along(vars)) {
  for (j in seq_along(vars)) {
    if (i < j) {
      var1 <- vars[i]
      var2 <- vars[j]
      
      p <- plot_cross_dimension(var1, var2)
      
      ggsave(
        filename = paste0(
          "figures_cross/",
          "cross_count_",
          var1,
          "_vs_",
          var2,
          ".pdf"
        ),
        plot = p,
        width = 7,
        height = 5
      )
    }
  }
}

# ======================================
# 12 Generate ALL cross-dimension percentage plots
# ======================================

for (i in seq_along(vars)) {
  for (j in seq_along(vars)) {
    if (i < j) {
      var1 <- vars[i]
      var2 <- vars[j]
      
      p <- plot_cross_dimension_precent(var1, var2)
      
      ggsave(
        filename = paste0(
          "figures_cross/",
          "cross_percent_",
          var1,
          "_vs_",
          var2,
          ".pdf"
        ),
        plot = p,
        width = 7,
        height = 5
      )
    }
  }
}

# ======================================
# 13 Selected exports
#    Only Platform_V_Display gets extra
#    internal vertical space
# ======================================

ggsave(
  "figures_cross/Platform_V_Display.png",
  plot_cross_dimension_precent("platform", "display", vertical_padding = 0.8),
  width = 7,
  height = 5.458,
  dpi = 300
)

ggsave(
  "figures_cross/Platform_V_Display.pdf",
  plot_cross_dimension_precent("platform", "display", vertical_padding = 0.8),
  width = 7,
  height = 5.458,
  dpi = 600
)

#--------------------------

ggsave(
  "figures_cross/Scenario_V_Measurements.png",
  plot_cross_dimension_precent("scenario", "measurements"),
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "figures_cross/Scenario_V_Measurements.pdf",
  plot_cross_dimension_precent("scenario", "measurements"),
  width = 7,
  height = 5,
  dpi = 600
)

#----------------------------

ggsave(
  "figures_cross/Driver_Presence_V_Measurements.png",
  plot_cross_dimension_precent("driver_presence_visibility", "measurements"),
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "figures_cross/driver_presence_v_measurements.pdf",
  plot_cross_dimension_precent("driver_presence_visibility", "measurements"),
  width = 7,
  height = 5,
  dpi = 600
)

