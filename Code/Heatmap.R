# ============================================================
# Faceted Count Matrices / Heatmaps
# Platform × (Driver Presence & Visibility × Motion DoF)
# Platform × (Cabin Structure Visibility × Motion DoF)
# Platform × (Measurement Domain × Motion DoF)
# Aggregated where needed
# ============================================================

required_packages <- c(
  "readr",
  "dplyr",
  "stringr",
  "tidyr",
  "ggplot2",
  "forcats",
  "janitor",
  "ggh4x",
  "patchwork"
)

installed <- rownames(installed.packages())
missing_packages <- required_packages[!required_packages %in% installed]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

invisible(lapply(required_packages, library, character.only = TRUE))

library(grid)

# ============================================================
# 1 Load dataset
# ============================================================

data <- read_csv("Data/TableLiterature.csv", show_col_types = FALSE)
data <- clean_names(data)


# ============================================================
# 2 Helpers
# ============================================================

norm_str <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "[\r\n]+", " ")
  x <- str_squish(x)
  na_if(x, "")
}

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

wrap_value <- function(x, width = 6) {
  str_wrap(as.character(x), width = width, whitespace_only = FALSE)
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

clean_cabin_label <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("^\\s*no\\s+cabin\\s*$", ignore_case = TRUE)) ~ "No Cabin",
    TRUE ~ str_trim(
      str_squish(
        str_remove_all(x, regex("\\bcabin\\b", ignore_case = TRUE))
      )
    )
  )
}

clean_driver_label <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    str_detect(x, regex("^\\s*no\\s+driver\\s*$", ignore_case = TRUE)) ~ "No Driver",
    TRUE ~ str_trim(
      str_squish(
        str_remove_all(x, regex("\\bdriver\\b", ignore_case = TRUE))
      )
    )
  )
}

# Optional generic cleaner for measurement domain labels
clean_measurement_label <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    TRUE ~ str_trim(str_squish(x))
  )
}

make_count_heatmap <- function(df_plot,
                               y_var,
                               y_levels = NULL,
                               y_wrap_width = 10,
                               facet_var = "platform_clean",
                               x_var = "motion_dof_num",
                               x_levels = c(0, 1, 2, 3, 6),
                               heatmap_cols = c("#E6F4F1", "#7FC8BE", "#2F7F77"),
                               x_lab = "Motion DoF",
                               y_lab = "Category",
                               fill_lab = "Number of Studies") {
  facet_levels <- levels(counts[[facet_var]])
  platform_strip_fills <- c(
    "Real-world Driving" = "#FF851B",
    "Simulator" = "#0070C0"
  )
  
  platform_strip_text_cols <- c(
    "Real-world Driving" = "white",
    "Simulator" = "white"
  )
  
  strip_backgrounds <- lapply(facet_levels, function(f) {
    fill_col <- if (f %in% names(platform_strip_fills)) platform_strip_fills[[f]] else "#ECEFF4"
    element_rect(fill = fill_col, color = fill_col, linewidth = 0.8)
  })
  
  strip_texts <- lapply(facet_levels, function(f) {
    text_col <- if (f %in% names(platform_strip_text_cols)) platform_strip_text_cols[[f]] else "black"
    element_text(color = text_col, face = "bold", size = 10)
  })
  
  y_sym <- rlang::sym(y_var)
  x_sym <- rlang::sym(x_var)
  facet_sym <- rlang::sym(facet_var)
  
  counts <- df_plot %>%
    filter(.data[[x_var]] %in% x_levels) %>%
    count(!!facet_sym, !!y_sym, !!x_sym, name = "n")
  
  counts <- counts %>%
    mutate(
      !!facet_sym := fct_infreq(!!facet_sym),
      !!x_sym := factor(.data[[x_var]], levels = x_levels)
    )
  
  if (!is.null(y_levels)) {
    counts <- counts %>%
      mutate(
        !!y_sym := factor(.data[[y_var]], levels = y_levels)
      )
  } else {
    auto_levels <- counts %>%
      distinct(!!y_sym) %>%
      pull(!!y_sym) %>%
      as.character() %>%
      sort(na.last = TRUE)
    
    counts <- counts %>%
      mutate(
        !!y_sym := factor(.data[[y_var]], levels = auto_levels)
      )
  }
  
  counts <- counts %>%
    complete(
      !!facet_sym,
      !!y_sym,
      !!x_sym,
      fill = list(n = 0)
    ) %>%
    mutate(
      y_label = factor(
        wrap_value(.data[[y_var]], width = y_wrap_width),
        levels = rev(wrap_value(levels(.data[[y_var]]), width = y_wrap_width))
      )
    )
  
  max_n <- max(counts$n, na.rm = TRUE)
  fill_lookup <- grDevices::colorRampPalette(heatmap_cols)(max(max_n, 1) + 1)
  
  counts <- counts %>%
    mutate(
      fill_hex = fill_lookup[n + 1],
      text_color = vapply(fill_hex, get_text_color_from_fill, character(1))
    )

  ggplot(
    counts,
    aes(
      x = .data[[x_var]],
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
      size = 4,
      fontface = "bold"
    ) +
    ggh4x::facet_wrap2(
      stats::as.formula(paste("~", facet_var)),
      nrow = 1,
      strip = ggh4x::strip_themed(
        background_x = strip_backgrounds,
        text_x = strip_texts
      )
    ) +
    scale_fill_gradientn(
      colours = heatmap_cols,
      limits = c(0, max_n),
      breaks = c(0, 5, 10, 15, 20)
    ) +
    labs(
      title = NULL,
      x = x_lab,
      y = NULL,
      fill = fill_lab
    ) +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      
      strip.text = element_text(
        color = "black",
        face = "bold",
        size = 9
      ),
      strip.background = element_rect(
        fill = "#ECEFF4",
        color = "grey40",
        linewidth = 0.4
      ),
      
      axis.text.x = element_text(
        color = "black",
        size = 11,
        hjust = 0.5,
        vjust = 0.5,
        margin = margin(t = 2)
      ),
      
      axis.text.y = element_text(
        color = "black",
        size = 11,
        hjust = 0,
        vjust = 0.5,
        margin = margin(r = -2)
      ),
      
      axis.title.x = element_text(
        color = "black",
        face = "bold",
        size = 11,
        hjust = 0.5,
        margin = margin(t = 6, b = 1)
      ),
      
      axis.title.y = element_text(
        color = "black",
        face = "bold",
        size = 9,
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
        size = 10,
        vjust = 0.7,
        margin = margin(r = 12, b = 8)
      ),
      legend.margin = margin(t = -2, r = 0, b = 0, l = 0),
      legend.box.margin = margin(t = -2, r = 0, b = 0, l = 0),
      
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
        title.position = "left",
        title.hjust = 0,
        title.vjust = 0.7,
        direction = "horizontal",
        barwidth = unit(4, "cm"),
        barheight = unit(0.27, "cm")
      )
    )
}

# ============================================================
# 3 Clean
# ============================================================

df <- data %>%
  mutate(across(where(is.character), norm_str)) %>%
  distinct(reference, .keep_all = TRUE) %>%
  mutate(
    motion_dof_num = suppressWarnings(as.integer(str_extract(as.character(motion_dof), "\\d+"))),
    platform_clean = norm_str(platform),
    driver_presence_clean = norm_str(driver_presence_visibility),
    cabin_structure_clean = norm_str(cabin_structure_visibility),
    measurement_domain_clean = norm_str(measurement_domain),
    focus_condition_clean = norm_str(focus_condition)
  ) %>%
  mutate(
    driver_presence_clean = clean_driver_label(driver_presence_clean),
    cabin_structure_clean = clean_cabin_label(cabin_structure_clean),
    measurement_domain_clean = clean_measurement_label(measurement_domain_clean)
  ) %>%
  filter(
    !is.na(platform_clean),
    !is.na(motion_dof_num)
  )

# Optional checks
print(sort(unique(df$platform_clean)))
print(sort(unique(df$driver_presence_clean)))
print(sort(unique(df$cabin_structure_clean)))
print(sort(unique(df$measurement_domain_clean)))
print(sort(unique(df$motion_dof_num)))
print(sort(unique(df$focus_condition_clean)))

# ============================================================
# 4 Levels
# ============================================================

dof_levels <- c(0, 1, 2, 3, 6)

focus_condition_levels <- c(
  "Affective",
  "Physiology",
  "Cognitive",
  "Interaction/Performance"
)

driver_presence_levels <- c(
  "Present-Visible",
  "Participant Driving",
  "Present-Concealed",
  "No Driver"
)

# If you want fixed cabin order, define it here.
# Otherwise it will use alphabetical order automatically.
cabin_structure_levels <- c(
  "Real-Visible",
  "Real-Not Visible",
  "Virtual-Visible",
  "No Cabin"
)

# If your dataset contains different values, either:
# 1) leave measurement_domain_levels as NULL for automatic ordering
# 2) define a manual order below
measurement_domain_levels <- NULL

# ============================================================
# 5 Plot 1: Driver Presence & Visibility × Motion DoF
# ============================================================

df_driver <- df %>%
  filter(
    !is.na(driver_presence_clean),
    motion_dof_num %in% dof_levels
  )

p_driver <- make_count_heatmap(
  df_plot = df_driver,
  y_var = "driver_presence_clean",
  y_levels = driver_presence_levels,
  y_wrap_width = 10,
  x_levels = dof_levels,
  x_lab = "Motion DoF",
  y_lab = "Driver Presence & Visibility",
  fill_lab = "Number of Studies",
  heatmap_cols = c("#FDEBE2", "#F2A97E", "#C65D2E")
)

p_driver

# ============================================================
# 6 Plot 2: Cabin Structure Visibility × Motion DoF
# ============================================================

df_cabin <- df %>%
  filter(
    !is.na(cabin_structure_clean),
    motion_dof_num %in% dof_levels
  )

# Use manual levels only if at least one appears in the data
cabin_levels_to_use <- if (any(df_cabin$cabin_structure_clean %in% cabin_structure_levels)) {
  cabin_structure_levels
} else {
  NULL
}

p_cabin <- make_count_heatmap(
  df_plot = df_cabin,
  y_var = "cabin_structure_clean",
  y_levels = cabin_levels_to_use,
  y_wrap_width = 10,
  x_levels = dof_levels,
  x_lab = "Motion DoF",
  y_lab = "Cabin Structure Visibility",
  fill_lab = "Number of Studies",
  heatmap_cols = c("#F3E8FF", "#B999E5", "#5B3C88")
)

p_cabin

# ============================================================
# 7 Plot 3: Measurement Domain × Motion DoF
#    Split combined values on "+" and count each separately
# ============================================================

df_measurement <- df %>%
  filter(
    !is.na(measurement_domain_clean),
    motion_dof_num %in% dof_levels
  ) %>%
  mutate(
    measurement_domain_clean = str_replace_all(measurement_domain_clean, "\\s*\\+\\s*", "+")
  ) %>%
  separate_rows(measurement_domain_clean, sep = "\\+") %>%
  mutate(
    measurement_domain_clean = str_trim(measurement_domain_clean),
    measurement_domain_clean = na_if(measurement_domain_clean, "")
  ) %>%
  filter(!is.na(measurement_domain_clean))

p_measurement <- make_count_heatmap(
  df_plot = df_measurement,
  y_var = "measurement_domain_clean",
  y_levels = measurement_domain_levels,
  y_wrap_width = 10,
  x_levels = dof_levels,
  x_lab = "Motion DoF",
  y_lab = "Measurement Domain",
  fill_lab = "Number of Studies",
  heatmap_cols = c("#FBE4EC", "#E78FB3", "#A63D6E")
  
)

p_measurement
# ============================================================
# 8 Plot 4: Focus Condition × Motion DoF
#    Split combined values on "+" and group them
# ============================================================

df_focus_condition <- df %>%
  filter(
    !is.na(focus_condition_clean),
    motion_dof_num %in% dof_levels
  ) %>%
  mutate(
    focus_condition_clean = str_replace_all(focus_condition_clean, "\\s*\\+\\s*", "+")
  ) %>%
  separate_rows(focus_condition_clean, sep = "\\+") %>%
  mutate(
    focus_condition_clean = str_trim(focus_condition_clean),
    focus_condition_clean = na_if(focus_condition_clean, ""),
    focus_condition_grouped = group_focus_condition(focus_condition_clean)
  ) %>%
  filter(!is.na(focus_condition_grouped))

p_focus_condition <- make_count_heatmap(
  df_plot = df_focus_condition,
  y_var = "focus_condition_grouped",
  y_levels = focus_condition_levels,
  y_wrap_width = 10,
  x_levels = dof_levels,
  x_lab = "Motion DoF",
  y_lab = "Focus Condition",
  fill_lab = "Number of Studies",
  heatmap_cols = c("#FBE4EC", "#E78FB3", "#A63D6E")
)

p_focus_condition

# ============================================================
# 9 Save
# ============================================================
combined_plot <- p_cabin | p_driver | p_focus_condition

combined_plot

dir.create("figures", showWarnings = FALSE)

# Driver Presence
ggsave(
  "figures/platform_driver_presence_motion_dof_count_matrix.png",
  p_driver,
  width = 5,
  height = 3,
  dpi = 600
)

ggsave(
  "figures/platform_driver_presence_motion_dof_count_matrix.pdf",
  p_driver,
  width = 5,
  height = 3,
  dpi = 600
)

# Cabin Structure
ggsave(
  "figures/platform_cabin_structure_motion_dof_count_matrix.png",
  p_cabin,
  width = 5,
  height = 3,
  dpi = 600
)

ggsave(
  "figures/platform_cabin_structure_motion_dof_count_matrix.pdf",
  p_cabin,
  width = 5,
  height = 3,
  dpi = 600
)

# Measurement Domain
ggsave(
  "figures/platform_measurement_domain_motion_dof_count_matrix.png",
  p_measurement,
  width = 5,
  height = 3,
  dpi = 600
)

ggsave(
  "figures/platform_measurement_domain_motion_dof_count_matrix.pdf",
  p_measurement,
  width = 5,
  height = 3,
  dpi = 600
)

# Focus Condition
ggsave(
  "figures/platform_focus_condition_motion_dof_count_matrix.png",
  p_focus_condition,
  width = 5,
  height = 3,
  dpi = 600
)

ggsave(
  "figures/platform_focus_condition_motion_dof_count_matrix.pdf",
  p_focus_condition,
  width = 5,
  height = 3,
  dpi = 600
)

ggsave(
  "figures/combined_three_heatmaps_one_row.pdf",
  combined_plot,
  width = 15.5,
  height = 3.2,
  dpi = 600
)

