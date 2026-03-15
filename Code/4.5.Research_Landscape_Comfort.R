# ============================================================
# Research landscape with left strips and grouped comfort factors
# Comfort Factor × Focus Condition, color = Scenario
# ============================================================

required_packages <- c(
  "tidyverse",
  "janitor",
  "viridis",
  "patchwork",
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

data <- read.csv("TableLiterature.csv", stringsAsFactors = FALSE, check.names = FALSE)
data <- janitor::clean_names(data)

# ============================================================
# 2 Find key columns
# ============================================================

find_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) {
    stop(paste("Could not find any of these columns:", paste(candidates, collapse = ", ")))
  }
  hit[1]
}

col_focus <- find_col(data, c("focus_condition"))
col_scenario <- find_col(data, c("scenario"))
col_cf_user <- find_col(data, c("comfort_factor_user"))
col_cf_vehicle <- find_col(data, c("comfort_factor_vehicle"))

study_id_col <- if ("study" %in% names(data)) {
  "study"
} else if ("paper" %in% names(data)) {
  "paper"
} else if ("title" %in% names(data)) {
  "title"
} else {
  NULL
}

if (is.null(study_id_col)) {
  data$study_id <- paste0("Study_", seq_len(nrow(data)))
  study_id_col <- "study_id"
}

# ============================================================
# 3 Helpers
# ============================================================

clean_text <- function(x) {
  x %>%
    replace(is.na(.), "") %>%
    stringr::str_trim() %>%
    stringr::str_replace_all("\\s+", " ")
}

is_none_value <- function(x) {
  x2 <- stringr::str_to_lower(clean_text(x))
  x2 %in% c("", "na", "n/a", "none", "nil", "not applicable")
}

normalize_factor <- function(x) {
  x <- clean_text(x)
  
  dplyr::case_when(
    stringr::str_to_lower(x) %in% c("trust") ~ "Trust",
    stringr::str_to_lower(x) %in% c("perceived safety") ~ "Perceived Safety",
    stringr::str_to_lower(x) %in% c("situation awareness", "situational awareness") ~ "Situation Awareness",
    stringr::str_to_lower(x) %in% c("task desirability") ~ "Task Desirability",
    stringr::str_to_lower(x) %in% c("task complexity") ~ "Task Complexity",
    stringr::str_to_lower(x) %in% c("visual focus", "focus of visual attention") ~ "Visual Focus",
    stringr::str_to_lower(x) %in% c("disposition to nausea") ~ "Disposition to Nausea",
    stringr::str_to_lower(x) %in% c("automated driving preferences", "automated driving preference", "automated driving style preferences") ~ "Automated Driving Preferences",
    stringr::str_to_lower(x) %in% c("features reducing nausea") ~ "Features Reducing Nausea",
    stringr::str_to_lower(x) %in% c("features supporting secondary activities") ~ "Features Supporting Secondary Activities",
    stringr::str_to_lower(x) %in% c("features supporting situation awareness") ~ "Features Supporting Situation Awareness",
    stringr::str_to_lower(x) %in% c("level of automation") ~ "Level of Automation",
    stringr::str_to_lower(x) %in% c("limitations") ~ "Limitations",
    stringr::str_to_lower(x) %in% c("unclear actions", "unclear behavior", "unclear behaviours") ~ "Unclear Actions",
    stringr::str_to_lower(x) %in% c("driving style") ~ "Driving Style",
    TRUE ~ stringr::str_to_title(x)
  )
}

split_expand <- function(df, factor_col, factor_group) {
  df %>%
    dplyr::select(all_of(c(study_id_col, col_focus, col_scenario, factor_col))) %>%
    dplyr::rename(
      study = all_of(study_id_col),
      focus_condition = all_of(col_focus),
      scenario = all_of(col_scenario),
      comfort_raw = all_of(factor_col)
    ) %>%
    dplyr::mutate(
      focus_condition = clean_text(focus_condition),
      scenario = clean_text(scenario),
      comfort_raw = clean_text(comfort_raw)
    ) %>%
    tidyr::separate_rows(comfort_raw, sep = "\\+") %>%
    dplyr::mutate(
      comfort_raw = stringr::str_trim(comfort_raw),
      comfort_factor = normalize_factor(comfort_raw),
      factor_group = factor_group
    ) %>%
    dplyr::filter(
      !is_none_value(comfort_raw),
      !is_none_value(comfort_factor),
      !is_none_value(focus_condition),
      !is_none_value(scenario)
    )
}

# ============================================================
# 4 Expand comfort factors
# ============================================================

user_long <- split_expand(data, col_cf_user, "User-related")
vehicle_long <- split_expand(data, col_cf_vehicle, "Vehicle-related")

comfort_long <- dplyr::bind_rows(user_long, vehicle_long) %>%
  dplyr::distinct(study, focus_condition, scenario, comfort_factor, factor_group)

if (nrow(comfort_long) == 0) {
  stop("No valid rows left after removing None/NA values.")
}

# ============================================================
# 5 Count combinations
# ============================================================

landscape_data <- landscape_data %>%
  dplyr::filter(comfort_factor %in% display_levels) %>%
  dplyr::mutate(
    comfort_factor = factor(comfort_factor, levels = display_levels),
    focus_condition = factor(focus_condition, levels = focus_levels),
    focus_condition_wrapped = stringr::str_wrap(as.character(focus_condition), width = 14),
    scenario = stringr::str_wrap(as.character(scenario), width = 16)
  )

# ============================================================
# 6 Force order
# ============================================================

vehicle_levels <- c(
  "Features Reducing Nausea",
  "Features Supporting Secondary Activities",
  "Features Supporting Situation Awareness",
  "Level of Automation",
  "Limitations",
  "Unclear Actions",
  "Driving Style"
)

user_levels <- c(
  "Task Desirability",
  "Task Complexity",
  "Visual Focus",
  "Disposition to Nausea",
  "Automated Driving Preferences",
  "Situation Awareness",
  "Perceived Safety",
  "Trust"
)

vehicle_levels <- vehicle_levels[vehicle_levels %in% unique(landscape_data$comfort_factor)]
user_levels <- user_levels[user_levels %in% unique(landscape_data$comfort_factor)]

display_levels <- c(vehicle_levels, user_levels)
focus_levels <- unique(landscape_data$focus_condition)

landscape_data <- landscape_data %>%
  dplyr::filter(comfort_factor %in% display_levels) %>%
  dplyr::mutate(
    comfort_factor = factor(comfort_factor, levels = display_levels),
    focus_condition = factor(focus_condition, levels = focus_levels),
    scenario = stringr::str_wrap(as.character(scenario), width = 16)
  )

n_y <- length(display_levels)

# ============================================================
# 7 Shared row map and band positions
# ============================================================

row_map <- tibble::tibble(
  comfort_factor = display_levels,
  y = seq_along(display_levels),
  group = dplyr::case_when(
    comfort_factor %in% vehicle_levels ~ "Vehicle-related",
    comfort_factor %in% user_levels ~ "User-related",
    TRUE ~ NA_character_
  )
)

shared_bands <- row_map %>%
  dplyr::filter(!is.na(group)) %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(
    ymin = min(y) - 0.5,
    ymax = max(y) + 0.5,
    ymid = mean(y),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    fill = dplyr::case_when(
      group == "Vehicle-related" ~ "#CFEBC8",
      group == "User-related" ~ "#F1D4D8"
    ),
    label = group
  )

group_bands <- shared_bands
tick_bands  <- shared_bands

plot_bands <- shared_bands %>%
  dplyr::transmute(
    xmin = 0.5,
    xmax = length(focus_levels) + 0.5,
    ymin = ymin,
    ymax = ymax,
    fill = fill
  )

tick_df <- row_map %>%
  dplyr::transmute(
    y = y,
    label = stringr::str_wrap(comfort_factor, width = 13)
  )

# ============================================================
# 8 Panel 1: main y title
# ============================================================

p_title <- ggplot() +
  geom_text(
    aes(x = 1, y = 1, label = "Comfort Factor"),
    angle = 90,
    fontface = "bold",
    size = 5.5
  ) +
  xlim(0, 2) +
  ylim(0, 2) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# ============================================================
# 9 Panel 2: group strip
# ============================================================

p_group <- ggplot() +
  geom_rect(
    data = group_bands,
    aes(xmin = 0, xmax = 0.72, ymin = ymin, ymax = ymax, fill = fill),
    color = NA,
    alpha = 0.35,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = group_bands,
    aes(x = 0.36, y = ymid, label = label),
    angle = 90,
    fontface = "bold",
    size = 4.3
  ) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0, 0.72), expand = c(0, 0)) +
  scale_y_continuous(
    limits = c(0.5, n_y + 0.5),
    breaks = NULL,
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# ============================================================
# 10 Panel 3: tick-label strip
# ============================================================

p_ticks <- ggplot() +
  geom_rect(
    data = tick_bands,
    aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax, fill = fill),
    inherit.aes = FALSE,
    color = NA,
    alpha = 0.35
  ) +
  scale_fill_identity() +
  ggtext::geom_richtext(
    data = tick_df,
    aes(
      x = 0.05,
      y = y,
      label = gsub("\n", "<br>", label)
    ),
    hjust = 0,
    vjust = 0.5,
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(c(0, 6, 0, 0), "pt"),
    label.margin = grid::unit(c(0, 0, 0, 0), "pt"),
    size = 4.4,
    lineheight = 1.0
  ) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(
    limits = c(0.5, n_y + 0.5),
    breaks = NULL,
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# ============================================================
# 11 Panel 4: bubble plot
# ============================================================

p_bubble <- ggplot(
  landscape_data,
  aes(
    x = focus_condition,
    y = comfort_factor,
    size = n,
    color = scenario
  )
) +
  geom_rect(
    data = plot_bands,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
    inherit.aes = FALSE,
    alpha = 0.35,
    color = NA
  ) +
  scale_fill_identity() +
  geom_point(alpha = 0.9) +
  scale_size(range = c(3, 14)) +
  scale_color_viridis_d(option = "D", end = 0.9) +
  scale_x_discrete(
    labels = function(x) stringr::str_wrap(x, width = 14),
    expand = c(0, 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    x = "Focus Condition",
    y = NULL,
    size = "Number of Studies",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(
      color = "black",
      size = 12,
      angle = 90,
      hjust = 0,
      vjust = 0.5
    ),
    axis.text.y = element_blank(),
    axis.title.x = element_text(
      face = "bold",
      size = 16,
      color = "black",
      margin = margin(t = 14)
    ),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.justification = "center",
    legend.title = element_text(face = "bold", color = "black"),
    legend.text = element_text(color = "black"),
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  guides(
    size = guide_legend(
      title.position = "left",
      title.hjust = 0,
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
      nrow = ceiling(length(unique(landscape_data$scenario)) / 4),
      byrow = TRUE,
      order = 2
    )
  )

# ============================================================
# 12 Combine panels
# ============================================================

final_plot <- patchwork::wrap_plots(
  p_title, p_group, p_ticks, p_bubble,
  nrow = 1,
  widths = c(0.60, 0.45, 1.55, 8.0),
  guides = "collect"
) & theme(
  plot.margin = margin(0, 0, 0, 0),
  legend.position = "bottom",
  legend.box = "vertical",
  legend.box.just = "left",
  legend.justification = "center"
)

final_plot

# ============================================================
# 13 Save
# ============================================================

dir.create("figures", showWarnings = FALSE)

ggsave(
  "figures/research_landscape_left_strips_fixed.png",
  final_plot,
  width = 8,
  height = 15,
  dpi = 300
)

ggsave(
  "figures/research_landscape_left_strips_fixed.pdf",
  final_plot,
  width = 8,
  height = 15
)
