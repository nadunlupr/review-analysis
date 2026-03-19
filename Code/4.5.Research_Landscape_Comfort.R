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

data <- read.csv("Data/TableLiterature.csv", stringsAsFactors = FALSE, check.names = FALSE)
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

normalize_focus <- function(x) {
  x <- clean_text(x)
  
  dplyr::case_when(
    stringr::str_to_lower(x) %in% c("anxiety") ~ "Anxiety",
    stringr::str_to_lower(x) %in% c("affectivity", "affective state", "affective states") ~ "Affectivity",
    stringr::str_to_lower(x) %in% c("discomfort", "general discomfort") ~ "Discomfort",
    stringr::str_to_lower(x) %in% c("motion sickness", "motionsickness") ~ "Motion Sickness",
    stringr::str_to_lower(x) %in% c("attention") ~ "Attention",
    stringr::str_to_lower(x) %in% c("decision making", "decision-making") ~ "Decision Making",
    stringr::str_to_lower(x) %in% c("mind wandering", "mind-wandering", "mind wondering") ~ "Mind Wandering",
    stringr::str_to_lower(x) %in% c("sense of agency") ~ "Sense of Agency",
    stringr::str_to_lower(x) %in% c("communication") ~ "Communication",
    stringr::str_to_lower(x) %in% c("taking over control", "take over control", "take-over control", "takeover control") ~ "Taking Over Control",
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
      focus_condition = normalize_focus(focus_condition),
      scenario = scenario %>%
        clean_text() %>%
        stringr::str_replace_all("\\s*\\+\\s*", "+"),
      comfort_raw = clean_text(comfort_raw)
    ) %>%
    tidyr::separate_rows(comfort_raw, sep = "\\+") %>%
    tidyr::separate_rows(scenario, sep = "\\+") %>%
    dplyr::mutate(
      comfort_raw = stringr::str_trim(comfort_raw),
      scenario = stringr::str_trim(scenario),
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

landscape_data <- comfort_long %>%
  dplyr::count(comfort_factor, factor_group, focus_condition, scenario, name = "n")

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

affective_levels <- c("Anxiety", "Affectivity", "Discomfort")
physiological_levels <- c("Motion Sickness")
cognitive_levels <- c("Attention", "Decision Making", "Mind Wandering")
interaction_levels <- c("Sense of Agency", "Communication", "Taking Over Control", "Driving Style")

focus_levels <- c(
  affective_levels,
  physiological_levels,
  cognitive_levels,
  interaction_levels
)

observed_focus <- unique(landscape_data$focus_condition)
unknown_focus <- setdiff(observed_focus, focus_levels)

if (length(unknown_focus) > 0) {
  stop(
    paste0(
      "These Focus Condition values were not assigned to a group: ",
      paste(unknown_focus, collapse = ", ")
    )
  )
}

focus_levels <- focus_levels[focus_levels %in% observed_focus]

landscape_data <- landscape_data %>%
  dplyr::filter(
    comfort_factor %in% display_levels,
    focus_condition %in% focus_levels
  ) %>%
  dplyr::mutate(
    comfort_factor = factor(comfort_factor, levels = display_levels),
    focus_condition = factor(focus_condition, levels = focus_levels),
    scenario = stringr::str_wrap(as.character(scenario), width = 16)
  )

n_y <- length(display_levels)
n_x <- length(focus_levels)

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
      group == "Vehicle-related" ~ "#E2F0D9",
      group == "User-related" ~ "#FBE5D6"
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
    alpha = 1,
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
    alpha = 1
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
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    x = NULL,
    y = NULL,
    size = "Number of Studies",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.box.just = "left",
    legend.justification = "center",
    legend.title = element_text(face = "bold", color = "black"),
    legend.text = element_text(color = "black"),
    legend.key.height = unit(0.9, "lines"),
    legend.key.width = unit(1.2, "lines"),
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.8),
    plot.margin = margin(0, 0, 0, 0)
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
      nrow = ceiling(length(unique(landscape_data$scenario)) / 4),
      byrow = TRUE,
      order = 2
    )
  )

# ============================================================
# 12 X-axis grouping data
# ============================================================

x_group_map <- tibble::tibble(
  focus_condition = focus_levels,
  x = seq_along(focus_levels),
  x_group = dplyr::case_when(
    focus_condition %in% affective_levels ~ "AS",
    focus_condition %in% physiological_levels ~ "PS",
    focus_condition %in% cognitive_levels ~ "CS",
    focus_condition %in% interaction_levels ~ "I/P"
  )
)

x_group_bands <- x_group_map %>%
  dplyr::group_by(x_group) %>%
  dplyr::summarise(
    xmin = min(x) - 0.5,
    xmax = max(x) + 0.5,
    xmid = mean(x),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    fill = dplyr::case_when(
      x_group == "AS" ~ "#DDEBF7",
      x_group == "PS" ~ "#EADCF8",
      x_group == "CS" ~ "#F4CCCC",
      x_group == "I/P" ~ "#D0E0E3"
    )
  )

x_tick_df <- x_group_map %>%
  dplyr::transmute(
    x = x,
    label = stringr::str_wrap(focus_condition, width = 12)
  )

# ============================================================
# 13 Panel 5: x tick-label strip
# ============================================================

p_xticks <- ggplot() +
  geom_rect(
    data = x_group_bands,
    aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = fill),
    inherit.aes = FALSE,
    color = NA,
    alpha = 1
  ) +
  scale_fill_identity() +
  ggtext::geom_richtext(
    data = x_tick_df,
    aes(
      x = x,
      y = 0.06,
      label = gsub("\n", "<br>", label)
    ),
    angle = 90,
    hjust = 0,
    vjust = 0.5,
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(c(0, 0, 0, 0), "pt"),
    label.margin = grid::unit(c(0, 0, 0, 0), "pt"),
    size = 3.9,
    lineheight = 1.0
  ) +
  scale_x_continuous(limits = c(0.5, n_x + 0.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# ============================================================
# 14 Panel 6: x group-label strip
# ============================================================

p_xgroup <- ggplot() +
  geom_rect(
    data = x_group_bands,
    aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = fill),
    inherit.aes = FALSE,
    color = NA,
    alpha = 1
  ) +
  geom_text(
    data = x_group_bands,
    aes(x = xmid, y = 0.5, label = x_group),
    fontface = "bold",
    size = 3.6,
    lineheight = 0.95
  ) +
  scale_fill_identity() +
  scale_x_continuous(limits = c(0.5, n_x + 0.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# ============================================================
# 15 Panel 7: x main title
# ============================================================

p_xtitle <- ggplot() +
  geom_text(
    aes(x = 0.5, y = 0.5, label = "Focus Condition"),
    fontface = "bold",
    size = 4.8
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void() +
  theme(plot.margin = margin(6, 0, 0, 0))

# ============================================================
# 16 Build top row and bottom row separately
# ============================================================

top_row <- patchwork::wrap_plots(
  p_title, p_group, p_ticks, p_bubble,
  nrow = 1,
  widths = c(0.60, 0.45, 1.55, 8.0)
)

p_blank1 <- ggplot() + theme_void() + theme(plot.margin = margin(0, 0, 0, 0))
p_blank2 <- ggplot() + theme_void() + theme(plot.margin = margin(0, 0, 0, 0))
p_blank3 <- ggplot() + theme_void() + theme(plot.margin = margin(0, 0, 0, 0))

bottom_right <- patchwork::wrap_plots(
  p_xticks,
  p_xgroup,
  p_xtitle,
  ncol = 1,
  heights = c(1.15, 0.55, 0.45)
)

bottom_row <- patchwork::wrap_plots(
  p_blank1, p_blank2, p_blank3, bottom_right,
  nrow = 1,
  widths = c(0.60, 0.45, 1.55, 8.0)
)

# ============================================================
# 17 Final combine
# ============================================================

final_plot <- patchwork::wrap_plots(
  top_row,
  bottom_row,
  ncol = 1,
  heights = c(8.0, 2.15),
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
# 18 Save
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
