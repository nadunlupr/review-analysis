# Load libraries
library(tidyverse)
library(janitor)

# ======================================
# 1 Load dataset
# ======================================

data <- read.csv("Data/TableLiterature.csv")

# Clean column names
data <- clean_names(data)

# Inspect data
str(data)
summary(data)

# ======================================
# 2 Identify variables to plot
# ======================================

exclude_vars <- c("study", "year_of_publication", "file", "no", "reference", "author_s")
vars <- setdiff(names(data), exclude_vars)

print(vars)

# ======================================
# 3 Create output folder
# ======================================

dir.create("figures", showWarnings = FALSE)

# ======================================
# 4 Variables where "+" means combined categories
#    Apply to all variables
# ======================================

split_plus_vars <- vars

# ======================================
# 5 Color gradients for different plots
# ======================================

plot_gradients <- list(
  platform = c("#F3E8FF", "#B999E5", "#5B3C88"),
  driving_mode = c("#F3E8FF", "#B999E5", "#5B3C88"),
  display_technology = c("#F3E8FF", "#B999E5", "#5B3C88"),
  scenario = c("#FDEBE2", "#F2A97E", "#C65D2E"),
  measurements = c("#FBE4EC", "#E78FB3", "#A63D6E"),
  motion_dof = c("#F3E8FF", "#B999E5", "#5B3C88"),
  driver_presence_visibility = c("#FDEBE2", "#F2A97E", "#C65D2E"),
  cabin_visibility = c("#F3E8FF", "#B999E5", "#5B3C88"),
  cabin_structure_visibility = c("#F3E8FF", "#B999E5", "#5B3C88"),
  focus_group = c("#FDEBE2", "#F2A97E", "#C65D2E"),
  participant_seating_position = c("#FDEBE2", "#F2A97E", "#C65D2E"),
  controlled_sensory_focus = c("#FDEBE2", "#F2A97E", "#C65D2E"),
  comfort_factor = c("#FBE4EC", "#E78FB3", "#A63D6E"),
  measurement_domain = c("#FBE4EC", "#E78FB3", "#A63D6E"),
  focus_condition = c("#FBE4EC", "#E78FB3", "#A63D6E")
)

# Default gradient if a variable is not listed above
default_gradient <- c("#FFF4D6", "#E7C768", "#A67C00")

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
# 7 Distribution plotting function
# ======================================

plot_distribution <- function(variable) {
  
  if (variable == "focus_condition") {
    
    plot_data <- data %>%
      select(all_of(variable)) %>%
      filter(!is.na(.data[[variable]]), .data[[variable]] != "") %>%
      separate_rows(all_of(variable), sep = "\\s*\\+\\s*") %>%
      mutate(
        focus_condition = str_trim(.data[[variable]]),
        focus_condition = na_if(focus_condition, ""),
        focus_condition_grouped = group_focus_condition(focus_condition)
      ) %>%
      filter(!is.na(focus_condition_grouped)) %>%
      count(focus_condition_grouped, name = "n") %>%
      mutate(
        label = factor(
          stringr::str_wrap(focus_condition_grouped, width = 14, whitespace_only = FALSE),
          levels = stringr::str_wrap(
            c(
              "Affective",
              "Physiology",
              "Cognitive",
              "Interaction/Performance"
            ),
            width = 14,
            whitespace_only = FALSE
          )
        )
      )
    
  } else if (variable %in% split_plus_vars) {
    
    plot_data <- data %>%
      select(all_of(variable)) %>%
      filter(!is.na(.data[[variable]]), .data[[variable]] != "") %>%
      separate_rows(all_of(variable), sep = "\\s*\\+\\s*") %>%
      mutate(
        !!variable := str_trim(.data[[variable]])
      )
    
    if (variable == "cabin_structure_visibility") {
      plot_data <- plot_data %>%
        mutate(
          !!variable := str_trim(
            str_squish(
              str_remove_all(
                .data[[variable]],
                regex("(?<!no\\s)\\bcabin\\b(?!\\s*$)", ignore_case = TRUE)
              )
            )
          )        
          )
      
    }
    if (variable == "driver_presence_visibility") {
      plot_data <- plot_data %>%
        mutate(
          !!variable := case_when(
            str_detect(.data[[variable]], regex("^\\s*no\\s+driver\\s*$", ignore_case = TRUE)) ~ "No Driver",
            TRUE ~ str_trim(
              str_squish(
                str_remove_all(.data[[variable]], regex("\\bdriver\\b", ignore_case = TRUE))
              )
            )
          )
        )
    }
    
    plot_data <- plot_data %>%
      filter(.data[[variable]] != "") %>%
      count(.data[[variable]], name = "n") %>%
      mutate(
        label = stringr::str_wrap(as.character(.data[[variable]]), width = 10, whitespace_only = FALSE)
      )
    
  } else {
    
    plot_data <- data %>%
      select(all_of(variable)) %>%
      filter(!is.na(.data[[variable]]), .data[[variable]] != "") %>%
      count(.data[[variable]], name = "n") %>%
      mutate(
        label = stringr::str_wrap(as.character(.data[[variable]]), width = 10, whitespace_only = FALSE)
      )
  }
  
  gradient_cols <- plot_gradients[[variable]]
  if (is.null(gradient_cols)) {
    gradient_cols <- default_gradient
  }
  
  p <- ggplot(
    plot_data,
    aes(
      x = reorder(label, n),
      y = n,
      fill = n
    )
  ) +
    geom_bar(stat = "identity", color = NA) +
    scale_fill_gradientn(colours = gradient_cols) +
    coord_flip() +
    labs(
      title = NULL,
      x = ifelse(
        variable == "motion_dof",
        "Motion DOF",
        stringr::str_to_title(gsub("_", " ", variable))
      ),
      y = NULL
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.8),
      axis.text.x = element_text(
        color = "black",
        size = 24,
        hjust = 0.5,
        vjust = 0.5,
        margin = margin(t = 2)
      ),
      axis.text.y = element_text(
        color = "black",
        size = 24,
        hjust = 0,
        vjust = 0.5,
        margin = margin(r = -2)
      ),
      axis.title.x = element_text(
        color = "black",
        face = "bold",
        size = 25,
        hjust = 0.5,
        margin = margin(t = 12)
      ),
      axis.title.y = element_text(
        color = "black",
        face = "bold",
        size = 25,
        hjust = 0.5,
        margin = margin(r = 12)
      ),
      panel.border = element_rect(
        color = "grey55",
        fill = NA,
        linewidth = 0.9
      ),
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
    )
  
  return(p)
}

# ======================================
# 8 Generate plots for all variables
# ======================================

plots <- lapply(vars, plot_distribution)

# ======================================
# 9 Save plots automatically
# ======================================

# for (v in vars) {
#   p <- plot_distribution(v)
#   
#   ggsave(
#     filename = paste0("figures/dist_", v, ".pdf"),
#     plot = p,
#     width = 6,
#     height = 4
#   )
# }

# ======================================
# 10 Optional: show plots in R
# ======================================

plots[[1]]
plots[[2]]
plots[[3]]
plots[[4]]
plots[[5]]
plots[[6]]
plots[[7]]
plots[[8]]
plots[[9]]
plots[[10]]
plots[[11]]
plots[[12]]
plots[[13]]
plots[[14]]
plots[[15]]
plots[[16]]
plots[[17]]
plots[[18]]
plots[[19]]
plots[[20]]
plots[[21]]
plots[[22]]
# ======================================
# 11 Save selected PNG/PDF files
# ======================================
ggsave(
  "figures/PlatformCount.png",
  plots[[1]],
  width = 7,
  height = 5,
  dpi = 600
)
ggsave(
  "figures/PlatformCount.pdf",
  plots[[1]],
  width = 7,
  height = 5,
  dpi = 600
)
#-----------------
ggsave(
  "figures/DisplayCount.png",
  plots[[5]],
  width = 7,
  height = 5,
  dpi = 600
)
ggsave(
  "figures/DisplayCount.pdf",
  plots[[5]],
  width = 7,
  height = 5,
  dpi = 600
)
#----------------
ggsave(
  "figures/ScenarioCount.png",
  plots[[8]],
  width = 7,
  height = 5,
  dpi = 600
)
ggsave(
  "figures/ScenarioCount.pdf",
  plots[[8]],
  width = 7,
  height = 5,
  dpi = 600
)
#----------------

ggsave(
  "figures/DrivingModeCount.png",
  plots[[2]],
  width = 7,
  height = 5,
  dpi = 600
)
ggsave(
  "figures/DrivingModeCount.pdf",
  plots[[2]],
  width = 7,
  height = 5,
  dpi = 600
)
#----------------

ggsave(
  "figures/CabinStructureVisibilityCount.png",
  plots[[3]],
  width = 7,
  height = 5,
  dpi = 600
)
ggsave(
  "figures/CabinStructureVisibilityCount.pdf",
  plots[[3]],
  width = 7,
  height = 5,
  dpi = 600
)
#----------------

ggsave(
  "figures/MotionDofCount.png",
  plots[[4]],
  width = 7,
  height = 5,
  dpi = 600
)
ggsave(
  "figures/MotionDofCount.pdf",
  plots[[4]],
  width = 7,
  height = 5,
  dpi = 600
)

#----------------
ggsave(
  "figures/ParticipatnSeatingCount.png",
  plots[[6]],
  width = 7,
  height = 5,
  dpi = 600
)
ggsave(
  "figures/ParticipatnSeatingCount.pdf",
  plots[[6]],
  width = 7,
  height = 5,
  dpi = 600
)
#----------------

ggsave(
  "figures/ControlledSensoryFocusCount.png",
  plots[[9]],
  width = 7,
  height = 5,
  dpi = 600
)
ggsave(
  "figures/ControlledSensoryFocusCount.pdf",
  plots[[9]],
  width = 7,
  height = 5,
  dpi = 600
)
#----------------

ggsave(
  "figures/DriverPresenceVisibilityCount.png",
  plots[[7]],
  width = 7,
  height = 5,
  dpi = 600
)
ggsave(
  "figures/DriverPresenceVisibilityCount.pdf",
  plots[[7]],
  width = 7,
  height = 5,
  dpi = 600
)
#----------------

ggsave(
  "figures/ComfortFactorCount.png",
  plots[[10]],
  width = 7,
  height = 5,
  dpi = 600
)
ggsave(
  "figures/ComfortFactorCount.pdf",
  plots[[10]],
  width = 7,
  height = 5,
  dpi = 600
)
#----------------

ggsave(
  "figures/MeasurementDomainCount.png",
  plots[[13]],
  width = 7,
  height = 5,
  dpi = 600
)
ggsave(
  "figures/MeasurementDomainCount.pdf",
  plots[[13]],
  width = 7,
  height = 5,
  dpi = 600
)
#----------------

ggsave(
  "figures/FocusConditionGroupedCount.png",
  plots[[14]],
  width = 7,
  height = 5,
  dpi = 600
)
ggsave(
  "figures/FocusConditionGroupedCount.pdf",
  plots[[14]],
  width = 7,
  height = 5,
  dpi = 600
)

setdiff(unique(group_focus_condition(data$focus_condition)), c("Affective", "Physiology", "Cognitive", "Interaction/Performance"))
