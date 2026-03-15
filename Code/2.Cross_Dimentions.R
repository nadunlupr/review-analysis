# ======================================
# Cross-Dimension Analysis
# ======================================

library(tidyverse)
library(janitor)
library(viridis)
# Load dataset
data <- read.csv("Data/TableLiterature.csv")
data <- clean_names(data)

# Inspect
glimpse(data)

# ======================================
# Variables used in design space
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
# Create output folder
# ======================================

dir.create("figures_cross", showWarnings = FALSE)

# ======================================
# Cross-dimension plotting function
# ======================================

plot_cross_dimension <- function(var1, var2){
  
  p <- ggplot(
    data,
    aes(
      x = stringr::str_to_title(gsub("_"," ", .data[[var1]])),
      fill = stringr::str_to_title(gsub("_"," ", .data[[var2]]))
    )
  ) +
    coord_flip() +
    scale_fill_viridis_d()+
    geom_bar(position = "stack") +
    labs(
      x = stringr::str_to_title(gsub("_"," ", var1)),
      y = "Number of Reviewed Studies",
      fill = stringr::str_to_title(gsub("_"," ", var2))
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# ======================================

plot_cross_dimension_precent <- function(var1, var2){
  
  cols <- viridisLite::viridis(length(unique(data[[var2]])))
  cols[6] <- "#C7E35A"
  cols[5] <- "#BDBDBD"
  
  data %>%
    count(.data[[var1]], .data[[var2]]) %>%
    group_by(.data[[var1]]) %>%
    mutate(percent = n / sum(n)) %>%
    ungroup() %>%
    ggplot(aes(
      x = percent,
      y = stringr::str_wrap(
        as.character(.data[[var1]]),
        width = 10,
        whitespace_only = FALSE
      ),
      fill = .data[[var2]]
    )) +
    scale_fill_manual(values = cols) +
    geom_col() +
    geom_text(
      aes(label = scales::percent(percent)),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 4
    ) +
    scale_x_continuous(labels = scales::percent_format()) +
    labs(
      x = "Percentage of Studies",
      y = stringr::str_to_title(gsub("_"," ", var1)),
      fill = stringr::str_to_title(gsub("_"," ", var2))
    ) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.minor = element_blank(),
      
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
        size = 14
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
  }

# ======================================
# Example plot in R viewer
# ======================================

plot_cross_dimension("platform", "driving_mode")
plot_cross_dimension_precent("platform", "driving_mode")


# ======================================
# Generate ALL cross-dimension plots
# ======================================

for(i in 1:length(vars)){
  
  for(j in 1:length(vars)){
    
    if(i < j){
      
      var1 <- vars[i]
      var2 <- vars[j]
      
      p <- plot_cross_dimension(var1, var2)
      
      ggsave(
        filename = paste0(
          "figures_cross/",
          "cross_",
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
# Generate ALL cross-dimension-percentage plots
# ======================================

for(i in 1:length(vars)){
  
  for(j in 1:length(vars)){
    
    if(i < j){
      
      var1 <- vars[i]
      var2 <- vars[j]
      
      p <- plot_cross_dimension_precent(var1, var2)
      
      ggsave(
        filename = paste0(
          "figures_cross/",
          "cross_",
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

plot_cross_dimension_precent("scenario", "measurements")
plot_cross_dimension_precent("platform", "display")
plot_cross_dimension_precent("driver_presence_visibility", "measurements")



ggsave(
  "figures_cross/Platform_V_Display.png",
  plot_cross_dimension_precent("platform", "display"),
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "figures_cross/Scenario_V_Measurements.png",
  plot_cross_dimension_precent("scenario", "measurements"),
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "figures_cross/Driver_Presence_V_Measurements.png",
  plot_cross_dimension_precent("driver_presence_visibility", "measurements"),
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "figures_cross/driver_presence_v_measurements.png",
  plot_cross_dimension_precent("driver_presence_visibility", "measurements"),
  width = 7,
  height = 5,
  dpi = 300
)
