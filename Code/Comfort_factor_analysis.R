# ============================================================
# Clustered heatmaps for comfort-factor coverage (multi-label +)
# Options:
#   (1) One heatmap + row annotations: Platform + Driving Mode
#   (3) Split by Platform (two panels), annotated by Driving Mode
#   (4) Split by Driving Mode (three panels), annotated by Platform
#   (5) Option (1) + top marginal coverage barplot
# ============================================================

# ----------------------------
# 0) Packages
# ----------------------------
if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  BiocManager::install("ComplexHeatmap")
}
library(ComplexHeatmap)
pkgs <- c(
  "readr", "dplyr", "stringr", "tidyr",
  "ComplexHeatmap", "circlize", "vegan", "grid"
)
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# ----------------------------
# 1) Load data
# ----------------------------
csv_path <- "Data/TableLiterature.csv"  # <-- EDIT
df <- readr::read_csv(csv_path, show_col_types = FALSE)

pick_col <- function(cands, nm) {
  ok <- cands[cands %in% nm]
  if (length(ok) == 0) return(NA_character_)
  ok[[1]]
}

col_platform <- pick_col(c("Platform"), names(df))
col_mode     <- pick_col(c("Driving Mode", "Driving mode"), names(df))
col_user     <- pick_col(c("Comfort Factor - User", "Comfort factor - User",
                           "Comfort Factor-User", "Comfort factor-User"), names(df))
col_vehicle  <- pick_col(c("Comfort Factor - Vehicle", "Comfort factor - Vehicle",
                           "Comfort Factor-Vehicle", "Comfort factor-Vehicle"), names(df))
col_study    <- pick_col(c("Study", "DOI", "Identifier"), names(df))

stopifnot(
  !is.na(col_platform),
  !is.na(col_mode),
  !is.na(col_user),
  !is.na(col_vehicle)
)

# ----------------------------
# 2) Normalize + split multi-label fields ("A + B + C")
# ----------------------------
clean_cell <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  x <- stringr::str_trim(x)
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA_character_
  x
}

split_plus <- function(x) {
  x <- clean_cell(x)
  if (is.na(x)) return(character(0))
  x <- stringr::str_replace_all(x, "\\s*\\+\\s*", " + ")
  parts <- unlist(stringr::str_split(x, " \\+ "))
  parts <- stringr::str_trim(parts)
  parts <- parts[parts != ""]
  unique(parts)
}

df <- df %>%
  mutate(
    Platform    = clean_cell(.data[[col_platform]]),
    DrivingMode = clean_cell(.data[[col_mode]]),
    StudyID     = if (!is.na(col_study)) clean_cell(.data[[col_study]]) else NA_character_,
    StudyNum    = seq_len(n())
  )

# ----------------------------
# 3) Comfort-model mapping (leaf factor -> subcategory)
#    EDIT these labels to match your CSV values exactly if needed.
# ----------------------------
user_activity <- c("Task desirability", "Task desirabillity", "Task complexity", "Visual focus")
user_personality <- c("Disposition to nausea", "Automated driving preferences")
user_understanding <- c("Situation awareness", "Perceived safety", "Trust")

veh_vehicle <- c(
  "Features reducing nausea",
  "Features supporting secondary activities",
  "Features supporting situation awareness"
)
veh_automation <- c(
  "Level of automation",
  "Limitations",
  "Unclear actions",
  "Driving style"
)

canonicalize_factor <- function(f) {
  f <- stringr::str_trim(f)
  f <- stringr::str_replace_all(f, "Task desirabillity", "Task desirability")
  f
}

factor_group <- c(
  setNames(rep("User: Activity", length(user_activity)), user_activity),
  setNames(rep("User: Personality", length(user_personality)), user_personality),
  setNames(rep("User: Understanding", length(user_understanding)), user_understanding),
  setNames(rep("Vehicle: Vehicle", length(veh_vehicle)), veh_vehicle),
  setNames(rep("Vehicle: Automation", length(veh_automation)), veh_automation)
)

group_levels <- c(
  "User: Activity", "User: Personality", "User: Understanding",
  "Vehicle: Vehicle", "Vehicle: Automation", "Other"
)

# ----------------------------
# 4) Build binary matrix: studies x leaf factors
# ----------------------------
all_user_factors <- df[[col_user]] %>% lapply(split_plus) %>% unlist() %>% unique() %>% canonicalize_factor()
all_veh_factors  <- df[[col_vehicle]] %>% lapply(split_plus) %>% unlist() %>% unique() %>% canonicalize_factor()

all_factors <- sort(unique(c(all_user_factors, all_veh_factors)))
all_factors <- all_factors[!is.na(all_factors) & all_factors != ""]

# Add unmapped factors to "Other" (so nothing is dropped)
missing_in_map <- setdiff(all_factors, names(factor_group))
if (length(missing_in_map) > 0) {
  factor_group <- c(factor_group, setNames(rep("Other", length(missing_in_map)), missing_in_map))
}

# Stable column order: group blocks then alphabetical within block
col_order <- unlist(lapply(group_levels, function(g) {
  sort(names(factor_group)[factor_group == g])
}))
col_order <- col_order[col_order %in% all_factors]

M <- matrix(0, nrow = nrow(df), ncol = length(col_order))
colnames(M) <- col_order
rownames(M) <- paste0(df$StudyNum)

for (i in seq_len(nrow(df))) {
  u <- canonicalize_factor(split_plus(df[[col_user]][i]))
  v <- canonicalize_factor(split_plus(df[[col_vehicle]][i]))
  present <- intersect(unique(c(u, v)), col_order)
  if (length(present) > 0) M[i, present] <- 1
}

# Column split (block grouping)
col_split <- factor(factor_group[col_order], levels = group_levels)

# ----------------------------
# 5) Clustering (Jaccard, binary)
# ----------------------------
row_d  <- vegan::vegdist(M, method = "jaccard", binary = TRUE)
row_hc <- hclust(row_d, method = "average")
row_dend <- as.dendrogram(row_hc)

# (Optional) column clustering if you want later:
# col_d <- vegan::vegdist(t(M), method="jaccard", binary=TRUE)
# col_hc <- hclust(col_d, method="average")
# col_dend <- as.dendrogram(col_hc)

# ----------------------------
# 6) Annotation colors (muted)
# ----------------------------
platform_colors <- c(
  "Simulator" = "#6BA3C7",
  "Real-world Driving" = "#7FB685"
)

mode_colors <- c(
  "Fully Automated" = "#A98BBF",
  "Supervised Automation" = "#D8B26E",
  "Wizard-of-Oz" = "#9E9E9E"
)

fallback_color <- "#B8B8B8"

plat_vals <- df$Platform;    plat_vals[is.na(plat_vals)] <- "NA"
mode_vals <- df$DrivingMode; mode_vals[is.na(mode_vals)] <- "NA"

for (lv in unique(plat_vals)) if (!lv %in% names(platform_colors)) platform_colors[lv] <- fallback_color
for (lv in unique(mode_vals)) if (!lv %in% names(mode_colors)) mode_colors[lv] <- fallback_color

row_ha <- rowAnnotation(
  Platform = plat_vals,
  `Driving mode` = mode_vals,
  col = list(
    Platform = platform_colors,
    `Driving mode` = mode_colors
  ),
  annotation_name_gp = gpar(fontface = "bold", fontsize = 10)
)

# Binary colormap
col_fun <- circlize::colorRamp2(c(0, 1), c("#FFFFFF", "#1F1F1F"))

# ============================================================
# OPTION 1: Single heatmap + Platform & Driving Mode annotations
# ============================================================
ht_option1 <- Heatmap(
  M,
  name = "Coverage",
  col = col_fun,
  cluster_rows = row_dend,
  cluster_columns = FALSE,
  show_row_names = TRUE,
  row_names_gp = gpar(fontsize = 8),
  show_column_names = TRUE,
  column_names_gp = gpar(fontsize = 8, fontface = "bold"),
  column_split = col_split,
  left_annotation = row_ha,
  heatmap_legend_param = list(at = c(0, 1), labels = c("Absent", "Present"))
)

pdf("heatmap_option1_platform_mode.pdf", width = 13, height = 8)
draw(ht_option1, merge_legends = TRUE)
dev.off()

png("heatmap_option1_platform_mode.png", width = 2400, height = 1400, res = 220)
draw(ht_option1, merge_legends = TRUE)
dev.off()

# ============================================================
# OPTION 3: Split by PLATFORM (Simulator vs Real-world Driving)
# ============================================================
make_platform_panel <- function(platform_label) {
  idx <- which(df$Platform == platform_label)
  if (length(idx) == 0) return(NULL)
  
  Mi <- M[idx, , drop = FALSE]
  rd <- vegan::vegdist(Mi, method = "jaccard", binary = TRUE)
  hc <- hclust(rd, method = "average")
  dend <- as.dendrogram(hc)
  
  ha <- rowAnnotation(
    `Driving mode` = df$DrivingMode[idx],
    col = list(`Driving mode` = mode_colors),
    annotation_name_gp = gpar(fontface = "bold", fontsize = 10)
  )
  
  Heatmap(
    Mi,
    name = platform_label,
    col = col_fun,
    cluster_rows = dend,
    cluster_columns = FALSE,
    show_row_names = TRUE,
    row_names_gp = gpar(fontsize = 7),
    show_column_names = TRUE,
    column_names_gp = gpar(fontsize = 7, fontface = "bold"),
    column_split = col_split,
    left_annotation = ha
  )
}

ht_sim  <- make_platform_panel("Simulator")
ht_real <- make_platform_panel("Real-world Driving")

if (!is.null(ht_sim) && !is.null(ht_real)) {
  pdf("heatmap_option3_split_platform.pdf", width = 16, height = 8)
  draw(ht_sim + ht_real, merge_legends = TRUE)
  dev.off()
  
  png("heatmap_option3_split_platform.png", width = 3000, height = 1400, res = 220)
  draw(ht_sim + ht_real, merge_legends = TRUE)
  dev.off()
}

# ============================================================
# OPTION 4: Split by DRIVING MODE (FA / SA / WoZ)
# ============================================================
make_mode_panel <- function(mode_label) {
  idx <- which(df$DrivingMode == mode_label)
  if (length(idx) == 0) return(NULL)
  
  Mi <- M[idx, , drop = FALSE]
  rd <- vegan::vegdist(Mi, method = "jaccard", binary = TRUE)
  hc <- hclust(rd, method = "average")
  dend <- as.dendrogram(hc)
  
  ha <- rowAnnotation(
    Platform = df$Platform[idx],
    col = list(Platform = platform_colors),
    annotation_name_gp = gpar(fontface = "bold", fontsize = 10)
  )
  
  Heatmap(
    Mi,
    name = mode_label,
    col = col_fun,
    cluster_rows = dend,
    cluster_columns = FALSE,
    show_row_names = TRUE,
    row_names_gp = gpar(fontsize = 7),
    show_column_names = TRUE,
    column_names_gp = gpar(fontsize = 7, fontface = "bold"),
    column_split = col_split,
    left_annotation = ha
  )
}

ht_fa  <- make_mode_panel("Fully Automated")
ht_sa  <- make_mode_panel("Supervised Automation")
ht_woz <- make_mode_panel("Wizard-of-Oz")

mode_panels <- list(ht_fa, ht_sa, ht_woz)
mode_panels <- mode_panels[!sapply(mode_panels, is.null)]

if (length(mode_panels) >= 2) {
  ht_stack <- Reduce(`%v%`, mode_panels)
  
  pdf("heatmap_option4_split_drivingmode.pdf", width = 13, height = 12)
  draw(ht_stack, merge_legends = TRUE)
  dev.off()
  
  png("heatmap_option4_split_drivingmode.png", width = 2400, height = 2200, res = 220)
  draw(ht_stack, merge_legends = TRUE)
  dev.off()
}

# ============================================================
# OPTION 5: Option 1 + top marginal coverage barplot (%)
# ============================================================
coverage_rate <- colMeans(M) * 100

top_ha <- HeatmapAnnotation(
  `Coverage (%)` = anno_barplot(
    coverage_rate,
    gp = gpar(fill = "#7A7A7A", col = NA),
    border = FALSE,
    height = unit(1.2, "cm")
  ),
  annotation_name_gp = gpar(fontface = "bold", fontsize = 10)
)

ht_option5 <- Heatmap(
  M,
  name = "Coverage",
  col = col_fun,
  cluster_rows = row_dend,
  cluster_columns = FALSE,
  show_row_names = TRUE,
  row_names_gp = gpar(fontsize = 8),
  show_column_names = TRUE,
  column_names_gp = gpar(fontsize = 8, fontface = "bold"),
  column_split = col_split,
  top_annotation = top_ha,
  left_annotation = row_ha,
  heatmap_legend_param = list(at = c(0, 1), labels = c("Absent", "Present"))
)

pdf("heatmap_option5_platform_mode_marginals.pdf", width = 13, height = 9)
draw(ht_option5, merge_legends = TRUE)
dev.off()

png("figures/heatmap_option5_platform_mode_marginals.png", width = 2400, height = 1600, res = 220)
draw(ht_option5, merge_legends = TRUE)
dev.off()

message("DONE. Heatmaps written to working directory.")

dir.create("figures", showWarnings = FALSE, recursive = TRUE)

png("figures/heatmap_option1.png", width = 7, height = 5, units = "in", res = 300)
ComplexHeatmap::draw(ht_option1, merge_legends = TRUE)
dev.off()
save_heatmap <- function(ht, filename, width = 7, height = 5, dpi = 300) {
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  png(filename, width = width, height = height, units = "in", res = dpi)
  on.exit(dev.off(), add = TRUE)
  ComplexHeatmap::draw(ht, merge_legends = TRUE)
}

ht_option1

save_heatmap(ht_option1, "figures/heatmap_option1.png", width = 7, height = 5, dpi = 300)
