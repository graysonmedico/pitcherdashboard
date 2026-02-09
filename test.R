# ==============================================================================
# 02_dashboard_report.R
# Purpose: Load cleaned data, filter for specific pitcher/date, generate visual report.
# ==============================================================================

library(tidyverse)
library(patchwork)
library(gridExtra)
library(grid)
library(cowplot)
library(magick)

# =========================================
# 1. USER INPUTS (EDIT THESE)
# =========================================
target_pitcher_name <- "Pitcher, Test"  # Exact name in CSV
target_game_date    <- "2025-01-30"     # Format YYYY-MM-DD

# =========================================
# 2. LOAD & FILTER DATA
# =========================================
# Load the file we created in Script 1
trackman_full <- readRDS("data/trackman_cleaned.rds")

# Filter for the specific game/pitcher
trackman_clean <- trackman_full %>%
  filter(
    Pitcher == target_pitcher_name,
    Date == as.Date(target_game_date)
  )

# Check if data exists
if(nrow(trackman_clean) == 0) {
  stop("No data found for this Pitcher/Date combo. Check spelling or cleaning script.")
}

# =========================================
# 3. SETUP STYLES & HELPERS
# =========================================
team_primary <- "#051A39"    # Banyan Navy
bg_color     <- "#AC975E"    # Banyan Gold

# Pitch Colors
pitch_colors <- c(
  "Fastball" = "#D92027",  "Two-seam Fastball" = "#ED7014", "Sinker" = "#ED7014",
  "Cutter" = "#FFC425",    "Slider" = "#00B1B0",            "Sweeper" = "#00B1B0",
  "Curveball" = "#005792", "ChangeUp" = "#A020F0",          "Splitter" = "#AC975E", 
  "Knuckleball" = "#808080"
)

# Theme
theme_report <- theme_minimal(base_size = 12) +
  theme(
    text = element_text(color = "#051A39"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.background = element_rect(fill = bg_color, color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
    panel.grid = element_line(color = "#eeeeee"),
    axis.title = element_blank(),
    legend.position = "none",
    plot.margin = margin(5, 5, 5, 5)
  )

theme_wrapper <- theme(plot.background = element_rect(fill = bg_color, color = NA))

# Helper: Get Mode (Most Frequent)
get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  if(length(uniqv)==0) return("-")
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# =========================================
# 4. TABLES
# =========================================

# --- A. Top Summary Table ---
main_tbl_data <- trackman_clean %>%
  group_by(`Pitch Type` = TaggedPitchType) %>%
  summarise(
    `#` = n(),
    `Avg Velo` = sprintf("%.1f", mean(RelSpeed, na.rm = TRUE)),
    `Max Velo` = sprintf("%.1f", max(RelSpeed, na.rm = TRUE)),
    `Avg Spin` = round(mean(SpinRate, na.rm = TRUE)),
    `Vert` = sprintf("%.1f", mean(InducedVertBreak, na.rm = TRUE)),
    `Horz` = sprintf("%.1f", mean(HorzBreak, na.rm = TRUE)),
    `Tilt` = as.character(get_mode(Tilt)),
    `Rel H` = sprintf("%.2f", mean(RelHeight, na.rm = TRUE)),
    `Rel S` = sprintf("%.2f", mean(RelSide, na.rm = TRUE)),
    `Ext` = sprintf("%.2f", mean(Extension, na.rm = TRUE)),
    `Strike%` = sprintf("%.0f%%", mean(isStrike, na.rm = TRUE) * 100),
    `Whiff%` = sprintf("%.0f%%", (sum(isWhiff, na.rm = TRUE) / max(1, sum(isSwing, na.rm = TRUE))) * 100)
  ) %>%
  arrange(desc(`#`))

# --- B. BIP Box ---
bip_data <- trackman_clean %>%
  filter(isInPlay) %>%
  summarise(
    BIP = as.character(n()),
    `Avg EV` = sprintf("%.1f", mean(ExitSpeed, na.rm = TRUE)),
    `Hard%`  = sprintf("%.0f%%", mean(ExitSpeed > 95, na.rm = TRUE) * 100),
    `Gnd%`   = sprintf("%.0f%%", mean(Angle < 10, na.rm = TRUE) * 100)
  ) %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Val")

if(nrow(bip_data) == 0) bip_data <- tibble(Metric="No BIP", Val="-")

# --- C. Count Box ---
target_counts <- c("0-0", "1-1", "Full") 
count_data <- trackman_clean %>%
  filter(CountStr %in% target_counts) %>%
  group_by(Count = CountStr) %>%
  summarise(
    Pitches = n(),
    `Strk%` = sprintf("%.0f%%", mean(isStrike, na.rm = TRUE) * 100)
  ) %>%
  arrange(factor(Count, levels = target_counts))

if(nrow(count_data) == 0) count_data <- tibble(Count="No Data", Pitches=0, `Strk%`="-")

# --- Create Table Grobs ---
make_pretty_table <- function(data, font_size = 10, padding_mm = 4) {
  t_theme <- ttheme_default(
    core = list(bg_params = list(fill = c("white", "#EFEFEF"), col = "white"),
                fg_params = list(fontsize = font_size), 
                padding = unit(c(padding_mm, padding_mm), "mm")),
    colhead = list(bg_params = list(fill = "#051A39", col = "white"),
                   fg_params = list(fontsize = font_size, fontface = "bold", col = "white"),
                   padding = unit(c(padding_mm, padding_mm), "mm"))
  )
  tableGrob(data, rows = NULL, theme = t_theme)
}

# EDIT: Reduced font_size from 13 to 12 to help fit more rows
grob_main  <- make_pretty_table(main_tbl_data, font_size = 12, padding_mm = 8)
grob_bip   <- make_pretty_table(bip_data, font_size = 12, padding_mm = 6)
grob_count <- make_pretty_table(count_data, font_size = 12, padding_mm = 6)

# =========================================
# 5. PLOTS
# =========================================

# Header
p_header <- ggplot() +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = team_primary) +
  annotate("text", x = 0.5, y = 0.5, label = paste("Banyan Pitcher:", target_pitcher_name), 
           fontface = "bold", size = 10, colour = "#FEFFFD") +
  annotate("text", x = 0.02, y = 0.2, label = "PITCHER REPORT", hjust = 0, size = 4, fontface = "bold", colour = "#FEFFFD") +
  annotate("text", x = 0.98, y = 0.2, label = paste("Date:", target_game_date), hjust = 1, size = 4, fontface = "bold",  colour = "#FEFFFD") +
  theme_void() + 
  theme(plot.background = element_rect(fill = bg_color, color = NA))

# 1. Release Batter View
p_release_bv <- trackman_clean %>%
  ggplot(aes(x = RelSide, y = RelHeight, fill = TaggedPitchType)) +
  geom_point(shape = 21, size = 4, color = "white", alpha = 0.9) +
  scale_fill_manual(values = pitch_colors) +
  scale_x_continuous(limits = c(-3, 3)) + 
  scale_y_continuous(limits = c(4, 7)) +
  labs(title = "RELEASE BATTER VIEW") + theme_report + theme(axis.text = element_blank())

# 2. Zone Plot
p_zone <- trackman_clean %>%
  ggplot(aes(x = PlateLocSide, y = PlateLocHeight, fill = TaggedPitchType)) +
  annotate("rect", xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5, fill = NA, color = "black", linewidth = 1) +
  geom_point(shape = 21, size = 4, color = "white", alpha = 0.9) +
  scale_fill_manual(values = pitch_colors) +
  coord_fixed(ratio = 1, xlim = c(-2.5, 2.5), ylim = c(0, 5)) +
  labs(title = "PITCH LOCATION") + theme_report + theme(axis.text = element_blank())

# 3. Movement
p_movement <- trackman_clean %>%
  ggplot(aes(x = HorzBreak, y = InducedVertBreak, fill = TaggedPitchType)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(shape = 21, size = 4, color = "white", alpha = 0.9) +
  scale_fill_manual(values = pitch_colors) +
  scale_x_continuous(limits = c(-25, 25)) + 
  scale_y_continuous(limits = c(-25, 25)) +
  labs(title = "MOVEMENT") + theme_report

# 4. Release Side View
p_release_side <- trackman_clean %>%
  ggplot(aes(x = Extension, y = RelHeight, fill = TaggedPitchType)) +
  geom_point(shape = 21, size = 4, color = "white", alpha = 0.9) +
  scale_fill_manual(values = pitch_colors) +
  scale_x_continuous(limits = c(4, 8)) + 
  scale_y_continuous(limits = c(4, 7)) +
  labs(title = "RELEASE SIDE VIEW") + theme_report 

# Legend
p_legend <- trackman_clean %>% distinct(TaggedPitchType) %>%
  ggplot(aes(x = 1, y = TaggedPitchType, color = TaggedPitchType)) +
  geom_point(size = 5) +
  geom_text(aes(label = TaggedPitchType), x = 1.2, hjust = 0, size = 5, fontface="bold", color="white") +
  scale_color_manual(values = pitch_colors) +
  scale_x_continuous(limits = c(0.8, 3)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black", color = NA), legend.position = "none")

# Logo
p_logo <- ggdraw() + theme(plot.background = element_rect(fill = bg_color, color = NA))
try({
  p_logo <- ggdraw() + 
    draw_image("mbanyan.webp", scale = 0.9) + 
    theme(plot.background = element_rect(fill = bg_color, color = NA))
}, silent = TRUE)

# =========================================
# 6. ASSEMBLE & SAVE
# =========================================
w_main   <- wrap_elements(grob_main) & theme_wrapper
w_bip    <- wrap_elements(grob_bip) & theme_wrapper
w_count  <- wrap_elements(grob_count) & theme_wrapper
w_legend <- wrap_elements(p_legend) & theme_wrapper
w_logo   <- wrap_elements(p_logo) & theme_wrapper

# EDIT: Added an extra row of 'B's to give the table more vertical space.
# This forces the plots (C, D, E) to start lower down the page.
design_layout <- "
AAAAAAAAAAAAAAAAAAAA
BBBBBBBBBBBBBBBBBBBB
BBBBBBBBBBBBBBBBBBBB
BBBBBBBBBBBBBBBBBBBB
CCCCCCDDDDDDDDEEEEEE
CCCCCCDDDDDDDDEEEEEE
CCCCCCDDDDDDDDEEEEEE
FFFFFGGGGHHHJJJJIIII
FFFFFGGGGHHHJJJJIIII
"

final_plot <- p_header +
  w_main +
  p_release_bv + p_zone + p_movement +
  p_release_side + w_bip + w_count + w_legend + w_logo +
  plot_layout(design = design_layout) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = bg_color, color = NA)))

# Display in Viewer
print(final_plot)

output_filename <- paste0("Report_", gsub(" ", "", target_pitcher_name), "_", target_game_date, ".png")
ggsave(output_filename, final_plot, width = 14, height = 10, units = "in", dpi = 300)
