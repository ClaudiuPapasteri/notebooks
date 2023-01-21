# ssm_map

# How to use:
# use Table Search box to filter (e.g.) for Object type
# select points on graph with Box Select (click the Home button on plot to deselect/refresh both plot and table)
# select rows one by one or range holding shift key on table
# export to xlsx/csv works on current selection (if you want the complete data be sure to have no active selection - refresh page for e.g.)

# Notes: 
# Every participant plays with 4 objects (except id 51.0 bot condition with a partial recording of only 2 objects).
# Hence, score for game states is participant mean on that object (some participants encounter some items on multiple trials).
# Object positions are identical within a config, but differ across configs. Every config has a sample of only 4 items, so there is no overarching correspondence between Object type and location, as this may be set in each individual config (this was done by Natalie, I was told).

# Clear insights:
# Some object locations are in middle (inside the radius 5 circle), the others are outside around the radius 7 circle.  


marker_def_vef <- c(     # meant to be used with dplyr::recode() 
  "0"  = "Gamepad", 
  "1"  = "RubicCube", 
  "2"  = "Sandwich", 
  "3"  = "BowlingPin", 
  "4"  = "Heart", 
  "5"  = "Bananas", 
  "6"  = "Extinguisher", 
  "7"  = "Laptop", 
  "8"  = "Arcade", 
  "9"  = "Pen", 
  "10" = "Trophy", 
  "11" = "Book", 
  "12" = "Lock", 
  "13" = "Diamond", 
  "14" = "Apple", 
  "15" = "Key", 
  "16" = "Scissors", 
  "17" = "Milk", 
  "18" = "Ventilator", 
  "19" = "Coin", 
  "20" = "Start", 
  "21" = "Flag"
)

# Game State Labels
gamestate_lab <- c(     # meant to be used with dplyr::recode(as.character(newGameState_f), !!!gamestate_lab) 
  "0"  = "0", 
  "1"  = "1", 
  "2"  = "2", 
  "3"  = "3", 
  "4"  = "4", 
  "5"  = "5", 
  "6"  = "6", 
  "7"  = "7", 
  "8"  = "8", 
  "9"  = "RO", 
  "10" = "10", 
  "11" = "11", 
  "12" = "12", 
  "13" = "13", 
  "14" = "RSf",  
  "15" = "RYf", 
  "16" = "R1O",
  "17" = "S", 
  "18" = "R2O", 
  "19" = "19", 
  "20" = "20", 
  "21" = "21"
)

# Package names
packages <- c(
  "tidyverse", "ggplot2", "ggforce", "scales",
  "crosstalk", "plotly", "DT",
  "ggstatsplot"
)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load
invisible(lapply(packages, library, character.only = TRUE))


# Read data
folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_01.09-24.11.2022/ssm_map"
setwd(folder)

data <- readRDS("recomputed_df.RDS")   # recomputed_df is from SSM_analyses_v2_nooutlier.R (without outlier exclusion)

# Transform data
df_ObjectMarker <-
  data %>%
  dplyr::select("id", "condition", "order", "frame", "newGameState_f",
                "xPos", "yPos", "zPos",
                "Marker_type", "ObjectMarker_x", "ObjectMarker_z",
                "score") %>%
  dplyr::filter(! Marker_type %in% c("Start", "Flag")) %>%
  dplyr::distinct(Marker_type, ObjectMarker_x, ObjectMarker_z) %>%
  dplyr::mutate(rowid = dplyr::row_number())  # 138

df_ObjectMarker_score <- 
  data %>%
  dplyr::select("id", "condition", "order", "frame", "newGameState_f",
                "xPos", "yPos", "zPos",
                "Marker_type", "ObjectMarker_x", "ObjectMarker_z",
                "score") %>%
  dplyr::filter(newGameState_f %in% c(9, 16, 17, 18)) %>%  # "9"  = "RO", "16" = "R1O", "17" = "S", "18" = "R2O" 
  dplyr::mutate(newGameState_c = dplyr::recode(as.character(newGameState_f), !!!gamestate_lab))  %>%
  dplyr::select(-newGameState_f) %>%
  dplyr::group_by(id, condition, Marker_type, newGameState_c, ObjectMarker_x, ObjectMarker_z) %>%
  dplyr::summarise(mean_score = mean(score, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = newGameState_c, values_from = mean_score) %>%  # id 51.0 bot has only 2 objects => 488/4=122
  dplyr::ungroup() %>%
  dplyr::mutate(rowid = dplyr::row_number()) 


df_ObservationMarker <- 
  data %>%
  dplyr::select("id", "condition", "order", "frame", 
                "ObservationMarker", "ObservationMarker_x", "ObservationMarker_z") %>%
  dplyr::distinct(ObservationMarker, ObservationMarker_x, ObservationMarker_z)   # 12 constant Observation points
  
# Plot
# ggplot() +
#   ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 17)) +
#   ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 20)) +
#   ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 5)) +
#   ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 7)) +
#   geom_point(aes(x = ObjectMarker_x, y = ObjectMarker_z), alpha = 0.4, data = df_ObjectMarker) +
#   ggforce::geom_circle(aes(x0 = ObservationMarker_x, y0 = ObservationMarker_z, r = 1), data = df_ObservationMarker) +
#   scale_x_continuous(breaks = seq(-25, 25, 5)) +
#   scale_y_continuous(breaks = seq(-25, 25, 5)) +
#   theme_bw()


# Interactive
circleFun <- function(center = c(0,0), r = 1, npoints = 100){
  tt <- seq(0,2*pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

circ_r20 <- circleFun(r = 20, npoints = 100)
circ_r17 <- circleFun(r = 17, npoints = 100)
circ_r7 <- circleFun(r = 7, npoints = 100)
circ_r5 <- circleFun(r = 5, npoints = 100)
# df_ObservationMarker_xy <- apply(df_ObservationMarker[c("ObservationMarker_x", "ObservationMarker_z")], 1, circleFun)
# df_ObservationMarker_xy <- bind_rows(df_ObservationMarker_xy, .id = "column_label")
# df_ObservationMarker_xy$ObservationMarker <- df_ObservationMarker$ObservationMarker[as.numeric(df_ObservationMarker_xy$column_label)]

df_ObjectMarker_sd <- crosstalk::SharedData$new(
  df_ObjectMarker_score[, c("rowid",
                            "id", "condition",      
                            "Marker_type", "ObjectMarker_x", "ObjectMarker_z",
                            "R1O", "R2O", "RO", "S")]) # key = ~rowid , group = "group"
                                              

plot <- 
  ggplot(data = df_ObjectMarker_sd) +
  # geom_point(data = df_ObjectMarker_sd, aes(x = ObjectMarker_x, y = ObjectMarker_z, type = Marker_type), alpha = 0.4) +
  geom_path(data = circ_r20, aes(x = x, y = y, radius = 20)) +
  geom_path(data = circ_r17, aes(x = x, y = y, radius = 17)) +
  geom_path(data = circ_r7, aes(x = x, y = y, radius = 7)) +
  geom_path(data = circ_r5, aes(x = x, y = y, radius = 5)) +
  # geom_path(data = df_ObservationMarker_xy, aes(x = x, y = y, type = ObservationMarker)) +
  geom_point(data = df_ObservationMarker, aes(x = ObservationMarker_x, y = ObservationMarker_z, type = ObservationMarker), size = 3) +
  scale_x_continuous(breaks = seq(-25, 25, 5)) +
  scale_y_continuous(breaks = seq(-25, 25, 5)) +
  coord_fixed() +
  theme_bw() +
  geom_point(data = df_ObjectMarker_sd, aes(x = ObjectMarker_x, y = ObjectMarker_z, type = Marker_type), color = "gray")


suppressWarnings({
  crosstalk::bscols(
    widths = 12,
    plotly::ggplotly(plot) %>% 
      # plotly::add_markers(data = df_ObjectMarker_sd, x = ~ObjectMarker_x, y = ~ObjectMarker_z, bla = ~Marker_type) %>%
      plotly::layout(dragmode = "select") %>%  # set Select Box as default tool
      plotly::highlight(color = "red", on = "plotly_selected", off = "plotly_relayout",
                        selected = plotly::attrs_selected(mode = "markers")
                        ),
    htmltools::br(),
    htmltools::br(),
    htmltools::div(style = "width: 100%", 
    DT::datatable(df_ObjectMarker_sd,
                  extensions = 'Buttons',
                  fillContainer = TRUE,
                  rownames = FALSE,
                  selection = 'multiple',
                 # style = "bootstrap", 
                  class = "compact", #width = "1000px",
                  options = list(#autowidth = TRUE,
                                 deferRender = FALSE,
                                 pageLength = 30,
                                 scrollX = '100px',
                                 # scrollY = '400px'
                                 dom = 'Bfrtip',
                                 buttons = c('excel', "csv")))
    )
  )
})  




