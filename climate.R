library(Cairo)
library(dplyr)
library(readr)
library(stringr)
############################
source("diagwl_new.R")

# Read and process all CSV files
dataHandling <-function(station_name, filename, elevation, per){
  files <- list.files("data", pattern = "*.csv", full.names = TRUE)
  
  # Columns to be selected
  filter_cols <- c("觀測時間(month)", "最高氣溫(℃)", "最低氣溫(℃)", "氣溫(℃)", "降水量(mm)")
  file_folder <- list() # Store filtered data
  
  # Loop through each file and read data
  for (file in files) {
    try({
      df <- read_csv(file, show_col_types = FALSE) %>%
        select(all_of(filter_cols)) %>%
        filter(!if_any(everything(), ~ str_detect(as.character(.x), "/")))
      file_folder[[length(file_folder) + 1]] <- df
    }, silent = TRUE)
  }
  
  # Combine all files
  all_data <- bind_rows(file_folder)
  # Define month list
  month_list <- sprintf("%02d", 1:12)
  # Initialize result matrix
  result <- matrix(nrow = 4, ncol = 12)
  rownames(result) <- c("Prec.", "Max.t.", "Min.t.", "Ab.m.t.")
  colnames(result) <- toupper(month.abb)
  # Calculate monthly statistics
  for (i in seq_along(month_list)) {
    m <- month_list[i]
    month_data <- filter(all_data, `觀測時間(month)` == m)
    result[1, i] <- round(mean(as.numeric(month_data$`降水量(mm)`), na.rm = TRUE), 2)
    result[2, i] <- round(mean(as.numeric(month_data$`最高氣溫(℃)`), na.rm = TRUE), 2)
    result[3, i] <- round(mean(as.numeric(month_data$`最低氣溫(℃)`), na.rm = TRUE), 2)
    result[4, i] <- round(mean(as.numeric(month_data$`氣溫(℃)`), na.rm = TRUE), 2)
  }
  
  clim_df <- as.matrix(as.data.frame(result,row.names = 1))
  
  # Export climate diagram
  exportClimateDiagram(clim_df, station_name, filename, elevation, per)
}

############################

exportClimateDiagram <- function(climdata, station_name, filename, elevation, per) {
  setwd("your path")
  CairoFonts(regular = "Noto Sans T Chinese:style=Light", bold = "Noto Sans T Chinese:style=Regular")
  par(family="BiauKaiTC-Regular")
  # Set output as PNG image
  Cairo(1600,1600,file= filename, type='png',bg="white",dpi=300)
  diagwl_new(climdata, stname = station_name, 
             alt = elevation, mlab = 'en', per = per, cols = NULL)
  dev.off()
}

# Execute for station "station name"
dataHandling('station name', "filename.png", 'station elevation', 'time range')
