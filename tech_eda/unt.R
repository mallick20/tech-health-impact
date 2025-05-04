library(tidyverse)


years <- c(1994, 1997, 1998, 2000, 2001, 2003, 2007, 2009, 
           2010, 2011, 2012, 2013, 2015, 2017, 2019, 2021, 2023)

combined_cps_data <- tibble()

for (year in years) {
  file_name <- paste0("cps_", year, ".csv")
  
  if (file.exists(file_name)) {
    message("Reading: ", file_name)
    
    # Read CSV with all columns as characters
    df <- read_csv(file_name, col_types = cols(.default = col_character()), show_col_types = FALSE)
    
    # Convert column names to lowercase
    colnames(df) <- tolower(colnames(df))
    
    # If dataset contains 'hryear', rename it to 'hryear4' and update value
    if ("hryear" %in% names(df)) {
      df <- df %>%
        rename(hryear4 = hryear) %>%
        mutate(hryear4 = case_when(
          hryear4 == "94" ~ "1994",
          hryear4 == "97" ~ "1997",
          TRUE ~ hryear4
        ))
    }
    
    # If hryear4 is still missing, add it manually
    if (!"hryear4" %in% names(df)) {
      df$hryear4 <- as.character(year)
    }
    
    # Make sure hryear4 is character (enforced again for safety)
    df$hryear4 <- as.character(df$hryear4)
    
    # Bind the row into the main dataset
    combined_cps_data <- bind_rows(combined_cps_data, df)
    
  } else {
    warning("File not found: ", file_name)
  }
}

# Final structure check
glimpse(combined_cps_data)