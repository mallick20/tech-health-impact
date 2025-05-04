library(tidyverse)

# Load 1998 CSV
df_1998 <- read_csv("cps_1998.csv", show_col_types = FALSE) %>%
  select(any_of(c("hryear4", "hescu1a", "hesiu3",
                  "prs11","pescu1", "pescu2", "pescu3",
                  "hescu1a","hescu1b","hescu2","hescu3",
                  "hesiu1","hesiu2","hesiu4","prs11",
                  "hesiu20")))

# Load 2000 CSV
df_2000 <- read_csv("cps_2000.csv", show_col_types = FALSE) %>%
  select(any_of(c("hryear4","hescu1a", "hesiu3","prs11",
                  "pescu1", "pescu2", "pescu3","hescu1a",
                  "hescu1b","hescu2","hescu3","hesiu1",
                  "hesiu2","hesiu4","prs11","hesiu20")))

# Load 2001 CSV
df_2001 <- read_csv("cps_2001.csv", show_col_types = FALSE) %>%
  select(any_of(c("hryear4","hesc1","hesc2","hesc3",
                  "hesint1","hesint2a","hesint41",
                  "hesint42","hesint43","hesint44",
                  "pesnet","pesneta","pesnetb","pesnetc",
                  "pesnetd","pesnete","pesnetf","pesnetg",
                  "pesch7","pesnetc","pesnetd","pescw2")))

# Load 2003 CSV
df_2003 <- read_csv("cps_2003.csv", show_col_types = FALSE) %>%
  select(any_of(c("hryear4","hesc1","hesc2","hesc3",
                  "hesint1","hesint2a","hesint6a",
                  "hesint6b","hesint6c","hesint6d",
                  "hesint6e","hesint6f","pesnet","pesneta",
                  "pesnetb","pesnetd","pesnete","pesnetf",
                  "pesnetg","pescw2")))

# Load 2007 CSV
df_2007 <- read_csv("cps_2007.csv", show_col_types = FALSE) %>%
  select(any_of(c("hryear4","henet1","henet3","henet4",
                  "hetelhhd","hetelavl")))

# Load 2009 CSV
df_2009 <- read_csv("cps_2009.csv", show_col_types = FALSE) %>%
  select(any_of(c("hryear4","henet1","henet3","henet4",
                  "hetelhhd","hetelavl")))

# Load 2010 CSV
df_2010 <- read_csv("cps_2010.csv", show_col_types = FALSE) %>%
  select(any_of(c("hryear4","hecomp11","hecomp12","hecomp13",
                  "henet2a","henet2b","henet4a1","henet4b3",
                  "henet5a","penet6","henet72","heserv31",
                  "heserv32","heserv33","heserv34","heserv35",
                  "heserv36","heserv37","hetelhhd","hetelavl")))

# Load 2011 CSV
df_2011 <- read_csv("cps_2011.csv", show_col_types = FALSE) %>%
  select(any_of(c("pesci1","pesc2a1","pesc2a2","pesc2a3",
                  "pesc2a4","pesc2a5","pesc2a6","pesc2a7",
                  "pesc2a9","pesc2a10","peprim1","hesci3",
                  "ptprim2","pepr3a1","pepr3a2","pepr3a3",
                  "pepr3a5","pepr3a6","pepr3a8","peprim10",
                  "peprim15b","peprim16b","hesci5","hesci3",
                  "hesci6","hesci75","pecell","pegame",
                  "petvba","pesc2a6","pesc2a7","pesc2a9",
                  "pesc2a10","peprm16b","pedesk","pelapt",
                  "petabl","hetelhhd","hetelavl")))

# Load 2012 CSV
df_2012 <- read_csv("cps_2012.csv", show_col_types = FALSE) %>%
  select(any_of(c("penet1","penet1a","henet2","henet3",
                  "henet3a","henet4","henet5","henet5b",
                  "henet5d","henet6","henet6b","henet7",
                  "henet7b","penet8","henet9","penet10",
                  "henet11","penet12")))

# Load 2013 CSV
df_2013 <- read_csv("cps_2013.csv", show_col_types = FALSE) %>%
  select(any_of(c("henet2","henet3","hesc2a8","hesc2a11",
                  "peprm32","peprm33","peprm310","hesci111")))

# Load 2015 CSV
df_2015 <- read_csv("cps_2015.csv", show_col_types = FALSE) %>%
  select(any_of(c("hryear4","helaptop","hedesktp",
                  "hetablet","hetvbox","hewearab",
                  "heinhome","heinwork","heinschl",
                  "hemphone", "pesocial","pegaming",
                  "pevideo","peaudio")))

# Load 2017 CSV
df_2017 <- read_csv("cps_2017.csv", show_col_types = FALSE) %>%
  select(any_of(c("hryear4","helaptop","hedesktp",
                  "hetablet","hetvbox","hewearab",
                  "heinhome","heinwork","heinschl",
                  "hemphone", "pesocial","pegaming",
                  "pevideo","peaudio")))

# Load 2019 CSV
df_2019 <- read_csv("cps_2019.csv", show_col_types = FALSE) %>%
  select(any_of(c("hryear4","helaptop","hedesktp",
                  "hetablet","hetvbox","hewearab",
                  "heinhome","heinwork","heinschl",
                  "hemphone", "pesocial","pevideo",
                  "peaudio")))

# Load 2021 CSV
df_2021 <- read_csv("cps_2021.csv", show_col_types = FALSE) %>%
  select(any_of(c("hryear4","helaptop","hedesktp",
                  "hetablet","hetvbox","hewearab",
                  "heinhome","heinwork","heinschl",
                  "hemphone", "pesocial","pegaming",
                  "pevideo","peaudio")))

# Load 2023 CSV
df_2023 <- read_csv("cps_2023.csv", show_col_types = FALSE) %>%
  select(any_of(c("hryear4","helaptop","hedesktp",
                  "hetablet","hetvbox","hewearab",
                  "heinhome","heinwork","heinschl",
                  "hemphone", "pesocial","pegaming",
                  "pevideo","peaudio")))

library(tidyverse)

# Summarize 2001
summary_2001 <- df_2001 %>%
  summarize(
    computer_home = mean(hesc1 == 1, na.rm = TRUE),
    internet_home = mean(hesint1 == 1, na.rm = TRUE),
    used_internet = mean(pesnet == 1, na.rm = TRUE)
  ) %>%
  mutate(year = 2001)

# Summarize 2003
summary_2003 <- df_2003 %>%
  summarize(
    computer_home = mean(hesc1 == 1, na.rm = TRUE),
    internet_home = mean(hesint1 == 1, na.rm = TRUE),
    used_internet = mean(pesnet == 1, na.rm = TRUE)
  ) %>%
  mutate(year = 2003)

# Combine
tech_trends <- bind_rows(summary_2001, summary_2003) %>%
  pivot_longer(cols = -year, names_to = "metric", values_to = "percent")

# Plot
ggplot(tech_trends, aes(x = year, y = percent, color = metric)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Computer and Internet Use Trends (2001 vs 2003)",
    x = "Year",
    y = "Percentage",
    color = "Metric"
  ) +
  theme_minimal()

library(tidyverse)

# Summarize 2010
summary_2010 <- df_2010 %>%
  summarize(
    internet_home = mean(henet2a == 1, na.rm = TRUE),
    broadband_home = mean(heserv31 == 1, na.rm = TRUE)
  ) %>%
  mutate(year = 2010)

# Summarize 2011
summary_2011 <- df_2011 %>%
  summarize(
    internet_home = mean(pesci1 == 1, na.rm = TRUE),
    broadband_home = mean(pecell == 1, na.rm = TRUE)  # Approximating broadband proxy (if available)
  ) %>%
  mutate(year = 2011)

# Summarize 2012
summary_2012 <- df_2012 %>%
  summarize(
    internet_home = mean(penet1 == 1, na.rm = TRUE),
    broadband_home = mean(henet5 == 1, na.rm = TRUE)
  ) %>%
  mutate(year = 2012)

# Summarize 2013
summary_2013 <- df_2013 %>%
  summarize(
    internet_home = mean(henet2 == 1, na.rm = TRUE),
    broadband_home = mean(hesc2a8 == 1, na.rm = TRUE)  # Approximation based on available fields
  ) %>%
  mutate(year = 2013)

# Combine all summaries
tech_trends <- bind_rows(summary_2010, summary_2011, summary_2012, summary_2013) %>%
  pivot_longer(cols = -year, names_to = "metric", values_to = "percent")

# Plot
ggplot(tech_trends, aes(x = year, y = percent, color = metric)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Internet and Broadband Use (2010–2013)",
    x = "Year",
    y = "Percentage",
    color = "Metric"
  ) +
  theme_minimal()



# Summarize 2015
summary_2015 <- df_2015 %>%
  summarize(
    laptop = mean(helaptop == 1, na.rm = TRUE),
    desktop = mean(hedesktp == 1, na.rm = TRUE),
    tablet = mean(hetablet == 1, na.rm = TRUE),
    tvbox = mean(hetvbox == 1, na.rm = TRUE),
    wearable = mean(hewearab == 1, na.rm = TRUE),
    home_internet = mean(heinhome == 1, na.rm = TRUE),
    work_internet = mean(heinwork == 1, na.rm = TRUE),
    school_internet = mean(heinschl == 1, na.rm = TRUE),
    mobile_phone_internet = mean(hemphone == 1, na.rm = TRUE)
  ) %>%
  mutate(year = 2015)

# Summarize 2017
summary_2017 <- df_2017 %>%
  summarize(
    laptop = mean(helaptop == 1, na.rm = TRUE),
    desktop = mean(hedesktp == 1, na.rm = TRUE),
    tablet = mean(hetablet == 1, na.rm = TRUE),
    tvbox = mean(hetvbox == 1, na.rm = TRUE),
    wearable = mean(hewearab == 1, na.rm = TRUE),
    home_internet = mean(heinhome == 1, na.rm = TRUE),
    work_internet = mean(heinwork == 1, na.rm = TRUE),
    school_internet = mean(heinschl == 1, na.rm = TRUE),
    mobile_phone_internet = mean(hemphone == 1, na.rm = TRUE)
  ) %>%
  mutate(year = 2017)
# Combine
device_trends <- bind_rows(summary_2015, summary_2017) %>%
  pivot_longer(cols = -year, names_to = "device_or_access", values_to = "percent")

# Plot
ggplot(device_trends, aes(x = year, y = percent, color = device_or_access)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Device Ownership and Internet Access (2015 vs 2017)",
    x = "Year",
    y = "Percentage",
    color = "Device / Internet Access"
  ) +
  theme_minimal()

library(tidyverse)

# Summarize 2019
summary_2019 <- df_2019 %>%
  summarize(
    laptop = mean(helaptop == 1, na.rm = TRUE),
    desktop = mean(hedesktp == 1, na.rm = TRUE),
    tablet = mean(hetablet == 1, na.rm = TRUE),
    tvbox = mean(hetvbox == 1, na.rm = TRUE),
    wearable = mean(hewearab == 1, na.rm = TRUE),
    home_internet = mean(heinhome == 1, na.rm = TRUE),
    work_internet = mean(heinwork == 1, na.rm = TRUE),
    school_internet = mean(heinschl == 1, na.rm = TRUE),
    mobile_phone_internet = mean(hemphone == 1, na.rm = TRUE)
  ) %>%
  mutate(year = 2019)

# Summarize 2021
summary_2021 <- df_2021 %>%
  summarize(
    laptop = mean(helaptop == 1, na.rm = TRUE),
    desktop = mean(hedesktp == 1, na.rm = TRUE),
    tablet = mean(hetablet == 1, na.rm = TRUE),
    tvbox = mean(hetvbox == 1, na.rm = TRUE),
    wearable = mean(hewearab == 1, na.rm = TRUE),
    home_internet = mean(heinhome == 1, na.rm = TRUE),
    work_internet = mean(heinwork == 1, na.rm = TRUE),
    school_internet = mean(heinschl == 1, na.rm = TRUE),
    mobile_phone_internet = mean(hemphone == 1, na.rm = TRUE)
  ) %>%
  mutate(year = 2021)

# Summarize 2023
summary_2023 <- df_2023 %>%
  summarize(
    laptop = mean(helaptop == 1, na.rm = TRUE),
    desktop = mean(hedesktp == 1, na.rm = TRUE),
    tablet = mean(hetablet == 1, na.rm = TRUE),
    tvbox = mean(hetvbox == 1, na.rm = TRUE),
    wearable = mean(hewearab == 1, na.rm = TRUE),
    home_internet = mean(heinhome == 1, na.rm = TRUE),
    work_internet = mean(heinwork == 1, na.rm = TRUE),
    school_internet = mean(heinschl == 1, na.rm = TRUE),
    mobile_phone_internet = mean(hemphone == 1, na.rm = TRUE)
  ) %>%
  mutate(year = 2023)

# Combine
device_trends_recent <- bind_rows(summary_2019, summary_2021, summary_2023) %>%
  pivot_longer(cols = -year, names_to = "device_or_access", values_to = "percent")


# Plot
ggplot(device_trends_recent, aes(x = year, y = percent, color = device_or_access)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Device Ownership and Internet Access (2019–2023)",
    x = "Year",
    y = "Percentage",
    color = "Device / Internet Access"
  ) +
  theme_minimal()

# Summarize 2019 Activities
activities_2019 <- df_2019 %>%
  summarize(
    social_media = mean(pesocial == 1, na.rm = TRUE),
    video_streaming = mean(pevideo == 1, na.rm = TRUE),
    audio_streaming = mean(peaudio == 1, na.rm = TRUE)
  ) %>%
  mutate(year = 2019)

# Summarize 2021 Activities
activities_2021 <- df_2021 %>%
  summarize(
    social_media = mean(pesocial == 1, na.rm = TRUE),
    gaming = mean(pegaming == 1, na.rm = TRUE),
    video_streaming = mean(pevideo == 1, na.rm = TRUE),
    audio_streaming = mean(peaudio == 1, na.rm = TRUE)
  ) %>%
  mutate(year = 2021)

# Summarize 2023 Activities
activities_2023 <- df_2023 %>%
  summarize(
    social_media = mean(pesocial == 1, na.rm = TRUE),
    gaming = mean(pegaming == 1, na.rm = TRUE),
    video_streaming = mean(pevideo == 1, na.rm = TRUE),
    audio_streaming = mean(peaudio == 1, na.rm = TRUE)
  ) %>%
  mutate(year = 2023)

# Combine
activities_trends <- bind_rows(activities_2019, activities_2021, activities_2023) %>%
  pivot_longer(cols = -year, names_to = "activity", values_to = "percent")

# Plot
ggplot(activities_trends, aes(x = year, y = percent, color = activity)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Internet Entertainment Activities (2019–2023)",
    x = "Year",
    y = "Percentage",
    color = "Activity"
  ) +
  theme_minimal()



download_ntia_by_year <- function(years, download_dir = "ntia_data") {
  
  # Create the download directory if it doesn't exist
  if (!dir.exists(download_dir)) {
    dir.create(download_dir, recursive = TRUE)
  }
  
  for (year in years) {
    if (year == 2023) {
      url <- "https://www.ntia.gov/sites/default/files/data/2023-survey-data/nov23-cps-csv.zip"
      file_name <- "nov23-cps-csv.zip"
    } else {
      # Figure out the month abbreviation and last two digits of year
      # You may adjust "nov", "jul", "oct", "sep" depending on real month if needed
      month_abbr <- if (year %in% c(2021, 2019, 2017)) {
        "nov"
      } else if (year %in% c(2015, 2013, 2011)) {
        "jul"
      } else if (year %in% c(2009, 2007, 2003)) {
        "oct"
      } else if (year == 2001) {
        "sep"
      } else {
        stop(paste("Unsupported year:", year))
      }
      
      year_suffix <- substr(as.character(year), 3, 4)
      file_name <- paste0(month_abbr, year_suffix, "-cps-csv.zip")
      url <- paste0("https://www.ntia.gov/sites/default/files/data_central_downloads/datasets/", file_name)
    }
    
    dest_path <- file.path(download_dir, file_name)
    
    message("Downloading: ", file_name)
    tryCatch({
      download.file(url, destfile = dest_path, mode = "wb")
    }, error = function(e) {
      warning("Failed to download: ", file_name, " - ", conditionMessage(e))
    })
  }
  
  message("✅ All downloads attempted. Check '", download_dir, "' for your files.")
}

# Set your download directory
download_dir <- "ntia_data"

# List all ZIP files in the folder
zip_files <- list.files(download_dir, pattern = "\\.zip$", full.names = TRUE)

# Unzip each ZIP file into a folder with the same name (without .zip)
for (zip_file in zip_files) {
  unzip_dir <- file.path(download_dir, tools::file_path_sans_ext(basename(zip_file)))
  if (!dir.exists(unzip_dir)) {
    dir.create(unzip_dir)
  }
  unzip(zip_file, exdir = unzip_dir)
  message("Unzipped: ", zip_file, " into ", unzip_dir)
}


# List all extracted CSV files
csv_files <- list.files(download_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Example: Read all CSV files into a list
library(readr)  # (or you can use base R read.csv)

data_list <- lapply(csv_files, read_csv)

# Now data_list is a list of dataframes
# You can access them like data_list[[1]], data_list[[2]], etc.

download_ntia_cps <- function(month_year) {
  # Handle special case for 2023
  if (month_year == "nov23") {
    url <- "https://www.ntia.gov/sites/default/files/data_central_downloads/nov23-cps-csv.zip"
  } else {
    base_url <- "https://www.ntia.gov/sites/default/files/data_central_downloads/datasets/"
    url <- paste0(base_url, month_year, "-cps-csv.zip")
  }
  
  # Define the filename for download
  destfile <- paste0(month_year, "-cps-csv.zip")
  
  # Download the file
  download.file(url, destfile, mode = "wb")
  
  # Optionally unzip the file
  unzip(destfile, exdir = paste0(month_year, "_data"))
  
  message("Downloaded and extracted data for ", month_year)
}

download_ntia_cps_zip <- function(month_year) {
  # Build the URL
  if (month_year == "nov23") {
    url <- "https://www.ntia.gov/sites/default/files/data_central_downloads/nov23-cps-csv.zip"
  } else {
    base_url <- "https://www.ntia.gov/sites/default/files/data_central_downloads/datasets/"
    url <- paste0(base_url, month_year, "-cps-csv.zip")
  }
  
  # Set the destination file name
  destfile <- paste0(month_year, "-cps-csv.zip")
  
  # Download only
  download.file(url, destfile, mode = "wb")
  
  message("Downloaded: ", destfile)
}



# Load necessary libraries
if (!requireNamespace("httr", quietly = TRUE)) {
  install.packages("httr")
}
library(httr)

# Set the URL and destination
url <- "https://www.ntia.gov/sites/default/files/publications/ntia_internet_use_survey_csv.zip"
destfile <- "ntia_internet_use_survey_csv.zip"

# Download the file
GET(url, write_disk(destfile, overwrite = TRUE))

# Optional: Unzip the file
unzip(destfile, exdir = "ntia_internet_use_data")

# Set the URL of the ZIP file
url <- "https://www.ntia.gov/page/download-ntia-internet-use-survey-datasets"

# Set the destination file path
destfile <- "ntia_internet_use_survey_csv.zip"

# Download the file
download.file(url, destfile, mode = "wb")

# Unzip the file
unzip(destfile, exdir = "ntia_internet_use_survey")

# List the files extracted
list.files("ntia_internet_use_survey")

