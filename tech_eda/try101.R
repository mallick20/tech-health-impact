library(rvest)

download_ntia_datasets <- function(url = "https://www.ntia.gov/page/download-ntia-internet-use-survey-datasets",
                                   file_types = c("csv.zip"),
                                   download_dir = ".") {
  page <- read_html(url)
  links <- html_attr(html_nodes(page, "a"), "href")
  
  # Filter for dataset file types
  dataset_links <- links[grepl(paste(file_types, collapse = "|"), links, ignore.case = TRUE)]
  
  # Make full URLs and download
  for (link in dataset_links) {
    full_link <- ifelse(grepl("^http", link), link, paste0("https://www.ntia.gov", link))
    file_name <- basename(full_link)
    dest_path <- file.path(download_dir, file_name)
    
    message("Downloading: ", file_name)
    tryCatch({
      download.file(full_link, destfile = dest_path, mode = "wb")
    }, error = function(e) {
      warning("Failed to download: ", file_name)
    })
  }
}
download_ntia_datasets()  # Downloads all dataset files to your current directory

# OR, specify a custom folder
download_ntia_datasets(download_dir = "data/ntia")

# OR, limit to ZIP files only
download_ntia_datasets(file_types = ".zip")



library(rvest)

download_ntia_datasets <- function(url = "https://www.ntia.gov/page/download-ntia-internet-use-survey-datasets",
                                   file_types = c("csv.zip"),
                                   download_dir = ".") {
  page <- read_html(url)
  links <- html_attr(html_nodes(page, "a"), "href")
  
  # Filter for dataset file types
  dataset_links <- links[grepl(paste(file_types, collapse = "|"), links, ignore.case = TRUE)]
  
  # Further filter: Only keep datasets from odd years
  odd_year_links <- dataset_links[sapply(dataset_links, function(link) {
    years <- regmatches(link, gregexpr("\\d{4}", link))[[1]]  # Extract all 4-digit numbers
    any(as.integer(years) %% 2 == 1)  # Keep if any year in link is odd
  })]
  
  # Make full URLs and download
  for (link in odd_year_links) {
    full_link <- ifelse(grepl("^http", link), link, paste0("https://www.ntia.gov", link))
    file_name <- basename(full_link)
    dest_path <- file.path(download_dir, file_name)
    
    message("Downloading (odd year): ", file_name)
    tryCatch({
      download.file(full_link, destfile = dest_path, mode = "wb")
    }, error = function(e) {
      warning("Failed to download: ", file_name)
    })
  }
}

download_ntia_datasets()  # Only downloads odd-year datasets




library(rvest)

download_ntia_datasets <- function(url = "https://www.ntia.gov/page/download-ntia-internet-use-survey-datasets",
                                   file_types = c("csv.zip"),
                                   download_dir = ".") {
  page <- read_html(url)
  links <- html_attr(html_nodes(page, "a"), "href")
  
  # Filter for dataset file types
  dataset_links <- links[grepl(paste(file_types, collapse = "|"), links, ignore.case = TRUE)]
  
  # Further filter: Only keep datasets from odd years
  odd_year_links <- dataset_links[sapply(dataset_links, function(link) {
    # Extract all 2-digit numbers
    two_digit_years <- regmatches(link, gregexpr("\\d{2}", link))[[1]]
    
    # Assume years are 2000 + two_digits (e.g., '01' becomes 2001)
    full_years <- as.integer(two_digit_years)
    full_years <- ifelse(full_years <= 30, 2000 + full_years, 1900 + full_years)  # Smart guess
    
    any(full_years %% 2 == 1)  # Keep if any interpreted year is odd
  })]
  
  # Make full URLs and download
  for (link in odd_year_links) {
    full_link <- ifelse(grepl("^http", link), link, paste0("https://www.ntia.gov", link))
    file_name <- basename(full_link)
    dest_path <- file.path(download_dir, file_name)
    
    message("Downloading (odd year): ", file_name)
    tryCatch({
      download.file(full_link, destfile = dest_path, mode = "wb")
    }, error = function(e) {
      warning("Failed to download: ", file_name)
    })
  }
}

download_ntia_datasets()  # Now properly handles "01", "03", etc.



library(rvest)

download_ntia_datasets <- function(url = "https://www.ntia.gov/page/download-ntia-internet-use-survey-datasets",
                                   file_types = c("csv.zip"),
                                   download_dir = ".") {
  page <- read_html(url)
  links <- html_attr(html_nodes(page, "a"), "href")
  
  # Filter for dataset file types
  dataset_links <- links[grepl(paste(file_types, collapse = "|"), links, ignore.case = TRUE)]
  
  # Further filter: Only keep datasets from odd years between 01 and 23
  odd_year_links <- dataset_links[sapply(dataset_links, function(link) {
    # Extract all 2-digit numbers
    two_digit_years <- regmatches(link, gregexpr("\\d{2}", link))[[1]]
    two_digit_years <- as.integer(two_digit_years)
    
    # Only keep numbers between 1 and 23
    two_digit_years <- two_digit_years[two_digit_years >= 1 & two_digit_years <= 23]
    
    # Now check if any are odd
    any(two_digit_years %% 2 == 1)
  })]
  
  # Make full URLs and download
  for (link in odd_year_links) {
    full_link <- ifelse(grepl("^http", link), link, paste0("https://www.ntia.gov", link))
    file_name <- basename(full_link)
    dest_path <- file.path(download_dir, file_name)
    
    message("Downloading (odd year 01–23): ", file_name)
    tryCatch({
      download.file(full_link, destfile = dest_path, mode = "wb")
    }, error = function(e) {
      warning("Failed to download: ", file_name)
    })
  }
}

download_ntia_datasets()  # Downloads files for odd years between 01 and 23



library(rvest)

download_ntia_datasets <- function(url = "https://www.ntia.gov/page/download-ntia-internet-use-survey-datasets",
                                   file_types = c("csv.zip"),
                                   download_dir = ".") {
  page <- read_html(url)
  links <- html_attr(html_nodes(page, "a"), "href")
  
  # Filter for dataset file types
  dataset_links <- links[grepl(paste(file_types, collapse = "|"), links, ignore.case = TRUE)]
  
  # Define allowed two-digit odd years between 01 and 23
  allowed_years <- sprintf("%02d", seq(1, 23, by = 2))  # "01", "03", ..., "23"
  
  # Further filter: Only keep datasets from allowed years
  filtered_links <- dataset_links[sapply(dataset_links, function(link) {
    two_digit_years <- regmatches(link, gregexpr("\\d{2}", link))[[1]]
    any(two_digit_years %in% allowed_years)
  })]
  
  # Make full URLs and download
  for (link in filtered_links) {
    full_link <- ifelse(grepl("^http", link), link, paste0("https://www.ntia.gov", link))
    file_name <- basename(full_link)
    dest_path <- file.path(download_dir, file_name)
    
    message("Downloading (years 01-23, odd only): ", file_name)
    tryCatch({
      download.file(full_link, destfile = dest_path, mode = "wb")
    }, error = function(e) {
      warning("Failed to download: ", file_name)
    })
  }
}

download_ntia_datasets()  # Now downloads only odd years from 01 to 23




load_ntia_csvs <- function(zip_dir = ".", unzip_dir = tempfile()) {
  # List all zip files
  zip_files <- list.files(zip_dir, pattern = "\\.zip$", full.names = TRUE)
  
  if (length(zip_files) == 0) {
    stop("No zip files found in the specified directory.")
  }
  
  # Create a place to unzip (if not using tempfile)
  if (!dir.exists(unzip_dir)) {
    dir.create(unzip_dir)
  }
  
  all_data <- list()
  
  for (zip_path in zip_files) {
    message("Unzipping: ", basename(zip_path))
    
    # Unzip into the target directory
    unzip(zip_path, exdir = unzip_dir)
  }
  
  # Now list all CSVs inside the unzip directory
  csv_files <- list.files(unzip_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    stop("No CSV files found after unzipping.")
  }
  
  for (csv_path in csv_files) {
    message("Reading: ", basename(csv_path))
    
    # Read each CSV and store in a named list
    data_name <- tools::file_path_sans_ext(basename(csv_path))
    all_data[[data_name]] <- read.csv(csv_path)
  }
  
  return(all_data)
}

# Example usage:
# First, download using your earlier function
# download_ntia_datasets()

# Then, load all datasets:
ntia_datasets <- load_ntia_csvs(zip_dir = ".", unzip_dir = "unzipped_ntia_files")
                           




unzip_ntia_files <- function(zip_dir = ".", unzip_dir = tempfile()) {
  zip_files <- list.files(zip_dir, pattern = "\\.zip$", full.names = TRUE)
  
  if (length(zip_files) == 0) {
    stop("No zip files found in the specified directory.")
  }
  
  if (!dir.exists(unzip_dir)) {
    dir.create(unzip_dir)
  }
  
  for (zip_path in zip_files) {
    message("Unzipping: ", basename(zip_path))
    unzip(zip_path, exdir = unzip_dir)
  }
  
  return(unzip_dir)  # Return the path where files were unzipped
}
unzipped_folder <- unzip_ntia_files(zip_dir = ".", unzip_dir = "unzipped_ntia_files")



read_ntia_csvs <- function(csv_dir) {
  csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    stop("No CSV files found in the specified directory.")
  }
  
  all_data <- list()
  
  for (csv_path in csv_files) {
    message("Reading: ", basename(csv_path))
    data_name <- tools::file_path_sans_ext(basename(csv_path))
    all_data[[data_name]] <- read.csv(csv_path)
  }
  
  return(all_data)  # Return a named list of data frames
}
ntia_datasets <- read_ntia_csvs(csv_dir = unzipped_folder)



library(dplyr)  # Needed for select()

read_ntia_csvs <- function(csv_dir, columns_to_select = NULL) {
  csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    stop("No CSV files found in the specified directory.")
  }
  
  all_data <- list()
  
  for (csv_path in csv_files) {
    message("Reading: ", basename(csv_path))
    data_name <- tools::file_path_sans_ext(basename(csv_path))
    
    # Read full CSV first
    df <- read.csv(csv_path)
    
    # If columns_to_select is provided, select only those columns
    if (!is.null(columns_to_select)) {
      # Only keep columns that exist in the dataset
      available_cols <- intersect(columns_to_select, names(df))
      if (length(available_cols) == 0) {
        warning("No matching columns found in ", data_name)
      } else {
        df <- dplyr::select(df, all_of(available_cols))
      }
    }
    
    all_data[[data_name]] <- df
  }
  
  return(all_data)  # Return a named list of dataframes
}

# Example: Only load ID, age, gender, internet usage
columns_needed <- c("ID", "AGE", "GENDER", "INTERNET_USE")

ntia_datasets <- read_ntia_csvs(csv_dir = "unzipped_ntia_files", columns_to_select = columns_needed)






library(dplyr)

read_ntia_csvs <- function(csv_dir, columns_to_select_list = NULL) {
  csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(csv_files) == 0) {
    stop("No CSV files found in the specified directory.")
  }
  
  all_data <- list()
  
  for (csv_path in csv_files) {
    file_base_name <- tools::file_path_sans_ext(basename(csv_path))
    message("Reading: ", basename(csv_path))
    
    # Read full CSV first
    df <- read.csv(csv_path)
    
    # Check if specific columns are given for this file
    if (!is.null(columns_to_select_list) && file_base_name %in% names(columns_to_select_list)) {
      cols_wanted <- columns_to_select_list[[file_base_name]]
      available_cols <- intersect(cols_wanted, names(df))
      
      if (length(available_cols) == 0) {
        warning("No matching columns found for ", file_base_name)
      } else {
        df <- dplyr::select(df, all_of(available_cols))
      }
    }
    
    all_data[[file_base_name]] <- df
  }
  
  return(all_data)
}
# Suppose your unzipped files are like:
# - internet_use_2001.csv
# - internet_use_2003.csv

columns_to_select_list <- list(
  "internet_use_2001" = c("ID", "AGE", "GENDER"),
  "internet_use_2003" = c("ID", "INCOME", "EDUCATION", "INTERNET_USE")
)

ntia_datasets <- read_ntia_csvs(
  csv_dir = "unzipped_ntia_files",
  columns_to_select_list = columns_to_select_list
)




head(ntia_datasets$`jul11-cps`)
for (dataset_name in names(ntia_datasets)) {
  cat(dataset_name, ": ", nrow(ntia_datasets[[dataset_name]]), " rows\n")
}




library(dplyr)

select_covars <- function(datasets, columns_to_select_list) {
  selected_data <- list()
  
  for (dataset_name in names(datasets)) {
    df <- datasets[[dataset_name]]
    
    if (dataset_name %in% names(columns_to_select_list)) {
      cols_wanted <- columns_to_select_list[[dataset_name]]
      available_cols <- intersect(cols_wanted, names(df))
      
      if (length(available_cols) == 0) {
        warning("No matching columns found for ", dataset_name)
        selected_data[[dataset_name]] <- df  # return full df if no matching columns
      } else {
        selected_data[[dataset_name]] <- dplyr::select(df, all_of(available_cols))
      }
    } else {
      # If no selection provided for this dataset, keep full dataset
      selected_data[[dataset_name]] <- df
    }
  }
  
  return(selected_data)
}

columns_to_select_list <- list(
  "jul11-cps" = c("hryear4","pesci1","pesc2a6","pelapt","petabl", "pegame","petvba","pehome","pewrka","peschl",
                  "peliba","peotha","peprim1","ptprim2","pepr3a2","peprim6","peprim7","peprim8","peprim9",
                  "peprim10","peprim11","peprim12","peprm141","peprm142","peprm143","peprm144","peprm145",
                  "peprm146", "peprm147"),
  "jul13-cps" = c("hryear4","hesci1","henet2","henet3","pedesk","pelapt","petabl","pecell","pegame","petvba","peprim1",
                  "ptprim2","peprm31","peprm32","peprm33","peprm34","peprm141","peprm142","peprm143","peprm144","peprm145",
                  "peprm146"),
  "jul15-cps" = c("hryear4","hedesktp","helaptop", "hetablet", "hemphome",
                  "heinhome", "peinhome", "peinwork","peemail", "petextim", "petelewk", "heevrout", "peincafe", "peinschl", "peinothr",
                  "pegames", "pevideo","pecybuly", "hecbully","peprivacy", "hepspre1","henohm1","henohm2","henohm3","henohm4","henohm5","henohm6","henohm7","henohm8","henohm9","henohm10","henohm11",
                  "henoou1","henoou2","henoou3","henoou4","henoou5","henoou6","henoou7","henoou8","henoou9","henoou10","henoou11"),
  "nov17-cps" = c("hryear4","hedesktp", "helaptop", "hetablet", "hemphone", "hewearab", "hetvbox",
                  "heinhome","heinwork", "heinschl", "heincafe", "heintrav", "heinlico", "heinelho", "heinothr",
                  "peemail", "petextim", "pesocial", "peconfer", "pevideo", "peaudio", "pepublish",
                  "petelewk", "pejobsch", "peedtrai", "peusesvc",
                  "hepspre1", "hepspre2", "hepspre3", "hepspre4", "hepspre5","hecbully", "hepscon1", "hepscon2", "hepscon3",
                  "hehomsu", "hepsensi","henohm1","henohm2","henohm3","henohm4","henohm5","henohm6","henohm7","henohm8","henohm9","henohm10", 
                  "heprinoh", "prnohs", "heevrout","henoou1","henoou2","henoou3","henoou4","henoou5","henoou6","henoou7","henoou8","henoou9","henoou10", "heprinoo", "noous"),
  "nov19-cps" = c("hryear4","hedesktp", "helaptop", "hetablet", "hemphone", "hewearab", "hetvbox",
                  "heinhome", "heinwork", "heinschl", "heincafe", "heinlico", "heintrav", "heinelho", "heinothr",
                  "peemail", "petextim", "pesocial", "peconfer", "pevideo", "peaudio", "pepublish", "petelewk", "pejobsch",
                  "hecbully", "hepspre1", "henoou7", "henoou8"),
  "nov21-cps" = c("hryear4","hedesktp","helaptop","hetablet","hemphone","hewearab","hetvbox","heinhome","heinwork",
                  "heinschl","heincafe","heintrav","peemail","petextim","pesocial","pegaming","peconfer","pevideo",
                  "peaudio","pepublish","hehomte1","hehomte2","hehomte3","hehomte4","henetql","henetst",
                  "hepscon1","hepscon2","hepscon3","hepscon4","hepscon5","hepscon6","hepscon7","hepscon8",
                  "hepscyba","hedevqua","hedevsta" ),
  "nov23-cps" = c("hryear4","helaptop", "hedesktp", "hetablet", "hemphone", "hewearab", "hetvbox",
                  "peemail", "petextim", "pesocial", "pegaming", "peconfer", "pevideo", "peaudio", "pepublish", "petelewk",
                  "hehomte1","hehomte2","hehomte3","hehomte4", "heinothr", "henetql", "henetst",
                  "henohm1","henohm2","henohm3","henohm4","henohm5","henohm6","henohm7","henohm8","henohm9","henohm10",
                  "hepscon1","hepscon2","hepscon3","hepscon4","hepscon5","hepscon6","hepscon7","hepscon8",
                  "hepscyba"),
  "oct03-cps" = c("hryear4","hesint2a","hesevr", "hesint5a","pesch","peschw","pesch2","pesch2na","prnet2","prnet3",
                  "prnet1","pesch7","pesnetd", "pesnetb","pesneti", "pesch5","pesch6","sch5","sch6","hescon2",
                  "hesint6","hesint6f"),
  "oct07-cps" = c("hryear4","henet1", "penet2", "henet3", "henet4"),
  "oct09-cps" = c("hryear4","henet1","penet2","henet3","henet4","henet5"),
  "sep01-cps" = c("hryear4", "hesc1","hesc2","hesc3","hesc4","hesint1","hesint2a","hesint41","hesint42","hesint43","hesint44",
                  "prnet1", "prnet2", "prnet3",
                  "pesnetsw", "pesnetsx", "pesnetsy", "pesnetsz",
                  "sneta", "snetb", "snetc", "snetd", "snete", "snetf", "snetg", "sneth", "sneti", "snetj", "snetk", "snetl", "snetm", "snetn", "sneto", "snetp", "snetq",
                  "pesch", "peschw", "sch5", "sch6", "sch7",
                  "hescon2", "hesint5a")
)

# Select specific covariates
ntia_selected <- select_covars(ntia_datasets, columns_to_select_list)

head(ntia_selected$`jul15-cps`)



,
hesc1 = "comp_user",
hesc2 = "no_of_comps",
hesc3 = "year_newest_comp",
hesc4 = "comp_owner",
hesint1 = "internet_home",
hesint2a = "internet_type",
hesint41 = "device_cellpager",
hesint42 = "device_pda",
hesint43 = "device_tv",
hesint44 = "device_other"
