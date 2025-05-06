library(rvest)
library(dplyr)
library(purrr)
library(rlang)
library(ggplot2)
library(tidyr)

#----------------- Webscraping  --------------------


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


#----------------- Unzipping files -----------------------


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


#---------------- reading csv -----------------------------


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


#---------------------- cleaning dataset-----------------------


clean_ntia_data <- function(dataset_list, na_value = -1) {
  cleaned_list <- lapply(dataset_list, function(df) {
    df[is.na(df)] <- na_value
    return(df)
  })
  return(cleaned_list)
}
ntia_datasets <- clean_ntia_data(ntia_datasets)


#----------------- Selecting covariates requied for Analysis ---------------


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

# Read the file as a string
if (file.exists("columns_to_select_list.txt")) {
  list_code <- readLines("columns_to_select_list.txt")
  list_code <- paste(list_code, collapse = "\n")
  eval(parse(text = list_code))
  message("Loaded: columns_to_select_list")
} else {
  columns_to_select_list <- list(
    "jul11-cps" = c("hryear4","peeduca","hufaminc","peage","pesex", "pesci1","pesc2a6","pelapt","petabl", "pegame","petvba","pehome","pewrka","peschl",
                    "peliba","peotha","peprim1","ptprim2","pepr3a2","peprim6","peprim7","peprim8","peprim9",
                    "peprim10","peprim11","peprim12","peprm141","peprm142","peprm143","peprm144","peprm145",
                    "peprm146", "peprm147","hesci3","pehome", "pecafe", "peelhs", "pecomm", "pelibr", "pewrka", "peschl","peperscr"
                    ,"hesci6","hesci5"),
    
    "jul13-cps" = c("hryear4","peeduca","hufaminc","prtage","pesex","hesci15","hesci11","hesci1","henet2","henet3","henet3a","henet41","henet42","henet43","henet44",
                    "henet45","henet46","henet47","henet7a","henet6a","hesci15","hesci121","hesci122","hesci123","hesci124",
                    "pedesk","pelapt","petabl","pecell","pegame","petvba","peprim1","peperscr",
                    "ptprim2","peprm31","peprm32","peprm33","peprm34","peprm141","peprm142","peprm143","peprm144","peprm145",
                    "peprm146","peprm147","peprm151","peprm153","peprm161","peprm162","peprm163","peprm164","pehome", "pecafe", "peelhs",
                    "pecomm", "pelibr", "pewrka", "peschl","hesc2a9","hesc2a8","hesc2a7","hesc2a10","hesc2a11","hesc2a6","hesc2a5","hesc2a4","hesci124"
                    ,"peprm35","peprm36","peprm37","peprm38","peprm39","peprm310","peprm311","peprm312","peprm313","peprm314","peprm315","peprm316","peprm317","peprim12"),
    
    "jul15-cps" = c("hryear4","peeduca","hefaminc","prtage","pesex","helaptop", "hedesktp", "hetablet", "hemphone", "hewearab", "hetvbox",
                    "heinhome","heinschl","heincafe","heintrav","heinlico","heinelho", "heinwork", "peinhome", "peinwork","peemail", "petextim", "petelewk", "heevrout", "peincafe", "peinschl", "peinothr",
                    "pegames", "pevideo","pecybuly", "hecbully","peprivacy", "hepspre1","henohm1","henohm2","henohm3","henohm4","henohm5","henohm6","henohm7","henohm8","henohm9","henohm10","henohm11",
                    "henoou1","henoou2","henoou3","henoou4","henoou5","henoou6","henoou7","henoou8","henoou9","henoou10","henoou11"),
    
    "nov17-cps" = c("hryear4","peeduca","hefaminc","prtage","pesex","hedesktp", "helaptop", "hetablet", "hemphone", "hewearab", "hetvbox",
                    "heinhome","heinwork", "heinschl", "heincafe", "heintrav", "heinlico", "heinelho", "heinothr",
                    "peemail", "petextim", "pesocial", "peconfer", "pevideo", "peaudio", "pepublish",
                    "petelewk", "pejobsch", "peedtrai", "peusesvc",
                    "hepspre1", "hepspre2", "hepspre3", "hepspre4", "hepspre5","hecbully", "hepscon1", "hepscon2", "hepscon3",
                    "hehomsu", "hepsensi","henohm1","henohm2","henohm3","henohm4","henohm5","henohm6","henohm7","henohm8","henohm9","henohm10", 
                    "heprinoh", "prnohs", "heevrout","henoou1","henoou2","henoou3","henoou4","henoou5","henoou6","henoou7","henoou8","henoou9","henoou10", "heprinoo", "noous"),
    
    "nov19-cps" = c("hryear4","peeduca","hefaminc","prtage","pesex","hedesktp", "helaptop", "hetablet", "hemphone", "hewearab", "hetvbox",
                    "heinhome", "heinwork", "heinschl", "heincafe", "heinlico", "heintrav", "heinelho", "heinothr",
                    "peemail", "petextim", "pesocial", "peconfer", "pevideo", "peaudio", "pepublish", "petelewk", "pejobsch",
                    "hecbully", "hepspre1", "henoou7", "henoou8"),
    
    "nov21-cps" = c("hryear4","peeduca","hefaminc","prtage","pesex","hedesktp","helaptop","hetablet","hemphone","hewearab","hetvbox",
                    "heinhome", "heinwork", "heinschl", "heincafe", "heinlico", "heintrav", "heinelho", "heinothr",
                    "peemail","petextim","pesocial","pegaming","peconfer","pevideo",
                    "peaudio","pepublish","hehomte1","hehomte2","hehomte3","hehomte4","henetql","henetst",
                    "hepscon1","hepscon2","hepscon3","hepscon4","hepscon5","hepscon6","hepscon7","hepscon8",
                    "hepscyba","hedevqua","hedevsta","henetchk" ,"hemobdat"),
    
    "nov23-cps" = c("hryear4","peeduca","hefaminc","prtage","pesex","helaptop", "hedesktp", "hetablet", "hemphone", "hewearab", "hetvbox",
                    "peemail", "petextim", "pesocial", "pegaming", "peconfer", "pevideo", "peaudio", "pepublish", "petelewk",
                    "pejobsch","peedtrai","pegovts","peusesvc","peesrvcs","peecomme","peegoods","pevoicea","pehomiot","hemedrec","hemeddoc",
                    "hepspre1","hepspre2","hepspre3","hepspre4","hepspre5",
                    "hehomte1","hehomte2","hehomte3","hehomte4", "heinhome", "heinwork", "heinschl", "heincafe", "heinlico",
                    "heintrav", "heinelho", "heinothr", "henetql", "henetst","henetchk",
                    "henohm1","henohm2","henohm3","henohm4","henohm5","henohm6","henohm7","henohm8","henohm9","henohm10",
                    "hepscon1","hepscon2","hepscon3","hepscon4","hepscon5","hepscon6","hepscon7","hepscon8",
                    "hepscyba","heinhome","heinwork", "heinschl", "heincafe", "heintrav", "heinlico", "heinelho",
                    "heinothr","henetchk","helaptop", "hedesktp", "hetablet", "hemphone", "hewearab", "hetvbox",
                    "peemail", "petextim", "pesocial", "pegaming", "peconfer", "pevideo", "peaudio", "petelewk",
                    "hehnetql","hehomte3","hehomte2","hehomte1","hehmint",
                    "hemobdat","henetchk"),
    
    "oct03-cps" = c("hryear4","peeduca","hufaminc","prtage","pesex", "hesc1","hesint1","hesint2a","hesevr", "hesint5a","pesch","peschw","pesch2","pesch2na","prnet2","prnet3",
                    "prnet1","pesch7","pesnetd", "pesnetb","pesneti", "pesch5","pesch6","sch5","sch6","hescon2",
                    "hesint6","hesint6f","hescon1","hescon2"),
    
    "oct07-cps" = c("hryear4","peeduca","hufaminc","peage","pesex","henet1", "penet2", "henet3", "henet4"),
    
    "oct09-cps" = c("hryear4","peeduca","hufaminc","peage","pesex","henet1","penet2","henet3","henet4","henet5"),
    
    "sep01-cps" = c("hryear4","peeduca","hufaminc","prtage","pesex","hesc1","hesc2","hesc3","hesc4","hesint1","hesint2a","hesint41","hesint42","hesint43","hesint44",
                    "prnet1", "prnet2","pesex", "prnet3","hescon1",
                    "pesnetsw", "pesnetsx", "pesnetsy", "pesnetsz",
                    "sneta", "snetb", "snetc", "snetd", "snete", "snetf", "snetg", "sneth", "sneti", "snetj", "snetk", "snetl", "snetm", "snetn", "sneto", "snetp", "snetq",
                    "pesch", "peschw", "sch5", "sch6", "sch7",
                    "hescon2", "hesint5a")
  )
}
# Select specific covariates
ntia_selected <- select_covars(ntia_datasets, columns_to_select_list)
ntia_selected <- ntia_selected[order(as.numeric(gsub("\\D", "", names(ntia_selected))))]


#----------------- getting population size for every year ------------


count_ntia_rows <- function(dataset_list) {
  row_counts <- sapply(dataset_list, nrow)
  print(row_counts)
  return(row_counts)
}
row_counts <- count_ntia_rows(ntia_selected)


#------------------ Merging all the data -----------------------------
standardize_columns <- function(df, all_columns, filler = -1) {
  missing_cols <- setdiff(all_columns, names(df))
  for (col in missing_cols) {
    df[[col]] <- filler
  }
  # Reorder to match all_columns
  df <- df[, all_columns]
  return(df)
}
all_columns <- unique(unlist(lapply(ntia_selected, names)))
standardized_list <- lapply(ntia_selected, standardize_columns, all_columns = all_columns)
merged_ntia <- do.call(rbind, standardized_list)


#----------------- Merging columns that are the same -----------------------
merge_columns <- function(col1, col2, na_value = -1) {
  merged <- ifelse(col1 != na_value, col1,
                   ifelse(col2 != na_value, col2, na_value))
  return(merged)
}
merged_ntia$peage <- merge_columns(merged_ntia$peage, merged_ntia$prtage)
merged_ntia$hufaminc <- merge_columns(merged_ntia$hufaminc, merged_ntia$hefaminc)

#---------------- Categorizing for the analysis by Income -------------------------

collapse_income_brackets <- function(df, income_var , new_var) {
  df[[new_var]] <- with(df, ifelse(df[[income_var]] %in% 1:6, 1,
                                   ifelse(df[[income_var]] %in% 7:11, 2,
                                          ifelse(df[[income_var]] %in% 12:13, 3,
                                                 ifelse(df[[income_var]] %in% 14:16, 4, -1)))))
  
  return(df)
}
merged_ntia <- collapse_income_brackets(merged_ntia,"hufaminc","hefaminc")

#---------------- Categorizing for the analysis by Age -------------------------
categorize_age_base <- function(age) {
  ifelse(age >= 18 & age <= 20, 4,
         ifelse(age >= 21 & age <= 25, 5,
                ifelse(age >= 26 & age <= 34, 6,
                       ifelse(age >= 35,             7,
                              -1))))
}
merged_ntia$prtage <- categorize_age_base(merged_ntia$peage)

#---------------- Categorizing for the analysis by Educational Qualifications -------------------------

categorize_education <- function(df, educ_var, age_var, new_var = "educ_group") {
  df[[new_var]] <- with(df, ifelse(df[[age_var]] <= 6, 5,
                                   ifelse(df[[educ_var]] <= 38 & df[[age_var]] >= 7, 1,
                                          ifelse(df[[educ_var]] == 39 & df[[age_var]] >= 7, 2,
                                                 ifelse(df[[educ_var]] %in% 40:42 & df[[age_var]] >= 7, 3,
                                                        ifelse(df[[educ_var]] >= 43 & df[[age_var]] >= 7, 4, NA))))))
  return(df)
}


valid_vars_by_year_net <- list(
  "2001" = c("hesint1"),
  "2003" = c("hesint1"),
  "2007" = c("henet1"),
  "2009" = c("henet1"),
  "2011" = c("hesci6", "hesci5"),
  "2013" = c("henet3","henet2","henet3a","pedesk","pelapt","petabl",
             "pecell","pegame","petvba","pehome", "pecafe", "peelhs", "pecomm", 
             "pelibr", "pewrka", "peschl","peperscr","hesci1","hesci11"),
  "2015" = c("heinhome","heinschl","heincafe","heintrav","heinlico","heinelho", "heinwork",
             "helaptop", "hedesktp", "hetablet", "hemphone", "hewearab", "hetvbox"),
  "2017" = c("heinhome","heinwork", "heinschl", "heincafe", "heintrav", "heinlico",
             "heinelho", "heinothr","helaptop", "hedesktp", "hetablet", "hemphone", "hewearab", "hetvbox"),
  "2019" = c("heinhome","heinwork", "heinschl", "heincafe", "heintrav", "heinlico", "heinelho",
             "heinothr","helaptop", "hedesktp", "hetablet", "hemphone", "hewearab", "hetvbox",
             "peemail", "petextim", "pesocial", "peconfer", "pevideo", "peaudio", "petelewk"),
  "2021" = c("hedesktp","helaptop","hetablet","hemphone","hewearab","hetvbox",
             "heinhome", "heinwork", "heinschl", "heincafe", "heinlico", "heintrav", "heinelho", "heinothr"),
  "2023" = c("heinhome","heinwork", "heinschl", "heincafe", "heintrav", "heinlico", "heinelho",
             "heinothr","henetchk","helaptop", "hedesktp", "hetablet", "hemphone", "hewearab", "hetvbox")
)


valid_vars_by_year_device <- list(
  "2001" = c("hesc1"),
  "2003" = c("hesc1"),
  "2011" = c("hesci3"),
  "2013" = c("henet3","henet2","peprim1","pedesk","pelapt","petabl","pecell","pegame","petvba",
             "pehome", "pecafe", "peelhs", "pecomm","pelibr", "pewrka", "peschl","hesci1"),
  "2015" = c("helaptop", "hedesktp", "hetablet", "hemphone", "hewearab", "hetvbox"),
  "2017" = c("helaptop", "hedesktp", "hetablet", "hemphone", "hewearab", "hetvbox"),
  "2019" = c("helaptop", "hedesktp", "hetablet", "hemphone", "hewearab", "hetvbox"),
  "2021" = c("hedesktp","helaptop","hetablet","hemphone","hewearab","hetvbox"),
  "2023" = c("helaptop","hedesktp","hetablet","hemphone","hewearab","hetvbox")
)

#--------------- Function for getting technology use by education qualifications

summarize_by_educ_group <- function(valid_vars_by_year) {
  result <- bind_rows(
    lapply(names(valid_vars_by_year), function(y) {
      vars <- valid_vars_by_year[[y]]
      vars <- vars[vars %in% colnames(merged_ntia)]  # Keep only existing vars
      
      if (length(vars) == 0) return(NULL)
      
      merged_ntia %>%
        filter(hryear4 == as.integer(y), educ_group %in% c(1, 2, 3, 4, 5)) %>%
        mutate(
          has_one = rowSums(select(., all_of(vars)) == 1 & select(., all_of(vars)) != -1, na.rm = TRUE) > 0
        ) %>%
        group_by(hryear4, educ_group) %>%
        summarise(count = sum(has_one), .groups = "drop")
    })
  )
  
  return(result)
}


#--------------- Function for getting technology use by particular age categories

summarize_by_age_group <- function(valid_vars_by_year) {
  result <- bind_rows(
    lapply(names(valid_vars_by_year), function(y) {
      vars <- valid_vars_by_year[[y]]
      vars <- vars[vars %in% colnames(merged_ntia)]  # Keep only existing vars
      
      if (length(vars) == 0) return(NULL)
      
      merged_ntia %>%
        filter(hryear4 == as.integer(y), prtage %in% c(4,5,6,7)) %>%
        mutate(
          has_one = rowSums(select(., all_of(vars)) == 1 & select(., all_of(vars)) != -1, na.rm = TRUE) > 0
        ) %>%
        group_by(hryear4, prtage) %>%
        summarise(count = sum(has_one), .groups = "drop")
    })
  )
  
  return(result)
}


#--------------- Function for getting technology use by particular gender categories

summarize_by_gender_group <- function(valid_vars_by_year) {
  result <- bind_rows(
    lapply(names(valid_vars_by_year), function(y) {
      vars <- valid_vars_by_year[[y]]
      vars <- vars[vars %in% colnames(merged_ntia)]  # Keep only existing vars
      
      if (length(vars) == 0) return(NULL)
      
      merged_ntia %>%
        filter(hryear4 == as.integer(y), pesex %in% c(1,2)) %>%
        mutate(
          has_one = rowSums(select(., all_of(vars)) == 1 & select(., all_of(vars)) != -1, na.rm = TRUE) > 0
        ) %>%
        group_by(hryear4, pesex) %>%
        summarise(count = sum(has_one), .groups = "drop")
    })
  )
  
  return(result)
}


#--------------- Function for getting technology use by particular income categories

summarize_by_income_group <- function(valid_vars_by_year) {
  result <- bind_rows(
    lapply(names(valid_vars_by_year), function(y) {
      vars <- valid_vars_by_year[[y]]
      vars <- vars[vars %in% colnames(merged_ntia)]  # Keep only existing vars
      
      if (length(vars) == 0) return(NULL)
      
      merged_ntia %>%
        filter(hryear4 == as.integer(y), hefaminc %in% c(1,2,3,4)) %>%
        mutate(
          has_one = rowSums(select(., all_of(vars)) == 1 & select(., all_of(vars)) != -1, na.rm = TRUE) > 0
        ) %>%
        group_by(hryear4, hefaminc) %>%
        summarise(count = sum(has_one), .groups = "drop")
    })
  )
  
  return(result)
}


#--------------- Function for getting technology use by total population

summarize_tech_use_total <- function(varlist_by_year) {
  result <- bind_rows(
    lapply(names(varlist_by_year), function(y) {
      vars <- varlist_by_year[[y]]
      vars <- vars[vars %in% colnames(merged_ntia)]  # Only keep existing variables
      
      if (length(vars) == 0) return(NULL)  # Skip if no valid columns
      
      merged_ntia %>%
        filter(hryear4 == as.integer(y)) %>%
        mutate(
          has_one = rowSums(select(., all_of(vars)) == 1 & select(., all_of(vars)) != -1, na.rm = TRUE) > 0
        ) %>%
        summarise(hryear4 = first(hryear4), count = sum(has_one))
    })
  )
  
  return(result)
}


#======================== Calculating Househlds with Internet ===========================

result_net_tot <- summarize_tech_use_total(valid_vars_by_year_net)

ggplot(result_net_tot, aes(x = hryear4, y = count)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Count of Internet Usage (Any 1) by Year",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal()


#=========================== Calculating Internet access according to Income categories======================

result_income <- summarize_by_income_group(valid_vars_by_year_net)

ggplot(result_income, aes(x = hryear4, y = count, color = factor(hefaminc), group = hefaminc)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Count of Rows with At Least One '1' in Relevant Variables",
    x = "Year",
    y = "Count",
    color = "income"
  ) +
  theme_minimal()


#=========================== Calculating Internet access according to Gender categories======================

result_gender <- summarize_by_gender_group(valid_vars_by_year_net)

ggplot(result_gender, aes(x = hryear4, y = count, color = factor(pesex), group = pesex)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Count of Rows with At Least One '1' in Relevant Variables",
    x = "Year",
    y = "Count",
    color = "Gender"
  ) +
  theme_minimal()


#=========================== Calculating Internet access according to Age categories======================

result_age <- summarize_by_age_group(valid_vars_by_year_net)

ggplot(result_age, aes(x = hryear4, y = count, color = factor(prtage), group = prtage)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Count of Rows with At Least One '1' in Relevant Variables",
    x = "Year",
    y = "Count",
    color = "Age"
  ) +
  theme_minimal()



#=========================== Calculating Internet access according to Education categories======================

result_education_net <- summarize_by_educ_group(valid_vars_by_year_net)

ggplot(result_education_net, aes(x = hryear4, y = count, color = factor(educ_group), group = educ_group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Count of Rows with At Least One '1' in Relevant Variables",
    x = "Year",
    y = "Count",
    color = "Education"
  ) +
  theme_minimal()


#======================= Calculating Device access to every household ===========================


result_device_tot <- summarize_tech_use_total(valid_vars_by_year_device)

ggplot(result_device_tot, aes(x = hryear4, y = count)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Count of Internet Usage (Any 1) by Year",
    x = "Year",
    y = "Count"
  ) +
  theme_minimal()


#=========================== Calculating technology access according to Income categories======================

result_income <- summarize_by_income_group(valid_vars_by_year_device)

ggplot(result_income, aes(x = hryear4, y = count, color = factor(hefaminc), group = hefaminc)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Count of Rows with At Least One '1' in Relevant Variables",
    x = "Year",
    y = "Count",
    color = "income"
  ) +
  theme_minimal()


#=========================== Calculating technology access according to Gender categories======================

result_gender <- summarize_by_gender_group(valid_vars_by_year_device)

ggplot(result_gender, aes(x = hryear4, y = count, color = factor(pesex), group = pesex)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Count of Rows with At Least One '1' in Relevant Variables",
    x = "Year",
    y = "Count",
    color = "Gender"
  ) +
  theme_minimal()


#=========================== Calculating technology access according to Age categories======================

result_age <- summarize_by_age_group(valid_vars_by_year_device)

ggplot(result_age, aes(x = hryear4, y = count, color = factor(prtage), group = prtage)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Count of Rows with At Least One '1' in Relevant Variables",
    x = "Year",
    y = "Count",
    color = "Age"
  ) +
  theme_minimal()


#=========================== Calculating technology access according to Education Qualifications categories======================


result_education <- summarize_by_educ_group("educ_group",valid_vars_by_year_device)

ggplot(result_education, aes(x = hryear4, y = count, color = factor(educ_group), group = educ_group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Count of Rows with At Least One '1' by Education Group",
    x = "Year",
    y = "Count",
    color = "Education"
  ) +
  theme_minimal()
