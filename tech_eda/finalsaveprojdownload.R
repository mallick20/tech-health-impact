library(rvest)
library(dplyr)
library(purrr)
library(rlang)
library(ggplot2)
library(tidyr)
library(scales)

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
  
  return(all_data)  # Saving a named list
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
    "jul11-cps" = c("hryear4","peeduca","hufaminc","peage","pesex","hesci6", "hesci5","hesci3"),
    
    "jul13-cps" = c("hryear4","peeduca","hesint1","henet3","henet2","henet3a","pedesk","pelapt","petabl",
                    "pecell","pegame","petvba","pehome", "pecafe", "peelhs", "pecomm", 
                    "pelibr", "pewrka", "peschl","peperscr","hesci1","hesci11","henet3","henet2","peprim1","pedesk","pelapt","petabl","pecell","pegame","petvba",
                    "pehome", "pecafe", "peelhs", "pecomm","pelibr", "pewrka", "peschl","hesci1"),
    
    "jul15-cps" = c("hryear4","peeduca","hefaminc","prtage","pesex","heinhome","heinschl","heincafe","heintrav","heinlico","heinelho", "heinwork",
                    "helaptop", "hedesktp", "hetablet", "hemphone", "hewearab", "hetvbox"),
    
    "nov17-cps" = c("hryear4","peeduca","hefaminc","prtage","pesex","heinhome","heinwork", "heinschl", "heincafe", "heintrav", "heinlico",
                    "heinelho", "heinothr","helaptop", "hedesktp", "hetablet", "hemphone", "hewearab", "hetvbox"),
    
    "nov19-cps" = c("hryear4","peeduca","hefaminc","prtage","pesex","heinhome","heinwork", "heinschl", "heincafe", "heintrav", "heinlico", "heinelho",
                    "heinothr","helaptop", "hedesktp", "hetablet", "hemphone", "hewearab", "hetvbox",
                    "peemail", "petextim", "pesocial", "peconfer", "pevideo", "peaudio", "petelewk"),
    
    "nov21-cps" = c("hryear4","peeduca","hefaminc","prtage","pesex","hedesktp","helaptop","hetablet","hemphone","hewearab","hetvbox",
                    "heinhome", "heinwork", "heinschl", "heincafe", "heinlico", "heintrav", "heinelho", "heinothr"),
    
    "nov23-cps" = c("hryear4","peeduca","hefaminc","prtage","pesex","heinhome","heinwork", "heinschl", "heincafe", "heintrav", "heinlico", "heinelho",
                    "heinothr","henetchk","helaptop", "hedesktp", "hetablet", "hemphone", "hewearab", "hetvbox"),
    
    "oct03-cps" = c("hryear4","peeduca","hufaminc","prtage","pesex","hesint1","hesc1"),
    
    "oct07-cps" = c("hryear4","peeduca","hufaminc","peage","pesex","henet1"),
    
    "oct09-cps" = c("hryear4","peeduca","hufaminc","peage","pesex","henet1"),
    
    "sep01-cps" = c("hryear4","peeduca","hufaminc","prtage","pesex","hesint1","hesc1")
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

merged_ntia <- categorize_education(merged_ntia, "peeduca","peage")

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
        summarise(
          tech_users = sum(has_one),
          total_people = n(),
          .groups = "drop"
        )
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
        filter(hryear4 == as.integer(y), prtage %in% c(4, 5,6,7)) %>%
        mutate(
          has_one = rowSums(select(., all_of(vars)) == 1 & select(., all_of(vars)) != -1, na.rm = TRUE) > 0
        ) %>%
        group_by(hryear4, prtage) %>%
        summarise(
          tech_users = sum(has_one),
          total_people = n(),
          .groups = "drop"
        )
    })
  )
  
  return(result)
}

  

#--------------- Function for getting technology use by particular gender categories

summarize_by_gender_group <-  function(valid_vars_by_year) {
  result <- bind_rows(
    lapply(names(valid_vars_by_year), function(y) {
      vars <- valid_vars_by_year[[y]]
      vars <- vars[vars %in% colnames(merged_ntia)]  # Keep only existing vars
      
      if (length(vars) == 0) return(NULL)
      
      merged_ntia %>%
        filter(hryear4 == as.integer(y), pesex %in% c(1, 2)) %>%
        mutate(
          has_one = rowSums(select(., all_of(vars)) == 1 & select(., all_of(vars)) != -1, na.rm = TRUE) > 0
        ) %>%
        group_by(hryear4, pesex) %>%
        summarise(
          tech_users = sum(has_one),
          total_people = n(),
          .groups = "drop"
        )
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
        filter(hryear4 == as.integer(y), hefaminc %in% c(1, 2, 3, 4)) %>%
        mutate(
          has_one = rowSums(select(., all_of(vars)) == 1 & select(., all_of(vars)) != -1, na.rm = TRUE) > 0
        ) %>%
        group_by(hryear4, hefaminc) %>%
        summarise(
          tech_users = sum(has_one),
          total_people = n(),
          .groups = "drop"
        )
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
        summarise(hryear4 = first(hryear4), tech_users = sum(has_one))
    })
  )
  
  return(result)
}


#======================== Calculating Households with Internet ===========================

result_users_net <- summarize_tech_use_total(valid_vars_by_year_net)

result_users_net_plots <- ggplot(result_users_net, aes(x = hryear4, y = tech_users)) +
  geom_line()+
  geom_point(size = 2) +
  labs(
    title = "Internet Usage Over The Years",
    x = "Year",
    y = "Number of Users"
  ) +
  theme_light()


#=========================== Calculating Internet access according to Income categories======================

result_income_net <- summarize_by_income_group(valid_vars_by_year_net)

result_income_net_plots <- ggplot(result_income_net, aes(x = hryear4, y = tech_users, color = factor(hefaminc), group = hefaminc)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Internet Usage By Income Bracket Over The Years",
    x = "Year",
    y = "Number of Users",
    color = "Income"
  ) +
  theme_light()


#=========================== Calculating Internet access according to Gender categories======================

result_gender_net <- summarize_by_gender_group(valid_vars_by_year_net)

result_gender_net_plots <- ggplot(result_gender_net, aes(x = hryear4, y = tech_users, color = factor(pesex), group = pesex)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Internet Usage By Gender Bracket Over The Years",
    x = "Year",
    y = "Number of Users",
    color = "Gender"
  ) +
  theme_light()


#=========================== Calculating Internet access according to Age categories======================

result_age_net <- summarize_by_age_group(valid_vars_by_year_net)

result_age_net_plots <- ggplot(result_age_net, aes(x = hryear4, y = tech_users, color = factor(prtage), group = prtage)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Internet Usage By Age Bracket Over The Years",
    x = "Year",
    y = "Number of Users",
    color = "Age"
  ) +
  theme_light()



#=========================== Calculating Internet access according to Education categories======================

result_education_net <- summarize_by_educ_group(valid_vars_by_year_net)

result_education_net_plots <- ggplot(result_education_net, aes(x = hryear4, y = tech_users, color = factor(educ_group), group = educ_group)) +
  geom_point(size = 2) +
  labs(
    title = "Internet Usage By Education Qualification Bracket Over The Years",
    x = "Year",
    y = "Number of Users",
    color = "Education"
  ) +
  theme_light()



#======================= Calculating Device access to every household ===========================


result_users_dev <- summarize_tech_use_total(valid_vars_by_year_device)

result_users_dev_plots <- ggplot(result_users_dev, aes(x = hryear4, y = tech_users)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = seq(min(result_users_dev$hryear4), max(result_users_dev$hryear4), by = 2)
  ) +
  scale_y_continuous(
    labels = label_number()  # No scientific notation, no commas
  ) +
  labs(
    title = "Technology Usage Over The Years",
    x = "Year",
    y = "User Count"
  ) +
  theme_light(base_size = 14)



#=========================== Calculating technology access according to Income categories======================

result_income_dev <- summarize_by_income_group(valid_vars_by_year_device)

result_income_dev_plots <- ggplot(result_income_dev, aes(x = hryear4, y = tech_users, color = factor(hefaminc), group = hefaminc)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Technology Usage By Income Bracket Over The Years",
    x = "Year",
    y = "Number of Users",
    color = "Income"
  ) +
  theme_light()


#=========================== Calculating technology access according to Gender categories======================

result_gender_dev <- summarize_by_gender_group(valid_vars_by_year_device)

result_gender_dev_plots <- ggplot(result_gender_dev, aes(x = hryear4, y = tech_users, color = factor(pesex), group = pesex)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Technology Usage By Gender Bracket Over The Years",
    x = "Year",
    y = "Number of Users",
    color = "Gender"
  ) +
  theme_light()


#=========================== Calculating technology access according to Age categories======================

result_age_dev <- summarize_by_age_group(valid_vars_by_year_device)

result_age_dev_plots <- ggplot(result_age_dev, aes(x = hryear4, y = tech_users, color = factor(prtage), group = prtage)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Technology Usage By Age Bracket Over The Years",
    x = "Year",
    y = "Number of Users",
    color = "Age"
  ) +
  theme_light()


#=========================== Calculating technology access according to Education Qualifications categories======================


result_education_dev <- summarize_by_educ_group(valid_vars_by_year_device)

result_education_dev_plots <- ggplot(result_education_dev, aes(x = hryear4, y = tech_users, color = factor(educ_group), group = educ_group)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Technology Usage By  Education Qualification Bracket Over The Years",
    x = "Year",
    y = "Number of Users",
    color = "Education"
  ) +
  theme_light()



library(ggplot2)
library(plotly)
library(dplyr)

# Calculate percentage
plot_income <- result_income_net %>%
  mutate(percent = 100 * tech_users / total_people)

# Interactive plot
p_income <- ggplot(plot_income, aes(x = hryear4, y = percent, color = factor(hefaminc))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Tech Use by Income Group",
       x = "Year", y = "Percent", color = "Income Group") +
  theme_minimal()

ggplotly(p_income)

# Gender
p_gender <- ggplot(result_gender_net %>%
                     mutate(percent = 100 * tech_users / total_people),
                   aes(x = hryear4, y = percent, color = factor(pesex))) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  labs(title = "Tech Use by Gender", x = "Year", y = "Percent", color = "Gender") +
  theme_minimal()

# Age
p_age <- ggplot(result_age_net %>%
                  mutate(percent = 100 * tech_users / total_people),
                aes(x = hryear4, y = percent, color = factor(prtage))) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  labs(title = "Tech Use by Age Group", x = "Year", y = "Percent", color = "Age Group") +
  theme_minimal()

# Education
p_education <- ggplot(result_education_net %>%
                        mutate(percent = 100 * tech_users / total_people),
                      aes(x = hryear4, y = percent, color = factor(educ_group))) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  labs(title = "Tech Use by Education", x = "Year", y = "Percent", color = "Education Group") +
  theme_minimal()

ggplotly(p_income)
ggplotly(p_gender)
ggplotly(p_age)
ggplotly(p_education)


#========================merging to interact with teammates data=====================

income_net <- result_income_net %>%
  mutate(
    category = "Income",
    subgroup = hefaminc,
    percent = 100 * tech_users / total_people,
    tech_inc_u = tech_users,
    tot_tech_inc_u = total_people
  ) %>%
  select(hryear4, category, subgroup,percent, tech_inc_u,tot_tech_inc_u)

gender_net <- result_gender_net %>%
  mutate(
    category = "Gender",
    subgroup = pesex,
    percent = 100 * tech_users / total_people,
    tech_gen_u = tech_users,
    tot_tech_gen_u = total_people
  ) %>%
  select(hryear4, category, subgroup,percent, tech_gen_u,tot_tech_gen_u)

age_net <- result_age_net %>%
  mutate(
    category = "Age",
    subgroup = prtage,
    percent = 100 * tech_users / total_people,
    tech_age_u = tech_users,
    tot_tech_age_u = total_people
  ) %>%
  select(hryear4, category, subgroup,percent, tech_age_u, tot_tech_age_u)

education_net <- result_education_net %>%
  mutate(
    category = "Education",
    subgroup = educ_group,
    percent = 100 * tech_users / total_people,
    tech_edu_u = tech_users, 
    tot_tech_edu_u = total_people
  ) %>%
  select(hryear4, category, subgroup,percent, tech_edu_u, tot_tech_edu_u)

# Combine them safely
combined_results_net <- bind_rows(income_net, gender_net, age_net, education_net)

wide_combined_results_net <- combined_results_net %>%
  pivot_wider(
    names_from = category,
    values_from = percent
  )
income_dev <- result_income_dev %>%
  mutate(
    category = "Income",
    subgroup = hefaminc,
    dev_inc_u = tech_users,
    tot_dev_inc_u = total_people,
    percent = 100 * tech_users / total_people
  ) %>%
  select(hryear4, category, subgroup, percent,dev_inc_u, tot_dev_inc_u)

gender_dev <- result_gender_dev %>%
  mutate(
    category = "Gender",
    subgroup = pesex,
    dev_gen_u = tech_users,
    tot_dev_gen_u = total_people,
    percent = 100 * tech_users / total_people
  ) %>%
  select(hryear4, category, subgroup, percent,dev_gen_u,tot_dev_gen_u)

age_dev <- result_age_dev %>%
  mutate(
    category = "Age",
    subgroup = prtage,
    dev_age_u = tech_users,
    tot_dev_age_u = total_people,
    percent = 100 * tech_users / total_people
  ) %>%
  select(hryear4, category, subgroup, percent,dev_age_u, tot_dev_age_u)

education_dev <- result_education_dev %>%
  mutate(
    category = "Education",
    subgroup = educ_group,
    dev_edu_u = tech_users,
    tot_dev_edu_u = total_people,
    percent = 100 * tech_users / total_people
  ) %>%
  select(hryear4, category, subgroup, percent,,dev_edu_u, tot_dev_edu_u)

# Combine them safely
combined_results_dev <- bind_rows(income_dev, gender_dev, age_dev, education_dev) 

wide_combined_results_dev <- combined_results_dev %>%
  pivot_wider(
    names_from = category,
    values_from = percent
  )


#========================================Saving plots===========================================

plot_list <- list(result_age_net_plots, result_users_net_plots, result_education_net_plots, result_gender_net_plots,
                  result_income_net_plots, result_age_dev_plots, result_education_dev_plots, result_gender_dev_plots,
                  result_income_dev_plots,result_users_dev_plots )
plot_names <- c(
  "age_net", "users_net", "education_net", "gender_net", "income_net",
  "age_dev", "education_dev", "gender_dev", "income_dev", "users_dev"
)

dir.create("plots", showWarnings = FALSE)

for (i in seq_along(plot_list)) {
  ggsave(
    filename = paste0("plots/", plot_names[i], ".png"),
    plot = plot_list[[i]],
    width = 8, height = 6, dpi = 300
  )
}


#====================================Saving Result data====================================
result_names <- c(
  "result_users_net","result_education_net", "result_age_net","result_gender_net","result_income_net",
  "result_users_dev", "result_age_dev", "result_education_dev","result_gender_dev","result_income_dev",
  "wide_combined_results_dev","wide_combined_results_net")

# Create a folder if it doesn't exist
dir.create("results", showWarnings = FALSE)

# Loop and save each one to a CSV
for (name in result_names) {
  df <- get(name)
  write.csv(df, file = paste0("results/", name, ".csv"), row.names = FALSE)
}


