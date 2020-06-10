library(tidyverse)


#fix race names function
fix_race_names_function <- function(df) {
  df$RACE[df$RACE == 'AMER IND'] <- 'Native American'
  df$RACE[df$RACE == 'ASIAN'] <- 'Asian'
  df$RACE[df$RACE == 'BLACK'] <- 'Black'
  df$RACE[df$RACE == 'WHITE'] <- 'White'
  df$RACE[df$RACE == 'HISPANIC'] <- 'Hispanic'
  return(df)
}


#clean names, set variable types, fix dates functions:
  #pretrial
  pretrial_names_variable_types_dates_cleaning_function <- function(df){
    df <- df %>% 
      clean_names() %>% 
      mutate(identifier = as.character(identifier),
             race = as.factor(race),
             gender = as.factor(gender),
             age = as.numeric(age),
             offense = as.character(offense),
             facility = as.factor(facility),
             detainer = as.factor(detainer),
             download_date = as.Date(download_date, '%m/%d/%Y'),
             latest_admission_date = as.Date(latest_admission_date, '%m/%d/%Y')) %>% 
      dplyr::rename(pretrial_age = age,
                    pretrial_facility = facility,
                    pretrial_detainer =  detainer,
                    pretrial_download_date = download_date, 
                    pretrial_latest_admission_date = latest_admission_date)
  
  }
  
  #sentenced
  sentenced_names_variable_types_dates_cleaning_function <- function(df) {
    df <- df %>% 
      clean_names() %>% 
      mutate(identifier = as.character(identifier),
             race = as.factor(race),
             gender = as.factor(gender),
             age = as.numeric(age),
             offense = as.character(offense),
             facility = as.factor(facility),
             detainer = as.factor(detainer),
             download_date = as.Date(download_date, '%m/%d/%Y'),
             end_sentence_date = as.Date(end_sentence_date, '%m/%d/%Y'),
             latest_admission_date = as.Date(latest_admission_date, '%m/%d/%Y'),
             special_parole_end_date = as.Date(special_parole_end_date, '%m/%d/%Y')) %>% 
      dplyr::rename(sentenced_age = age, 
                    sentenced_facility = facility, 
                    sentenced_detainer = detainer,
                    sentenced_download_date = download_date,
                    sentenced_latest_admission_date = latest_admission_date)
  }


#remove inconsistently coded people function
remove_inconsistently_coded_people_function <- function(df){
  #create dfs of unique people
  df_people <- unique(df[,c("identifier", "race", "gender")])
  
  #get difference between number of unique people, and number of unique people by race and gender as well to see if some people are being coded multiple times as different races/genders
  print("Race/gender inconsistently coded people removed:")
  print(nrow(df_people) - length(unique(df[,"identifier"])))
  
  
  #exclude the people coded with different race and/or genders from both pretrial and sentenced dfs
  exclude_inconsistent_people_vec <- df_people$identifier[duplicated(df_people$identifier)]
  df <- df %>% 
    filter(!identifier %in% exclude_inconsistent_people_vec)
}
