trilogy_ezb <- read_xlsx("~/Documents/Rstudio/eh_263/Group Project/co_hobo/CO_Flow_Temp_Hobo_12-06-24_TRILOGY_401PARK_EZB.xlsx", 
                    skip = 1)

city_place_ezb <- read_csv("~/Documents/Rstudio/eh_263/Group Project/co_hobo/CO_Flow_Temp_Hobo_11-14-24_CityPlace_BostonCommon_EZB.csv", 
                       skip = 1)

prudential_ezb <- read_xlsx("~/Documents/Rstudio/eh_263/Group Project/co_hobo/CO_Flow_Temp_Hobo_11-19-24_COPLEY_PRUDENTIAL_EZB.xlsx", 
                        skip = 1)

kingston_ezb <- read_xlsx("~/Documents/Rstudio/eh_263/Group Project/co_hobo/CO_Flow_Temp_Hobo_11-21-24_Kingston_EZB.xlsx", 
                      skip = 1)


# Function to clean and format the dataset
clean_co_dataset <- function(df, timestamp_format) {
  df_cleaned <- df[2:nrow(df), c(2, 3, 4, 6)]  # Assuming the first column is 'Date' and the second is 'Time'
  colnames(df_cleaned) <- c("Time", "Co_low", "Co_aux", "Co_high")
  
  # Convert columns to numeric (handling missing or non-numeric data)
  df_cleaned$Co_low <- as.numeric(df_cleaned$Co_low)
  df_cleaned$Co_aux <- as.numeric(df_cleaned$Co_aux)
  df_cleaned$Co_high <- as.numeric(df_cleaned$Co_high)
  
  # Calculate the CO_ppm values using the conditional formula
  df_cleaned$CO_ppm <- ifelse(df_cleaned$Co_low > 2.49,
                              (df_cleaned$Co_high - df_cleaned$Co_aux) * 2.42,
                              (df_cleaned$Co_low - df_cleaned$Co_aux) * 2.42)
  
  # Create the Timestamp column
  df_cleaned <- df_cleaned %>%
    mutate(
      Timestamp = as.POSIXct(paste(Time), format = timestamp_format, tz = "America/New_York")
    )
  df_cleaned <- df_cleaned %>% select(-Time)
  return(df_cleaned)
}

# Clean each dataset and create the Timestamp column
trilogy_ezb_cleaned <- clean_co_dataset(trilogy_ezb, "%Y-%m-%d %H:%M:%S")      
city_place_ezb_cleaned <- clean_co_dataset(city_place_ezb, "%m/%d/%y %I:%M:%S %p")
prudential_ezb_cleaned <- clean_co_dataset(prudential_ezb, "%Y-%m-%d %H:%M:%S")
kingston_ezb_cleaned <- clean_co_dataset(kingston_ezb, "%Y-%m-%d %H:%M:%S")


# Combine all the datasets
co_ezb <- bind_rows(trilogy_ezb_cleaned, city_place_ezb_cleaned, prudential_ezb_cleaned, kingston_ezb_cleaned)

co_ezb <- co_ezb %>%
  fuzzy_inner_join(
    garage_info,
    by = c("Timestamp" = "Start_Time", "Timestamp" = "End_Time"),
    match_fun = list(`>=`, `<=`)
  ) %>%
  select(Timestamp, everything(), Garage_name, Level) %>% 
  filter(CO_ppm > 0 )


# Check outliers
co_ezb <- co_ezb %>%
  mutate(Z_Score = (CO_ppm - mean(CO_ppm, na.rm = TRUE)) / sd(CO_ppm, na.rm = TRUE))

# Separate outliers (Z-Score > 3 or < -3)
outliers_ezb <- co_ezb %>%
  filter(abs(Z_Score) > 3)

# View removed observations
print(outliers_ezb)

# Remove outlier
co_ezb <- co_ezb %>%
  filter(abs(Z_Score) <= 3) %>%  # Keep data within 3 standard deviations
  select(-Z_Score)


# Check for duplicates within each group
duplicates_ezb <- co_ezb %>%
  group_by(Garage_name, Timestamp) %>%
  filter(n() > 1)

# Print duplicates
print(duplicates_ezb)

# Remove duplicates (keep the first occurrence)
co_ezb <- co_ezb %>%
  distinct(Garage_name, Timestamp, .keep_all = TRUE)


# Remove duplicates
co_ezb <- co_ezb %>%
  distinct(Garage_name, Timestamp, .keep_all = TRUE)






# ezf ---------------------------------------------------------------------

trilogy_ezf <- read_xlsx("~/Documents/Rstudio/eh_263/Group Project/co_hobo/CO_Flow_Temp_Hobo_12-06-24_TRILOGY_401PARK_EZF.xlsx", 
                        skip = 1)

city_place_ezf <- read_csv("~/Documents/Rstudio/eh_263/Group Project/co_hobo/CO_Flow_Temp_HOBO_11-14-24_CityPlace_BostonCommon_EZF.csv", 
                          skip = 1)

prudential_ezf <- read_xlsx("~/Documents/Rstudio/eh_263/Group Project/co_hobo/CO_Flow_Temp_HOBO_11-19-24_COPLEY_PRUDENTIAL_EZF.xlsx", 
                           skip = 1)

kingston_ezf <- read_xlsx("~/Documents/Rstudio/eh_263/Group Project/co_hobo/CO_Flow_Temp_HOBO_11-21-24_Kingston_EZF.xlsx", 
                         skip = 1)



# Clean each dataset
trilogy_ezf_cleaned <- clean_co_dataset(trilogy_ezf, "%Y-%m-%d %H:%M:%S")      
city_place_ezf_cleaned <- clean_co_dataset(city_place_ezf, "%m/%d/%y %I:%M:%S %p")
prudential_ezf_cleaned <- clean_co_dataset(prudential_ezf, "%Y-%m-%d %H:%M:%S")
kingston_ezf_cleaned <- clean_co_dataset(kingston_ezf, "%Y-%m-%d %H:%M:%S")


# Combine all the datasets
co_ezf <- bind_rows(trilogy_ezf_cleaned, city_place_ezf_cleaned, prudential_ezf_cleaned, kingston_ezf_cleaned)

co_ezf <- co_ezf %>%
  fuzzy_inner_join(
    garage_info,
    by = c("Timestamp" = "Start_Time", "Timestamp" = "End_Time"),
    match_fun = list(`>=`, `<=`)
  ) %>%
  select(Timestamp, everything(), Garage_name, Level) %>% 
  filter(CO_ppm > 0 )



# Define a function to check and remove outliers
remove_outliers <- function(data, column) {
  data <- data %>%
    mutate(Z_Score = ({{ column }} - mean({{ column }}, na.rm = TRUE)) / sd({{ column }}, na.rm = TRUE)) 
  outliers <- data %>% filter(abs(Z_Score) > 4)  
  print(outliers) 
  data <- data %>% filter(abs(Z_Score) <= 4) %>% select(-Z_Score)  
  return(data)
}

# Apply the function to both datasets
co_ezb <- remove_outliers(co_ezb, CO_ppm)
co_ezf <- remove_outliers(co_ezf, CO_ppm)

# Average two devices
merged_co <- bind_rows(
  co_ezb %>% mutate(Source = "ezb"), 
  co_ezf %>% mutate(Source = "ezf")  
) %>%
  group_by(Timestamp, Garage_name, Level) %>%  
  summarise(
    CO_ppm = mean(CO_ppm, na.rm = TRUE), 
    .groups = "drop"  
  )

# Aggregate to 1-minute intervals
merged_co <- merged_co %>%
  mutate(Aggregated_Timestamp = floor_date(Timestamp, "minute")) %>%
  group_by(Garage_name, Aggregated_Timestamp, Level) %>%
  summarise(CO_ppm = mean(CO_ppm, na.rm = TRUE), .groups = "drop") %>%
  rename(Timestamp = Aggregated_Timestamp)

# Remove duplicates
merged_co <- merged_co %>%
  distinct(Garage_name, Timestamp, .keep_all = TRUE)




