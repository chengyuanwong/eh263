
# Load PM data ------------------------------------------------------------

# 3001

kingston <-
  read_csv(
    "~/Documents/Rstudio/eh_263/Group Project/pm_sidepak/Test 9_kingston_3001_raw.csv"
  )

city_place <-
  read_csv(
    "~/Documents/Rstudio/eh_263/Group Project/pm_sidepak/Test 7_cityplace_3001_raw.csv"
  )

prudential <-
  read_csv(
    "~/Documents/Rstudio/eh_263/Group Project/pm_sidepak/Test 8_prudential_3001_raw.csv"
  )

trilogy <-
  read_csv(
    "~/Documents/Rstudio/eh_263/Group Project/pm_sidepak/12_6_trilogy_401_3001_raw.csv"
  )

trilogy <- trilogy %>%
  mutate(
    Time = as.POSIXct(Time, format = "%H:%M:%S", tz = "UTC"), 
    Time = format(Time, "%H:%M:%S"),
    Time = as_hms(Time)
  )


df <- bind_rows(kingston, city_place, prudential, trilogy) %>%
  mutate(
    # Combine Date and Time into Timestamp
    Timestamp = as.POSIXct(
      paste(Date, Time),
      format = "%m/%d/%y %H:%M:%S",  
      tz = "America/New_York"       
    )
  ) %>%
  select(-Date, -Time)



garage_info <- read_csv("~/Documents/Rstudio/eh_263/Group Project/pm_sidepak/pm_log.csv")

garage_info <- garage_info %>%
  mutate(
    Start_Time = as.POSIXct(paste(Date, Start_Time), format = "%m/%d/%y %H:%M:%S", tz = "America/New_York"),
    End_Time = as.POSIXct(paste(Date, End_Time), format = "%m/%d/%y %H:%M:%S", tz = "America/New_York"),
    Level = as.factor(Level)  
  ) %>%
  select(-Date) 


df <- df %>%
  fuzzy_inner_join(
    garage_info,
    by = c("Timestamp" = "Start_Time", "Timestamp" = "End_Time"),
    match_fun = list(`>=`, `<=`)
  ) %>%
  select(Timestamp, everything(), Garage_name, Level) %>% 
  mutate(MC = MC * 1000)




# Check outliers
df <- df %>%
  mutate(Z_Score = (MC - mean(MC, na.rm = TRUE)) / sd(MC, na.rm = TRUE))

# Separate outliers (Z-Score > 3 or < -3)
outliers <- df %>%
  filter(abs(Z_Score) > 3)

# View removed observations
print(outliers)

# Remove outlier
df <- df %>%
filter(abs(Z_Score) <= 3) %>%  # Keep data within 3 standard deviations
  select(-Z_Score)



# 3003 --------------------------------------------------------------------

kingston_3003 <-
  read_csv(
    "~/Documents/Rstudio/eh_263/Group Project/pm_sidepak/Test 5_kingston_3003_raw.csv"
  )

city_place_3003 <-
  read_csv(
    "~/Documents/Rstudio/eh_263/Group Project/pm_sidepak/Test 2_cityplace_3003_raw.csv"
  )

prudential_3003 <-
  read_csv(
    "~/Documents/Rstudio/eh_263/Group Project/pm_sidepak/Test 3_prudential_3003_raw.csv"
  )

trilogy_3003 <-
  read_csv(
    "~/Documents/Rstudio/eh_263/Group Project/pm_sidepak/12_6_trilogy_401_3003_raw.csv"
  )

trilogy_3003 <- trilogy_3003 %>%
  mutate(
    Time = as.POSIXct(Time, format = "%H:%M:%S", tz = "UTC"), 
    Time = format(Time, "%H:%M:%S"),
    Time = as_hms(Time)
  )


df_3003 <- bind_rows(kingston_3003, city_place_3003, prudential_3003, trilogy_3003) %>%
  mutate(
    Timestamp = as.POSIXct(
      paste(Date, Time),
      format = "%m/%d/%y %H:%M:%S",  
      tz = "America/New_York"        
    )
  ) %>%
  select(-Date, -Time)


df_3003 <- df_3003 %>%
  fuzzy_inner_join(
    garage_info,
    by = c("Timestamp" = "Start_Time", "Timestamp" = "End_Time"),
    match_fun = list(`>=`, `<=`)
  ) %>%
  select(Timestamp, everything(), Garage_name, Level) %>% 
  mutate(MC = MC * 1000)

# Define a function to check and remove outliers
remove_outliers <- function(data, column) {
  data <- data %>%
    mutate(Z_Score = ({{ column }} - mean({{ column }}, na.rm = TRUE)) / sd({{ column }}, na.rm = TRUE)) 
  outliers <- data %>% filter(abs(Z_Score) > 4)  # Identify outliers
  print(outliers)  # Print outliers
  data <- data %>% filter(abs(Z_Score) <= 4) %>% select(-Z_Score)  # Remove outliers
  return(data)
}

# Apply the function to both datasets
df <- remove_outliers(df, MC)
df_3003 <- remove_outliers(df_3003, MC)


# Average two devices
merged_df <- bind_rows(
  df %>% mutate(Source = "3001"),  # Add a column to identify the source of the data
  df_3003 %>% mutate(Source = "3003")  # Add a column to identify the source of the data
) %>%
  group_by(Timestamp, Garage_name, Level) %>%  # Group by Timestamp and Garage_name
  summarise(
    MC = mean(MC, na.rm = TRUE),  # Calculate mean, ignoring NA values
    .groups = "drop"  # Drop grouping after summarizing
  )


# Aggregate to 1-minute intervals
merged_df <- merged_df %>%
  mutate(Aggregated_Timestamp = floor_date(Timestamp, "minute")) %>%
  group_by(Garage_name, Aggregated_Timestamp, Level) %>%
  summarise(MC = mean(MC, na.rm = TRUE), .groups = "drop") %>%
  rename(Timestamp = Aggregated_Timestamp)


# Remove duplicates
merged_df <- merged_df %>%
  distinct(Garage_name, Timestamp, .keep_all = TRUE)