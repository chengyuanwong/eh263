# Aggregate data to 1-minute intervals

df_3003_adjusted <- df_3003 %>%
  mutate(Timestamp = Timestamp + seconds(37))

# Merge the datasets directly based on the adjusted Timestamp
aligned_data <- inner_join(
  df %>% rename(MC_Device1 = MC),
  df_3003_adjusted %>% rename(MC_Device2 = MC),
  by = c("Timestamp", "Garage_name", "Level")
)

# Calculate overall precision
overall_average_pm <- mean((aligned_data$MC_Device1 + aligned_data$MC_Device2) / 2, na.rm = TRUE)
overall_precision <- mean((abs(aligned_data$MC_Device1 - aligned_data$MC_Device2) / overall_average_pm) * 100, na.rm = TRUE)

# Print the overall precision
overall_precision





correlation_devices <- inner_join(df %>% rename(Device1_MC = MC), 
                             df_3003 %>% rename(Device2_MC = MC),
                             by = c("Timestamp", "Garage_name", "Level"))

bland_devices <- correlation_devices %>%
  mutate(
    Mean = (Device1_MC + Device2_MC) / 2,  # Mean of the two devices
    Difference = Device1_MC - Device2_MC   # Difference between the devices
  )

ggplot(bland_devices, aes(x = Mean, y = Difference)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Reference line
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add trend line
  labs(
    x = "Mean of 3001 and 3003",
    y = "Difference (3001 - 3003)"
  ) +
  theme_classic() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 14),  
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 14, hjust = 0),  
    strip.background = element_blank(), 
    plot.tag = element_text(size = 16, face = "bold")  
  ) 



# Create violin plot stratified by Level within each Garage
custom_labels <- c(
  "401 Park Garage" = "A",
  "88 Kingston St garage" = "B",
  "Back Bay Garage" = "C",
  "Boston Common Garage" = "D",
  "Boylston st garage" = "E",
  "City Place Garage" = "F",
  "Prudential Center Garage" = "G",
  "Trilogy" = "H"
)

# Reshape data to long format for the two devices
correlation_devices_long <- correlation_devices %>%
  pivot_longer(cols = c(Device1_MC, Device2_MC),
               names_to = "Device",
               values_to = "MC")


# Time-series plot with smoothing faceted by garage name
ggplot(correlation_devices_long, aes(x = Timestamp, y = MC, color = Device)) +
  geom_line(size = 0.2, alpha = 1) + 
  scale_y_continuous(limits = c(0, 40)) +
  facet_wrap(~ Garage_name, labeller = as_labeller(custom_labels), scales = "free_x") +
  labs(
    x = "Timestamp",
    y = "PM 2.5 (ppm)",
    color = "Device"
  ) +
  scale_color_manual(
    values = c("Device1_MC" = "#9E7CD7", "Device2_MC" = "#6ACF65"),
    labels = c("3001", "3003"))  +
      scale_fill_manual(
        values = c("Device1_MC" = "#9E7CD7", "Device2_MC" = "#6ACF65"),  # Custom CI fill colors
        labels = c("3001", "3003")
  ) +
  theme_classic() +
  guides(
    fill = guide_legend(override.aes = list(alpha = 0.5)),  # Combine fill into one legend
    color = guide_legend(override.aes = list(size = 1))
  ) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 17),
    axis.title = element_text(size = 17),
    strip.text = element_text(size = 17, hjust = 0),
    strip.background = element_blank(),
    legend.text = element_text(size = 17),
    axis.text.x = element_text(angle = 17, hjust = 1)
  )



ggplot(merged_df, aes(x = factor(Level), y = MC, fill = factor(Level))) + 
  geom_violin(trim = FALSE) +  
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.7) +  
  geom_jitter(shape = 21, color = "black", width = 0.2, size = 0.4, alpha = 0.3) +  
  facet_wrap(~ Garage_name, labeller = as_labeller(custom_labels)) + 
  labs(
    x = "Floor Level",
    y = "PM2.5 (ppm)",
    fill = "Level"
  ) + 
  scale_y_continuous(limits = c(0, 40)) + 
  guides(color = "none", shape = "none", fill = "none") +
  theme_classic() +  
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 25),  
    axis.title = element_text(size = 25),
    strip.text = element_text(size = 25, hjust = 0),  
    strip.background = element_blank(), 
    plot.tag = element_text(size = 25, face = "bold")  
  ) + 
  scale_fill_brewer(palette = "Set3")


# Calculate descriptive statistics stratified by floor 
descriptive_stats <- merged_df %>%
  group_by(Level) %>%
  summarize(
    `Mean` = round(mean(MC, na.rm = TRUE), 3),
    `Median` = round(median(MC, na.rm = TRUE), 3),
    `SD` = round(sd(MC, na.rm = TRUE), 3),
    `Min` = round(min(MC, na.rm = TRUE), 3),
    `Max` = round(max(MC, na.rm = TRUE), 3),
    `Count` = n(),
    .groups = "drop"
  )

# Render datatable
gt_table <- descriptive_stats %>%
  gt() %>%
  tab_header(
    title = "Descriptive Statistics of PM2.5 (ug/m3) by Level"
  )

# lme ---------------------------------------------------------------------

# LME model
lme_model <- lme(
  MC ~ Level,
  random = ~ Level | Garage_name,
  data = merged_df,
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 100)
)


summary(lme_model)
intervals(lme_model, level = 0.95, which = "fixed")


# Obtain residuals 
residuals_lme <- resid(lme_model)

# Plot ACF 
acf(residuals_lme, main = "Autocorrelation of Residuals")

r.squaredGLMM(lme_model)


# forest plot ---------------------------------------------------------------

# Create a data frame for the fixed effects
fixed_effects <- data.frame(
  Variable = c("(Intercept)", "Level1", "Level2", "Level3"),
  Estimate = c(8.533317, 5.048065, 3.805581, 5.850498),
  Lower = c(5.33997348, 0.81754055, 1.05967760, 0.06881678),
  Upper = c(11.726660, 9.278589, 6.551484, 11.632179)
)

# Create the forest plot
ggplot(fixed_effects, aes(x = Variable, y = Estimate)) +
  geom_point(size = 3, color = "#9E7CD7") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "#6ACF65") +  
  coord_flip() +  
  scale_y_continuous(limits = c(-5, 15), breaks = seq(-5, 15, 5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    x = "",  # Remove x-axis label
    y = "Estimate with 95% CI",
  ) +
  theme_classic() +  
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 14),  
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 14, hjust = 0),  
    strip.background = element_blank(), 
    plot.tag = element_text(size = 16, face = "bold")  
  )





# Precision calculation ---------------------------------------------------


