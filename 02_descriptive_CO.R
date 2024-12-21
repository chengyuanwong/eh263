# Correlation between the two device  -------------------------------------
correlation_devices <- inner_join(co_ezb %>% rename(co_ezb = CO_ppm), 
                                  co_ezf %>% rename(co_ezf = CO_ppm),
                             by = c("Timestamp", "Garage_name", "Level"))


bland_devices <- correlation_devices %>%
  mutate(
    Mean = (co_ezb + co_ezf) / 2,  
    Difference = co_ezb - co_ezf  
  )

ggplot(bland_devices, aes(x = Mean, y = Difference)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "black") +  
  labs(
    x = "Mean of ezb and ezf",
    y = "Difference (ezb - ezf)"
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



# comparison between two device time series --------------------------------


# Reshape data to long format 
correlation_devices_long <- correlation_devices %>%
  pivot_longer(cols = c(co_ezb, co_ezf), 
               names_to = "Device", 
               values_to = "CO_ppm")

# Time-series plot by garage name
ggplot(correlation_devices_long, aes(x = Timestamp, y = CO_ppm, color = Device)) +
  geom_line(size = 0.6, alpha = 1) +  
  facet_wrap(~ Garage_name, labeller = as_labeller(custom_labels), scales = "free_x") +  
  labs(
    x = "Timestamp",
    y = "CO (ppm)",
    color = NULL
  ) +
  scale_color_manual(values = c("co_ezb" = "#9E7CD7", "co_ezf" = "#6ACF65"),
                     labels = c("EZB", "EZF")) +
  guides(
    fill = guide_legend(override.aes = list(alpha = 0.5)),  # Combine fill into one legend
    color = guide_legend(override.aes = list(size = 1))
  ) +
  theme_classic() +
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
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Calculate descriptive statistics for CO_ppm
descriptive_stats <- merged_co %>%
  group_by(Level) %>%
  summarize(
    `Mean` = round(mean(CO_ppm, na.rm = TRUE), 3),
    `Median` = round(median(CO_ppm, na.rm = TRUE), 3),
    `SD` = round(sd(CO_ppm, na.rm = TRUE), 3),
    `Min` = round(min(CO_ppm, na.rm = TRUE), 3),
    `Max` = round(max(CO_ppm, na.rm = TRUE), 3),
    `Count` = n(),
    .groups = "drop"
  )

# Render datatable
gt_table <- descriptive_stats %>%
  gt() %>%
  tab_header(
    title = "Descriptive Statistics of CO (ppm) by Level"
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

ggplot(merged_co, aes(x = factor(Level), y = CO_ppm, fill = factor(Level))) + 
  geom_violin(trim = FALSE) +  # Violin plot for each level
  geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.7) +  
  geom_jitter(shape = 21, color = "black", width = 0.2, size = 0.4, alpha = 0.3) +  
  facet_wrap(~ Garage_name, labeller = as_labeller(custom_labels)) + 
  labs(
    x = "Floor Level",
    y = "CO (ppm)",
    fill = "Level"
  ) + 
  scale_y_continuous(limits = c(0, 9)) + 
  guides(color = "none", shape = "none", fill = "none") +
  theme_classic() +  # Clean and minimal theme
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




# Fit a simple linear regression model
lm_co <- lm(CO_ppm ~ Level, data = merged_co)
summary(lm_co)
AIC(lm_co)

# LME model with random intercept only 
lme_co <- lme(
  CO_ppm ~ Level,
  random = ~ 1 | Garage_name,
  data = merged_co
)
summary(lme_co)
plot(ACF(lme_co))


# LME model with random slope and random intercept
lme_co_random_slopes <- lme(
  CO_ppm ~ Level,
  random = ~ Level | Garage_name,  # Random slopes for Level
  data = merged_co,
  control = lmeControl(opt = "optim", maxIter = 100, msMaxIter = 100)
)

summary(lme_co_random_slopes)
intervals(lme_co_random_slopes, level = 0.95, which = "fixed")


acf(residuals(lme_co_random_slopes, type = "normalized"), main = "Autocorrelation of Residuals")

r.squaredGLMM(lme_co_random_slopes)


# Obtain residuals 
residuals_lme <- resid(lme_co_random_slopes)

lm_resid <- lm(residuals_lme ~ 1)  # Model residuals with no predictors

dwtest(lm_resid)



# forest plot  --------------------------------------------------------------

fixed_effects <- data.frame(
  Variable = c("(Intercept)", "Level1", "Level2", "Level3"),
  Lower = c(0.7601060, 0.5219291, 0.9492330, 1.3276186),
  Estimate = c(1.468287, 1.229226, 1.810906, 2.639012),
  Upper = c(2.176469, 1.936522, 2.672580, 3.950405)
)

# Create the forest plot
ggplot(fixed_effects, aes(x = Variable, y = Estimate)) +
  geom_point(size = 3, color = "#9E7CD7") +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, color = "#6ACF65") +  
  coord_flip() +  
  scale_y_continuous(limits = c(-3, 8), breaks = seq(-3, 8, 2)) +
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

