##########################################################################################################
## Author: GREG CHISM
## Date: MAY 2022
## email: gchism@arizona.edu
## Project: Nest shape influences colony organization in ants (movement / activity)
## Title: Performance against a nest invader
##########################################################################################################

#######DESCRIPTION OF THE SCRIPT##########
# Aggression performance analyses & visualizations

##########################################################################################################
# INSTALL & LOAD REQUIRED PACKAGES
##########################################################################################################
install.packages("pacman") # Download package with function to load multiple packaged at once

pacman::p_load(assertthat, # Loading required packages for code below. p_load() will download packages that aren't in system library
               data.table,
               forcats,
               ggpubr,
               lme4,
               lmerTest,
               magrittr,
               MuMIn,
               readr,
               tidyverse)

##########################################################################################################
# LOAD THE REQUIRED DATA SETS
##########################################################################################################

# RAW DATA PROCESSING
Aggression_Data_Full <- read.csv("Aggression_Data_Working.csv") %>% # Raw data set
  mutate(Removed = Inv.Remov - Inv.Insert) # Add a column for invader removal time (invader removal time - invader insertion time, in seconds)

##########################################################################################################
# COLONY PERFORMANCE AGAINST A CONSPECIFIC NEST INVADER
# Data processing, visualization, analyses of the data for invader removal in each nest shape data
##########################################################################################################

# Function to produce larger scatterplot legends
large_points <- function(data, params, size) {
  # Multiply by some number
  data$size <- data$size * 2
  draw_key_point(data = data, params = params, size = size)
}

# Invader removal time mean and standard deviation
Aggression_Data_Full %>% # Full invader performance data
  group_by(Nest) %>% # Group by the desired columns
  mutate(AvgRemove = mean(Removed), # Average number of rows in each group
         StdDev = sd(Removed)) %>% # Standard deviation number of rows in each group
  dplyr::select(c(Nest, AvgRemove, StdDev)) %>% # Select the mean and standard deviation columns only
  distinct() # Remove duplicates

# INVADER REMOVAL TIME IN EACH NEST SHAPE: OVERALL REMOVAL
# Boxplot showing the relationship between nest shape and invader removal time 
AggnPlot1 <- ggplot(data = Aggression_Data_Full, aes(y = Removed, x = Nest, fill = Nest)) +
  stat_boxplot_custom(qs = c(0, 0.25, 0.5, 0.75, 1.00), alpha = 0.65, width = 0.33) +
  xlab("Nest shape") +
  ylab("Time to remove invader (secs)") +
  theme_pubclean() +
  theme(axis.text.x = element_text(size = 18, color = "black"),
        axis.title = element_text(size = 18, color = "black"),
        axis.text.y = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = "none") +
  scale_fill_manual(breaks = c("Circle", "Tube"), 
                    name = "Nest",
                    values = c("blue", "red"))

# INVADER REMOVAL TIME IN EACH NEST SHAPE: INVADER NEST PENETRATION
# Scatterplot and regression line fits showing the relationship between invader scaled distance to nest entrance and removal time
AggnPlot2 <- ggplot(data = Aggression_Data_Full, aes(y = Removed, x = ScaledDist)) +
  geom_point(key_glyph = large_points, size = 3, alpha = 0.5, aes(color = Nest, shape = Nest)) +
  xlab("Invader scaled distance to entrance ") +
  ylab("Time to remove invader (secs)") +
  geom_smooth(method = lm, se = FALSE, aes(color = Nest)) +
  theme_pubclean() +
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"),
        axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, colour = "black"),
        legend.title = element_text(size = 18, colour = "black")) +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red")) + 
  xlim (0, 1)

# INVADER REMOVAL TIME IN EACH NEST SHAPE: MAXIMUM ATTACKING WORKERS
# Scatterplot and regression line fits  showing the relationship between Max attacking workers and removal time
AggnPlot3 <- ggplot(data = Aggression_Data_Full, aes(y = Removed, x = Attacking.Max)) +
  geom_jitter(key_glyph = large_points, size = 3, alpha = 0.5, aes(color = Nest, shape = Nest), width = 0.15) +
  xlab("Maximum number of attacking workers") +
  ylab("Time to remove invader (secs)") +
  geom_smooth(method = lm, se = FALSE, aes(color = Nest)) +
  theme_pubclean() +
  theme(axis.text.x = element_text(size = 18, colour = "black"),
        axis.title = element_text(size = 18, colour = "black"),
        axis.text.y = element_text(size = 18, colour = "black"),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.justification = c(1, 1),
        legend.text = element_text(size = 18, colour = "black"),
        legend.title = element_text(size = 18, colour = "black")) +
  scale_color_manual(breaks = c("Circle", "Tube"), 
                     name = "Nest",
                     values = c("blue", "red")) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8))

# Compile the plots 
AggnPerformPlot <- ggarrange(AggnPlot1, AggnPlot2, AggnPlot3,
          labels = c("(a)", "(b)", "(c)"),
          ncol = 2, nrow = 2,
          common.legend = FALSE,
          font.label = list(size = 20,
                            color = "black", face = "plain"),
          vjust = 1) 

# Save plot as a PDF
ggsave(file = "Fig2.pdf", plot = AggnPerformPlot, width = 10.4, height = 10.4, units = "in")

# LINEAR MIXED EFFECTS MODEL
# RESPONSE VARIABLE 
# Removed - Time (in seconds) taken to remove the conspecific nest invader
# FIXED EFFECTS
# Nest - Nest shape (Tube / Circle)
# RANDOM EFFECT
# (1|Colony) - Colony identification 
summary(lmer(Removed ~ Nest + (1|Colony), data = Aggression_Data_Full))

# Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(Removed ~ Nest + (1|Colony), data = Aggression_Data_Full))

# LINEAR MIXED EFFECTS MODEL
# RESPONSE VARIABLE 
# Removed - Time (in seconds) taken to remove the conspecific nest invader
# FIXED EFFECTS
# Nest - Nest shape (Tube / Circle)
# ScaledDist - Maximum scaled distance to the nest entrance of the nest invader
# RANDOM EFFECT
# (1|Colony) - Colony identification 
summary(lmer(Removed ~ Nest * ScaledDist + (1|Colony), data = Aggression_Data_Full))

# Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(Removed ~ Nest * ScaledDist + (1|Colony), data = Aggression_Data_Full))

# LINEAR MIXED EFFECTS MODEL
# RESPONSE VARIABLE 
# Removed - Time (in seconds) taken to remove the conspecific nest invader
# FIXED EFFECTS
# Nest - Nest shape (Tube / Circle)
# Attacking.Max - Maximum worker attacking the nest invader
# RANDOM EFFECT
# (1|Colony) - Colony identification 
summary(lmer(Removed ~ Nest * Attacking.Max + (1|Colony), data = Aggression_Data_Full))

# Marginal and conditional R-squared, showing the influence of the random effect on the model
r.squaredGLMM(lmer(Removed ~ Nest * Attacking.Max + (1|Colony), data = Aggression_Data_Full))
