
# Loading of packages/libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

# Load necessary libraries
library(tidyverse)

# Read the CSV file
Data <- read.csv("cleaned_trial_for_anova.csv")

colnames(Data)
table(Data$Isolate)
table(Data$Antibiotic)
table(Data$Zone)


# Antibiotic (factor), Isolate (factor), and Zone (numeric)

anova_model <- aov(Zone ~ Antibiotic + Isolate, data = Data)
summary(anova_model)


# Calculate F critical value
alpha <- 0.05
Data1 <- 3  # between groups
Data2 <- 36 # within groups

f_crit <- qf(1 - alpha, Data1, Data2)
f_crit


alpha <- 0.05
df1 <- 3  # between groups
df2 <- 16 # within groups

f_crit2 <- qf(1 - alpha, df1, df2)
f_crit2

#-------------------------------------------------------------------
#                         Plot
#------------------------------------------------------------------

# Ensure Antibiotic and Isolate are factors
Data$Antibiotic <- as.factor(Data$Antibiotic)
Data$Isolate <- as.factor(Data$Isolate)

# Adjust margins to make room for long x-axis labels
par(mar = c(10, 5, 4, 2) + 0.1)

antibiotic_levels <- levels(Data$Antibiotic)

# interaction plot
interaction.plot(
  x.factor = Data$Antibiotic,
  trace.factor = Data$Isolate,
  response = Data$Zone,
  fun = mean,
  type = "b",           # Line + points
  col = 1:length(levels(Data$Isolate)),  # color per isolate
  pch = 1:length(levels(Data$Isolate)),  # shape per isolate
  xlab = "Antibiotic",
  ylab = "Mean Zone of Inhibition (mm)",
  trace.label = "Isolate",
  main = "Interaction Plot: Antibiotic × Isolate",
  cex.axis = 0.8,
  cex.lab = 1,
  cex.main = 1.2,
  xaxt = "n"            # Disable automatic x-axis
)

# manually addition of x-axis labels using the variable
axis(
  side = 1,
  at = 1:length(antibiotic_levels),   # Match positions
  labels = antibiotic_levels,
  las = 2,                            # Rotate labels vertically
  cex.axis = 0.8
)




#     Grouped Bar Plot (Bar Chart) with Error Bars

ggplot(Data, aes(x = Antibiotic, y = Zone, fill = Isolate)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(0.9), width = 0.2) +
  labs(title = "Zone of Inhibition by Antibiotic and Isolate",
       y = "Mean Zone of Inhibition (mm)",
       x = "Antibiotic") +
  theme_minimal()



#     Jittered scatter plot

ggplot(Data, aes(x = Antibiotic, y = Zone, color = Isolate)) +
  geom_jitter(width = 0.2, height = 0) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 18, position = position_dodge(width = 0.75)) +
  theme_minimal()




#       Faceted grouped bar chart 

ggplot(Data, aes(x = Antibiotic, y = Zone, fill = Antibiotic)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(0.9), width = 0.2) +
  facet_wrap(~ Isolate) +
  labs(title = "Zone of Inhibition per Antibiotic by Bacterial Isolate",
       y = "Mean Zone of Inhibition (mm)",
       x = "Antibiotic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






#     faceted bar + jitter plot with error bars

ggplot(Data, aes(x = Antibiotic, y = Zone, fill = Antibiotic)) +
  geom_jitter(position = position_jitter(width = 0.2), shape = 21, size = 2, alpha = 0.5) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.7) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  facet_wrap(~ Isolate) +
  labs(title = "Zone of Inhibition per Antibiotic by Isolate",
       y = "Zone of Inhibition (mm)",
       x = "Antibiotic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#         faceted violin-boxplot

ggplot(Data, aes(x = Antibiotic, y = Zone, fill = Antibiotic)) +
  geom_violin(trim = FALSE, alpha = 0.6) +  # Show full distribution
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +  # Add boxplot inside violin
  facet_wrap(~ Isolate) +
  labs(title = "Distribution of Zone of Inhibition by Antibiotic and Isolate",
       y = "Zone of Inhibition (mm)",
       x = "Antibiotic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


#         faceted comparative boxplot

ggplot(Data, aes(x = Antibiotic, y = Zone, fill = Antibiotic)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2, alpha = 0.7) +
  facet_wrap(~ Isolate) +
  labs(title = "Zone of Inhibition by Antibiotic (Faceted by Isolate)",
       y = "Zone of Inhibition (mm)",
       x = "Antibiotic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
