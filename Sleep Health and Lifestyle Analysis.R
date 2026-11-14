library(dplyr)
library(ggplot2)
library(readr)
library(viridis)
library(ggpubr)

shl <- read.csv("Sleep_health_and_lifestyle_dataset.csv")

# How are age, stress levels, and physical activity levels related to sleep disorders, quality, and duration? 
# Question 1: How does the presence of sleep disorders vary across age groups? 
# Age categories
summary(shl$Age)
shl$agecat <- shl$Age |>
  cut(
    breaks = c(-Inf, 35, 45, 55, Inf),
    labels = c("35 and under", "35 - 45", "46 - 55", "55 and older"))

# Frequency table
TAB <- table(shl$agecat, shl$`Sleep.Disorder`)

# Relative frequency table
round(prop.table(table(shl$agecat, shl$Sleep.Disorder), 1), digits = 3)

# 100% stacked bar chart
shl |>
  group_by(agecat, Sleep.Disorder) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(agecat) |>
  mutate(pct = n/sum(n)) |>
  ggplot(aes(x = agecat, y = pct, fill = Sleep.Disorder)) +
  geom_bar(stat = 'identity',
           position = position_stack()) +
  labs(x = "Age Group",
       y = "Proportion",
       fill = "Sleep Disorder",
       title = "Distribution of Sleep Disorders Across Age Groups") +
  theme_classic() +
  scale_fill_viridis(discrete = TRUE, option = "C")

# Chi square test 
# Null Hypothesis: Age group and sleep disorder are independent.
# Alternative Hypothesis: Age group and sleep disorder are not independent.
chisq.test(TAB)


#Question 2: Does sleep quality differ across age groups? 
#Mean and standard deviation
shl |>
  group_by(agecat) |>
  summarise(
    Mean = mean(Quality.of.Sleep, na.rm = TRUE),
    SD = sd(Quality.of.Sleep, na.rm = TRUE)
  )

# Bar chart
shl |>
  group_by(agecat) |>
  summarise(
    Mean = mean(Quality.of.Sleep, na.rm = TRUE),
    Median = median(Quality.of.Sleep, na.rm = TRUE),
    SD = sd(Quality.of.Sleep, na.rm = TRUE)) |>
  ggplot(aes(x = agecat, y = Mean, fill = agecat)) +
  geom_bar(stat = 'identity', color = 'black') +
  geom_text(aes(label = round(Mean, 2)), vjust = -0.5) +
  labs(x = "Age Group",
       y = "Mean Quality of Sleep",
       title = "Average Sleep Quality Across Age Groups",
       fill = "Age Range") +
  theme_classic() +
  theme(legend.position = "right") +
  scale_y_continuous(limits = c(0, 10)) +
  scale_fill_viridis(discrete = TRUE, option = "C")

# ANOVA 
# Null Hypothesis: There is no difference in mean sleep quality across age groups.
# Alternative Hypothesis: There is a difference in mean sleep quality across age groups.
table(shl$agecat)
sleep_anova <- aov(Quality.of.Sleep ~ agecat, data = shl)
summary(sleep_anova)

# Question 3: How is stress level related to sleep quality? 
# Correlation coefficient
cor(shl$Stress.Level, shl$Quality.of.Sleep)

# Scatterplot with line of best fit
shl |>
  ggplot(aes(x = Stress.Level, y = Quality.of.Sleep)) +
  geom_point(color = "violetred4", alpha = 0.5) +
  geom_smooth(method = "lm", se = F, color = "chartreuse4") +
  stat_regline_equation(label.x = 6.5, label.y = 9) +
  labs(x = "Stress Level",
       y = "Quality of Sleep",
       title = "Relationship Between Stress Level and Sleep Quality") +
  theme_classic()

# Pearson correlation hypothesis test 
# p-value = 0.05
# Null Hypothesis: There is no linear correlation between stress level and sleep quality.
# Alternative Hypothesis: There is a linear correlation between stress level and sleep quality.
cor.test(shl$Stress.Level, shl$Quality.of.Sleep)


# Question 4: How does sleep duration vary across stress level groups? 
# Mean and median
shl |>
  group_by(Stress.Level) |>
  summarise(
    Mean = mean(Sleep.Duration),
    Median = median(Sleep.Duration),
  )

# Bar chart
shl |>
  group_by(Stress.Level) |>
  summarise(Mean = mean(Sleep.Duration)) |>
  ggplot(aes(x = factor(Stress.Level), y = Mean, fill = factor(Stress.Level))) +
  geom_bar(stat = 'identity', color = 'black') +
  geom_text(aes(label = round(Mean, 2)), vjust = -0.5) +
  labs(x = "Stress Level",
       y = "Mean Sleep Duration (Hours)",
       fill = "Stress Level",
       title = "Average Sleep Duration Across Stress Levels") +
  theme_classic() +
  theme(legend.position = "right") +
  scale_y_continuous(limits = c(0, 10)) +
  scale_fill_viridis(discrete = TRUE, option = "C")

# ANOVA 
# Null Hypothesis: There is no difference in mean sleep duration across stress levels.
# Alternative Hypothesis: There is a difference in mean sleep duration across stress levels.
sleep_duration_anova <- aov(Sleep.Duration ~ factor(Stress.Level), data = shl)
summary(sleep_duration_anova)


# Question 5: How is physical activity level related to sleep duration? 
# Correlation coefficient
cor(shl$Physical.Activity.Level, shl$Sleep.Duration)

# Scatterplot with line of best fit
shl |>
  ggplot(aes(x = Physical.Activity.Level, y = Sleep.Duration)) +
  geom_point(color = "violetred4", alpha = 0.5) +
  geom_smooth(method = "lm", se = F, color = "chartreuse4") +
  stat_regline_equation(label.x = 40, label.y = 8.5) +
  labs(x = "Physical Activity Level",
       y = "Sleep Duration (Hours)",
       title = "Relationship Between Physical Activity Level and Sleep Duration") +
  theme_classic()

# Correlation hypothesis test 
# Null Hypothesis: There is no linear correlation between physical activity level and sleep duration.
# Alternative Hypothesis: There is a linear correlation between physical activity level and sleep duration.
cor.test(shl$Physical.Activity.Level, shl$Sleep.Duration)
