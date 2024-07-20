### MASTERPROEF: VOLUNTARY TASK SWITCHING w EXPLICIT/MASKED REWARD 
### written by Helena Corens

### ensure to document your preprocessing steps thoroughly to maintain transparency and reproducibility in your analysis

# To remove all previous data
rm(list=ls())
# If graphic error: dev.off() 

# Packages
library(tidyr) # tidy data
library(ggplot2)
#library(lattice)
library(ggthemes)
library(tidyverse)
library(car) # univariate and multivariate tests
library(lme4) # linear mixed models (lmer & glmer models)
library(lmerTest)
library(dplyr)
library(ggpubr)
library(lattice) # boxplots
library(sjPlot) #for plotting lmer and glmer mods
library(sjstats)
library(emmeans)
library(jtools)
library(ROCR)
library(effects)
library(sjPlot)
#library(knitr)
library(DHARMa)
library(ggdist)
# library(lmerTest)

# library(reshape2) # reshape data from wide to long or vice versa
# library(effects) # plots
# library(RColorBrewer)
# library(lmerTest)
# library(cowplot) #for manuscript ready figures
# library(sjmisc) 
# library(sjstats)
# library(afex)
# library(ggplot2)
# library(ggthemes)  # For additional themes
# library(scales)
# library(sjPlot)

# Working directory 
getwd()
setwd("/Users/HC/MP_DATA/")
# Load data
data_all<- read.csv("datalog_complete.csv")


# ----------------------- PREPROCESSING ----------------------- #

# 1. Exclude rejected subjectIDs
excluded_pp <- c("oqe3fa7novfjq7anh5nlnkpynw3obvuq", "yrh96kvpm40jb4hols0d5rpm7fuqksqo", "1z8mdxevdznh8ejg6azbw2ynevccwydp",
                 "e2yuqxbgrx72co6ab6jkks82s7xgtb6h", "seg7h7julqwa3qwm0oh4yf7r0rno8jyc", "zkoxr13g3vlvbzr9vk97b9tsegrrl41j",
                 "n3hkdjov6g8ehptm0cxrof3ak0jfb1px")
data_all <- data_all[!data_all$subjID %in% excluded_pp, ]
print(length(unique(data_all$subjID)))

# Subjects w accuracy < 60% are already filtered out in previous R script (see file: MP_ppSelectionR.R)

# 2. Exclude practice & bisbas ('training'/'questionnaire') block
# If we want to investigate questionnaire at the end: include questionnaire phase again
data_all <- subset(data_all, phase!="training" & phase!= "questionnaire")

# 3. Remove NULL answers; 'postnull', 'first' answers (transition_new) and post-error trials (both for accuracy analysis as RT analysis)
# Replace empty values with '0' in the 'correcttarresp' variable
data_all <- subset(data_all, !(tarresp == "NULL"))
data_all <- subset(data_all, transition_new != "first" & transition_new != "postnull")
data_all$correcttarresp <- ifelse(data_all$correcttarresp == "", 0, data_all$correcttarresp)
# 'choice' still holds 3 Null responses (don't know where they're coming from) but remove
data_all <- subset(data_all, !(choice == "NULL"))

# Remove all post-error trials
table(data_all$correcttarresp) #  when data_all: 0: 1152 / 17495; when using 'analysis'  0: 1017 / 1:16733
# 1) Identify indices of incorrect trials
incorrect_indices <- which(data_all$correcttarresp == 0)
# 2) Determine indices of subsequent trials to be removed # Ensure that we do not exceed the number of trials
trials_to_remove <- incorrect_indices + 1
trials_to_remove <- trials_to_remove[trials_to_remove <= nrow(data_all)]
# 3) Subset the dataset to exclude trials following errors
data_all <- data_all[-trials_to_remove, ]
# 4) Verify that both correct and incorrect trials are still in the data
 # 'data_all':  0: 1051 - 1: 16444; analysis'0: 925 / 1: 15808


#data_all <- data_all %>%
#  mutate(correcttarresp = as.numeric(correcttarresp),
#         switch = as.numeric(switch))

# 4. Select variables to use
analysis <- data_all %>% select(subjID, group, rt, category, choice, transition_new, tarresp, correcttarresp, trialnr, 
                                 blocknr, phase, reward, congruency, payout1, payout2, payout3, payout4, totalpayout, 
                                 stimulus)

# analysis <- select(data_all, all_of(c("subjID", "group", "rt", "category", "choice", "transition_new","tarresp", 
 #                                     "correcttarresp", "trialnr","blocknr", "phase", "reward", "congruency", 
 #                                     "payout1", "payout2", "payout3", "payout4", "totalpayout", "stimulus"  )))

# 5. Conversion data type 
analysis$group <- as.factor(analysis$group)
# analysis$group <- factor(data$group, levels = c("condswitch", "condrepetition"))
analysis$rt <- as.numeric(analysis$rt)
analysis$choice <- as.factor(analysis$choice)
analysis$transition_new <- as.factor(analysis$transition_new)
analysis$tarresp <- as.factor(analysis$tarresp)
analysis$correcttarresp  <- as.numeric(analysis$correcttarresp )
analysis$trialnr <- as.numeric(analysis$trialnr)
analysis$blocknr <- as.numeric(analysis$blocknr)
analysis$phase <- as.factor(analysis$phase)
analysis$reward <- as.numeric(analysis$reward)
analysis$congruency <- as.factor(analysis$congruency)
analysis$payout1 <- as.numeric(analysis$payout1)
analysis$payout2 <- as.numeric(analysis$payout2)
analysis$payout3 <- as.numeric(analysis$payout3)
analysis$payout4 <- as.numeric(analysis$payout4)
analysis$totalpayout <- as.numeric(analysis$totalpayout)

# Zero-sum coding group (intercept is grand mean across all conditions)
contrasts(analysis$group) <- contr.sum(levels(analysis$group))
contrasts(analysis$group)

# 6.REMOVE OUTLIERS
# RT outliers: less than 200ms - more than ?3000ms?
analysis <- subset(analysis, rt > 200) #lenght(analysis$rt): 18642

analysis$rtoutlier <- 0
for (i in sort(unique(analysis$subjID))) {
  subi <- subset(analysis, subjID == i)
  Q1 <- quantile(subi$rt, 0.25, na.rm = TRUE)
  Q3 <- quantile(subi$rt, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  analysis$rtoutlier[analysis$subjID == i & (analysis$rt < lower_bound | analysis$rt > upper_bound)] <- 100
}

table(analysis$rtoutlier) # 892 rt outliers; 17755 within IQR // 0: 16679; 1: 812
length(analysis$rtoutlier) #18642 #17491
analysis <- subset(analysis,rtoutlier == 0) #length(analysis$rt) = 16679

# Code switch to use in models
analysis$switch <- dplyr::recode(analysis$transition_new,
                                 "repetition" = 0,
                                 "switch" = 1)

table(analysis$correcttarresp)
table(analysis$switch)

# 7. RECODE & SUBSET: MASKED PHASE & REMOVE POST-ERRORS

# Recode blocknr: 1,2,3,4; then centre it (helps with lme4 models)
data_masked <- subset(analysis, phase =="masked")

data_masked$blocknr <- dplyr::recode(data_masked$blocknr, 
                                "1" = 1, #-1.5
                                "3" = 2, #-0.5
                                "5" = 3, # 0.5
                                "7" = 4) # 1.5
# Center recoded block numbers
data_masked$centered_blocknr <- scale(data_masked$blocknr, center = T, scale = F)
#unique(data_masked$blocknr)
unique(data_masked$centered_blocknr) 
data_masked$centered_blocknr <- as.numeric(data_masked$centered_blocknr)

# Log-transform rt (skewness is compared down below @eyeball data -> RT PLOTS)
data_masked$log_rt <- log(data_masked$rt)
# Check distribution of log-transformed rt's
hist(data_masked$log_rt, breaks = 20, main = "Histogram of Log-Transformed RTs")
# watch out for conversions

# write.csv(data_masked, file = "datamasked.csv")



# ------------ EYEBALL DATA ------------ #
# Check distributions, violin plots, switch rates, switch rates over time

# General info
data_all %>%
  group_by(gender) %>%
  summarise(participant_count = n_distinct(subjID))

data_all %>%
  summarize(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE))

data_all %>%
  group_by(handedness) %>%
  summarise(participant_count = n_distinct(subjID))


# mean & sd rt, payout, accuracy
analysis %>%
  summarize(
    mean_rt = mean(rt, na.rm = TRUE),
    sd_rt = sd(rt, na.rm = TRUE),
    min_rt = min(rt, na.rm = TRUE),
    max_rt = max(rt, na.rm = TRUE),
    
    mean_totalpayout = mean(totalpayout, na.rm = TRUE),
    sd_totalpayout = sd(totalpayout, na.rm = TRUE),
    min_totalpayout = min(totalpayout, na.rm = TRUE),
    max_totalpayout = max(totalpayout, na.rm = TRUE)
  )

# ___ 

overall_accuracy <- analysis %>%
  summarize(mean_accuracy = format(mean(correcttarresp) * 100,3),
            sd_accuracy = format(sd(correcttarresp) * 100,3))

# Calculate accuracy (percentage) by group and reward phase
accuracy_by_phase <- analysis %>%
  group_by(group, phase) %>%
  summarize(mean_accuracy = format(mean(correcttarresp, na.rm = TRUE) * 100,3),
            sd_accuracy = format(sd(correcttarresp, na.rm = TRUE) * 100,3))

# Calculate accuracy (percentage) by group
analysis %>%
  group_by(group) %>%
  summarize(mean_accuracy = format(mean(correcttarresp, na.rm = TRUE) * 100,3),
            sd_accuracy = format(sd(correcttarresp, na.rm = TRUE) * 100,3))

# Calculate overall reaction times
overall_reaction_time <- analysis %>%
  group_by(group) %>%
  summarize(mean_reaction_time = format(mean(rt, na.rm = TRUE),3),
            sd_reaction_time = format(sd(rt, na.rm = TRUE),3))

# Calculate reaction times by reward phase
reaction_time_by_phase <- analysis %>%
  group_by(group, phase) %>%
  summarize(mean_reaction_time = format(mean(rt, na.rm = TRUE),3),
            sd_reaction_time = format(sd(rt, na.rm = TRUE),3))

# Calculate switch rates (percentage)
switch_rate <- analysis %>%
  group_by(group) %>%
  summarize(mean_switch_rate = round(mean(switch, na.rm = TRUE) * 100,3),
            sd_switch_rate = round(sd(switch, na.rm = TRUE) * 100,3))

# Calculate switch rates (percentage) by reward phase
switch_rate_by_phase <- analysis %>%
  group_by(group, phase) %>%
  summarize(mean_switch_rate = format(mean(switch, na.rm = TRUE) * 100,3),
            sd_switch_rate = format(sd(switch, na.rm = TRUE) * 100,3))

# Calculate mean reward earned
mean_reward_earned <- analysis %>%
  group_by(group) %>%
  summarize(mean_reward = round(mean(totalpayout, na.rm = TRUE),3),
            sd_reward = round(sd(totalpayout, na.rm = TRUE),3))

# Print the results
print("Overall Accuracy (Percentage):")
print(overall_accuracy)

print("Accuracy by Reward Phase (Percentage):")
print(accuracy_by_phase)

print("Overall Reaction Times:")
print(overall_reaction_time)

print("Reaction Times by Reward Phase:")
print(reaction_time_by_phase)

print("Switch Rates (Percentage):")
print(switch_rate)

print("Switch Rates by Reward Phase (Percentage):")
print(switch_rate_by_phase)

#print("Number of Correct Trials:")
#print(correct_trials)

print("Mean Reward Earned:")
print(mean_reward_earned)

# COUNT HOW MANY PP CHOOSE ONLY 'REPEAT' ' (categorise pp for amount of times they switch or not)

# Ensure switch variable is numeric
analysis <- analysis %>%
  mutate(switch = as.numeric(switch))

# Summarize the number of 'repeat' and 'switch' choices for each participant, group, and phase
choices_summary <- analysis %>%
  group_by(subjID, group) %>%
  summarize(repeat_count = sum(switch == 0, na.rm = TRUE),
            switch_count = sum(switch == 1, na.rm = TRUE),
            total_trials = n())

switchrate <- data_masked %>%
  group_by(subjID,group) %>%
  summarize(switchrate = mean(switch, na.rm = TRUE) * 100)
print(switchrate, n = nrow(switchrate))

# Define the categories based on switch rate ranges
library(dplyr)
switchrate <- switchrate %>%
  mutate(
    category = cut(
      switchrate,
      breaks = c(-Inf, 0, 0.1, 5, 10, 20, 35, 50, 75, 100),
      labels = c("0%", "0-0.1%", "0.1-5%", "5-10%", "10-20%", "20-35%", "35-50%", "50-75%", "75-100%"),
      right = FALSE,
      include.lowest = TRUE
    )
  )
# Print the categorized data
print(switchrate, n = nrow(switchrate))

category_counts <- switchrate %>%
  group_by(category) %>%
  summarize(
    total_count = n(),
    count_condswitch = sum(group == 'condswitch'),
    count_condrepetition = sum(group == 'condrepetition')
  )
print(category_counts)

#Just a check: switchrate %>%
                # filter(subjID == "la8l60rud8gox4qhbnnyukotel61ql3r")

library(reshape2)
category_counts_melted <- melt(category_counts, id.vars = "category", 
                               measure.vars = c("count_condswitch", "count_condrepetition"),
                               variable.name = "group", value.name = "Count")

# Create a stacked bar plot
plotppcountVSR <- ggplot(category_counts_melted, aes(x = category, y = Count, fill = group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("count_condswitch" = "dodgerblue2", "count_condrepetition" = "lightskyblue1"),
                    labels = c("count_condswitch" = "CondSwitch", "count_condrepetition" = "CondRepetition")) +
  labs(title = "Participants by their Voluntary Switch Rate and Experimental Condition",
       x = "VSR",
       y = "Participant Count") +
  theme_minimal()
print(plotppcountVSR)
# ggsave("PPcountVSR.png", plotppcountVSR, width = 10, height = 6, units = "in", dpi = 300)


plotdistrswitchratesGroup <- ggplot(switchrate, aes(x = group, y = switchrate, fill = group)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_jitter(width = 0.2, size = 1, alpha = 0.8) +
  labs(title = "Distribution of Switch Rates by Group",
       x = "Group",
       y = "Switch Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")
print(plotdistrswitchratesGroup)
# ggsave("plotDistrSwitchrateGroup", plotdistrswitchratesGroup, width = 10, height = 6, units = "in", dpi = 300)

# CHECK SWITCH COUNT DISTRIBUTION PER PHASE

switch_rate <- analysis %>%
  group_by(subjID, group, phase) %>%
  summarize(switch_rate = mean(switch, na.rm = TRUE) * 100)

# Plot the distribution of the switch rate using a histogram
ggplot(switch_rate, aes(x = switch_rate, fill = group)) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
  facet_wrap(~phase) +
  labs(title = "Distribution of Switch Rate by Group and Reward Phase",
       x = "Switch Rate (%)",
       y = "Count",
       fill = "Group") +
  theme_minimal()

ggplot(switch_rate, aes(x = group, y = switch_rate, fill = group)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~phase) +
  labs(title = "Boxplot of Switch Rate by Group and Reward Phase",
       x = "Group",
       y = "Switch Rate (%)",
       fill = "Group") +
  theme_minimal()


# --- VSR PLOTS

# Distribution of switch rate over the course of each trial
color_palette <- c("condswitch" = "dodgerblue2", "condrepetition" = "lightskyblue1")
VSR_trialnr <- aggregate(transition_new == "switch" ~ blocknr + trialnr + group, data = data_masked, FUN = mean)

# Switch rates over trial number for each group, faceted by blocknr
plotVSR <- ggplot(data = VSR_trialnr, aes(x = trialnr, y = `transition_new == "switch"`, group = group, color = group)) +
  geom_smooth() +
  labs(title = "Switch Rates Over Trial Number by Block & Group",
       x = "Trial Number",
       y = "Switch Rate") +
  scale_x_continuous(breaks = VSR_trialnr$trialnr) +  # Set breaks explicitly
  theme_minimal() +
  facet_wrap(~ blocknr, nrow = 2) +
  scale_color_manual(values = color_palette)
print(plotVSR)
ggsave("VSR_plot.png", plotVSR, width = 10, height = 6, units = "in", dpi = 300)


# Bar plot of switch and repeat events per group - DATASET: 'data_masked'
ggplot(data = data_masked, aes(x = group, fill = transition_new)) +
  geom_bar(position = "stack") +
  labs(title = "Frequency of Switch and Repeat Events by Group",
       x = "Group",
       y = "Frequency") +
  scale_fill_manual(values = c("switch" = "dodgerblue2", "repetition" = "lightskyblue1"), 
                    name = "Transition Type") +
  theme_minimal()

# Summarize the data by group and phase, and calculate counts and percentages
summary_data <- analysis %>%
  group_by(group, phase, transition_new) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
# Bar plot with the percentage of transition types by group and phase/ DATASET: 'analysis'
plottransition <- ggplot(data = summary_data, aes(x = group, y = percentage, fill = transition_new)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(label = paste0(sprintf("%.1f", percentage), "%")), position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Percentage of Switch and Repeat Events by Group and Phase",
       x = "Group",
       y = "Percentage") +
  scale_fill_manual(values = c("switch" = "dodgerblue2", "repetition" = "lightskyblue1"), name = "Transition Type") +
  facet_wrap(~ phase) +
  theme_minimal()
print(plottransition)

ggsave("transition_plot.png", plottransition, width = 10, height = 6, units = "in", dpi = 300)

# --- RT PLOTS

# rt skewed to the right: use log_rt!!!!
qqnorm(data_masked$rt)
qqline(data_masked$rt)

qqnorm(data_masked$log_rt)
qqline(data_masked$log_rt)

# Density plot for reaction times (rt) -> Skewed to the right
ggplot(data = data_masked, aes(x = log_rt)) +
  geom_density(fill = "skyblue") +
  labs(title = "Density Plot of Reaction Times",
       x = "Reaction Time",
       y = "Density") +
  theme_minimal()

# Boxplot rt by group
boxplotrt_transition <- bwplot(log_rt ~ transition_new, data = data_masked)

ggplot(data_masked, aes(x = transition_new, y = log_rt, fill = transition_new)) +
  geom_boxplot(color = "black", outlier.shape = 16, outlier.size = 2) +  # Customize boxplot appearance
  geom_jitter(width = 0.2, alpha = 0.5) +  # Add jittered points for better visualization
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "white") +  # Add median points
  labs(title = "Reaction Time by Transition Type", x = "Transition Type", y = "Log Reaction Time") +  # Label axes
  scale_fill_brewer(palette = "Set3") +  # Choose a color palette
  theme_minimal() +  # Customize theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Alternative boxplot
ggplot(data = data_masked, aes(x = transition_new, y = log_rt, fill = transition_new)) +
  geom_boxplot() +
  labs(title = "Boxplot of Reaction Times by Group",
       x = "transition_new",
       y = "Reaction Time") +
  theme_minimal()

# violin plot
ggplot(data = data_masked, aes(x = group, y = log_rt)) +
  geom_violin(fill = "skyblue", color = "black") +
  labs(title = "Violin Plot of Reaction Times by Group",
       x = "Group",
       y = "Reaction Time") +
  theme_minimal()

# Ultimate plot (violin plot with dots of data points) 
# PROBLEM: DATA POINT CHANGE???

ggplot(data_masked, aes(x = transition_new, y = log_rt, fill = transition_new)) +
  geom_violin(trim = FALSE) +  # Expand the width of the violins
  geom_jitter(width = 0.2, alpha = 0.5) +  # Add jittered points for better visualization
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "white") +  # Add median points
  labs(title = "Reaction Time by Transition Type", x = "Transition Type", y = "Log Reaction Time") +  # Label axes
  scale_fill_brewer(palette = "Set3") +  # Choose a color palette
  theme_minimal() +  # Customize theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels


# --- REWARD PLOTS

bwplot(totalpayout ~ group, data = data_masked)

# Boxplot reward amount per group 
ggplot(data = data_masked, aes(x = group, y = totalpayout, fill = transition_new)) +
  geom_boxplot() +
  labs(title = "Boxplot of Total reward by Group",
       x = "Group",
       y = "Reward amount") +
  theme_minimal()


# --- ACCURACY PLOTS

#  NOT GOOD ENOUGH, still have to figure out how to visualise this: PROBLEM FOR LATER
# Calculate error rate
data_masked$error_rate <- 1 - data_masked$correcttarresp

# Create violin plot
ggplot(data_masked, aes(x = transition_new, y = error_rate, fill = transition_new)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(title = "Error Rate by Transition Type", x = "Transition Type", y = "Error Rate") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ________________________________________________________________________________________________
# --- PREP FOR ANALYSIS

# RT analysis: remove postnull; remove errors
RTdata_masked <- data_masked[data_masked$correcttarresp != 0, ]

# ER analysis (accuracy): remove postnull; keep errors
# since we already did this above, we just use data_masked vor ER - rename for convenience
ERdata_masked <- data_masked

# write.csv(RTdata_masked, file = "RTdata_masked.csv")
# write.csv(ERdata_masked, file = "ERdata_masked.csv")


# ----------------------- ANALYSIS ----------------------- #


options(contrasts = c("contr.sum", "contr.poly"))
contrasts(RTdata_masked$group)
#contrasts(RTdata_masked$switch)
#contrasts(RTdata_masked$ ??? )

# --------- VSR instead of 'transition_new'--> ALREADY DONE AT 'RECODE' part of preprocessing ("switch" variable)
# 
# switch_rate <- aggregate(transition_new ~ subjID, data = RTdata_masked, function(x) mean(x == "switch"))
# # Merge switch rates with the original dataset based on subjID
# RTdata_masked <- merge(RTdata_masked, switch_rate, by = "subjID", suffixes = c("", "_switch_rate"))
# # Rename the new column
# colnames(RTdata_masked)[ncol(RTdata_masked)] <- "switch_rate"
# # Print or inspect the updated dataset
# head(RTdata_masked)
# 
# bwplot(switch_rate ~ group, data = RTdata_masked)
# hist(RTdata_masked$centered_switchrate, breaks = 20, main = "Histogram of centered switchrate")

# RTdata_masked$centered_switchrate <- scale(RTdata_masked$switch_rate, center = T, scale = F)
# Scaling doesn't help with the converisons

# ----------------------------------------

# M1 (random intercept for each subject)

modelVSR1 <- glmer(switch ~ group + (1 | subjID), data = RTdata_masked,  family = binomial(link = "logit"))
summary(modelVSR1)
plot(modelVSR1)
summ(modelVSR1, exp = T) # Being in condrepetition group lowers the odds of VSR by (1 – 0.23)% = 77%, in comparison to being in the condswitch group, assuming everything else stays constant.
plot(allEffects(modelVSR1, fixed.predictors = list(given.values=c(group1=0))))

# dispersion_glmer(modelVSR1) # 0.6609282 --> underdispersion: problem?

# M2 + centered_blocknr
modelVSR2 <- glmer(switch ~ group + centered_blocknr + (1 | subjID), 
                   data = RTdata_masked, 
                   family = binomial(link = "logit"))
summary(modelVSR2)
plot(modelVSR2)
summ(modelVSR2, exp = T)
plot(allEffects(modelVSR2)) # probabilities are very low

### binomial data and residual plot don't match well

# dispersion_glmer(modelVSR2) # 0.6548242 --> underdispersion: problem?

# M3

modelVSR3 <- glmer(switch ~ group *centered_blocknr + (1 | subjID), data = RTdata_masked, family = binomial(link = "logit"))
summary(modelVSR3)
plot(modelVSR3)
summ(modelVSR3, exp = T)
plot(allEffects(modelVSR3))

cor_matrix <- cor(RTdata_masked[, c("switch_rate")])
eigen_values <- eigen(cor_matrix)$values
  
  # dispersion_glmer(modelVSR3) # 0.6504212 --> underdispersion: problem?

modeltest <- glmer(switch ~ group*centered_blocknr*congruency*choice + (1 | subjID), data = RTdata_masked, family = binomial(link = "logit"))
summary(modeltest)

 

# M4
modelVSR4 <- glmer(switch ~ group *centered_blocknr + (1 + centered_blocknr | subjID), data = RTdata_masked, family = binomial(link = "logit"))
summary(modelVSR4)
plot(modelVSR4)
summ(modelVSR4, exp = T)
#plot(allEffects(modelVSR4))

# FULL VSR MODEL
# binary outcome with a logit link, so the raw estimates are on the log-odds scale
# e.g. 1 unit increase in blocknr, is a decrease of 2.49507 log-odds of switch

# VSR_data <- RTdata_masked %>%
#   group_by(subjID) %>%
#   summarise(VSR = sum(transition_new == "switch") / n())
# 
# # View the first few rows of the VSR_data to check if VSR is calculated correctly
# head(VSR_data)
# _________

fullVSR <- glmer(switch~ group *centered_blocknr*choice + (1 + centered_blocknr| subjID), data = RTdata_masked, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))
summary(fullVSR)
summ(fullVSR, exp = T)

conf_intervals <- confint(fullVSR,parm = "beta_", method = "Wald")
fixed_effects <- summary(fullVSR)$coefficients
fixed_effects_with_ci <- cbind(fixed_effects, conf_intervals)

data.frame(
  Estimate = fixed_effects_with_ci[, 1],
  `Std. Error` = fixed_effects_with_ci[, 2],
  `z value` = fixed_effects_with_ci[, 3],
  `Pr(>|z|)` = fixed_effects_with_ci[, 4],
  `2.5 %` = fixed_effects_with_ci[, 5],
  `97.5 %` = fixed_effects_with_ci[, 6]
)

# ________________________

fullVSRtest <- glmer(switch ~ group *centered_blocknr*choice*totalpayout + (1 + centered_blocknr| subjID), data = RTdata_masked, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 35000)))
summary(fullVSRtest)


fullVSRtest2 <- glmer(switch ~ group *centered_blocknr*choice*totalpayout + (1 + totalpayout| subjID), data = RTdata_masked, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))
summary(fullVSRtest2)

anova(fullVSR, fullVSRtest, fullVSRtest2, test = "Chisq")

# ________________________

# INTERPRETATION
# INTERCEPT: average of VSR, when group = repetition, blocknr = 1 and choice == animacy, (baseline levels)
    # The estimated coefficient for the intercept, -7.873, is the log odds of "switch" being 1 
    # when blocknr is equal to zero, and group and choice take their reference value (we compare to the average mean)

    # The estimate for blocknr of -2.372 means that a 1 unit increase in blocknr is associated with a -2.372 decrease in the log-odds of 
    # switch being 1, compared to switch being 0. If we exponentiate this number then we obtain the odds ratio of 0.9427445, which means that 
    # for a 1 unit increase in Length we expect to see (approximately) a 6% decrease in the odds of RespYN being 1.

# COMPARE MODELS: 
anova(modelVSR1, modelVSR2, test = "Chisq")
anova(modelVSR3, modelVSR4, test = "Chisq")
anova(modelVSR1, modelVSR4, test = "Chisq")
anova(fullVSR, fullVSRtest, test = "Chisq")
anova(fullVSR, modelVSR1, modelVSR2, modelVSR3, modelVSR4, fullVSRtest, test = "Chisq")
# implementing 'centered_blocknr' as a random slope improves the model a lot!!!

# Correct classification rate (other test to check fit of model)
Pred <- predict(modelVSR4, type = "response")
Pred <- if_else(Pred > 0.5, 1, 0)
ConfusionMatrix <- table(Pred, pull(RTdata_masked, transition_new)) #`pull` results in a vector
#correct classification rate
sum(diag(ConfusionMatrix))/sum(ConfusionMatrix) #model correctly classifies 89% of all observations
ConfusionMatrix

performance::icc(modelVSR1)
performance::icc(modelVSR2)
performance::icc(modelVSR3)
performance::icc(modelVSR4)
performance::icc(fullVSR)

# AUC
Prob <- predict(modelVSR3, type="response")
Pred <- prediction(Prob, as.vector(pull(RTdata_masked, transition_new)))
AUC <- performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC # 94%: model discriminates very well btwn repeating and switching // for group: 77.8%

# instead of binary; try binomial with implementing 'totalreward'? to see if this has an effect?

# ----------------- TESTTTT
RTdata_masked$subjID
data_vsr <- RTdata_masked %>%
  group_by(subjID) %>%
  summarize(
    VSR = mean(transition_new == "switch"),
    blocknr = first(blocknr),  # Or use another appropriate method to assign blocknr
    group = first(group),
    choice = first(choice),    # This assumes the choice doesn't change. Modify if needed.
    n = n()
  )

model <- glmer(VSR ~ blocknr + group + choice + (1 | subjID), 
               family = binomial(link = "logit"),
               data = data_vsr,
               weights = n  # Optional: weight by the number of trials per participant
)

summary(model)
# -----------------------


# --------- TASK PERFORMANCE
# --- RT
# heteroscedasticity concerns with lmer / watch out for conversions

options(contrasts = c("contr.sum", "contr.poly"))
# Basic intercept model RT
interceptRT <- lmer(log_rt ~ 1 + (1|subjID), data = RTdata_masked)
summary(interceptRT)
performance::icc(interceptRT)
plot(interceptRT, subjID ~ resid(.), abline = 0 )
plot(interceptRT)

# Full RT model
modelRT_full1 <-lmer(log_rt ~ group * transition_new * congruency * centered_blocknr * choice + (1|subjID), data = RTdata_masked) 
summary(modelRT_full1)

coefficients <- summary(modelRT_full1)$coefficients
estimates <- coefficients[, "Estimate"]
se <- coefficients[, "Std. Error"]

# Exponentiate the estimates and calculate confidence intervals
exp_estimates <- exp(estimates)
exp_confint_lower <- exp(estimates - 1.96 * se)
exp_confint_upper <- exp(estimates + 1.96 * se)

# Create a data frame for easier interpretation
results <- data.frame(
  Term = rownames(coefficients),
  Estimate = exp_estimates,
  CI_lower = exp_confint_lower,
  CI_upper = exp_confint_upper
)
results


modelRT_full2 <-lmer(log_rt ~ group * transition_new * congruency * centered_blocknr * choice + (1|subjID) + (1 + choice | stimulus) , data = RTdata_masked) 
summary(modelRT_full2)

modelRT_full3 <-lmer(log_rt ~ group * transition_new * (congruency + centered_blocknr + choice) + (1 + transition_new |subjID) + (1 + choice | stimulus) , data = RTdata_masked) 
summary(modelRT_full3)

modelRT_full3_random <- lmer(log_rt ~ group * transition_new * (congruency + centered_blocknr + choice) + 
                               (1 + transition_new + congruency | subjID) + (1 + choice | stimulus),
                             data = RTdata_masked)

modelRT_A <- lmer(log_rt ~ group * transition_new * (congruency + centered_blocknr + choice) + 
                               (1 + transition_new *group | subjID) + (1 + choice | stimulus),
                             data = RTdata_masked)
summary(modelRT_A)

modelRT_full4 <-lmer(log_rt ~ group * transition_new * (congruency + centered_blocknr + choice) + (1 + transition_new * (congruency+centered_blocknr+choice) |subjID) + (1 + choice | stimulus) , data = RTdata_masked) 
summary(modelRT_full4)

plot(modelRT_full2)
qqnorm(resid(modelRT_full2))
qqline(resid(modelRT_full2))

anova(modelRT_full1, modelRT_full2, modelRT_full3 )

######### LESLIE PROPOSITION FOR MAKING IT MORE SIMPLE
modelRT_leslie <- lmer(log_rt ~ group * transition_new * (congruency + centered_blocknr + choice) + (1+ transition_new * (congruency + centered_blocknr + choice) | subjID) + (1 + choice | stimulus), data = RTdata_masked,control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(modelRT_leslie)

model_choice2 <- lmer(log_rt ~ group * switch * (congruency + centered_blocknr + choice) + 
                        (1 + switch * (congruency + centered_blocknr + choice) | subjID) + (1 | stimulus), data = RTdata_masked)
summary(model_choice2)

model_choice3 <- lmer(log_rt ~ group + switch + congruency + choice + (group:switch) + (group:congruency) + (switch:congruency) +
                        (1 + switch * (congruency + centered_blocknr + choice) | subjID) + (1 | stimulus), data = RTdata_masked)
summary(model_choice3)

model_full <- lmer(log_rt ~ group * switch * congruency * centered_blocknr * choice + 
                     (1 + switch * (congruency + centered_blocknr + choice) | subjID) + 
                     (1 + choice | stimulus), 
                   data = RTdata_masked, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(model_full)

# ______________ TEEEEEESTTTTT GPT
model_basic <- lmer(log_rt ~ group * transition_new * congruency * centered_blocknr * choice + 
                      (1 | subjID) + (1 | stimulus), data = RTdata_masked)
summary(model_basic)

model_transition <- lmer(log_rt ~ group * transition_new * congruency * centered_blocknr * choice + 
                           (1 + transition_new | subjID) + (1 | stimulus), data = RTdata_masked)
summary(model_transition)


model_congruency <- lmer(log_rt ~ group * transition_new * congruency * centered_blocknr * choice + 
                           (1 + transition_new + congruency | subjID) + (1 | stimulus), data = RTdata_masked)
summary(model_congruency)


model_blocknr <- lmer(log_rt ~ group * transition_new * congruency * centered_blocknr * choice + 
                        (1 + transition_new + congruency + centered_blocknr | subjID) + (1 | stimulus), data = RTdata_masked)
summary(model_blocknr)


model_choice <- lmer(log_rt ~ group * transition_new * congruency * centered_blocknr * choice + 
                       (1 + transition_new + congruency + centered_blocknr + choice | subjID) + (1 | stimulus), data = RTdata_masked)
summary(model_choice)

model_choice2 <- lmer(log_rt ~ group * switch * congruency * centered_blocknr * choice + 
                       (1 + switch * (congruency + centered_blocknr + choice) | subjID) + (1 | stimulus), data = RTdata_masked)
summary(model_choice2)

model_hc1 <- lmer(log_rt ~ group * switch * (congruency + centered_blocknr + choice) + (1+ transition_new * (congruency + centered_blocknr + choice) | subjID) + (1 | stimulus), data = RTdata_masked,control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
summary(model_hc1)


# Compare AIC and BIC for each model
aic_basic <- AIC(model_basic)
bic_basic <- BIC(model_basic)

aic_transition <- AIC(model_transition)
bic_transition <- BIC(model_transition)

aic_congruency <- AIC(model_congruency)
bic_congruency <- BIC(model_congruency)

aic_blocknr <- AIC(model_blocknr)
bic_blocknr <- BIC(model_blocknr)

aic_choice <- AIC(model_choice)
bic_choice <- BIC(model_choice)
aic_choice2 <- AIC(model_choice2)
bic_choice2 <- BIC(model_choice2)
aic_choice2 <- AIC(model_full)
bic_choice2 <- BIC(model_full)

# Create a data frame to summarize the comparison
comparison <- data.frame(
  Model = c("Basic", "Transition", "Congruency", "Blocknr", "Choice","Choice2", "model full" ),
  AIC = c(aic_basic, aic_transition, aic_congruency, aic_blocknr, aic_choice, aic_choice2,model_full),
  BIC = c(bic_basic, bic_transition, bic_congruency, bic_blocknr, bic_choice, bic_choice2,model_full)
)
comparison



# scatterplot: observed vs. predicted 
plot(fitted(modelRT_full), RTdata_masked$log_rt, main = "Predicted vs. Observed", xlab = "Predicted", ylab = "Observed")
abline(0, 1, col = "red")
# e.g.: transition_new: F(1, 8107.8) = 71.9396, p < .0001


# plotting RT

RTplot_transition1 <- ggplot(RTdata_masked, aes(x = transition_new, y = rt, fill = group)) +
  geom_violin(trim = FALSE) +  # Expand the width of the violins
  geom_jitter(position = position_jitter(height = 0, width = 0.4), alpha = 0.25, size = 0.3) +  # Adjust jitter position and size
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "white") +  # Add median points
  labs(title = "Reaction Time by Transition Type", x = "Transition Type", y = "Log Reaction Time") +  # Label axes
  scale_fill_brewer(palette = "Set3") +  # Choose a color palette
  theme_minimal() +  # Customize theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

ggsave("RTtransition_plotgroup.png", RTplot_transition, width = 10, height = 6, units = "in", dpi = 300)


RTplot_transition2 <- ggplot(RTdata_masked, aes(x = transition_new, y = rt, fill = congruency)) +
  geom_violin(trim = FALSE) +  # Expand the width of the violins
  geom_jitter(position = position_jitter(height = 0, width = 0.4), alpha = 0.25, size = 0.3) +  # Adjust jitter position and size
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "white") +  # Add median points
  labs(title = "Reaction Time by Transition Type", x = "Transition Type", y = "Log Reaction Time") +  # Label axes
  scale_fill_brewer(palette = "Set3") +  # Choose a color palette
  theme_minimal() +  # Customize theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

ggsave("RTtransition_plotcongr.png", RTplot_transition, width = 10, height = 6, units = "in", dpi = 300)


# LESLIE MODEL PROPOSITION: 
# RT ~ Group * Sequence Choice * Congruency * Block + Task + (1+ Sequence * Congruency * Block + Task | Subject) + (1 + Task | Stimulus)
# DOES NOT WORK! --> conversions problem: how to fix?

# Reduced RT model: HOW? MAIN EFFECT OF ALL FIXED VARIABLES; HOW IMPLEMENT RANDOM SLOPES ON TOP?

      # modelRT_red <-lmer(log_rt ~ group * transition_new * congruency * centered_blocknr * choice + (1+ centered_blocknr * congruency * transition_new + choice  |subjID) + (1 + choice | stimulus) , data = RTdata_masked) 
      # summary(modelRT_red)
      # plot(modelRT_red)
      # qqnorm(resid(modelRT_red))
      # qqline(resid(modelRT_red))


# --- ACCURACY

# Logistic regression
# Accuracy ~ Group * Sequence Choice * Congruency * Block + Task + (1+ Sequence * Congruency * Block + Task | Subject) + (1 + Task | Stimulus)

# DOES NOT WORK (modelacc): CONVERSION: make model more simple!!!
modelacc <- glmer(correcttarresp ~ group * transition_new * congruency *centered_blocknr + choice + (1 + transition_new * congruency * centered_blocknr + choice| subjID) + (1 + choice | stimulus), data = ERdata_masked, family = binomial)
summary(modelacc)

# Full acc model - without random slopes
modelacc_full <- glmer(correcttarresp ~ group * transition_new * congruency *centered_blocknr + choice + (1 | subjID) + (1 | stimulus), data = ERdata_masked, family = binomial)
summary(modelacc_full)

plot(modelacc_full)
qqnorm(resid(modelacc_full))
qqline(resid(modelacc_full))

# Reduced acc model - without random slopes
modelacc_red <- glmer(correcttarresp ~ transition_new + centered_blocknr + choice + (1 | subjID) + (1 | stimulus), data = ERdata_masked, family = binomial)
summary(modelacc_red)

plot(modelacc_red)
qqnorm(resid(modelacc_red))
qqline(resid(modelacc_red))

# modelacc_redslope DOES NOT WORK: CONVERSION when wanting to add random slope: why?
modelacc_redslope <- glmer(correcttarresp ~ transition_new + centered_blocknr + choice + (1 + centered_blocknr | subjID) + (1 + choice | stimulus), data = ERdata_masked, family = binomial)
summary(modelacc_redslope)

      # plot(modelacc_redslope)
      # qqnorm(resid(modelacc_redslope))
      # qqline(resid(modelacc_redslope))
