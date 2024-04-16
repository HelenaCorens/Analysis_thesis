### MASTERPROEF: VOLUNTARY TASK SWITCHING w EXPLICIT/MASKED REWARD 
### written by Helena Corens

# To remove all previous data
rm(list=ls())
# If graphic error: dev.off() 

# Packages
library(plyr)
library(car) # univariate and multivariate tests
library(lme4) # linear mixed models (lmer & glmer models)
# library(sjPlot) #for plotting lmer and glmer mods
library(lattice) # boxplots
library(reshape2) # reshape data from wide to long or vice versa
library(effects) # plots
library(tidyr) # tidy data
library(tidyverse)
library(RColorBrewer)
library(lmerTest)
library(ggpubr)
library(cowplot) #for manuscript ready figures
library(sjmisc) 
library(effects)
library(sjstats)
library(afex)
library(dplyr)
library(ggplot2)

# Working directory 
getwd()
setwd("/Users/HC/MP_DATA/")
# Load data
data_all<- read.csv("datalog_complete.csv")


# --- PREPROCESSING --- #

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

# 3. Remove NULL answers; 'postnull' and 'first' answers (transition_new) (both for accuracy analysis as RT analysis)
# Replace empty values with '0' in the 'correcttarresp' variable
data_all <- subset(data_all, !(tarresp == "NULL"))
data_all <- subset(data_all, transition_new != "first" & transition_new != "postnull")
data_all$correcttarresp <- ifelse(data_all$correcttarresp == "", 0, data_all$correcttarresp)

# 4. Select variables to use
analysis <- select(data_all, all_of(c("subjID", "group", "rt", "category", "choice", "transition_new","tarresp", 
                                      "correcttarresp", "trialnr","blocknr", "phase", "reward", "congruency", 
                                      "payout1", "payout2", "payout3", "payout4", "totalpayout"  )))

# 5. Conversion data type 
analysis$group <- as.factor(analysis$group)
analysis$rt <- as.numeric(analysis$rt)
analysis$choice <- as.factor(analysis$choice)
analysis$transition_new <- as.factor(analysis$transition_new)
analysis$tarresp <- as.factor(analysis$tarresp)
analysis$correcttarresp  <- as.numeric(analysis$correcttarresp )
analysis$trialnr <- as.numeric(analysis$trialnr)
analysis$blocknr <- as.numeric(analysis$blocknr)
analysis$reward <- as.numeric(analysis$reward)
analysis$congruency <- as.factor(analysis$congruency)
analysis$payout1 <- as.numeric(analysis$payout1)
analysis$payout2 <- as.numeric(analysis$payout2)
analysis$payout3 <- as.numeric(analysis$payout3)
analysis$payout4 <- as.numeric(analysis$payout4)
analysis$totalpayout <- as.numeric(analysis$totalpayout)

# Zero-sum coding group
contrasts(analysis$group) <- contr.sum(levels(analysis$group))
contrasts(analysis$group)

# 6.REMOVE OUTLIERS
# RT outliers: less than 200ms - more than ?3000ms?
analysis <- subset(analysis, rt > 200)

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

table(analysis$rtoutlier) # 892 rt outliers; 17755 within IQR
sum(analysis$rtoutlier)
analysis <- subset(analysis,rtoutlier == 0)

# 6. RECODE & SUBSET: MASKED PHASE
# Masked phase
# Recode blocknr: 1,2,3,4; then centre it (helps with lme4 models)
data_masked <- subset(analysis, phase =="masked")

data_masked$blocknr <- recode(data_masked$blocknr, 
                                "1" = 1, #-1.5
                                "3" = 2, #-0.5
                                "5" = 3, # 0.5
                                "7" = 4) # 1.5
# Center recoded block numbers
data_masked$centered_blocknr <- scale(data_masked$blocknr, center = T, scale = F)
#unique(data_masked$blocknr)
#unique(data_masked$centered_blocknr) 


# 7. EYEBALL DATA
# Check distributions, violin plots, switch rates, switch rates over time just to get a feeling for the data

str(data_masked)
summary(data_masked)

# Histogram for reaction times (rt)
ggplot(data = data_masked, aes(x = rt)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Reaction Times",
       x = "Reaction Time",
       y = "Frequency") +
  theme_minimal()

# Density plot for reaction times (rt)
ggplot(data = data_masked, aes(x = rt)) +
  geom_density(fill = "skyblue") +
  labs(title = "Density Plot of Reaction Times",
       x = "Reaction Time",
       y = "Density") +
  theme_minimal()

# Boxplot for reaction times (rt) by group
ggplot(data = data_masked, aes(x = group, y = rt, fill = group)) +
  geom_boxplot() +
  labs(title = "Boxplot of Reaction Times by Group",
       x = "Group",
       y = "Reaction Time") +
  theme_minimal()

# violin plot
ggplot(data = data_masked, aes(x = group, y = rt)) +
  geom_violin(fill = "skyblue", color = "black") +
  labs(title = "Violin Plot of Reaction Times by Group",
       x = "Group",
       y = "Reaction Time") +
  theme_minimal()

# Switch rates
switch_rates <- aggregate(transition_new == "switch" ~ trialnr, data = data_masked, FUN = mean)

# Create a line plot of switch rates over trial number
ggplot(data = switch_rates, aes(x = trialnr, y = `transition_new == "switch"`)) +
  geom_line() +
  labs(title = "Switch Rates Over Trial Number",
       x = "Trial Number",
       y = "Switch Rate") +
  theme_minimal()


# ______________________________

# PREP FOR RT & ER ANALYSIS

# RT analysis: remove postnull; remove errors

RTdata_masked <- data_masked[data_masked$correcttarresp != 0, ]
# ER analysis: remove postnull; keep errors
# since we already did this above, we just use data_masked vor ER - rename for convenience

ERdata_masked <- data_masked


# ______________________________

# --- ANALYSIS --- #



