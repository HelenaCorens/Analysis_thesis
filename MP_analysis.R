### MASTERPROEF: VOLUNTARY TASK SWITCHING w EXPLICIT/MASKED REWARD 
### written by Helena Corens

### ensure to document your preprocessing steps thoroughly to maintain transparency and reproducibility in your analysis

# To remove all previous data
rm(list=ls())
# If graphic error: dev.off() 

# Packages
library(tidyr) # tidy data
library(ggplot2)
library(lattice)
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
# library(lmerTest)
# install.packages("flexplot")

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

# 1. Exclude rejected subjectIDs: Prolific preselection of participants (see file 'PP_preselection.R')
excluded_pp <- c("oqe3fa7novfjq7anh5nlnkpynw3obvuq", "yrh96kvpm40jb4hols0d5rpm7fuqksqo", "1z8mdxevdznh8ejg6azbw2ynevccwydp",
                 "e2yuqxbgrx72co6ab6jkks82s7xgtb6h", "seg7h7julqwa3qwm0oh4yf7r0rno8jyc", "zkoxr13g3vlvbzr9vk97b9tsegrrl41j",
                 "n3hkdjov6g8ehptm0cxrof3ak0jfb1px")
data_all <- data_all[!data_all$subjID %in% excluded_pp, ]
print(length(unique(data_all$subjID)))

    # Subjects w accuracy < 60% are already filtered out -> see file 'PP_preselection.R'


# 2. Exclude practice & bisbas ('training'/'questionnaire') block
# If we want to investigate questionnaire at the end: include questionnaire phase again
data_all <- subset(data_all, phase!="training" & phase!= "questionnaire")


# 3. Remove NULL answers; 'postnull' and 'first' answers (transition_new) (both for accuracy analysis as RT analysis)
# Replace empty values with '0' in the 'correcttarresp' variable
data_all <- subset(data_all, !(tarresp == "NULL"))
data_all <- subset(data_all, transition_new != "first" & transition_new != "postnull")
data_all$correcttarresp <- ifelse(data_all$correcttarresp == "", 0, data_all$correcttarresp)
# 'choice' still holds 3 Null responses (don't know where they're coming from) but remove
data_all <- subset(data_all, !(choice == "NULL"))


# 4. Select variables to use
analysis <- data_all %>% select(subjID, group, rt, category, choice, transition_new, tarresp, correcttarresp, trialnr, 
                                 blocknr, phase, reward, congruency, payout1, payout2, payout3, payout4, totalpayout, 
                                 stimulus)

# analysis <- select(data_all, all_of(c("subjID", "group", "rt", "category", "choice", "transition_new","tarresp", 
 #                                     "correcttarresp", "trialnr","blocknr", "phase", "reward", "congruency", 
 #                                     "payout1", "payout2", "payout3", "payout4", "totalpayout", "stimulus"  )))


# 5. Conversion data type 
analysis$group <- as.factor(analysis$group)
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

# Zero-sum coding group
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

table(analysis$rtoutlier) # 892 rt outliers; 17755 within IQR
length(analysis$rtoutlier)
analysis <- subset(analysis,rtoutlier == 0) #lenght(analysis$rt): 17750


# 7. RECODE & SUBSET: MASKED PHASE

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

# Log-transform rt (skewness is compared down below @eyeball data -> RT PLOTS)
data_masked$log_rt <- log(data_masked$rt)
# Check distribution of log-transformed rt's
hist(data_masked$log_rt, breaks = 20, main = "Histogram of Log-Transformed RTs")
# watch out for conversions


# ------------ EYEBALL DATA ------------ #
# Check distributions, violin plots, switch rates, switch rates over time


# --- VSR PLOTS

# Switch rates 
VSR_trialnr <- aggregate(transition_new == "switch" ~ blocknr + trialnr + group, data = data_masked, FUN = mean)
# Switch rates over trial number for each group, faceted by blocknr
ggplot(data = VSR_trialnr, aes(x = trialnr, y = `transition_new == "switch"`, group = group, color = group)) +
  geom_smooth() +
  labs(title = "Switch Rates Over Trial Number by Block & Group",
       x = "Trial Number",
       y = "Switch Rate") +
  scale_x_continuous(breaks = VSR_trialnr$trialnr) +  # Set breaks explicitly
  theme_minimal() +
  facet_wrap(~ blocknr, nrow = 2) 

# Bar plot of switch and repeat events per group - DATASET: 'data_masked'
ggplot(data = data_masked, aes(x = group, fill = transition_new)) +
  geom_bar(position = "stack") +
  labs(title = "Frequency of Switch and Repeat Events by Group",
       x = "Group",
       y = "Frequency") +
  scale_fill_manual(values = c("switch" = "skyblue", "repetition" = "orange"), 
                    name = "Transition Type") +
  theme_minimal()

# Summarize the data by group and phase, and calculate counts and percentages
summary_data <- analysis %>%
  group_by(group, phase, transition_new) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
# Bar plot with the percentage of transition types by group and phase/ DATASET: 'analysis'
ggplot(data = summary_data, aes(x = group, y = percentage, fill = transition_new)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(label = paste0(sprintf("%.1f", percentage), "%")), position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Percentage of Switch and Repeat Events by Group and Phase",
       x = "Group",
       y = "Percentage") +
  scale_fill_manual(values = c("switch" = "skyblue", "repetition" = "orange"), name = "Transition Type") +
  facet_wrap(~ phase) +
  theme_minimal()


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
bwplot(log_rt ~ transition_new, data = data_masked)

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


# --- REWARD PLOTS

bwplot(totalpayout ~ group, data = data_masked)

# Boxplot reward amount per group 
ggplot(data = data_masked, aes(x = group, y = totalpayout, fill = transition_new)) +
  geom_boxplot() +
  labs(title = "Boxplot of Total reward by Group",
       x = "Group",
       y = "Reward amount") +
  theme_minimal()


# --- PREP FOR ANALYSIS

# RT analysis: remove postnull; remove errors
RTdata_masked <- data_masked[data_masked$correcttarresp != 0, ]

# ER analysis: remove postnull; keep errors
# since we already did this above, we just use data_masked vor ER - rename for convenience
ERdata_masked <- data_masked


# ----------------------- ANALYSIS ----------------------- #

# options(contrasts = c("contr.sum", "contr.poly"))

# --------- VSR

# M1 (random intercept for each subject)
modelVSR1 <- glmer(transition_new == "switch" ~ group + (1 | subjID), data = RTdata_masked, family = binomial)
summary(modelVSR1)
plot(modelVSR1)

qqnorm(resid(modelVSR1))
qqline(resid(modelVSR1))

# Create a density plot of the residuals
plot(density(resid(modelVSR1)), main = "Density Plot of Residuals")

ggplot(data = data.frame(residuals = residuals(modelVSR1), fitted = fitted(modelVSR1)),
       aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals")

dispersion_glmer(modelVSR1) # 0.6609282 --> underdispersion: problem?

# M2 + centered_blocknr
modelVSR2 <- glmer(transition_new == "switch" ~ group + centered_blocknr + (1 | subjID), data = RTdata_masked, family = binomial)
summary(modelVSR2)
plot(modelVSR2)
qqnorm(resid(modelVSR2))
qqline(resid(modelVSR2))

# Create a density plot of the residuals
plot(density(resid(modelVSR2)), main = "Density Plot of Residuals")

ggplot(data = data.frame(residuals = residuals(modelVSR2), fitted = fitted(modelVSR2)),
       aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals")

dispersion_glmer(modelVSR2) # 0.6548242 --> underdispersion: problem?

# M3
modelVSR3 <- glmer(transition_new == "switch" ~ group + centered_blocknr + group*centered_blocknr + (1 | subjID), data = RTdata_masked, family = binomial)
summary(modelVSR3)
plot(modelVSR3)

ggplot(data = data.frame(residuals = residuals(modelVSR3), fitted = fitted(modelVSR3)),
       aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Residuals vs. Fitted Values",
       x = "Fitted Values",
       y = "Residuals")

qqnorm(resid(modelVSR3))
qqline(resid(modelVSR3))

# Create a density plot of the residuals
plot(density(resid(modelVSR3)), main = "Density Plot of Residuals")

dispersion_glmer(modelVSR3) # 0.6504212 --> underdispersion: problem?

# COMPARE MODELS: 
anova(modelVSR1, modelVSR2, modelVSR3, test = "Chisq")
# modelVSR2 & modelVSR3 significant


# --------- TASK PERFORMANCE

# --- RT
# heteroscedasticity concerns with lmer / watch out for conversions

# Basic intercept model RT
interceptRT <- lmer(log_rt ~ 1 + (1|subjID), data = RTdata_masked)
summary(interceptRT)
performance::icc(interceptRT)
plot(interceptRT, subjID ~ resid(.), abline = 0 )

# Full RT model
modelRT_full <-lmer(log_rt ~ group * transition_new * congruency * centered_blocknr * choice + (1|subjID) + (1 + choice | stimulus) , data = RTdata_masked) 
summary(modelRT_full)

plot(modelRT_full)
qqnorm(resid(modelRT_full))
qqline(resid(modelRT_full))

# scatterplot: observed vs. predicted 
plot(fitted(modelRT_full), RTdata_masked$log_rt, main = "Predicted vs. Observed", xlab = "Predicted", ylab = "Observed")
abline(0, 1, col = "red")
# e.g.: transition_new: F(1, 8107.8) = 71.9396, p < .0001

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


# --- Other things to look at: 
      # General descriptives
      # Difference VSR masked / explicit phase
      
