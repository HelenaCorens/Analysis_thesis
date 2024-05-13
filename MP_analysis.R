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
library(DHARMa)
# library(lmerTest)
#install.packages("flexplot")


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

# 3. Remove NULL answers; 'postnull' and 'first' answers (transition_new) (both for accuracy analysis as RT analysis)
# Replace empty values with '0' in the 'correcttarresp' variable
data_all <- subset(data_all, !(tarresp == "NULL"))
data_all <- subset(data_all, transition_new != "first" & transition_new != "postnull")
data_all$correcttarresp <- ifelse(data_all$correcttarresp == "", 0, data_all$correcttarresp)
# 'choice' still holds 3 Null responses (don't know where they're coming from) but remove
data_all <- subset(data_all, !(choice == "NULL"))

# Remove all post-error trials
# 1) Identify indices of incorrect trials
incorrect_indices <- which(data_all$correcttarresp == 0)
# 2) Determine indices of subsequent trials to be removed
trials_to_remove <- c(incorrect_indices, incorrect_indices + 1)
# 3) Subset the dataset to exclude trials following errors
data_all <- data_all[-trials_to_remove, ]

#data_all <- subset(data_all, !cumsum(correcttarresp == 0)) # NACHECKEN!!!!!!!!


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

# Zero-sum coding group
contrasts(analysis$group) <- contr.sum(levels(analysis$group))
contrasts(analysis$group)
# Reverse the contrasts for the group variable (so that condswitch is reference group for model interpretation)
### contrasts(analysis$group) <- contr.sum(levels(analysis$group))[2:1, ]

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

# 6. RECODE & SUBSET: MASKED PHASE

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
plotVSR <- ggplot(data = VSR_trialnr, aes(x = trialnr, y = `transition_new == "switch"`, group = group, color = group)) +
  geom_smooth() +
  labs(title = "Switch Rates Over Trial Number by Block & Group",
       x = "Trial Number",
       y = "Switch Rate") +
  scale_x_continuous(breaks = VSR_trialnr$trialnr) +  # Set breaks explicitly
  theme_minimal() +
  facet_wrap(~ blocknr, nrow = 2)

ggsave("VSR_plot.png", plotVSR, width = 10, height = 6, units = "in", dpi = 300)


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
plottransition <- ggplot(data = summary_data, aes(x = group, y = percentage, fill = transition_new)) +
  geom_bar(position = "stack", stat = "identity") +
  geom_text(aes(label = paste0(sprintf("%.1f", percentage), "%")), position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Percentage of Switch and Repeat Events by Group and Phase",
       x = "Group",
       y = "Percentage") +
  scale_fill_manual(values = c("switch" = "dodgerblue2", "repetition" = "lightskyblue1"), name = "Transition Type") +
  facet_wrap(~ phase) +
  theme_minimal()

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

#  not good enough, still have to figure out how to visualise this: PROBLEM FOR LATER

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


# --- PREP FOR ANALYSIS

# RT analysis: remove postnull; remove errors
RTdata_masked <- data_masked[data_masked$correcttarresp != 0, ]

# ER analysis: remove postnull; keep errors
# since we already did this above, we just use data_masked vor ER - rename for convenience
ERdata_masked <- data_masked


# ----------------------- ANALYSIS ----------------------- #

options(contrasts = c("contr.sum", "contr.poly"))

# --------- VSR instead of 'transition_new'

switch_rate <- aggregate(transition_new ~ subjID, data = RTdata_masked, function(x) mean(x == "switch"))
# Merge switch rates with the original dataset based on subjID
RTdata_masked <- merge(RTdata_masked, switch_rate, by = "subjID", suffixes = c("", "_switch_rate"))
# Rename the new column
colnames(RTdata_masked)[ncol(RTdata_masked)] <- "switch_rate"
# Print or inspect the updated dataset
head(RTdata_masked)

bwplot(switch_rate ~ group, data = RTdata_masked)
hist(RTdata_masked$centered_switchrate, breaks = 20, main = "Histogram of centered switchrate")

# RTdata_masked$centered_switchrate <- scale(RTdata_masked$switch_rate, center = T, scale = F)
# Scaling doesn't help with the converisons

# ----------------------------------------

# M1 (random intercept for each subject)

modelVSR1 <- glmer(transition_new ~ group + (1 | subjID), data = RTdata_masked,  family = binomial(link = "logit"))
summary(modelVSR1)
plot(modelVSR1)
summ(modelVSR1, exp = T) # Being in condrepetition group lowers the odds of VSR by (1 – 0.23)% = 77%, in comparison to being in the condswitch group, assuming everything else stays constant.
plot(allEffects(modelVSR1, fixed.predictors = list(given.values=c(group1=0))))

# dispersion_glmer(modelVSR1) # 0.6609282 --> underdispersion: problem?

# M2 + centered_blocknr
modelVSR2 <- glmer(transition_new ~ group + centered_blocknr + (1 | subjID), 
                   data = RTdata_masked, 
                   family = binomial(link = "logit"))
summary(modelVSR2)
plot(modelVSR2)
summ(modelVSR2, exp = T)
plot(allEffects(modelVSR2)) # probabilities are very low

### binomial data and residual plot don't match well

# dispersion_glmer(modelVSR2) # 0.6548242 --> underdispersion: problem?

# M3

modelVSR3 <- glmer(transition_new ~ group *centered_blocknr + (1 | subjID), data = RTdata_masked, family = binomial(link = "logit"))
summary(modelVSR3)
plot(modelVSR3)
summ(modelVSR3, exp = T)
plot(allEffects(modelVSR3))

cor_matrix <- cor(RTdata_masked[, c("switch_rate")])
eigen_values <- eigen(cor_matrix)$values
  
  # dispersion_glmer(modelVSR3) # 0.6504212 --> underdispersion: problem?


# M4
modelVSR4 <- glmer(transition_new == "switch" ~ group *centered_blocknr + (1 + centered_blocknr | subjID), data = RTdata_masked, family = binomial(link = "logit"))
summary(modelVSR4)
plot(modelVSR4)
summ(modelVSR4, exp = T)
plot(allEffects(modelVSR4))

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

fullVSR <- glmer(transition_new ~ group *centered_blocknr*choice + (1 + centered_blocknr| subjID), data = RTdata_masked, family = binomial(link = "logit"), control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))
summary(fullVSR)

# INTERPRETATION
# INTERCEPT: average of VSR, when group = repetition, blocknr = 1 and choice == animacy, (baseline levels)

# COMPARE MODELS: 
anova(modelVSR1, modelVSR2, test = "Chisq")
anova(modelVSR3, modelVSR4, test = "Chisq")
anova(modelVSR1, modelVSR4, test = "Chisq")
anova(fullVSR, modelVSR4, test = "Chisq")
anova(fullVSR, modelVSR1, modelVSR2, modelVSR3, modelVSR4, test = "Chisq")
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

# Basic intercept model RT
interceptRT <- lmer(log_rt ~ 1 + (1|subjID), data = RTdata_masked)
summary(interceptRT)
performance::icc(interceptRT)
plot(interceptRT, subjID ~ resid(.), abline = 0 )
plot(interceptRT)

# Full RT model
modelRT_full1 <-lmer(log_rt ~ group * transition_new * congruency * centered_blocknr * choice + (1|subjID), data = RTdata_masked) 
summary(modelRT_full1)

modelRT_full2 <-lmer(log_rt ~ group * transition_new * congruency * centered_blocknr * choice + (1|subjID) + (1 + choice | stimulus) , data = RTdata_masked) 
summary(modelRT_full2)


plot(modelRT_full2)
qqnorm(resid(modelRT_full2))
qqline(resid(modelRT_full2))

anova(modelRT_full1, modelRT_full2, test = "Chisq")

######### LESLIE PROPOSITION FOR MAKING IT MORE SIMPLE
modelRT_leslie <- lmer(log_rt ~ group * transition_new * (congruency + centered_blocknr + choice) + (1+ transition_new * (congruency + centered_blocknr + choice) | subjID) + (1 + choice | stimulus), data = RTdata_masked)
summary(modelRT_leslie)

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

