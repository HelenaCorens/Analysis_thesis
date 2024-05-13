### THESIS: ANALYSIS ONLY
### Helena Corens

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

# Working directory 
getwd()
setwd("/Users/HC/MP_DATA/")
# Load data
data_all <- read.csv("RTdata_masked.csv")


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

nDF <- with(RTdata_masked, expand.grid(centered_blocknr = seq(min(centered_blocknr), max(centered_blocknr), length.out = 15),
                                       group = levels(group)))

plot_data <- effectPlotData(fullVSR, nDF)

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

