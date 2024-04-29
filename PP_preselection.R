### MASTERPROEF: VOLUNTARY TASK SWITCHING w EXPLICIT/MASKED REWARD 
### written by Helena Corens

# PROLIFIC PARTICIPANTS SELECTION / REJECTION
# My method was not the most efficient; I did check every suspicious and non suspicious participant thoroughly
# starting from these codes, I switched back and forth to my csv file in excel to check suspicious participants
# e.g. I excluded a participant that did a good job overall, but when looking into each blocks specifically, their effort/accuracy
# was lower in the last blocks. I then went to the csv file to check what was going on in that last block. It was very visible that 
# they did not answer for a few trials in a row in the last block, which differentiated a lot compared to their previous blocks)
# The exclusion of each participant is written in more detail in my paper. 

# I also made a separate excel file (see it as a little summary) to keep track of who I already checked and what to decide
# let me know if you want this too

# To remove all previous data
rm(list=ls())
# If graphic error: dev.off() 

# Working directory 
getwd()
setwd("/Users/HC/MP_DATA/")
# Load data
data_all<- read.csv("datalog_complete.csv")

#####################
# PARTICIPANT CHECK #
#####################

# Exclude practice & bisbas ('training'/'questionnaire') block 
data_all= subset(data_all, phase!="training")
data_all= subset(data_all, phase!="questionnaire")

# List of subject IDs to be removed (which I noticed during the process of removing these one by one)
excluded_pp <- c("oqe3fa7novfjq7anh5nlnkpynw3obvuq", "yrh96kvpm40jb4hols0d5rpm7fuqksqo", "1z8mdxevdznh8ejg6azbw2ynevccwydp",
                 "e2yuqxbgrx72co6ab6jkks82s7xgtb6h", "seg7h7julqwa3qwm0oh4yf7r0rno8jyc", "zkoxr13g3vlvbzr9vk97b9tsegrrl41j",
                 "n3hkdjov6g8ehptm0cxrof3ak0jfb1px")

# Subset the data frame to exclude the specified participants / data_clean should contain data without specified pps
data_clean <- data_all[!data_all$subjID %in% excluded_pp, ]

# ---------- CHECK PARTICIPANTS: APPROVED OR EXCLUDE? 
# 1. Overall accuracy rates:
  # Replace empty values with '0' in the 'correcttarresp' variable
data_clean$correcttarresp <- ifelse(data_clean$correcttarresp == "", 0, data_clean$correcttarresp)

  # Convert 'correcttarresp' to numeric
data_clean$correcttarresp <- as.numeric(data_clean$correcttarresp)

  # Check if there are any NA values in the 'correcttarresp' column
any(is.na(data_clean$correcttarresp))

accuracy_rates <- aggregate(correcttarresp ~ subjID + prolificID, data = data_clean, FUN = function(x) mean(x, na.rm = TRUE) * 100)
  # Rename the column for clarity (optional)
  # names(accuracy_rates)[2] <- "accuracy_rate_percent"
  # Print the resulting accuracy rates per participant
print(accuracy_rates)

# Get unique subjID and their corresponding totalpayout values
unique_rewards <- unique(data_clean[c("prolificID", "totalpayout", "group")])

# Sort the data frame by totalpayout in descending order
unique_rewards <- unique_rewards[order(-unique_rewards$totalpayout), ]

# Print the resulting data frame with subjID and total reward amount
print(unique_rewards)

# 1.2 Accuracy rates per task:
  # Remove rows with NULL values in the 'choice' variable
data_clean <- data_clean[!is.na(data_clean$choice), ]

# 1.3 Acurracy per block
  # accuracy_by_block <- aggregate(correcttarresp ~ prolificID + blocknr, data = data_clean, FUN = function(x) mean(x == 1, na.rm = TRUE) * 100)
  # print(accuracy_by_block)

  # Accuracy per task choice for each participant
accuracy_by_choice <- aggregate(correcttarresp ~ subjID + choice, data = data_clean, FUN = function(x) mean(x == 1, na.rm = TRUE) * 100)
print(accuracy_by_choice)

# 1.4 Choices per person
  # Count occurrences of 'choice' per participant and per block
choice_counts <- table(data_clean$subjID, data_clean$choice, useNA = "always")
print(choice_counts)

# 2. RT PER BLOCK
  # Calculate summary statistics of reaction times per block for each participant
rt_summary <- aggregate(rt ~ subjID + blocknr, data = data_clean, FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE), below_200 = sum(x < 200, na.rm = TRUE)))
  # Rename the columns for clarity
names(rt_summary)[3:7] <- c("mean_rt", "sd_rt", "min_rt", "max_rt", "below_200_count")

print(rt_summary)

# 3. Fullscreen out?? 1 = on / 0 = away from fullscreen
  # Create a contingency table of fullscreen values per participant
fullscreen_counts <- table(data_clean$subjID, data_clean$fullscreen)
print(fullscreen_counts)

# 4.CHECK SPECIFIC PP 
  # Specify participant ID
participant_id_to_check <- "63e5eb956eab1f2740ac6289"

  # Subset the data frame to include only the rows corresponding to the specified participant
participant_data <- data_clean[data_clean$prolificID == participant_id_to_check, ]

  # Calculate accuracy rates per block
accuracy_by_block <- aggregate(correcttarresp ~ blocknr, data = participant_data, FUN = function(x) mean(x, na.rm = TRUE) * 100)
print(accuracy_by_block)

