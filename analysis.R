# VR Interaction Analysis

# Load required libraries
library(psych)
library(ggplot2)
library(stats)
library(reshape2)

# ----------------------------
# Data Preparation Section
# ----------------------------

# Read and prepare survey data with numeric conversion
survey_data <- read.csv("SurveyData.csv", header = FALSE)
names(survey_data) <- c("Gender", "Interaction", "PriorVR", paste0("Q", 1:29))

# Convert factors and ensure numeric questionnaire items
survey_data$Interaction <- as.factor(survey_data$Interaction)
survey_data$Gender <- as.factor(survey_data$Gender)
survey_data$PriorVR <- as.factor(survey_data$PriorVR)
survey_data[, 4:32] <- lapply(survey_data[, 4:32], function(x) as.numeric(as.character(x)))

# Read and prepare observation data with numeric conversion
observation_data <- read.csv("Observation.csv", header = FALSE)
names(observation_data) <- c("Gender", "Interaction", "Obs1", "Obs2", "Obs3", 
                             "Obs4", "Obs5", "Obs6", "Notes")

# Convert factors and ensure numeric observation items
observation_data$Interaction <- as.factor(observation_data$Interaction)
observation_data$Gender <- as.factor(observation_data$Gender)
observation_data[, 3:8] <- lapply(observation_data[, 3:8], function(x) as.numeric(as.character(x)))

# ----------------------------
# Utility Functions
# ----------------------------

safe_stats <- function(x) {
  if(all(is.na(x))) {
    return(c(Mean = NA, SD = NA, Min = NA, Max = NA))
  }
  c(Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE))
}

handle_empty_groups <- function(data, group_var, value_var) {
  groups <- split(data[[value_var]], data[[group_var]])
  lapply(groups, function(x) if(length(x) == 0) NA else x)
}

safe_test <- function(x, y = NULL, test_type = "t.test") {
  # Handle single-sample tests
  if(test_type == "shapiro") {
    if(all(is.na(x))) return(list(p.value = NA, statistic = NA))
       tryCatch(
         shapiro.test(x),
         error = function(e) list(p.value = NA, statistic = NA)
       )
  } else {
    # Handle two-sample tests
    if(all(is.na(x)) || all(is.na(y))) return(list(p.value = NA, statistic = NA))
    tryCatch({
      switch(test_type,
             "t.test" = t.test(x, y),
             "wilcox" = wilcox.test(x, y))
    }, error = function(e) list(p.value = NA, statistic = NA))
  }
}

# ----------------------------
# Presence Questionnaire Analysis
# ----------------------------

# Reliability analysis with reversed items
presence_items <- survey_data[, 4:32]
alpha_presence_check <- psych::alpha(presence_items, check.keys = TRUE)

# Reverse scoring
keys_vector <- unlist(alpha_presence_check$keys)
reversed_items <- keys_vector[keys_vector == -1]
reversed_item_names <- names(reversed_items)

presence_items_corrected <- presence_items
if(length(reversed_item_names) > 0) {
  for(item in reversed_item_names) {
    presence_items_corrected[[item]] <- 6 - presence_items[[item]]
  }
}

# Calculate total presence score
survey_data$TotalPresence <- rowSums(presence_items_corrected, na.rm = TRUE)

# Group analysis
presence_groups <- handle_empty_groups(survey_data, "Interaction", "TotalPresence")
hand_presence <- presence_groups$Hand
controller_presence <- presence_groups$Controller

# ----------------------------
# Performance Observation Analysis
# ----------------------------

# Initialize performance scores
observation_data$TotalPerformance <- rowSums(observation_data[, 3:8], na.rm = TRUE)

# Reverse scoring for observation items
alpha_observation_check <- psych::alpha(observation_data[, 3:8], check.keys = TRUE)
keys_obs_vector <- unlist(alpha_observation_check$keys)
reversed_obs_items <- keys_obs_vector[keys_obs_vector == -1]
reversed_obs_item_names <- names(reversed_obs_items)

if(length(reversed_obs_item_names) > 0) {
  for(item in reversed_obs_item_names) {
    max_value <- max(observation_data[[item]], na.rm = TRUE)
    observation_data[[item]] <- (max_value + 1) - observation_data[[item]]
  }
  observation_data$TotalPerformance <- rowSums(observation_data[, 3:8], na.rm = TRUE)
}

# Group analysis
performance_groups <- handle_empty_groups(observation_data, "Interaction", "TotalPerformance")
hand_performance <- performance_groups$Hand
controller_performance <- performance_groups$Controller

# ----------------------------
# Statistical Testing
# ----------------------------

# Presence analysis
presence_t_test <- safe_test(hand_presence, controller_presence)
presence_wilcox <- safe_test(hand_presence, controller_presence, "wilcox")
shapiro_hand_presence <- safe_test(hand_presence, test_type = "shapiro")
shapiro_controller_presence <- safe_test(controller_presence, test_type = "shapiro")

# Performance analysis
performance_t_test <- safe_test(hand_performance, controller_performance)
performance_wilcox <- safe_test(hand_performance, controller_performance, "wilcox")
shapiro_hand_performance <- safe_test(hand_performance, test_type = "shapiro")
shapiro_controller_performance <- safe_test(controller_performance, test_type = "shapiro")

# ----------------------------
# Visualization
# ----------------------------

create_boxplot <- function(data, y_var, title) {
  ggplot(data, aes(x = Interaction, y = !!sym(y_var), fill = Interaction)) +
    geom_boxplot() +
    labs(title = title) +
    theme_minimal() +
    scale_fill_manual(values = c("Hand" = "#66c2a5", "Controller" = "#fc8d62"))
}

presence_boxplot <- create_boxplot(survey_data, "TotalPresence", "Presence scores by interaction type")
performance_boxplot <- create_boxplot(observation_data, "TotalPerformance", "Performance scores by interaction type")

# ===============================
# Create Results File
# ===============================

# Capture all outputs to a file
sink("AnalysisResults.txt")

cat("================================================\n")
cat("         Analysis of VR interaction             \n")
cat("================================================\n\n")

cat("This analysis compares two VR interaction methods: Hand-based and Controller-based\n")
cat("interactions on user presence and performance in a virtual learning environment.\n\n")

cat("=============================================================\n")
cat("1. PRESENCE QUESTIONNAIRE ANALYSIS\n")
cat("=============================================================\n\n")

cat("1.1 Reliability Analysis\n")
cat("------------------------\n")
cat("Cronbach's Alpha for Presence Questionnaire:", round(alpha_presence_check$total$raw_alpha, 3), "\n")
if(alpha_presence_check$total$raw_alpha > 0.7) {
  cat("Interpretation: The presence questionnaire shows good internal consistency reliability.\n\n")
} else {
  cat("Interpretation: The presence questionnaire shows questionable internal consistency reliability.\n\n")
}

cat("1.2 Descriptive Statistics for Presence Scores\n")
cat("---------------------------------------------\n")
print(presence_descriptives)
cat("\n")

cat("1.3 Normality Testing\n")
cat("--------------------\n")
cat("Shapiro-Wilk Test for Hand interaction Presence Scores:\n")
cat("  W =", round(shapiro_hand_presence$statistic, 3), 
    ", p-value =", round(shapiro_hand_presence$p.value, 3), "\n")
if(shapiro_hand_presence$p.value > 0.05) {
  cat("  Interpretation: Hand presence scores are normally distributed.\n\n")
} else {
  cat("  Interpretation: Hand presence scores are not normally distributed.\n\n")
}

cat("Shapiro-Wilk Test for Controller interaction Presence Scores:\n")
cat("  W =", round(shapiro_controller_presence$statistic, 3), 
    ", p-value =", round(shapiro_controller_presence$p.value, 3), "\n")
if(shapiro_controller_presence$p.value > 0.05) {
  cat("  Interpretation: Controller presence scores are normally distributed.\n\n")
} else {
  cat("  Interpretation: Controller presence scores are not normally distributed.\n\n")
}

cat("1.4 Group Comparison Tests\n")
cat("-------------------------\n")
cat("Independent Samples t-test (Hand vs. Controller Presence Scores):\n")
cat("  t =", round(presence_t_test$statistic, 3),
    ", df =", round(presence_t_test$parameter, 1),
    ", p-value =", round(presence_t_test$p.value, 3), "\n")
cat("  95% Confidence Interval: [",
    round(presence_t_test$conf.int[1], 2), ", ",
    round(presence_t_test$conf.int[2], 2), "]\n")
cat("  Mean difference:", round(presence_t_test$estimate[2] - presence_t_test$estimate[1], 2), "\n")
if(presence_t_test$p.value < 0.05) {
  cat(" Interpretation: There is a significant difference in presence scores between Hand and Controller interactions.\n\n")
} else {
  cat(" Interpretation: There is no significant difference in presence scores between Hand and Controller interactions.\n\n")
}

cat("Mann-Whitney U Test (Hand vs. Controller Presence Scores):\n")
cat("  W =", round(presence_wilcox$statistic, 3),
    ", p-value =", round(presence_wilcox$p.value, 3), "\n")
if(presence_wilcox$p.value < 0.05) {
  cat(" Interpretation: There is a significant difference in presence scores between Hand and Controller interactions.\n\n")
} else {
  cat(" Interpretation: There is no significant difference in presence scores between Hand and Controller interactions.\n\n")
}


cat("=============================================\n")
cat("   2. STRUCTURED OBSERVATION FORM ANALYSIS   \n")
cat("=============================================\n\n")

cat("2.1 Reliability Analysis\n")
cat("------------------------\n")
cat("Cronbach's Alpha for Observation Form:", round(alpha_observation_check$total$raw_alpha, 3), "\n") 
if(alpha_observation_check$total$raw_alpha > 0.7) {
  cat("Interpretation: The observation form shows good internal consistency reliability.\n\n")
} else {
  cat("Interpretation: The observation form shows questionable internal consistency reliability.\n\n")
}

cat("2.2 Descriptive Statistics for Performance Scores\n")
cat("------------------------------------------------\n")
print(performance_descriptives)
cat("\n")

cat("2.3 Group Comparison Tests\n")
cat("-------------------------\n")
cat("Independent Samples t-test (Hand vs. Controller Performance Scores):\n")
cat("  t =", round(performance_t_test$statistic, 3), 
    ", df =", round(performance_t_test$parameter, 1),
    ", p-value =", round(performance_t_test$p.value, 3), "\n")
cat("  95% Confidence Interval: [", 
    round(performance_t_test$conf.int[1], 2), ", ", 
    round(performance_t_test$conf.int[2], 2), "]\n")
cat("  Mean difference:", round(performance_t_test$estimate[2] - performance_t_test$estimate[1], 2), "\n")
if(performance_t_test$p.value < 0.05) {
  cat("  Interpretation: There is a significant difference in performance scores between Hand and Controller interactions.\n\n")
} else {
  cat("  Interpretation: There is no significant difference in performance scores between Hand and Controller interactions.\n\n")
}
cat("Mann-Whitney U Test (Hand vs. Controller Performance Scores):\n")
cat("  W =", round(performance_wilcox$statistic, 3), 
    ", p-value =", round(performance_wilcox$p.value, 3), "\n")
if(performance_wilcox$p.value < 0.05) {
  cat("  Interpretation: There is a significant difference in performance scores between Hand and Controller interactions.\n\n")
} else {
  cat("  Interpretation: There is no significant difference in performance scores between Hand and Controller interactions.\n\n")
}

cat("=====================================\n")
cat("     3. SUPPLEMENTARY ANALYSES       \n")
cat("=====================================\n\n")

cat("3.1 Effects of Gender\n")
cat("--------------------\n")
gender_presence_test <- t.test(TotalPresence ~ Gender, data = survey_data)
cat("Independent Samples t-test (Gender Effect on Presence Scores):\n")
cat("  t =", round(gender_presence_test$statistic, 3), 
    ", df =", round(gender_presence_test$parameter, 1),
    ", p-value =", round(gender_presence_test$p.value, 3), "\n")
if(gender_presence_test$p.value < 0.05) {
  cat("  Interpretation: There is a significant effect of gender on presence scores.\n\n")
} else {
  cat("  Interpretation: There is no significant effect of gender on presence scores.\n\n")
}

cat("3.2 Effects of Prior VR Experience\n")
cat("--------------------------------\n")
vr_experience_presence_test <- t.test(TotalPresence ~ PriorVR, data = survey_data)
cat("Independent Samples t-test (Prior VR Experience Effect on Presence Scores):\n")
cat("  t =", round(vr_experience_presence_test$statistic, 3), 
    ", df =", round(vr_experience_presence_test$parameter, 1),
    ", p-value =", round(vr_experience_presence_test$p.value, 3), "\n")
if(vr_experience_presence_test$p.value < 0.05) {
  cat("  Interpretation: There is a significant effect of prior VR experience on presence scores.\n\n")
} else {
  cat("  Interpretation: There is no significant effect of prior VR experience on presence scores.\n\n")
}

cat("3.3 Interaction Effects (interaction × Gender)\n")
cat("-----------------------------------------\n")
interaction_gender_model <- aov(TotalPresence ~ Interaction * Gender, data = survey_data)
interaction_gender_summary <- summary(interaction_gender_model)
performance_gender_model <- aov(TotalPerformance ~ Interaction * Gender, data = observation_data)
performance_gender_summary <- summary(performance_gender_model) 

cat("Two-way ANOVA for Presence Scores:\n")
print(interaction_gender_summary)
cat("\n")

cat("Two-way ANOVA for Performance Scores:\n")
print(performance_gender_summary)
cat("\n")

cat("=============================================================\n")
cat("4. SUMMARY AND CONCLUSIONS\n")
cat("=============================================================\n\n")

# Presence scores comparison
cat("Presence Questionnaire:\n")
if(presence_t_test$p.value < 0.05) {
  if(mean(controller_presence) > mean(hand_presence)) {
    cat("- Controller-based interaction yielded significantly higher presence scores\n")
    cat("  (mean difference =", round(mean(controller_presence) - mean(hand_presence), 2), ")\n")
  } else {
    cat("- Hand-based interaction yielded significantly higher presence scores\n")
    cat("  (mean difference =", round(mean(hand_presence) - mean(controller_presence), 2), ")\n")
  }
} else {
  cat("- No significant difference was found in presence scores between interactions\n")
}

# Performance scores comparison
cat("\nPerformance Observation:\n")
if(performance_t_test$p.value < 0.05) {
  if(mean(controller_performance) > mean(hand_performance)) {
    cat("- Controller-based interaction yielded significantly higher performance scores\n")
    cat("  (mean difference =", round(mean(controller_performance) - mean(hand_performance), 2), ")\n")
  } else {
    cat("- Hand-based interaction yielded significantly higher performance scores\n")
    cat("  (mean difference =", round(mean(hand_performance) - mean(controller_performance), 2), ")\n")
  }
} else {
  cat("- No significant difference was found in performance scores between interactions\n")
}

# Gender effects
cat("\nGender Effects:\n")
if(gender_presence_test$p.value < 0.05) {
  cat("- Gender had a significant effect on presence scores\n")
} else {
  cat("- Gender did not significantly affect presence scores\n")
}

# Gender effects
cat("\nGender Effects:\n")
if(gender_presence_test$p.value < 0.05) {
  cat("- Gender had a significant effect on presence scores.\n")
} else {
  cat("- Gender did not significantly affect presence scores.\n")
}

# Gender effect on performance
cat("Independent Samples t-test (Gender Effect on Performance Scores):\n")
gender_performance_test <- t.test(TotalPerformance ~ Gender, data = observation_data) # Calculate t-test for performance
cat("  t =", round(gender_performance_test$statistic, 3), 
    ", df =", round(gender_performance_test$parameter, 1),
    ", p-value =", round(gender_performance_test$p.value, 3), "\n")
if(gender_performance_test$p.value < 0.05) {
  cat("  Interpretation: There is a significant effect of gender on performance scores.\n\n")
  # Add interpretation for the direction of the effect
  female_mean <- mean(observation_data$TotalPerformance[observation_data$Gender == "F"], na.rm = TRUE)
  male_mean <- mean(observation_data$TotalPerformance[observation_data$Gender == "M"], na.rm = TRUE)
  
  if(!is.na(female_mean) && !is.na(male_mean) && female_mean > male_mean) {
    cat("  Females had significantly higher performance scores than males.\n\n")
  } else if(!is.na(female_mean) && !is.na(male_mean) && male_mean > female_mean) {
    cat("  Males had significantly higher performance scores than females.\n\n")
  }
}
  
# Prior VR experience effects
cat("\nPrior VR Experience Effects:\n")
if(vr_experience_presence_test$p.value < 0.05) {
  cat("- Prior VR experience had a significant effect on presence scores\n")
} else {
  cat("- Prior VR experience did not significantly affect presence scores\n")
}

# Interaction effects
cat("\nInteraction Effects:\n")

# For presence scores
presence_interaction_p <- tryCatch({
  interaction_gender_summary[[1]]["Interaction:Gender", "Pr(>F)"]
}, error = function(e) NA)

if(!is.na(presence_interaction_p)) {
  if(presence_interaction_p < 0.05) {
    cat("- Significant interaction between interaction type and gender on presence scores (p =", 
        round(presence_interaction_p, 3), ")\n")
  } else {
    cat("- No significant interaction between interaction type and gender on presence scores (p =", 
        round(presence_interaction_p, 3), ")\n")
  }
} else {
  cat("- Interaction effect analysis unavailable for presence scores (insufficient data)\n")
}

# For performance scores
performance_interaction_p <- tryCatch({
  performance_gender_summary[[1]]["Interaction:Gender", "Pr(>F)"]
}, error = function(e) NA)

if(!is.na(performance_interaction_p)) {
  if(performance_interaction_p < 0.05) {
    cat("- Significant interaction between interaction type and gender on performance scores (p =", 
        round(performance_interaction_p, 3), ")\n")
  } else {
    cat("- No significant interaction between interaction type and gender on performance scores (p =", 
        round(performance_interaction_p, 3), ")\n")
  }
} else {
  cat("- Interaction effect analysis unavailable for performance scores (insufficient data)\n")
}

cat("\n=============================================================\n")
cat("END OF REPORT\n")
cat("=============================================================\n")

# Save plots
ggsave("presence_boxplot.pdf", presence_boxplot, width = 8, height = 6, device=pdf)
ggsave("performance_boxplot.pdf", performance_boxplot, width = 8, height = 6, device=pdf)

cat("Analysis complete. Output files:\n",
    "- AnalysisResults.txt\n",
    "- presence_boxplot.pdf\n",
    "- performance_boxplot.pdf\n")
sink()
print("checkpoint 1")