
# =====================================================================
#  REPLICATION SCRIPT FOR: Thesis on Media Framing of Failed Resistance
#  Author: Marvin Katsole
#  Description: This script reproduces all key steps from the thesis.
#  Includes preprocessing, sampling, prompt use, API calls, regression,
#  validation, and visualisation.
# =====================================================================


# =====================================================================
# PROMPTS USED FOR FRAMING EVALUATION (AS DESCRIBED IN THE THESIS)
# =====================================================================

# Prompt 1 (Zero-shot, without definition)
# "Your task is to evaluate how the following article frames the 2024 Gen Z-led protests against the Finance Bill in Kenya."
# "Provide both:"
# "- A categorical framing classification: Positive, Neutral, or Negative."
# "- A continuous framing score ranging from -1 (most negative) to +1 (most positive). with 0 = neutral."

# Prompt 2 (Zero-shot, with basic definition)
# "Your task is to evaluate how the following article frames the 2024 Gen Z-led protests against the Finance Bill in Kenya."
# "Framing refers to how specific aspects of an event are highlighted to promote a particular interpretation, evaluation, or meaning."
# "Provide both:"
# "- A categorical framing classification: Positive, Neutral, or Negative."
# "- A continuous framing score ranging from -1 (most negative) to +1 (most positive). with 0 = neutral."

# Prompt 3 (Zero-shot, with detailed framing definition)
# "Your task is to evaluate how the following article frames the 2024 Gen Z-led protests against the Finance Bill in Kenya."
# [Detailed framing guidelines and framing logic here...]

# Prompt 4 (Few-shot with examples, simplified)
# [Includes step-by-step reasoning and 3 example analyses]

# Prompt 5 (Final few-shot prompt used in thesis)
# [Includes detailed instruction + 6 annotated examples used for GPT-4 scoring]



library(httr)
library(stringr)
library(readr)
library(progress)
library(jsonlite)

# 🔐 Step 1: Set your OpenAI API key
api_key <- "(insert your api key here)"

# 🔁 Step 2: Define the GPT call function
send_to_gpt <- function(text, api_key, model = "gpt-4o-mini") {
  url <- "https://api.openai.com/v1/chat/completions"
  
  response <- POST(
    url = url,
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = model,
      messages = list(list(role = "user", content = text))
    )
  )
  
  parsed_response <- content(response, as = "parsed")
  
  if (!is.null(parsed_response$choices) && length(parsed_response$choices) > 0) {
    return(parsed_response$choices[[1]]$message$content)
  } else {
    print("Error: No valid response from API.")
    return(NULL)
  }
}

#analyse prompt
analyze_article_framing <- function(article_text) {
  prompt <- paste(
    "You are a political discourse analyst.",
    "Your task is to evaluate how the following article frames the 2024 Gen Z-led protests against the Finance Bill in Kenya.",
    "Framing refers to how specific aspects of an event are highlighted to promote a particular interpretation, evaluation, or meaning.",
    "You are NOT evaluating general political tone or emotional content.",
    "You are ONLY evaluating how the resistance movement is framed.",
    "",
    "Use this framing logic:",
    "- Positive framing (+0.3 to +1): The protests are portrayed as morally justified, courageous, or necessary.",
    "- Neutral framing (-0.3 to +0.3): The protests are described factually or both criticism and support are presented equally.",
    "- Negative framing (-1 to -0.3): The protests are portrayed as chaotic, criminal, misguided, or illegitimate.",
    "",
    "Important: Do not infer framing based on unrelated policy criticism or background information.",
    "Only judge the framing of the protests if they are directly discussed, supported, or critiqued.",
    "",
    "Use step-by-step reasoning before deciding.",
    "Then format your response exactly like this:",
    "Framing Category: [Positive / Neutral / Negative]; Continuous: [score from -1 to +1]; Justification: [brief explanation]",
    "",
    "Here are examples:",
    "",
    "Quote: “They’re just noisy wakora shouting ‘Ruto must go’. He’s not going anywhere.”",
    "Step 1: Protestors are described using insulting language.",
    "Step 2: The ‘Ruto must go’ slogan is mocked and dismissed.",
    "Step 3: No justification is given for protest demands.",
    "Framing Category: Negative; Continuous: -0.5; Justification: The article mocks protestors and delegitimizes their demands without offering counterbalance.",
    "",
    "Quote: “Pressure from the Gen Z protests forced the President to withdraw the Finance Bill 2024.”",
    "Step 1: The protests are credited with achieving a political outcome.",
    "Step 2: The framing implies that the resistance was effective and justified.",
    "Step 3: No undermining or negative language is used.",
    "Framing Category: Positive; Continuous: +0.7; Justification: The article frames the protests as legitimate and impactful in forcing a policy reversal.",
    "",
    "Quote: “The protests led to the withdrawal of the Finance Bill. The president is now focused on youth employment.”",
    "Step 1: The article mentions the protest as context.",
    "Step 2: It does not evaluate the protest positively or negatively.",
    "Step 3: The tone is factual.",
    "Framing Category: Neutral; Continuous: 0.0; Justification: The protest is referenced as background without moral judgment or framing.",
    "",
    "Quote: “The majority of the participants said it was better for them to die but make sure their voices were heard... I knew in my heart that I was fighting for the right thing.”",
    "Step 1: The article centers a protestor who faced abduction and torture but affirms his moral commitment to the protests.",
    "Step 2: The resistance is framed as a righteous stand against corruption and oppression.",
    "Step 3: The government response acknowledges violations.",
    "Framing Category: Positive; Continuous: +0.8; Justification: The article frames the protests as morally justified, highlighting personal sacrifice and civic commitment.",
    "",
    "Quote: “Protesters lit bonfires, blocked highways, looted businesses, and criminal gangs disguised as demonstrators raided shops. Clergy later called for peaceful protest and an end to police brutality.”",
    "Step 1: The article describes legitimate protest goals but focuses heavily on violence, looting, and chaos.",
    "Step 2: While peaceful acts and public support are mentioned, they are overshadowed by reports of criminality.",
    "Step 3: The framing emphasizes disorder and instability.",
    "Framing Category: Negative; Continuous: -0.5; Justification: Although the article includes some defense of protest rights, it overwhelmingly emphasizes chaos, which delegitimizes the resistance.",
    "",
    "Quote: “President Ruto took his first international trip after a two-month hiatus following the Gen Z protests, aiming to secure jobs for youth abroad.”",
    "Step 1: The protest is mentioned purely to provide temporal context for the president’s travel.",
    "Step 2: The article does not evaluate the protest’s legitimacy, goals, or methods.",
    "Step 3: The rest of the article focuses on labor migration and diaspora policy.",
    "Framing Category: Neutral; Continuous: 0.0; Justification: The protest is not morally framed and is only used to explain timing. No judgment is made about the movement itself.",
    "",
    "Now analyze the following article:",
    article_text
  )
  
  response <- send_to_gpt(prompt, api_key)
  
  print(response)  # ✅ Debugging: Print raw API response
  
  if (!is.null(response)) {
    tryCatch({
      framing_match <- str_extract(response, "Framing Category:\\s*(Positive|Neutral|Negative)")
      continuous_match <- str_extract(response, "Continuous:\\s*([+-]?\\d+(\\.\\d+)?)")
      
      justification_match <- str_match(response, "Justification:\\s*(.*)$")[,2]
      
      
      
      sentiment_category <- ifelse(!is.na(framing_match), gsub("Framing Category:\\s*", "", framing_match), NA)
      continuous <- ifelse(!is.na(continuous_match), gsub("Continuous:\\s*", "", continuous_match), NA)
      justification <- ifelse(!is.na(justification_match), justification_match, NA)
      
      return(c(sentiment_category, continuous, justification))
    }, error = function(e) {
      print(paste("⚠️ Regex Parsing Error:", e$message))
      return(c(NA, NA, NA))
    })
  } else {
    return(c(NA, NA, NA))
  }
}



# Create a results matrix
results_newprompt <- matrix(NA, nrow = nrow(Data_newprompt1), ncol = 3)
colnames(results_newprompt) <- c("sentiment_category", "Continuous", "justification")



# Set up progress bar
pb <- txtProgressBar(min = 0, max = nrow(Data_newprompt1), style = 3)

# Run the framing analysis
for (i in 1:nrow(Data_newprompt1)) {
  results[i, ] <- analyze_article_framing(Data_newprompt1$Full_text[i])
  setTxtProgressBar(pb, i)
}

close(pb)

#merge results and save

# Convert to dataframe
results_dfnew <- as.data.frame(results_newprompt, stringsAsFactors = FALSE)
colnames(results_dfnew) <- c("Framing_Category", "Framing_Score", "Justification")
results_dfnew$Framing_Score <- as.numeric(results_dfnew$Framing_Score)


# Merge with original article data
Data_sample_test <- cbind(Data_newprompt1, results_dfnew)

# Optional: Save the results to CSV
write_csv(Data_subset_annotated, "framing_results_subset.csv")


set.seed(123)  # Optional: Ensures same random sample each time

# Assuming your data is called `Data_corrected_annotatednew`
# and the column identifying outlets is `Outlet`

# Load dplyr if not already loaded
library(dplyr)

# Sample 50 articles per outlet
sampled_data <- Data_corrected_annotatednew %>%
  group_by(Outlet) %>%
  slice_sample(n = 50, replace = FALSE) %>%
  ungroup()


model1 <- lm(Framing_Score ~ state_affiliated, data = sampled_data)
summary (model1)

model4 <- lm(Framing_Score ~ state_affiliated + Alignment_binary + Inflation + 
               Super_Petrol + Gender_of_author + Media_format_type + 
               factor(Month) + factor(Outlet),
             data = sampled_data)

summary (model4)



# Ensure the date column is in Date format
Data_corrected_annotatednew$date_published <- as.Date(Data_corrected_annotatednew$date_published)

# Optionally aggregate by week or month
Data_time_series <- Data_corrected_annotatednew %>%
  mutate(month = floor_date(date_published, unit = "month")) %>%
  group_by(month, state_affiliated) %>%  # or use Outlet instead of state_affiliated
  summarise(
    avg_sentiment = mean(Framing_Score, na.rm = TRUE),
    n_articles = n()
  ) %>%
  ungroup()

# Plot
ggplot(Data_time_series, aes(x = month, y = avg_sentiment, color = state_affiliated)) +
  geom_line(size = 1.2) +
  geom_point(aes(size = n_articles), alpha = 0.7) +
  scale_color_manual(values = c("Independent" = "#1b9e77", "Gov't-Affiliated" = "#d95f02")) +
  labs(
    title = "Average Media Framing Score of Civil Resistance Over Time",
    x = "Month",
    y = "Framing Score",
    color = "Media Ownership",
    size = "Number of Articles"
  ) +
  theme_minimal(base_family = "Times New Roman")


### zero shot prompting 
library(httr)
library(stringr)
library(readr)
library(progress)
library(jsonlite)

# 🔐 Step 1: Set your OpenAI API key
api_key <- "(insert your api key here)"

# 🔁 Step 2: Define the GPT call function
send_to_gpt <- function(text, api_key, model = "gpt-4o-mini") {
  url <- "https://api.openai.com/v1/chat/completions"
  
  response <- POST(
    url = url,
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = model,
      messages = list(list(role = "user", content = text))
    )
  )
  
  parsed_response <- content(response, as = "parsed")
  
  if (!is.null(parsed_response$choices) && length(parsed_response$choices) > 0) {
    return(parsed_response$choices[[1]]$message$content)
  } else {
    print("Error: No valid response from API.")
    return(NULL)
  }
}

## analyse prompt



analyze_framing <- function(article_text) {
  prompt <- paste(
    "Your task is to evaluate how the following article frames the 2024 Gen Z-led protests against the Finance Bill in Kenya.",
    "Framing refers to how specific aspects of an event are highlighted to promote a particular interpretation, evaluation, or meaning.",
    "Provide continuous framing score, with values ranging from -1 (=most negative) to +1 (=most positive), with 0 = neutral.",
    "Provide the response in the following format:",
    "'Framing: [Framing]'",
    "\n\narticle:", article_text
  )
  
  
  response <- send_to_gpt(prompt, api_key)
  
  if (!is.null(response)) {
    
    # Extract sentiment  using regex
    sentiment_match <- str_extract(response, "Framing:\\s*([+-]?\\d+(\\.\\d+)?)")
    sentiment <- ifelse(!is.na(sentiment_match), gsub("Framing:\\s*", "", sentiment_match), NA)
    
    #extract instrad of sentiment, the label of the topic 
    
    return(sentiment)
  } else {
    return(NA)
  }
}

#random selected articles
set.seed(123)  # For reproducibility
sampled_articles10 <- Data_newprompt[sample(nrow(Data_newprompt), 10), ]

# Initialize vector to store scores

results <- matrix(NA, nrow = nrow(Data_newprompt), ncol = 1)

# Progress bar
pb <- txtProgressBar(min = 0, max = nrow(Data_newprompt), style = 3)

# Loop through each tweet/text
for (i in 1:nrow(Data_newprompt)) {
  results [i] <- analyze_framing(Data_newprompt$Full_text[i])
  Sys.sleep(1)  # avoid rate limits
  setTxtProgressBar(pb, i)
}

close(pb)

# Convert results to a dataframe
results_df <- as.data.frame(results, stringsAsFactors = FALSE)
colnames(results_df) <- "sentiment_2"
results_df$sentiment_2 <- as.numeric(results_df$sentiment_2)

# Combine results and save
Data_newprompt2 <- cbind(df_cleaned, results_df)

# Remove all columns that start with 'sentiment_2' except the last one
df_cleaned <- Data_newprompt[, !grepl("^sentiment_2(\\.|$)", names(Data_newprompt)) | names(Data_newprompt) == "sentiment_2.2"]

summary (Data_newprompt)

length(unique(Data_newprompt2$sentiment_2))

model1 <- lm(sentiment_2 ~ state_affiliated, data = Data_newprompt2)
summary (model1)

model2 <- lm(sentiment_2 ~ state_affiliated + Alignment_binary + Inflation + Super_Petrol + 
               Gender_of_author + Media_format_type,
             data = Data_newprompt2)
summary (model2)

model3 <- lm(Framing_Score ~  factor (Outlet) + state_affiliated + Alignment_binary + Inflation + 
               Super_Petrol + Gender_of_author + Media_format_type + factor(biweek),
             data = Data_corrected_annotatednew)
summary (model3)


# Set a seed for reproducibility
set.seed(123)

# Draw a random sample of 70 observations
sample_data1 <- Data_corrected_annotatednew %>%
  dplyr::sample_n(69)

# Run the linear regression model on the sample
model3_sample <- lm(Framing_Score ~ state_affiliated + Alignment_binary + Inflation + 
                      Super_Petrol + Gender_of_author + Media_format_type + factor (Outlet) + factor (Month),
                    data = sample_data1)

# Show the summary
summary(model3_sample)


library(writexl)

write_xlsx(Data_newprompt2 , "Data_newprompt.xlsx")




### evaluation metrics f1 score, accuracy ###

# Ensure packages are loaded
library(caret)

# Optional: Check the levels used
unique(Data_40_humanannotated$Framing_Category)
unique(Data_40_GPTannotated$Framing_Category)

# Standardize levels
levels_used <- c("Negative", "Neutral", "Positive")

# Convert to factors with same levels
actual <- factor(Data_40_humanannotated$Framing_Category, levels = levels_used)
predicted <- factor(Data_40_GPTannotated$Framing_Category, levels = levels_used)

# Confusion matrix
conf_matrix <- confusionMatrix(predicted, actual)

# Show performance summary
print(conf_matrix)

# Compute F1 score per class
precision <- conf_matrix$byClass[, "Precision"]
recall <- conf_matrix$byClass[, "Recall"]
f1 <- 2 * ((precision * recall) / (precision + recall))

# Display F1 for each class
f1

# Macro-averaged F1
macro_f1 <- mean(f1, na.rm = TRUE)
cat("Macro F1 Score:", round(macro_f1, 3), "\n")


### visualisation for the appendix (confusion matrix)

library(ggplot2)
library(reshape2)

# Create the matrix
conf_mat <- matrix(c(10,3,1, 2,17,1, 0,1,5), nrow=3, byrow=TRUE)
colnames(conf_mat) <- c("Negative", "Neutral", "Positive")
rownames(conf_mat) <- c("Negative", "Neutral", "Positive")
conf_df <- melt(conf_mat)

# Plot
ggplot(conf_df, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(x = "Reference (Human)", y = "Prediction (GPT-4)", fill = "Count") +
  theme_minimal()

## bar plot for f1 scores
f1_scores <- c(Negative = 0.77, Neutral = 0.83, Positive = 0.77)
barplot(f1_scores,
        col = "steelblue",
        ylim = c(0, 1),
        main = "F1 Scores by Framing Category",
        ylab = "F1 Score")

#### for the summary table ###

library(knitr)
library(kableExtra)

# Extract metrics
overall_metrics <- data.frame(
  Metric = c("Overall Accuracy", "95% CI (Lower)", "95% CI (Upper)", 
             "No Information Rate", "P-Value (Acc > NIR)", 
             "Kappa", "McNemar's Test P-Value", "Macro F1 Score"),
  Value = c(
    round(conf_matrix$overall["Accuracy"], 3),
    round(conf_matrix$overall["AccuracyLower"], 3),
    round(conf_matrix$overall["AccuracyUpper"], 3),
    round(conf_matrix$overall["No Information Rate"], 3),
    signif(conf_matrix$overall["P-Value"], 3),
    round(conf_matrix$overall["Kappa"], 3),
    signif(conf_matrix$overall["McnemarPValue"], 3),
    round(macro_f1, 3)
  )
)

# Display overall metrics table
kable(overall_metrics, caption = "Model Evaluation Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

# Extract per-class metrics
f1 <- 2 * ((conf_matrix$byClass[, "Precision"] * conf_matrix$byClass[, "Recall"]) /
             (conf_matrix$byClass[, "Precision"] + conf_matrix$byClass[, "Recall"]))

class_metrics <- data.frame(
  Class = rownames(conf_matrix$byClass),
  Precision = round(conf_matrix$byClass[, "Precision"], 3),
  Recall = round(conf_matrix$byClass[, "Recall"], 3),
  F1_Score = round(f1, 3)
)

# Display class-wise metrics table
kable(class_metrics, caption = "Per-Class Performance Metrics") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)







### zero shot prompting 
library(httr)
library(stringr)
library(readr)
library(progress)
library(jsonlite)

# 🔐 Step 1: Set your OpenAI API key
api_key <- "(insert your api key here)"

# 🔁 Step 2: Define the GPT call function
send_to_gpt <- function(text, api_key, model = "gpt-4o-mini") {
  url <- "https://api.openai.com/v1/chat/completions"
  
  response <- POST(
    url = url,
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = model,
      messages = list(list(role = "user", content = text))
    )
  )
  
  parsed_response <- content(response, as = "parsed")
  
  if (!is.null(parsed_response$choices) && length(parsed_response$choices) > 0) {
    return(parsed_response$choices[[1]]$message$content)
  } else {
    print("Error: No valid response from API.")
    return(NULL)
  }
}

## analyse prompt



analyze_framing <- function(article_text) {
  prompt <- paste(
    "Your task is to evaluate how the following article frames the 2024 Gen Z-led protests against the Finance Bill in Kenya.",
    "Framing refers to how specific aspects of an event are highlighted to promote a particular interpretation, evaluation, or meaning.",
    "Provide both:",
    "- A categorical Framing classification: Positive, Neutral, or Negative.",
    "- A continuous Framing score ranging from -1 (most negative) to +1 (most positive).",
    "Then format your response exactly like this:",
    "Framing Category: [Positive / Neutral / Negative]; Continuous: [score from -1 to +1]; Justification: [brief explanation]",
    "Now analyze the following article:",
    article_text
  )
  
  response <- send_to_gpt(prompt, api_key)
  
  print(response)  # ✅ Debugging: Print raw API response
  
  if (!is.null(response)) {
    tryCatch({
      framing_match <- str_extract(response, "Framing Category:\\s*(Positive|Neutral|Negative)")
      continuous_match <- str_extract(response, "Continuous:\\s*([+-]?\\d+(\\.\\d+)?)")
      
      justification_match <- str_match(response, "Justification:\\s*(.*)$")[,2]
      
      
      
      sentiment_category <- ifelse(!is.na(framing_match), gsub("Framing Category:\\s*", "", framing_match), NA)
      continuous <- ifelse(!is.na(continuous_match), gsub("Continuous:\\s*", "", continuous_match), NA)
      justification <- ifelse(!is.na(justification_match), justification_match, NA)
      
      return(c(sentiment_category, continuous, justification))
    }, error = function(e) {
      print(paste("⚠️ Regex Parsing Error:", e$message))
      return(c(NA, NA, NA))
    })
  } else {
    return(c(NA, NA, NA))
  }
}

#randomly selcted 40 articles 
# Remove the last three rows
Data_40_zeroshot  <- Data_40_GPTannotated[, 1:(ncol(Data_40_GPTannotated) - 3)]


# Initialize vector to store scores

results <- matrix(NA, nrow = nrow(Data_40_zeroshot), ncol = 3)

# Progress bar
pb <- txtProgressBar(min = 0, max = nrow(Data_40_zeroshot), style = 3)

# Loop through each tweet/text
for (i in 1:nrow(Data_40_zeroshot)) {
  results [i] <- analyze_framing(Data_40_zeroshot$Full_text[i])
  Sys.sleep(1)  # avoid rate limits
  setTxtProgressBar(pb, i)
}

close(pb)

# Convert results to a dataframe
results_df4 <- as.data.frame(results, stringsAsFactors = FALSE)
colnames(results_df4) <- c("Framing_Category", "Framing_Score", "Justification")
results_df2$Framing_Score <- as.numeric(results_df2$Framing_Score)

# Combine results and save
Data_40_zeroshot <- cbind(Data_40_zeroshot, results_df4)




#### for the zero shot model accuracy and F1 ###
unique(Data_40_humanannotated$Framing_Category)
unique(Data_40_zeroshot$Framing_Category)

# Standardize levels
levels_used <- c("Negative", "Neutral", "Positive")

# Convert to factors with same levels
actual <- factor(Data_40_humanannotated$Framing_Category, levels = levels_used)
predicted_zero <- factor(Data_40_zeroshot$Framing_Category, levels = levels_used)

# Confusion matrix
conf_matrix <- confusionMatrix(predicted_zero, actual)

print(conf_matrix)




# Load required library
library(ggplot2)
library(tidyr)
library(dplyr)

# Define F1 scores for zero-shot and few-shot prompting
f1_data <- data.frame(
  Method = rep(c("Zero-shot", "Few-shot"), each = 4),
  Metric = rep(c("Negative", "Neutral", "Positive", "Macro Avg"), 2),
  F1_Score = c(0.60, 0.55, 0.50, 0.553,   # Replace with your actual Zero-shot F1s
               0.74, 0.805, 0.74, 0.76)   # Replace with your actual Few-shot F1s
)

# Plot F1 score comparison
ggplot(f1_data, aes(x = Metric, y = F1_Score, fill = Method)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ylim(0, 1) +
  labs(
    title = "Comparison of F1 Scores for Framing Classification",
    x = "Framing Category",
    y = "F1 Score"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  geom_text(aes(label = round(F1_Score, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5)

# Optional: Accuracy plot
accuracy_data <- data.frame(
  Method = c("Zero-shot", "Few-shot"),
  Accuracy = c(0.57, 0.80)  # Replace with your actual accuracy values
)

ggplot(accuracy_data, aes(x = Method, y = Accuracy, fill = Method)) +
  geom_bar(stat = "identity", width = 0.5) +
  ylim(0, 1) +
  labs(
    title = "Overall Accuracy Comparison",
    x = "Prompting Method",
    y = "Accuracy"
  ) +
  theme_minimal(base_family = "Times New Roman") +
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  geom_text(aes(label = round(Accuracy, 2)), vjust = -0.5, size = 4)


# Recode political alignment into a binary variable
Data_corrected_annotatednew$ethnic_alignment <- ifelse(Data_corrected_annotatednew$Political_Alignment == "aligned", "Kalenjin", "Other")


#author fixed effects 

# Step 1: Identify the top 20 most frequent authors
top_authors <- names(sort(table(Data_corrected_annotatednew$Author_name), decreasing = TRUE))[1:30]

# Step 2: Create a new variable that retains top authors, and groups others as "Other"
Data_corrected_annotatednew$Author_FE <- ifelse(Data_corrected_annotatednew$Author_name %in% top_authors, Data_corrected_annotatednew$Author_name, "Other")

# Step 3: Convert the new variable to a factor (needed for fixed effects)
Data_corrected_annotatednew$Author_FE <- factor(Data_corrected_annotatednew$Author_FE)

# Step 4: Run regression with Author_FE as fixed effect
model <- lm(Framing_Score ~ state_affiliated + Alignment_binary + Inflation + 
              Crude_Oil_USD.x + Gender_of_author + Media_format_type + 
              factor(Month) + factor(Author_FE),
            data = Data_corrected_annotatednew)
# Step 5: Summary
summary(model)

library(lubridate)
library(dplyr)

# Make sure the date column is in Date format
Data_corrected_annotated.2$date <- as.Date(Data_corrected_annotated.2$date_published)

# Create a biweekly variable by cutting weeks into 2-week bins
Data_corrected_annotated.2 <- Data_corrected_annotated.2 %>%
  mutate(
    year = year(date),
    week = isoweek(date),
    biweek = paste0(year, "-B", (week %/% 2) + 1)  # Biweekly group identifier
  )


