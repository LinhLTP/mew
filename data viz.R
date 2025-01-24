# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Data preparation

## Select relevant columns
valid_questions <- df %>%
  select(c(c5a_1:c5a_5))  

## Convert wide format to long format
valid_questions <- valid_questions %>%
  gather(key = "Question_num", value = "Answer")

## Summarise data to calculate frequency and proportion for each answer
valid_questions_summary <- valid_questions %>%
  group_by(Question_num, Answer) %>%
  summarise(freq = n(), .groups = "drop") %>%  # Calculate frequencies
  group_by(Question_num) %>%
  mutate(proportion = round(freq / sum(freq), 1))  # Calculate proportions

# Data Visualisation
ggplot(valid_questions, aes(x=Question_num)) +
  geom_bar(aes(fill=Answer), position = "fill") +
  geom_text(
    data = valid_questions_summary,
    aes(y = freq, label = scales::percent(proportion), group = Answer),
    position = position_fill(vjust = 0.5),
    color = 'gray25', size = 3
  ) +
  scale_fill_brewer(palette = 'Spectral', direction = -1) +
  scale_y_continuous(expand = expansion(0), labels = scales::percent_format()) +
  labs(
    title = "Likert Plot of Question c5", subtitle = "Valid responses",
    x = "", 
    y = ""
  ) +
  theme(legend.text  = element_text(size= 8.5),
        legend.title = element_text(size= 9)) +
  theme_classic()

ggplot(valid_questions, aes(x=Question_num)) +
  geom_bar(aes(fill=Answer), position = "fill") +
  geom_text(
    data = valid_questions_summary,
    aes(y = freq, label = scales::percent(proportion), group = Answer),
    position = position_fill(vjust = 0.5),
    color = 'gray25', size = 3
  ) +
  scale_fill_brewer(palette = 'Spectral', direction = -1) +
  scale_y_continuous(expand = expansion(0), labels = scales::percent_format()) +
  labs(
    title = "Likert Plot of Question c5", subtitle = "Valid responses",
    x = "", 
    y = ""
  ) +
  theme_classic() +
  theme(legend.position = "top") +
  theme(legend.text  = element_text(size= 8.5),
        legend.title = element_text(size= 9)) +
  coord_flip()

## in case we don’t want to showcase the answer No answer:
valid_questions_filtered <- valid_questions %>%
  filter(Answer != "No answer")

## Summarise data to calculate frequency and proportion for valid responses
valid_questions_summary <- valid_questions_filtered %>%
  group_by(Question_num, Answer) %>%
  summarise(freq = n(), .groups = "drop") %>%  # Calculate frequencies and drop grouping
  group_by(Question_num) %>%
  mutate(proportion = round(freq / sum(freq), 1))  # Calculate proportions

ggplot(valid_questions12, aes(x=Question_num)) +
  geom_bar(aes(fill=Answer), position = "fill") +
  geom_text(
    data = valid_questions_summary,
    aes(y = freq, label = scales::percent(proportion), group = Answer),
    position = position_fill(vjust = 0.5),
    color = 'gray25', size = 3
  ) +
  scale_fill_brewer(palette = 'Spectral', direction = -1) +
  scale_y_continuous(expand = expansion(0), labels = scales::percent_format()) +
  labs(
    title = "Likert Plot of Question c5", 
    subtitle = "Valid responses",
    x = "",
    y = "") +
  theme_classic() +
  theme(legend.position = "top") +
  theme(legend.text  = element_text(size= 8.5),
        legend.title = element_blank()) +        # remove legend title 
  coord_flip()

# Data Visualisation for age group

age <- df %>%
  select(h1_1, h2) %>%
  # Filter out invalid or undesired responses
  filter(h1_1 != "No answer") %>%
  filter(!h2 %in% c("Prefer not to say", "No answer", "Non-binary", "Any other gender identity")) %>%
  group_by(h1_1, h2) %>%
  summarise(freq = n(), .groups = "drop") %>%  
  mutate(
    age = factor(h1_1, ordered = TRUE),  # Convert age group to an ordered factor
    freq = ifelse(h2 == "Male", -freq, freq)  # Make male frequencies negative
  ) %>%
  rename(
    gender = h2,
    ageGroup = age
  ) %>%
  select(-h1_1)  # Remove the original age column

age

ggplot(age, aes(y = ageGroup, x = freq, fill = gender)) +
  geom_bar(data=subset(age,gender=="Male"), stat = "identity") +
  geom_bar(data=subset(age,gender=="Female"), stat = "identity") +
  scale_x_continuous(breaks=c(-15, -50, -75, -100, 0, 25, 75, 100, 150, 180)) +
  labs(title = "Age group ~ Gender",
       subtitle = "Valid responses",
       x = "",
       y = "") +
  theme_classic()+
  scale_fill_manual(values = c('darkred', '#254061')) +
  theme(legend.text  = element_text(size= 8.5),
        legend.title = element_blank()) 

