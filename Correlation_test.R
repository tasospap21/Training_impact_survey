library(broom)
library(dplyr)
library(forcats)
library(tidyr)
library(correlation)
library(corrplot)

# Behaviour

# test <- Only_Responses %>%
#   mutate(behaviour_corrected = as.factor(
#     fct_recode(
#       behaviour_corrected,
#       "5" = "5 - Frequently",
#       "4" = "4 - Often",
#       "3" = "3 - Sometimes",
#       "2" = "2 - Rarely",
#       "1" = "1 - Never"
#     )
#   )) %>% 
#   mutate(faculty_corrected = as.factor(
#     fct_recode(
#       faculty,
#       "1" = "Business School",
#       "2" = "Faculty of Medicine and Health",
#       "3" = "Faculty of Science",
#       "4" = "Faculty of Architecture, Design and Planning",
#       "5" = "Faculty of Engineering",
#       "6" = "Other/Not Applicable",
#       "7" = "Faculty of Arts and Social Sciences",
#       "8" = "Faculty of Law"
#     )
#   ))
# 
# test$behaviour_corrected <- as.numeric(test$behaviour_corrected)
# test$faculty_corrected <- as.numeric(test$faculty_corrected)
# 
# tidy(cor.test(test$behaviour_corrected, test$faculty_corrected,
#      method = "spearman",
#      exact = FALSE))

test <- Only_Responses %>% 
  drop_na(faculty)

test <- test %>%
  mutate(behaviour_corrected = as.factor(
    fct_recode(
      behaviour_corrected,
      "5" = "5 - Frequently",
      "4" = "4 - Often",
      "3" = "3 - Sometimes",
      "2" = "2 - Rarely",
      "1" = "1 - Never"
    )
  ))

test$behaviour_corrected <- as.numeric(test$behaviour_corrected)

## Behaviour and Faculty

faculties_list <- unique(test$faculty)

for (x in faculties_list) {
  test_small <- test %>% 
    filter(faculty==x)
  
  print(x)
  print(tidy(shapiro.test(test_small$behaviour_corrected)))
} 

## Behaviour and Role

roles_list <- unique(test$Role)

for (x in roles_list[1:1]) {
  test_small <- test %>% 
    filter(Role==x)
  
  print(x)
  print(tidy(shapiro.test(test_small$behaviour_corrected)))
  
} 


faculties_list <- unique(Only_Responses$faculty)

for (x in faculties_list[1:5]){

test2 <- Only_Responses %>% 
  select(reaction_corrected,behaviour_corrected,learning_corrected,results_corrected,faculty) %>% 
  filter(faculty==x) %>% 
  select(-c(faculty)) %>% 
  mutate(reaction_corrected = as.factor(fct_recode(reaction_corrected,
                                                   "5"="5 - Extremely worthwhile",
                                                   "4"="4 - Very worthwhile",
                                                   "3"="3 - Slightly worthwhile",
                                                   "2"="2 - Not very worthwhile",
                                                   "1"="1 - Not at all worthwhile"))) %>% 
  mutate(learning_corrected = as.factor(fct_recode(learning_corrected,
                                                   "5"="5 - Much more confident",
                                                   "4"="4 - More confident",
                                                   "3"="3 - No more confident",
                                                   "2"="2 - Less confident",
                                                   "1"="1 - Much less confident"))) %>% 
  mutate(behaviour_corrected = as.factor(fct_recode(behaviour_corrected,
                                                    "5"="5 - Frequently",
                                                    "4"="4 - Often",
                                                    "3"="3 - Sometimes",
                                                    "2"="2 - Rarely",
                                                    "1"="1 - Never"))) %>%
  mutate(results_corrected = as.factor(fct_recode(results_corrected,
                                                  "5"="5 - Extremely helpful",
                                                  "4"="4 - Very helpful",
                                                  "3"="3 - Somewhat helpful",
                                                  "2"="2 - Not very helpful",
                                                  "1"="1 - Not at all helpful"))) %>% 
  mutate(reaction_corrected=as.numeric(reaction_corrected),
         learning_corrected=as.numeric(learning_corrected),
         behaviour_corrected=as.numeric(behaviour_corrected),
         results_corrected=as.numeric(results_corrected))

print(x)
print(round(cor(test2),
      digits = 2))

}










test <- Only_Responses %>% 
  select(`Record ID`,Course,Technology,Role,University,for_code,faculty,Level,
         starts_with("Did the knowledge acquired in the [course] course[output_calctext]")) %>%
  pivot_longer(
    cols = starts_with("Did the knowledge acquired in the [course] course[output_calctext]"),
    names_to = "Research Output",
    values_to = "CheckedOrNot"
  ) %>% 
  filter(CheckedOrNot == "Checked") %>% 
  select(-c(CheckedOrNot)) %>% 
  mutate(`Research Output` = as.factor(fct_recode(`Research Output`,
                                                  "Journal Article"="Did the knowledge acquired in the [course] course[output_calctext]contribute to your ability to produce materials that led, or may lead to, any of these following research outputs?Select all that apply (choice=Journal Article - <em>article, letter, review, etc.</em>)",
                                                  "Conference"="Did the knowledge acquired in the [course] course[output_calctext]contribute to your ability to produce materials that led, or may lead to, any of these following research outputs?Select all that apply (choice=Conference - <em>abstract, conference paper, proceedings, etc</em>)",
                                                  "Presentation"="Did the knowledge acquired in the [course] course[output_calctext]contribute to your ability to produce materials that led, or may lead to, any of these following research outputs?Select all that apply (choice=Presentation - <em>conference, non-conference</em>)",
                                                  "Poster"="Did the knowledge acquired in the [course] course[output_calctext]contribute to your ability to produce materials that led, or may lead to, any of these following research outputs?Select all that apply (choice=Poster)",
                                                  "Book or Book Chapter"="Did the knowledge acquired in the [course] course[output_calctext]contribute to your ability to produce materials that led, or may lead to, any of these following research outputs?Select all that apply (choice=Book or Book Chapter)",
                                                  "Report"="Did the knowledge acquired in the [course] course[output_calctext]contribute to your ability to produce materials that led, or may lead to, any of these following research outputs?Select all that apply (choice=Report - <em>technical report, working paper, confidential report, etc.</em>)",
                                                  "Thesis/Dissertation"="Did the knowledge acquired in the [course] course[output_calctext]contribute to your ability to produce materials that led, or may lead to, any of these following research outputs?Select all that apply (choice=Thesis/Dissertation)",
                                                  "Media"="Did the knowledge acquired in the [course] course[output_calctext]contribute to your ability to produce materials that led, or may lead to, any of these following research outputs?Select all that apply (choice=Media - <em>Film, TV, Video</em>)",
                                                  "Published Software"="Did the knowledge acquired in the [course] course[output_calctext]contribute to your ability to produce materials that led, or may lead to, any of these following research outputs?Select all that apply (choice=Published Software - <em>open-source or not</em>)",
                                                  "Published Dataset"="Did the knowledge acquired in the [course] course[output_calctext]contribute to your ability to produce materials that led, or may lead to, any of these following research outputs?Select all that apply (choice=Published Dataset)",
                                                  "Published Figure/s"="Did the knowledge acquired in the [course] course[output_calctext]contribute to your ability to produce materials that led, or may lead to, any of these following research outputs?Select all that apply (choice=Published Figure/s)",
                                                  "Other"="Did the knowledge acquired in the [course] course[output_calctext]contribute to your ability to produce materials that led, or may lead to, any of these following research outputs?Select all that apply (choice=Other {outputs_other})"
  )))



