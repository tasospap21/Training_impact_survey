# Post-training support checkbox fix ####


# Fix the issue with Survey timestamp and convert it to a Date
# Exlcude the responses that don't include a survey timestamp
Time_diff_df <- Only_Responses %>% 
  filter(`Survey Timestamp`!="[not completed]")

# Convert to a date
Time_diff_df$`Survey Timestamp` <- sapply(Time_diff_df$`Survey Timestamp`,`[[`,1) %>% 
  as.POSIXct(origin="1970-01-01")

# Convert to a date
# temp = as.data.frame(do.call(rbind, lapply(Time_diff_df$`Survey Timestamp`, as.data.frame)))
# 
# temp$`X[[i]]` <- as.POSIXct(temp$`X[[i]]`,tz = "UTC",format = "%Y-%m-%d %H:%M:%S")

# Calculate the time difference and add a new column for Time diff case
Time_diff_df <- Time_diff_df %>%
  mutate(Time_diff = round(`Survey Timestamp` - `Course date`)) %>% 
  mutate(Time_diff_case = case_when(
    Time_diff <=400 ~ "0-400",
    Time_diff >400 & Time_diff <=500 ~ "400-500",
    Time_diff >500 & Time_diff <=600 ~ "500-600",
    Time_diff >600 & Time_diff <=700 ~ "600-700",
    Time_diff >700 & Time_diff <=800 ~ "700-800",
    Time_diff >800 & Time_diff <=1000 ~ "800+"
  ))



# Convert wide to long format and delete the Unchecked options
Support_checkbox_long <- Only_Responses %>% 
  select(Course,Technology,Role,University,for_code,faculty,Level,
         starts_with("Have you received further support after attending")) %>% 
  pivot_longer(
    cols = starts_with("Have you received further support after attending"),
    names_to = "Support_Type",
    values_to = "CheckedOrNot"
  ) %>% 
  filter(CheckedOrNot == "Checked") %>% 
  select(-c(CheckedOrNot)) %>% 
  filter(Support_Type != "Have you received further support after attending the [course] course? Select all that apply (choice=I didnt need any support)") %>% 
  mutate(Support_Type = as.factor(fct_recode(Support_Type,
                                             "eResearch Analyst"="Have you received further support after attending the [course] course? Select all that apply (choice=Consultation with local eResearch Analyst/s)",
                                             "Other Uni Services"="Have you received further support after attending the [course] course? Select all that apply (choice=Consultation with other support services at [uni_name])",
                                             "Other Intersect training"="Have you received further support after attending the [course] course? Select all that apply (choice=Other Intersect training)",
                                             "Peer Support"="Have you received further support after attending the [course] course? Select all that apply (choice=Peer support)",
                                             "Self-paced training"="Have you received further support after attending the [course] course? Select all that apply (choice=Self-paced training <em>e.g. LinkedIn Learning, Coursera</em>)")))

# Reorder the levels
Support_checkbox_long$Support_Type = factor(Support_checkbox_long$Support_Type, 
        levels = c(sort(levels(Support_checkbox_long$Support_Type), decreasing=T)))



Support_checkbox_long_time_diff <- Time_diff_df %>% 
  select(Course,Technology,Role,University,for_code,faculty,Level,Time_diff_case,
         starts_with("Have you received further support after attending")) %>% 
  pivot_longer(
    cols = starts_with("Have you received further support after attending"),
    names_to = "Support_Type",
    values_to = "CheckedOrNot"
  ) %>% 
  filter(CheckedOrNot == "Checked") %>% 
  select(-c(CheckedOrNot)) %>% 
  filter(Support_Type != "Have you received further support after attending the [course] course? Select all that apply (choice=I didnt need any support)") %>% 
  mutate(Support_Type = as.factor(fct_recode(Support_Type,
                                             "eResearch Analyst"="Have you received further support after attending the [course] course? Select all that apply (choice=Consultation with local eResearch Analyst/s)",
                                             "Other Uni Services"="Have you received further support after attending the [course] course? Select all that apply (choice=Consultation with other support services at [uni_name])",
                                             "Other Intersect training"="Have you received further support after attending the [course] course? Select all that apply (choice=Other Intersect training)",
                                             "Peer Support"="Have you received further support after attending the [course] course? Select all that apply (choice=Peer support)",
                                             "Self-paced training"="Have you received further support after attending the [course] course? Select all that apply (choice=Self-paced training <em>e.g. LinkedIn Learning, Coursera</em>)")))


# Research outputs ####

# Convert wide to long format and delete the Unchecked options
ResearchOutputs_checkbox_long <- Only_Responses %>% 
  select(Course,Technology,Role,University,for_code,faculty,Level,
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

# Reorder the levels
ResearchOutputs_checkbox_long$`Research Output` = factor(ResearchOutputs_checkbox_long$`Research Output`, 
                                            levels = c(sort(levels(ResearchOutputs_checkbox_long$`Research Output`), decreasing=T)))


# Convert wide to long format and delete the Unchecked options
ResearchOutputs_checkbox_long_time_diff <- Time_diff_df %>% 
  select(Course,Technology,Role,University,for_code,faculty,Level,Time_diff_case,
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

# Grants ####

Grants_checkbox_wide <- Responses %>% 
  select(Course,Technology,Role,University,for_code,faculty,
         ends_with("Grant/Funding Application (i.e., Industry and/or Government)?"),
         `Was the grant application successful?`) %>% 
  filter(`Did the knowledge acquired in the [course] course[output_calctext]contribute to your ability to produce materials that led, or may lead to, a Grant/Funding Application (i.e., Industry and/or Government)?` %in% c("No","Yes"))





# No eRA support fix ####

# Convert wide to long format and delete the Unchecked options
# NoeRA_Support_checkbox_long <- Responses %>% 
#   select(Course,Technology,Role,University,for_code,faculty,
#          starts_with("You indicated that you have not used the support of your local eResearch Analyst")) %>% 
#   pivot_longer(
#     cols = starts_with("You indicated that you have not used the support of your local eResearch Analyst"),
#     names_to = "Answer",
#     values_to = "CheckedOrNot"
#   ) %>% 
#   filter(CheckedOrNot == "Checked") %>% 
#   select(-c(CheckedOrNot))
