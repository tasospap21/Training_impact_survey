# Convert wide to long format for:
# a) the Reason to attend a course
# b) How did you hear about the course


# Google ID for the Attendees data - Other use GSheet
Attendees_Sheet_ID <- "1tCj_HwKKQVZ8slSylJHj_1H-ncSjcgzlWV3CH1jrUwM"

# Read the Attendees rows that are for year 2022 
Attendees_2022 <- read_sheet(Attendees_Sheet_ID,
                        sheet = "Attendees",
                        range = "Attendees!A37066:AK46251")

# Replace with the actual Headers
colnames(Attendees_2022) <- colnames(Attendees)


### Reason wide to long
Reason_to_attend <- Attendees_2022 %>% 
  select(event_date,course_code,institution,reason,Faculty_Modified,
         `Tool/Technology`,Role_Modified,FoR_Modified,State,status) %>% 
  separate(reason, c("reason1","reason2","reason3","reason4","reason5"),
           "\\s\\|\\s", extra = "merge") %>% 
  pivot_longer(
    cols = c(starts_with("reason")),
    names_to = "reason_together",
    values_to = "reason"
  ) %>% 
  select(-c(reason_together)) %>% 
  drop_na("reason")


### Hear about course wide to long
hear_about_course <- Attendees_2022 %>% 
  select(event_date,course_code,institution,hear_about_course,Faculty_Modified,
         `Tool/Technology`,Role_Modified,FoR_Modified,State,status) %>% 
  separate(hear_about_course, c("hear1","hear2","hear3","hear4","hear5",
                                "hear6","hear7","hear8"),
           "\\s\\|\\s", extra = "merge") %>% 
  pivot_longer(
    cols = c(starts_with("hear")),
    names_to = "hear_together",
    values_to = "hear_about_course"
  ) %>% 
  select(-c(hear_together)) %>% 
  drop_na("hear_about_course")
