### Reason wide to long

reason_clean <- Attendees_researchers %>% 
  select(Role_Modified,Faculty_Modified,status,institution,FoR_Modified,reason) %>%
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
hear_clean <- Attendees_researchers %>% 
  select(Role_Modified,Faculty_Modified,status,institution,FoR_Modified
         ,hear_about_course) %>%
  separate(hear_about_course, c("hear1","hear2","hear3","hear4","hear5",
                                "hear6","hear7","hear8"),
           "\\s\\|\\s", extra = "merge") %>% 
  pivot_longer(
    cols = c(starts_with("hear")),
    names_to = "hear_together",
    values_to = "hear_about_course"
  ) %>% 
  select(-c(hear_together)) %>% 
  drop_na("hear_about_course") %>% 
  filter(!(hear_about_course %in% c("Yes","No")))









