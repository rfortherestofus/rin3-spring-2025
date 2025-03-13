library(tidyverse)
library(rfortherestofus)

post_course_participants(
  course_title = "R in 3 Months (Spring 2025)",
  sheet_name = "Spring 2025 Participants",
  google_sheets_url = "https://docs.google.com/spreadsheets/d/1Rz7_1_CzxURdhPHRj9YAzV-tgQP6xQmY_qV0VPjhPYs/edit?gid=812055131#gid=812055131"
)

rin3_participants <-
  get_course_participants(
    course_title = "R in 3 Months (Spring 2025)"
  )
