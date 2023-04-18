library(vroom)
library(tidyverse)
library(data.table)
library(tictoc)
library(dbplyr)
library(lubridate)

## First the imports:

col_types <- list(
  id = col_character(),
  type = col_skip(),
  number = col_skip(),
  country = col_skip(),
  date = col_date("%Y-%m-%d"),
  abstract = col_skip(),
  title = col_skip(),
  kind = col_skip(),
  num_claims = col_skip(),
  filename = col_skip(),
  withdrawn = col_double()
)

patent_tbl <- vroom(
  file       = "patent.tsv",
  delim      = "\t",
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

#import assignee.tsv

col_types2 <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_skip(),
  name_last = col_skip(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types2,
  na         = c("", "NA", "NULL")
)

# import patent_assignee.tsv
col_types3 <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_skip()
)

patentassignee_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types3,
  na         = c("", "NA", "NULL")
)

# Patent ----
class(patent_tbl)
setDT(patent_tbl)

# Patent Assignee ----
class(patentassignee_tbl)
setDT(patentassignee_tbl)


# Assignee ----
class(assignee_tbl)
setDT(assignee_tbl)



# Question One

#rename assignee_id to id to match pattent_assignee.tsv
setnames(patentassignee_tbl, "assignee_id", "id")
tic()
combined_data <- merge(x = patentassignee_tbl, y = assignee_tbl, 
                       by    = "id", 
                       all.x = TRUE, 
                       all.y = TRUE)
toc()

# Selecting only US company/corporation (type 2), then summing unique patent ids
setorderv(combined_data, c("type", "organization"))
combined_data_type2 <- combined_data %>%
  filter(type == 2)
number_distinct_patents_tbl <- combined_data_type2[, .(number_of_distinct_patents = length(unique(patent_id))), by = organization]
head(arrange(number_distinct_patents_tbl,desc(number_of_distinct_patents)), n = 10)


#Question Two

#adding the year from patent_tbl and filtering by 2019
setnames(patent_tbl, "id", "patent_id")
combined_data_type2 <- merge(x = combined_data_type2, y = patent_tbl, 
                       by    = "patent_id", 
                       all.x = TRUE, 
                       all.y = TRUE)
patents_2019 <- combined_data_type2 %>%
  mutate(year = year(date)) %>%
  filter(year == 2019)

#displaying the first 11 entries (as first is NA)
patents_2019_freq <- patents_2019[, .(number = length(unique(patent_id))), by = organization]
head(arrange(patents_2019_freq,desc(number)), n = 11)


#Question Three
col_types4 <- list(
  uuid = col_skip(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_skip(),
  sequence = col_skip()
)

uspc_tbl <- vroom(
  file       = "uspc.tsv",
  delim      = "\t",
  col_types  = col_types4,
  na         = c("", "NA", "NULL")
)

class(uspc_tbl)
setDT(uspc_tbl)

combined_data_uspc <- merge(x = combined_data, y = uspc_tbl, 
                             by    = "patent_id", 
                             all.x = TRUE, 
                             all.y = TRUE)
combined_data_uspc_wrang <- combined_data_uspc[, .(number_of_distinct_patents = length(unique(patent_id))), by = mainclass_id]
head(arrange(combined_data_uspc_wrang,desc(number_of_distinct_patents)), n = 10)
