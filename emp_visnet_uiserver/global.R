#packages = c('DT','tm','shiny','lubridate', 'hms','tidytext','widyr','tidyverse','tidygraph','stringr','tidyr','visNetwork', 'utf8', 'knitr','textmineR')

library(DT)
library(shiny)
library(lubridate)
library(stringr)
library(tidyr)
library(visNetwork)
library(utf8)
library(dplyr)
library(readr)


# Data Preparation
employees_records <- readxl::read_xlsx("EmployeeRecords.xlsx")
employees_records_cleaned <- employees_records %>%
  unite(fullname, FirstName, LastName, sep = " ", remove=FALSE) %>%
  mutate_if(is.character, utf8_encode) %>% 
  mutate(fullname = gsub("\\.","",fullname))

employees_records_cleaned$fullname <- trimws(employees_records_cleaned$fullname, which = c("both"))

email_records <- read_csv("email headers.csv")
email_records_cleaned <- email_records %>%
  mutate(To = str_remove_all(To,"@gastech.com.kronos|@gastech.com.tethys")) %>%
  mutate(To = str_replace_all(To, "[.]", " ")) %>%
  mutate(From = str_remove_all(From,"@gastech.com.kronos|@gastech.com.tethys")) %>%
  mutate(From = str_replace_all(From, "[.]", " ")) %>%
  mutate(Date = parse_date_time(x = Date, orders =c("%m%d%y %H%M","%m%d%y"))) %>%
  mutate_if(is.character, utf8_encode)

email_records_cleaned <- email_records_cleaned %>% 
  mutate(recipient_no = str_count(To,",")+1) %>% 
  mutate(Subject = gsub("[[:punct:]]", "",Subject)) %>% 
  mutate(Subject = str_remove_all(Subject,"RE")) %>%
  mutate(Subject = trimws(Subject, which = c("both")))

email_records_cleaned2 <- strsplit(email_records_cleaned$To, split=",")
email_records_cleaned2 <- data.frame(From = rep(email_records_cleaned$From, sapply(email_records_cleaned2, length)), 
                                     To = unlist(email_records_cleaned2), Date = rep(email_records_cleaned$Date, 
                                                                                     sapply(email_records_cleaned2, length)),
                                     Subject =  rep(email_records_cleaned$Subject, sapply(email_records_cleaned2, length)))
email_records_cleaned2$To <- trimws(email_records_cleaned2$To, which = c("both"))
email_records_cleaned2$From <- trimws(email_records_cleaned2$From, which = c("both"))



# Get Relevant emails only

email_records_cleaned_relevant <- email_records_cleaned %>% 
  filter(recipient_no < 10)

email_records_cleaned_relevant_agg <- email_records_cleaned_relevant %>% 
  group_by(From, To, Subject) %>% 
  summarise(Weight=n(), recipient_no=max(recipient_no))

email_records_analysis2 <- email_records_cleaned_relevant_agg %>% 
  rename(c("Sender" = "From","Recipient" = "To","Count"="Weight"))

email_records_cleaned_relevant_net <- strsplit(email_records_cleaned_relevant$To, split=",")
email_records_cleaned_relevant_net <- data.frame(From = rep(email_records_cleaned_relevant$From, sapply(email_records_cleaned_relevant_net, length)), 
                                                 To = unlist(email_records_cleaned_relevant_net), Date = rep(email_records_cleaned_relevant$Date, 
                                                                                                             sapply(email_records_cleaned_relevant_net, length)),
                                                 Subject =  rep(email_records_cleaned_relevant$Subject, sapply(email_records_cleaned_relevant_net, length)))

email_records_cleaned_relevant_net$To <- trimws(email_records_cleaned_relevant_net$To, which = c("both"))
email_records_cleaned_relevant_net$From <- trimws(email_records_cleaned_relevant_net$From, which = c("both"))

#Tidying edge list

email_records_cleaned_relevant_net_rename <- email_records_cleaned_relevant_net %>%
  rename (from = From) %>%
  rename (to = To)


email_records_cleaned_relevant_net_rename_agg <- email_records_cleaned_relevant_net_rename %>%
  group_by (from, to, Subject) %>%
  summarise(Weight = n()) %>%
  filter(from!=to) %>%
  filter(to!=from) %>%
  filter(Weight > 1) %>%
  ungroup()

#Tidying node list
employees_records_cleaned_rename <- employees_records_cleaned %>%
  rename(id = fullname) %>%
  rename (group = CurrentEmploymentType) %>%
  arrange(id)

employees_records_cleaned_rename_impt <- employees_records_cleaned_rename %>% 
  transmute(id=id, BirthCountry=BirthCountry, group=group,
            CurrentEmploymentTitle=CurrentEmploymentTitle,
            MilitaryServiceBranch=MilitaryServiceBranch)