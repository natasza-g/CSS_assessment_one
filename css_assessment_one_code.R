
pacman::p_load(
  tidyverse, # tidyverse pkgs including purrr
  glue, #combining strings and objects
  gapminder, # dataset
  ggplot2, #plotting
  gridExtra, #arranging plots
  skimr
) 

library(knitr)

library(readr)
ds <- read_csv("~/Desktop/Vaccine Coverage and Disease Burden - WHO (2017).csv")

glimpse(ds)
str(ds)

ds %>%
  group_by(Entity) %>%
  summarise(
    mean(`BCG immunization coverage among 1-year-olds (WHO 2017)`, na.rm=TRUE),
    sd(`BCG immunization coverage among 1-year-olds (WHO 2017)`, na.rm=TRUE),
    sum(is.na(`BCG immunization coverage among 1-year-olds (WHO 2017)`)),
    mean(`Hepatitis B (HepB3) immunization coverage among 1-year-olds (WHO 2017)`, na.rm=TRUE),
    sd(`Hepatitis B (HepB3) immunization coverage among 1-year-olds (WHO 2017)`, na.rm=TRUE),
    sum(is.na(`Hepatitis B (HepB3) immunization coverage among 1-year-olds (WHO 2017)`)),
    mean(`DTP3 immunization coverage among 1-year-olds (WHO 2017)`, na.rm=TRUE),
    sd(`DTP3 immunization coverage among 1-year-olds (WHO 2017)`, na.rm=TRUE),
    sum(is.na(`DTP3 immunization coverage among 1-year-olds (WHO 2017)`)),
    mean(`Polio (Pol3) immunization coverage among 1-year-olds (WHO 2017)`, na.rm=TRUE),
    sd(`Polio (Pol3) immunization coverage among 1-year-olds (WHO 2017)`, na.rm=TRUE),
    sum(is.na(`Polio (Pol3) immunization coverage among 1-year-olds (WHO 2017)`)),
    mean(`Measles (MCV) immunization coverage among 1-year-olds (WHO 2017)`, na.rm=TRUE),
    sd(`Measles (MCV) immunization coverage among 1-year-olds (WHO 2017)`, na.rm=TRUE),
    sum(is.na(`Measles (MCV) immunization coverage among 1-year-olds (WHO 2017)`)),
    mean(`Number of confirmed tetanus cases (WHO 2017)`, na.rm=TRUE),
    sd(`Number of confirmed tetanus cases (WHO 2017)`, na.rm=TRUE),
    sum(is.na(`Number of confirmed tetanus cases (WHO 2017)`)),
    mean(`Number confirmed polio cases (WHO 2017)`, na.rm=TRUE),
    sd(`Number confirmed polio cases (WHO 2017)`, na.rm=TRUE),
    sum(is.na(`Number confirmed polio cases (WHO 2017)`)),
    mean(`Number of confirmed pertussis cases (WHO 2017)`, na.rm=TRUE),
    sd(`Number of confirmed pertussis cases (WHO 2017)`, na.rm=TRUE),
    sum(is.na(`Number of confirmed pertussis cases (WHO 2017)`)),
    mean(`Number of confirmed measles cases (WHO 2017)`, na.rm=TRUE),
    sd(`Number of confirmed measles cases (WHO 2017)`, na.rm=TRUE),
    sum(is.na(`Number of confirmed measles cases (WHO 2017)`)),
    mean(`Number of confirmed diphtheria cases (WHO 2017)`, na.rm=TRUE),
    sd(`Number of confirmed diphtheria cases (WHO 2017)`, na.rm=TRUE),
    sum(is.na(`Number of confirmed diphtheria cases (WHO 2017)`)),
    mean(`Estimated deaths due to tuberculosis per 100,000 population, excluding HIV (WHO 2017)`, na.rm=TRUE),
    sd(`Estimated deaths due to tuberculosis per 100,000 population, excluding HIV (WHO 2017)`, na.rm=TRUE),
    sum(is.na(`Estimated deaths due to tuberculosis per 100,000 population, excluding HIV (WHO 2017)`)),
    mean(`Estimated number of deaths due to tuberculosis, excluding HIV (WHO 2017)`, na.rm=TRUE),
    sd(`Estimated number of deaths due to tuberculosis, excluding HIV (WHO 2017)`, na.rm=TRUE),
    sum(is.na(`Estimated number of deaths due to tuberculosis, excluding HIV (WHO 2017)`))
  )



sum(is.na())
#part 1A data overview 
skim(ds)



#part 1B
#Vaccine_Coverage_and_Disease_Burden_WHO_2017_ <- Vaccine_Coverage_and_Disease_Burden_WHO_2017_ %>%
#rename(polio_imm = `Polio (Pol3) immunization coverage among 1-year-olds (WHO 2017)`, measles_imm = `Measles (MCV) immunization coverage among 1-year-olds (WHO 2017)`, polio_cases = `Number confirmed polio cases (WHO 2017)`, measles_cases = `Number of confirmed measles cases (WHO 2017)`)
#view(Vaccine_Coverage_and_Disease_Burden_WHO_2017_)

ds <- ds %>%
  select(Entity, Year, `Polio (Pol3) immunization coverage among 1-year-olds (WHO 2017)`, `Measles (MCV) immunization coverage among 1-year-olds (WHO 2017)`, `Number confirmed polio cases (WHO 2017)`, `Number of confirmed measles cases (WHO 2017)`) %>%
  rename(
    polio_imm = `Polio (Pol3) immunization coverage among 1-year-olds (WHO 2017)`,
    measles_imm = `Measles (MCV) immunization coverage among 1-year-olds (WHO 2017)`,
    polio_cases = `Number confirmed polio cases (WHO 2017)`,
    measles_cases = `Number of confirmed measles cases (WHO 2017)`
  ) %>%
  #filter(Entity == "China")
  view(ds)
skim(ds)
glimpse(ds)

ds %>%
  group_by(Entity) %>%
  summarise(
    across(c(mean(polio_imm, measles_imm, polio_cases, measles_cases, na.rm = TRUE)) %>% round(2))
  )

#my code
entity_range <- function(variable){
  ds %>%
    group_by(Entity) %>%
    summarize(range = max(variable, na.rm = TRUE) - min(variable, na.rm = TRUE))
}

polio_cases_range <- entity_range(ds$polio_cases)
print(polio_cases_range)

#chatgpt refined code
entity_range <- function(variable) {
  ds %>%
    group_by(Entity) %>%
    summarize(range = max({{ variable }}, na.rm = TRUE) - min({{ variable }}, na.rm = TRUE))
}
entity_range <- function(variable) {
  ds %>%
    group_by(Entity) %>%
    summarize(range = max(.data[[variable]], na.rm = TRUE) - min(.data[[variable]], na.rm = TRUE))
}

#i think this is final??
entity_range <- function(i) {
  ds %>%
    group_by(Entity) %>%
    summarize(range = max(ds[[i]], na.rm = TRUE) - min(ds[[i]], na.rm = TRUE))
}

for (i in c(5, 6)) {
  print(entity_range(i))
}

#function applied to polio cases
polio_cases_range <- entity_range(5)
print(polio_cases_range)

#function applied to measles cases 
measles_cases_range <- entity_range(6)
print(measles_cases_range)


#function applied to polio cases
polio_cases_range <- entity_range("polio_cases")
print(polio_cases_range)

#finding entity with most polio cases
max_range_entity_polio <- polio_cases_range%>%
  arrange(desc(range)) %>%
  slice(1)
print(max_range_entity_polio)

max_range_entity_polio <- polio_cases_range%>%
  arrange(desc(range))
print(max_range_entity_polio)

#function applied to measles cases
measles_cases_range <- entity_range("measles_cases")
print(measles_cases_range)

#finding entity with most measles cases. first, my code didnt include the filter, and my result was world. so i edited teh code so that it doesnt include world and reran it, and got china. 
max_range_entity_measles <- measles_cases_range %>%
  filter(Entity != "World") %>%
  arrange(desc(range)) %>%
  slice(1)

max_range_entity_measles <- measles_cases_range %>%
  filter(Entity != "World") %>%
  arrange(desc(range))



print(max_range_entity_measles)

#due to the large difference in polio and measles cases, and measles being a lot more of an issue, i chose to focus on measles in this research.
ds <- ds %>%
  select(Entity, Year, measles_cases, measles_imm) %>%
  filter(Entity == "China")
view(ds)

#china only has info for immunization from 1983 onwards, so we focus on those years.
ds <- ds %>%
  filter(Year>1982)
view(ds)
glimpse(ds)
#next, we add a column that shows cumulative cases
ds <- ds %>%
  mutate(cumulative_measles_cases = cumsum(measles_cases))
view(ds)


#we now have the final table of data that we're going to use for visualisation

#data visualising 
#create a plot for china measles case number and immunization rate 


ds %>%
  ggplot(aes(x = Year, y = measles_cases)) + geom_point() + labs(
    title = "Number of Measles Cases in China per year",
    x = "Year",
    y = "Number of Measles Cases"
  ) +
  geom_smooth() 

ds %>%
  ggplot(aes(x = Year, y = measles_imm)) + geom_point() + labs(
    title = "Measles Immunization Rate in China per year",
    x = "Year",
    y = "Measles Immunization Rate"
  ) +
  geom_smooth()

ds %>%
  ggplot(aes(x = Year, y = cumulative_measles_cases)) + geom_point() + labs(
    title = "Cumulative Measles Cases in China per year",
    x = "Year",
    y = "Cumulative Measles Cases"
  ) +
  geom_smooth()

ds %>%
  ggplot(aes(x = measles_imm, y = measles_cases)) + geom_point() + labs(
    title = "Number of Measles Cases vs Measles Immunization Rate in China between 1983-2015",
    x = "Measles Immunization Rate",
    y = "Number of Measles Cases"
  ) +
  geom_smooth() 

#final visualisation code
create_scatter_plot <- function(i) {
  ds %>%
    ggplot(aes_string(x = names(ds)[2], y = names(ds)[i])) +
    geom_point() +
    labs(
      title = glue("{names(ds)[i]} in China per year"),
      y = glue("{names(ds)[i]}")
    ) +
    geom_smooth()
}

plots_list <- map(3:ncol(ds), create_scatter_plot)
plots_grid <- gridExtra::grid.arrange(grobs = plots_list, ncol = 3)


#max(Vaccine_Coverage_and_Disease_Burden_WHO_2017_$measles_imm) - min(Vaccine_Coverage_and_Disease_Burden_WHO_2017_$measles_imm)

#entity_greatest_range <- function(x){
#Vaccine_Coverage_and_Disease_Burden_WHO_2017_ %>%
#group_by(Entity)
#summarize(
#range = max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
# )
#greatest_range_entity = filter(range == max(range, na.rm = TRUE))
#return(greatest_range_entity)


#entity_greatest <- entity_greatest_range(measles_imm)
#print(entity_greatest)






--
  
  #greatest_range_entity <- result %>%
  #filter(range == max(range, na.rm = TRUE))
  
  # Print the final result for debugging
  #print(greatest_range_entity)  # Show the entity with the greatest range
  
  #return(greatest_range_entity)
  #}
  
  
  --
  #creating function for renaming variables 
  
  rename_variable <- function(disease, action)
    rename()



