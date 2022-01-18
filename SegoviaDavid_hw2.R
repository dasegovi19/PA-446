# HW2
# your final answers and your final R script can be uploaded via the link below:
# https://forms.gle/pJLZUkj8zpHvqUDi6
# FROM HERE ON OUT, MAKE SURE YOU USE THE FULL SALARIES DATASET, DETAILS BELOW
# Blackboard > PA 446 > Data files > salary_data_full


```{r}
library(tidyverse)
library(readr)
setwd("~/Downloads/PA 446 Coding Civic Data Applications/Homeworks/HW 2")


data <- read_csv("pa446_chicago_full.csv")

data2 <- read.csv("~/Downloads/PA 446 Coding Civic Data Applications/Homeworks/HW 2/IL.Txt", header=FALSE)




```


"Now that you have a more complete dataset, 
aligned on goals with the client,
and have a basic understanding of the data wrangling needed, 
you are finally ready to begin data wrangling.
"


#---PROBLEM 1---

"
Break out first, middle and last name into their distinct columns.
Name your new columns first_name, middle_name, last_name.
"

#if you write any code for the problem, please include your code/work here

```{r}
data

## \\s= any white-space
## \\b= matches empty string at either side

data1R = 
  data %>% separate(Name, c("last_name", "first_name"), sep=',') %>%
  separate(first_name,c("first_name","middle_name"),sep="\\b\\s\\b") %>%
  select(last_name, middle_name, first_name, everything())

#strip leading and trailing space in first data so I can  merge this dataset later
data1R$first_name <- trimws(data1R$first_name, which = c("both"))





```






#---PROBLEM 2---
"
We need to one (unite?) annual_salary for both hourly and salaried employees.
Figure out how to coalesce hourly and annual salary together for these
2 types of workers and create a single column called annual_salary
"

#if you write any code for the problem, please include your code/work here



```{r}

data1R$annual_salary = 
  paste(data1R$`Annual Salary`, data1R$`Hourly Rate`)


data1R$annual_salary <- gsub("[^0-9.-]", "", data1R$annual_salary) #remove NA values
data1R




```








#---PROBLEM 3---
"
You are missing gender data. However, you are very scrappy and found the
Social Service Administration's data (https://www.ssa.gov/OACT/babynames/limits.html)
for Illinois residents.
The file is called IL.TXT and is available on Blackboard.
Use this data to identify the gender of as many individuals
in the Chicago dataset as possible.
The end product should be a new column in the Chicago dataset, called new_gender.
While new_gender will have more non-NA values than the old gender column,
some rows can still be NA.
REMEMBER TO SHOW YOUR WORK
"


#if you write any code for the problem, please include your code/work here

rename column names and capitalize gender names
```{r}
data2 <- read.csv("~/Downloads/PA 446 Coding Civic Data Applications/Homeworks/HW 2/IL.Txt", header=FALSE)
names(data2) <- c('state', 'new_gender', 'year', 'firstname', 'occurences')
data2$first_name = toupper(data2$firstname)
data2

```


### I got first_name and gender by itself, just once. However, some names appear twice because they can be both female and male. 
```{r}

data3= 
  data2 %>%
  group_by(first_name,new_gender) %>%
  summarize(total = n())
  
data3 #12,232 rows


table(data3$first_name) # some names can be both male and female


```

Now, we will use a case_when function that will remove duplicates.

Use duplicate and case_when functions here. Only keep the row if the "total column" is higher. In this case, we will assume gender based on the 'total' column.


```{r}

data4 <- data3 %>%
  arrange(first_name, -total) %>%
  filter(duplicated(first_name) == FALSE) %>%
  select(first_name, new_gender)
  

data4 #11,402 rows



```



combine data. 

```{r}

data1R #31,770 rows
data4 #11, 402 rows

finaldata= left_join(data1R, data4, by = "first_name") # merge
finaldata = select(finaldata, -9, -10) # remove hourly rate and annual salary columns because we already have annual_salary
finaldata

 

```

Check for descriptive statistics

```{r}
table(finaldata$new_gender)  #7890 Male;  21,197 Female


sum(is.na(finaldata$new_gender)) #2683 missing values

```



FINAL DATA

```{r}

finaldata


```


#---PROBLEM 4---
"
A large part of equity also has to do with race. Find two data
sources that can help you create a race column in the Chicago data.
YOU DO NOT NEED TO ACTUALLY WRANGLE A RACE COLUMN, 
JUST FIND THE DATA SOURCES ONLINE AND SHARE THE URL.
"


1) https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/TYJKEZ/MPMHFE&version=1.3

2) https://www.nature.com/articles/sdata201825

3) https://cran.r-project.org/web/packages/predictrace/vignettes/Predict-race-of-surname.html 



#if you write any code for the problem, please include your code/work here





