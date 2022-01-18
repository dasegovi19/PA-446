#David Segovia
# PA 446

# HW1
#your final answers and your final R script can be uploaded via the link below:
#https://forms.gle/qo6XoLamepNuJNcT9

"One of the mayor of Chicago's priority this year is equity in pay for city employees,
especially in some of the city's largest departments. Her office has provided you a dataset
to help figure out where pay inequity might currently exist.
"

#---PROBLEM 1---

"
Pay equity amongst city employees is a large and vague problem.
See if you can build more clarity by asking 4 clarifying questions for
the prompt above.
"


#1) Do men and women in the same department with the same experience and job title receive the same pay? Is there any evidence of gender inequality? 

#2) Is the median/mean pay of some departments more than others ? For example, do first respondents like CFD and CPD make the same or is one department paid more than the other?

#3) How much more money do the top earners in big departments make compared to the median/average pay in that same department?

#4) Do some city departments pay their workers well below the minimum wage? 



#if you write any code for the problem, please include your code/work here

```{r}
library(readr)
library(tidyverse)

data1 <- read_csv("data1.csv")
data1 #31,770 entries. 8 columns
dim(data1)
ncol(data1)
nrow(data1)

head(data1$`Job Titles`)

null_vector <- c('a', 'b', NULL, 'd', NULL, 'f')
na_vector <- c('a', 'b', NA, 'd', NA, 'f')
print(null_vector)
print(na_vector)

summary(null_vector)
summary(na_vector)



```



```{r}
summary(data1) # 31,770 missing values. This also gives descriptive statistics for hourly and yearly salary. Notice how for yearly salary, the minimum salary is -260,004. However, this is because the salaries of the police department have a negative sign placed in front of the number. This salary belongs to superintendent of police David Brown, who makes $260K a year.

sum(is.na(data1$gender)) # tibble that is T or F

```






#---PROBLEM 2---
"
1. Open the dataset
2. Before you do any analysis on the data, you have to ensure the data is ready for analysis.
Use the R functions you been taught to carefully examine each column - SHOW YOUR WORK. 
Find 3 issues with the data that are either problematic for future analysis you want to do
or just not best practice for tidyverse data analysis.
"



#1) We don't have any data on the Gender of the city employees. This is really important because pay equity should include gender. 
#While we can assume ( but probably shouldn't ) the gender of each individual based on the name, we would still need the values for the variable so we can run some descriptive statistics and run functions such as grouping average salary by gender. This would have been useful if we wanted to compare the average/median salaries of city workers in certain departments with the same job titles.

#2) There are way too many missing values because  employees are either paid annually or hourly. When they are paid annually, the hourly wages are NA and vice versa. 

#3) The salaries of police department are negative. This makes it hard to run summary statistics. I will probably need to take the absolute value of the yearly and hourly salary columns.


#if you write any code for the problem, please include your code/work here


#I will make salary numeric by removing dollar sign and commas. I will also remove negative values by taking the absolute value of yearly and hourly salaries

```{r}

data1$`Yearly Salary` = as.numeric(gsub("[\\$,]", "", data1$`Yearly Salary`))
data1$`Hourly Salary` = as.numeric(gsub("[\\$,]", "", data1$`Hourly Salary`))
data1$`Yearly Salary` <- abs(data1$`Yearly Salary`)
data1$`Hourly Salary` <- abs(data1$`Hourly Salary`)

```





#Examine each column

```{r}
table(data1$Department) # there are 36 departments
table(data1$`Full or Part-Time`) #30,553 city workers are considered full-time city employees, 1217 employees considered part-time
table(data1$`Salary or Hourly`) # 6939 city employees are paid hourly whereas 24,831 employees are paid by salary.

```




```{r}

summary(data1$`Yearly Salary`) # the minimum yearly salary is $20,400. The median salary is $90,024. The mean yearly salary is $92,386. The maximum yearly salary is $275,004. 
#There are 6939 missing values here because these employees are paid by salary.

summary(data1$`Hourly Salary`)  # for those who are paid hourly, the minimum hourly wage is $3.00 per hr, the median is $38.35  per hr, mean is $37.60 per hr, and Max is $128 hr.

```




 
#Summarize mean/median yearly salary by department

```{r}
yearlysalary= 
  data1 %>% 
  group_by(Department) %>% 
  summarize(meansalary= mean(`Yearly Salary`, na.rm=T), 
            mediansalary= median(`Yearly Salary`, na.rm = T)) %>%
  arrange(mediansalary)
  
yearlysalary




hourlysalary= 
  data1 %>% 
  group_by(Department) %>% 
     summarize(meansalary= mean(`Hourly Salary`, na.rm=T), 
            mediansalary= median(`Hourly Salary`, na.rm = T)) %>%
  arrange(meansalary) %>%
   filter(meansalary != "NaN")

hourlysalary

```





#For city workers paid by yearly salary, it seems like, on average, city workers at the buildings department make more than others. 
#For example, they make $109,789 yearly on average ($110,460 median) compared to Board of Election workers who only make $53,331 on average ($45,720 median pay). 
#Fire department workers make $105,733 yearly on average whie CPD workers who make $90,618.47. Since both CPD and CFD are first responders, they should both probably make the same.

#For city workers paid by hourly, family & support workers make the least out of all the departments. They only make $5.83 hourly on average while public safety administrators make $50.17 hourly on average. 
#In fact, out of the 19 departments that pay some of their employees hourly,  3 of them ( Family & Support, Housing & Economic Development, Business Affairs) pay them below the minimum wage of $15.00 on average. 
#The median pay for 4 departments is below the minimum wage, which includes the previous 3 departments plus the Law department.



#Let's look at a department like CPD


```{r}
CPD= 
data1 %>% 
    filter(Department == "POLICE") %>%
  arrange(`Yearly Salary`)
  
CPD
mean(CPD$`Yearly Salary`, na.rm = T)

```



  

#The top earner in CPD is David Brown, SuperIntendent of Police, who makes $260,004 annually. On average, police officers make $90,618.47. There is some pay inequity here but let's look at CFD


```{r}
CFD= 
data1 %>% 
    filter(Department == "FIRE") %>%
  arrange(desc(`Yearly Salary`))
  

mean(CFD$`Yearly Salary`, na.rm = T)



```





#The top earner in CFD is Fire Commissioner Richard C Ford, who makes $217,728 annually. This is less than David Brown's salary. However, workers at CFD on average make $105,733. So while CPD has the top earner, CFD pays their city workers more on average. 






## in class

```{r}
library(readr)
library(tidyverse)

data1 <- read_csv("data1.csv")
```


Make this an integer using str_replace and not gsub()



```{r}
max(data1$`Yearly Salary`)


data1 <- data1 %>%
  mutate(
    yearly_salary_numeric=str_replace(data1$`Yearly Salary`, "\\$", "") 
    ) %>% 
  mutate(
    yearly_salary_numeric=str_replace(yearly_salary_numeric, "\\,", "")
    %>% as.numeric() 
)






data1

tail(data1$yearly_salary_numeric)

max(data1$yearly_salary_numeric, na.rm=T)
summary(data1$yearly_salary_numeric)
  



```


```{r}

#rbind creates a row in the dataset

df_millionaire <- data1 %>%
  rbind(c('Jeffery P Bezos', 'Chief Technology Officer', 
          "Department of Innovation and Technology", 
          "F", "Salary", NA,'$100,000,000.00', NA))
tail(df_millionaire$`Yearly Salary`)



df_millionaire_numeric <- df_millionaire %>%
  mutate(
    yearly_salary_numeric=str_replace(`Yearly Salary`, "\\$", "")
  ) %>%
  mutate(
    yearly_salary_numeric=str_replace(yearly_salary_numeric, "\\,", "")
    %>% as.numeric()
  )



tail(df_millionaire_numeric$yearly_salary_numeric)




```

