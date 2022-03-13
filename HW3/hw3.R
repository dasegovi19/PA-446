David Segovia

# HW3
# your final answers and your final R script can be uploaded via the link below:
# 
# FROM HERE ON OUT, MAKE SURE YOU USE THE FULL SALARIES DATASET, DETAILS BELOW
# Blackboard > PA 446 > Data files > salary_data_full
#

#---PROBLEM 1---
"Now that you are done with data wrangling, it is time to
synthesize everything you learned. Use the original data files: 
IL.TXT, census_2010_race.csv, and pa446_chicago_full.csv.
Clean all of them and create 1 master table.
The master table need to have the following columns:
last_name, first_name, job_titles, department, 
annual_salary (for both salaried and hourly employees),
race, gender.
SHOW YOUR WORK.


The final master table is titled "finaldf" and you can find it in line 174.



#if you write any code for the problem, please include your code/work here


```{r}

library(readr)
library(here)
library(tidyverse)
library(report) # useful to find statistical significance results
library(nortest) # normality test 


df <- read_csv("pa446_chicago_full.csv")
df_gender<- read.csv("IL.Txt", header=FALSE)
df_race <- read_csv("census_2010_race.csv")


```


Clean up the Chicago_Full dataset


```{r}
#separate name column
df_sep <- df %>%
  separate(Name, c('last_name', 'first_mid_name'), sep= "," )

df_sep2 <- df_sep %>%
  mutate(first_mid_name = str_trim(first_mid_name)) %>% 
  separate(first_mid_name, c('first_name', 'mid_name'), sep= " ")

df_sep2$first_name <- tolower(df_sep2$first_name) # change to lower




#unite annual_salary into one


df_sep2$annual_salary = 
  paste(df_sep2$`Annual Salary`, df_sep2$`Hourly Rate`)


df_sep2$annual_salary <- gsub("[^0-9.-]", "", df_sep2$annual_salary) #remove NA values
df_sep2


```

Clean up gender dataset

```{r}
colnames(df_gender) <- c("state","gender", "born_year", "first_name", "count")

# change classes
df_gender= 
  df_gender %>%
  mutate(state = as.character(state),
         gender = as.character(gender),
         born_year = as.integer(born_year),
         first_name = as.character(first_name),
         count = as.integer(count))

# distinct name regardless of year
df_gender_distinct <- df_gender %>% 
  distinct(gender, first_name) # distinct by name regardless of year

dim(df_gender_distinct) #12232 x 2

# remove duplicates
df_gender_slice <- df_gender %>%
  group_by(first_name) %>% 
  slice(which.max(count))     #

dim(df_gender_slice) # 11,402 x 5

df_gender_slice$first_name <- tolower(df_gender_slice$first_name)

```

Merge salary and gender dataset
```{r}
df_joined_3= left_join(df_sep2, df_gender_slice)
df_joined_3= select(df_joined_3, -born_year, -count) # remove born_year, count column
```

Clean up race column

```{r}
df_race$pctaian <- as.numeric(df_race$pctaian) # change classes
df_race$pctblack <- as.numeric(df_race$pctblack)
df_race$pctwhite <- as.numeric(df_race$pctwhite)
df_race$pctapi <- as.numeric(df_race$pctapi)
df_race$pct2prace <- as.numeric(df_race$pct2prace)
df_race$pcthispanic <- as.numeric(df_race$pcthispanic)



final_col <- c('name', 'pctwhite', 'pctblack', 'pctapi', 'pctaian', 'pct2prace', 'pcthispanic')

race_col <- c('pctwhite', 'pctblack', 'pctapi', 'pctaian', 'pct2prace', 'pcthispanic')

# select cols above

df_race_filter <- df_race %>%
  select(final_col)

# pivot longer
df_long <- 
  df_race_filter %>%
  pivot_longer(
    col= !name,  # or race_col
    names_to= 'race', 
    values_to= 'percent'
  )



# slice max

df_long_slice <- df_long %>%
  group_by(name) %>%
  slice(which.max(percent))

# remove pct
df_long_slice$race <- str_replace(df_long_slice$race, 
                                      '^pct', 
                                      '' )

# change to lower
df_long_slice$name <- tolower(df_long_slice$name)

# FINAL Dataset

df_finalrace = select(df_long_slice, name, race)


```

Merge gender and race
```{r}

DATA= 
  left_join(df_joined_3, df_finalrace, by = c("first_name" = "name"))
#rename 
DATA= rename(DATA, c("job_titles" = "Job Titles"))

#make salary numeric

DATA$annual_salary <- as.numeric(DATA$annual_salary)
```

#FINAL DATASET: 

FINAL Master Table with the following columns: 
last_name, first_name, job_titles, department, 
annual_salary (for both salaried and hourly employees),
race, gender.

```{r}
finaldf= 
  DATA %>% select(last_name, first_name, job_titles, Department, annual_salary, race, gender)

sum(is.na(finaldf$race)) #5174 missing race values
sum(is.na(finaldf$gender)) #2683 missing gender


```






#---PROBLEM 2---
"
As you already know, one of the mayor of Chicago's priority this year is equity in pay for city employees,
especially in some of the city's largest departments. 

Furthermore, equity is defined as pay equality between 
1) different genders and 
2) different races

Use your master table and see if there are general pay differences
between genders in the city's 5 largest departments.
Please also calculate the n-size for males and females in each department.
"


### Answer
In the Police Department, there is evidence of pay inequity. There are 2970 females and 9105 males. The male median wage is $90,024 which is $3,018 more than the median female wage of $87,006. The maximum male earner makes $260,004 while the maximum female earner makes $185,364. However, this is explained because the Police SuperIntendent is a male, and the position of "Superintendent" comes with a higher salary than the rest.

In the Fire Department, there is evidence of pay inequity. There are 422 females and 4133 males. The male median wage is $103,914 while the female median wage is $100,560. When looking at the median wage, males make ~$3,000 more. The maximum male earner in the Fire Department also makes $217,728 while the maximum female earner makes $197,736. The pay inequity between the max male and female earners in the Fire Department is much larger than the Police Department.

In the Streets & Sanitation Department, there is some evidence of pay inequity. There are 234 females and 1499 males. While the median pay for both male ($38.35/hr) and females ($39.02) are almost the same, females make 67 cents more. However, the max male earner makes $175,092 while the max female earner makes $131,664 a difference of $43,428.

In the water management department, there is some evidence of pay inequity. There are 234 females and 1499 males. The median pay per hr is basically the same for both male and females. They both make around $49/hr. Compared to the other departments, the Water Management Department pays the max female earner $159,036 and the max male earner $152,832. 

In the aviation department,  similarly to the water management department, there is some evidence of pay inequity in favor of females. There are 378 females and 1185 Males. The median pay per hr is $4 higher for females and the max female earner makes $87,276 more than the max male earner.



### End 

#if you write any code for the problem, please include your code/work here


First, let's define the 5 largest departments. We will define this as the department with the most number of employees. These are: Police, Fire, Streets & San, Water Management, and Aviation.

```{r}

table(finaldf$Department)


#Police: 13,135
#Fire: 4730 
#Streets & San: 2040
# Water MGMNT: 1867
# Aviation: 1766




```



Now, let's try to find pay differences between genders. We will use the fivenum() function to find minimum, 1st quartile, median, 3rd quartile, max. 

```{r}

#Police Dept: 

POLICE= finaldf %>%
  filter(Department == "POLICE")

POLICE %>%
  group_by(gender) %>%
  summarize(salarysummary= fivenum(annual_salary, na.rm=TRUE))

table(POLICE$gender) #2970 FEMALES, 9105 MALES 

#Females                        Males
#minimum: $9.74                 minimum: $12    
#1st quartile: $76,266          1st quartile:$84,054
# median: $87,006               median: $90,024
#3rd quartile: $93,354	        3rd quartile: $96,060
#max: $185,364	                max: $260,004


```



Fire Department!

```{r}

#Fire Dept:

FIRE= finaldf %>%
  filter(Department == "FIRE")

FIRE %>%
  group_by(gender) %>%
  summarize(salarysummary= fivenum(annual_salary, na.rm=TRUE))
  table(FIRE$gender)  #422 F, 4133 M

#Females                        Males
#minimum: $45288	             minimum: $52896    
#1st quartile: $88338          1st quartile:$98424	
# median: $100560              median: $103914
#3rd quartile: $114096	       3rd quartile: $116808
#max: $197736	                  max: $217728

  

```


Streets & San Department

```{r}
#Streets & San

STREETS= finaldf %>%
  filter(Department == "STREETS & SAN") 

STREETS %>%
  group_by(gender) %>%
  summarize(salarysummary= fivenum(annual_salary, na.rm=TRUE))

table(STREETS$gender) #367 F, 1472 M

#Females                        Males
#minimum: $21.73                minimum: $21.73    
#1st quartile: $30.68           1st quartile:$38.35
# median: $39.02                median: $38.35
#3rd quartile: $39.39	         3rd quartile: $39.39
#max: $131,664	               max: $175,092



```





Water Management Department

```{r}
# Water MGMNT

WATER= finaldf %>%
  filter(Department == "WATER MGMNT")

WATER%>%
  group_by(gender) %>%
  summarize(salarysummary= fivenum(annual_salary, na.rm=TRUE))

table(WATER$gender) # 234 F, 1499 M

#Females                        Males
#minimum: $18.950               minimum: $20.550    
#1st quartile: $44.400          1st quartile:$44.400
# median: $49.915               median: $49.830
#3rd quartile: $76932	         3rd quartile: $52.320
#max: $159036	                 max: $152832



```






Aviation Department

```{r}
# Aviation

AVIATION= finaldf %>%
  filter(Department == "AVIATION")

AVIATION %>%
  group_by(gender) %>%
  summarize(salarysummary= fivenum(annual_salary, na.rm=TRUE))

table(AVIATION$gender) #378 F, 1185 M

275004-187728

#Females                        Males
#minimum: $14                 minimum: $15    
#1st quartile: $38.35          1st quartile:$38.35
# median: $50.37              median: $46.50
#3rd quartile: $80,484	      3rd quartile: $66,852
#max: $275,004	               max: $187,728
```









#---PROBLEM 3---

"
Is the difference you observed in problem 2 statistically significant?
"

### Answer

POLICE: Yes, at a 95% confidence level, there is a significant difference in pay in the Police Dept among Males and Females.Males on average make $4,874 more than females. This is statistically significant p< .05.  

FIRE: Yes, at a 95% confidence level, there is a significant difference in pay between gender in FIRE Dept. This is border line significant p<.05. Males on average make $2537 more than females.

Streets & San: No, no significant difference in pay between gender in Streets & Sanitation department.

Water Management: Yes, at 95% confidence interval, there is a significant difference in gender pay in the Water Management Department. p<.05. Females on average make $21,070 more than males. 

Aviation: Yes, at 95% confidence interval, there is a significant difference in gender pay in the Aviation Department.p<.05  Females on average make $13977 more than males. 



### End 

#if you write any code for the problem, please include your code/work here



For this, we will use two sample t-Test (Independent Samples t-Test) for the 5 departments:







```{r}

# Police
t.test(POLICE$annual_salary ~ POLICE$gender) # Yes, at a 95% confidence level, there is a significant difference in pay in the Police Dept among Males and Females.


#Fire

t.test(FIRE$annual_salary ~ FIRE$gender) # Yes, at a 95% confidence level, there is a significant difference in pay between gender in FIRE Dept. This is border line significant p<.05 


#Streets & San

t.test(STREETS$annual_salary ~ STREETS$gender) # No, no significant difference in pay between gender in Streets & San department.



# Water MGMNT: 

t.test(WATER$annual_salary ~ WATER$gender) # Yes, at 95% confidence interval, there is a significant difference in gender pay in the Water Management Department. p<.05 

# Aviation: 

t.test(AVIATION$annual_salary ~ AVIATION$gender) # Yes, at 95% confidence interval, there is a significant diff. in gender pay in the Aviation Dept. 

```



#---PROBLEM 4---
"
Use your master table and see if there are general pay differences
between races in the city's 5 largest departments.
Please also calculate the n-size for each race subgroup in each department.
"


### Answer

In the Police Department, this is the n-size for each race.
  2 races     Akaska Native(aian)      Asian or Pacific Islander(API)      black   hispanic    white 
     1            36                      297                               1007     1232      8524 
By looking at the median pay, Black and white workers seem to make a little more than the rest. Alaska Native and those classified as 2 races ( which is only 1 worker) make less than the rest. There is evidence of pay inequity in the Police Dept. 



In the Fire department, this is the n-size for each race.
aian      api    black   hispanic    white 
  14      115      391      292      3583 
Looking at the median pay, black and white still make more than everybody else while Alaska Native make the least. These results are similar in the Police Department. There is evidence of pay inequity.


In the Streets & Sanitation department, this is the n-size for each race.
aian      api    black   hispanic    white 
 9        41      189      245       1203 
Looking at the median-pay, all races make the same, $38.35 per hr,  except for Alaska Native workers who make $39.39.


In the water management department, this is the n-size for each race.
aian      api    black hispanic    white 
 7       45      191      150     1277 
Looking at the median pay, Alaska Native and Asian or Pacific Islander(API) workers make a little more compared to the rest. Hispanic workers make the least in this department. There is evidence of pay inequity here.


In the aviation department, this is the n-size for each race.
aian      api    black hispanic    white 
  6       38      172      169     1051 


Looking at the median pay, Hispanic workers make significantly more than the rest, followed closely behind by white workers. API, AIAN, and black workers make $12 less than Hispanic workers at the median pay. 




### End


#if you write any code for the problem, please include your code/work here

We will use the median function to find pay differences between races in the 5 departments: Police, Fire, Streets & San, Water Management, and Aviation.





Visualize it first using boxplots. There seems to be more outliers in the white category compared to the rest. Also, 2 races seem to have only 1 value.This particular person who identifies as 2 races works for the Police Department only. 

```{r}
ggplot(finaldf) +
  aes(x = race, y = annual_salary) +
  geom_boxplot()
```





Police Department:
```{r}

#Police Dept: 
POLICE %>%
  group_by(race) %>%
  summarize(salarysummary= median(annual_salary, na.rm=TRUE)) %>%
  arrange(salarysummary)

table(POLICE$race)

  # 2 races     Alaska Native(aian)      Asian or Pacific Islander(API)      black   hispanic    white 
      #1            36                      297                               1007     1232      8524 






```





Fire Department
```{r}
#Fire Dept:

FIRE %>%
  group_by(race) %>%
  summarize(salarysummary= median(annual_salary, na.rm=TRUE)) %>%
  arrange(salarysummary)

  table(FIRE$race) 
  
  # aian      api    black hispanic    white 
   # 14      115      391      292     3583 
```







Streets & Sanitation
```{r}
#Streets & San

STREETS %>%
  group_by(race) %>%
  summarize(salarysummary= median(annual_salary, na.rm=TRUE)) %>%
  arrange(salarysummary)

table(STREETS$race) 
#aian      api    black   hispanic    white 
# 9        41      189      245       1203 

```








Water Management
```{r}
# Water MGMNT

WATER%>%
  group_by(race) %>%
  summarize(salarysummary= median(annual_salary, na.rm=TRUE)) %>%
  arrange(salarysummary)

table(WATER$race) 
#aian      api    black hispanic    white 
# 7       45      191      150     1277 

```







Aviation Department

```{r}
# Aviation

AVIATION %>%
  group_by(race) %>%
  summarize(salarysummary= median(annual_salary, na.rm=TRUE)) %>%
  arrange(salarysummary)

table(AVIATION$race) 
 #aian      api    black hispanic    white 
  # 6       38      172      169     1051 

```






#---PROBLEM 5---
"
Is the differences statistically significant?
"

### Answer

In the Police Department, when running the ANOVA test, there is a significant difference (p < .001) in pay between races. The Tukey-post Hoc result shows that there are significant pay differences between Hispanic and Black Officers as well as Hispanic and White officers. 

In the Fire Department, when running the ANOVA test, there is a significant difference in pay between races (p < .001). The Tukey Post-Hoc test shows significant pay differences between Hispanic and Black workers and Hispanic and White workers. The results are similar in the Police Department.

In the Streets & San Department, when running the ANOVA test, the P-value is 0.33, so we can conclude that there are no significant differences in pay between races. 

In the Water Management Department,  the ANOVA test shows p = 0.018 (p<.05), so there is a significant difference in pay between races. When running the post-hoc test, there appears to be significant pay differences between Asian or Pacific Islander (API)  and Black workers, API and Hispanic workers, and API and White workers. 

In the Aviation Department, the ANOVA test shows p = 0.008 (p<.05), so there is significant difference in pay between races. When running the post-hoc test, there appears to be significant pay differences between Hispanic and Black workers. 


### End 




#if you write any code for the problem, please include your code/work here


Police: p<.001, main effect of race is statistically significant

```{r}
#POLICE

### Anova 
police_aov <- aov(annual_salary ~ race,
  data = POLICE
)

summary(police_aov)
report(police_aov) # Yes, the main effect of race is statistically significant

# Post hoc

TukeyHSD(aov(POLICE$annual_salary ~ POLICE$race), conf.level=0.95) # The only significant difference in pay is between: Hispanic and Blacks, Hispanic and White


```


For the Fire Dept: effect of race on pay is statistically significant
```{r}
#FIRE

## Anova
fire_aov <- aov(annual_salary ~ race,
  data = FIRE
)

summary(fire_aov)
report(fire_aov)


#Tukey post hoc
TukeyHSD(aov(FIRE$annual_salary ~ FIRE$race), conf.level=0.95) # The only significant difference is between: Hispanic and Blacks, Hispanic and White

```


In the Streets & San: no evidence of pay inequity. Main effect of race is statistically not significant, p= 0.33
```{r}
#Streets & San


## Anova
streets_aov <- aov(annual_salary ~ race,
  data = STREETS
)

summary(streets_aov)
report(streets_aov)



#Tukey post hoc
TukeyHSD(aov(STREETS$annual_salary ~ STREETS$race), conf.level=0.95) # No significant difference to report here

```


Water management:

```{r}
# Water MGMNT


## Anova
water_aov <- aov(annual_salary ~ race,
  data = WATER
)

summary(water_aov)
report(water_aov)

# Tukey post hoc
TukeyHSD(aov(WATER$annual_salary ~ WATER$race), conf.level=0.95) # There is significant difference in pay between Asian or Pacific Islander (API)  and Black workers, API and Hispanic workers, and API and White workers. 



```


In the Aviation Department, there is significant difference in pay, p = 0.008
```{r}
# Aviation


## Anova
aviation_aov <- aov(annual_salary ~ race,
  data = AVIATION
)

summary(aviation_aov) 
report(aviation_aov)

#Tukey post hoc test
TukeyHSD(aov(AVIATION$annual_salary ~ AVIATION$race), conf.level=0.95) #Yes, only 1 sig. difference between Hispanic and Black p<.05


```








