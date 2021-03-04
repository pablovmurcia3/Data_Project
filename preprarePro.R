################################################################################
                        # Exploration
################################################################################

fileUrl <- "https://d3c33hcgiwev3.cloudfront.net/_6ee2a5c3100b9237616844a52883e240_intro_data_prob_project.Rmd?Expires=1614902400&Signature=iFkNCx-BPGyrbScCW6Oo93XrgZO0hlr44w8hQNjQANFeohzO0WlR4owFi04~vH7-hE~BMMjd6-y9oAYkV2uO2cuBd1ICCmvNVe893LAz023gDlkUO5SBVFhxU2zE8PW7HOueRnQs2vkAvCps0mNW9gzCCczt0VMPLruj5tdARL0_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
download.file( fileUrl, destfile = "Project.Rmd", method = "curl")

library(dplyr)
library(ggplot2)
# Download and load data
fileUrl <- "https://d3c33hcgiwev3.cloudfront.net/4tiY2fqCQa-YmNn6gnGvzQ_1e7320c30a6f4b27894a54e2de50a805_brfss2013.RData?Expires=1614902400&Signature=cj3ZrzacK4iKJSar8jn3UrI1noO0MmmYrN4qf68nA9nMwVYyhWUd52vosLmnC4p4ti9tbsJAdShV~QsJHE2cnEgzxspWk2YeYaBYW6SZq-86MpZd4VgyY72bZ~OnHB0UFxrHqmZQpD8JiNM9w-m61nj9tFq3hDGgOrt78xczoIY_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
download.file( fileUrl, destfile = "brfss2013.gz", method = "curl")
library(R.utils)
gunzip("brfss2013.gz")
load("brfss2013.RData")


# Understand data

str(brfss2013)

#  Some interesting variables

# Year and months
unique(brfss2013$iyear)
table(brfss2013$iyear)
unique(brfss2013$fmonth)
table(brfss2013$fmonth)

brfss2013 <- brfss2013 %>% mutate(day = paste(fmonth, iday, sep = " "))
unique(brfss2013$day)
grep("NA",brfss2013$day, value = TRUE )
sort(table(brfss2013$day))

# Satisfaction with life ------ One response variable
unique(brfss2013$lsatisfy)
table(brfss2013$lsatisfy, useNA = "ifany")

# Time spent sleeping
unique(brfss2013$sleptim1)
table(brfss2013$sleptim1)

# Questionnaire version
unique(brfss2013$qstver)
table(brfss2013$qstver)


# Numbers od days mental health not good
class(brfss2013$menthlth)
unique(brfss2013$menthlth)
table(brfss2013$menthlth)

# sleep pattern
class(brfss2013$sleptim1)
unique(brfss2013$sleptim1)
table(brfss2013$sleptim1)

# Body mass index

table(brfss2013$X_bmi5, useNA = "ifany")

# Income categories

table(brfss2013$X_incomg, useNA = "ifany")

# Vegetables and fruit

table(brfss2013$fruitju1, useNA = "ifany")


#### Consumption vegetables and fruit reduce blood pressure


brfss2013 %>% filter(brfss2013$qstver == "Only Version Landline")  %>% View()
brfss2013 %>% filter(brfss2013$qstver == "Only Version Cell Phone")  %>% View()
################################################################################
# Look variables with few NA

noNA <- apply(brfss2013, 2, function(x) {
        sum(!is.na(x))
        })

sort(noNA)

noNA[names(noNA) == "lsatisfy"]
################################################################################

class(brfss2013$lsatisfy)

# Exploratory graph

expe <- brfss2013 %>% filter(!is.na(lsatisfy)) %>% group_by(fmonth) %>% 
        summarise(percVerySatis = sum(lsatisfy == "Very dissatisfied",
                                      na.rm = TRUE)/n()*100, count =n())  
        
ggplot(data = expe , aes(x=fmonth, y=percVerySatis, group = 1)) + geom_line()


expe <- brfss2013 %>% filter(!is.na(lsatisfy)) %>% group_by(fmonth) %>% 
        summarise(percVerySatis = mean(sleptim1, na.rm = TRUE), count =n())  

ggplot(data = expe , aes(x=fmonth, y=percVerySatis, group = 1)) + geom_line()


