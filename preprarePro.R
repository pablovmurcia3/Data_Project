################################################################################
                        # Exploration
################################################################################

fileUrl <- "https://d3c33hcgiwev3.cloudfront.net/_6ee2a5c3100b9237616844a52883e240_intro_data_prob_project.Rmd?Expires=1614902400&Signature=iFkNCx-BPGyrbScCW6Oo93XrgZO0hlr44w8hQNjQANFeohzO0WlR4owFi04~vH7-hE~BMMjd6-y9oAYkV2uO2cuBd1ICCmvNVe893LAz023gDlkUO5SBVFhxU2zE8PW7HOueRnQs2vkAvCps0mNW9gzCCczt0VMPLruj5tdARL0_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
download.file( fileUrl, destfile = "Project.Rmd", method = "curl")

install.packages("viridis")
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(gmodels)
library(gridExtra)
library(viridis)
library(viridisLite)

# Download and load data
fileUrl <- "https://d3c33hcgiwev3.cloudfront.net/4tiY2fqCQa-YmNn6gnGvzQ_1e7320c30a6f4b27894a54e2de50a805_brfss2013.RData?Expires=1614902400&Signature=cj3ZrzacK4iKJSar8jn3UrI1noO0MmmYrN4qf68nA9nMwVYyhWUd52vosLmnC4p4ti9tbsJAdShV~QsJHE2cnEgzxspWk2YeYaBYW6SZq-86MpZd4VgyY72bZ~OnHB0UFxrHqmZQpD8JiNM9w-m61nj9tFq3hDGgOrt78xczoIY_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
download.file( fileUrl, destfile = "brfss2013.gz", method = "curl")
library(R.utils)
gunzip("brfss2013.gz")
load("brfss2013.RData")


# Understand data


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
##### first question - ¿relationship between eating food and vegetables and good health?

# create variable
brfss2013Food <- brfss2013 %>% 
        filter(fruitju1 %in% (301:399) &
                       fruit1 %in% (301:399) &
                       fvgreen %in% (301:399) &
                       fvorang %in% (301:399) &
                       vegetab1 %in% (301:399))  %>% 
        mutate(totFruitVegetables = 
                       fruitju1 + fruit1 + fvgreen + fvorang + vegetab1-1500) %>%
        mutate(averageDayFruitVeg = totFruitVegetables/30.4167)


brfss2013Food <- brfss2013Food %>%
        mutate(fruitVegeGroups = 
                       cut(brfss2013Food$averageDayFruitVeg,breaks = c(0,1,3,16.27395)))

levels(brfss2013Food$fruitVegeGroups) <- c("Less than 1 fruit/vegetable per day", 
                                       "From 1 to 3 fruit/vegetable per day",
                                       "More than 3 fruit/vegetable per day")


crosstable <- CrossTable(brfss2013Food$X_rfhlth, brfss2013Food$fruitVegeGroups)
crosstable1 <- as.data.frame(crosstable[["prop.col"]])

ggplot(crosstable1, aes(fill = x ,y = Freq , x = y)) + 
        geom_bar(position ="stack", stat = "identity")


##### Second question - ¿There are possible coufounders to the relationship?

## income

# income vs vege and Fruit
crosstable <- CrossTable(brfss2013Food$fruitVegeGroups, brfss2013Food$X_incomg)
crosstable1 <- as.data.frame(crosstable[["prop.col"]])
ggplot(crosstable1, aes(fill = crosstable1$x ,y = crosstable1$Freq , x =crosstable1$y)) + 
        geom_bar(position ="stack", stat = "identity")

# income vs health

crosstable <- CrossTable(brfss2013Food$X_rfhlth, brfss2013Food$X_incomg)
crosstable1 <- as.data.frame(crosstable[["prop.col"]])
ggplot(crosstable1, aes(fill = crosstable1$x ,y = crosstable1$Freq , x =crosstable1$y)) + 
        geom_bar(position ="stack", stat = "identity")

## exercise

# exercise vs vege and fruit
brfss2013FoodExce <- brfss2013Food %>% 
        filter(exeroft1 %in% (201:299)) %>% 
        mutate(ExerciPerMonth = 
                       exeroft1 - 200 )
# exercise vs health
p <- ggplot(brfss2013FoodExce, aes(x=ExerciPerMonth, y=averageDayFruitVeg)) +
        geom_point(alpha = 1/5, size = 1.5) +
        geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) 


crosstable <- CrossTable(brfss2013Food$X_rfhlth, brfss2013Food$exerany2)
crosstable1 <- as.data.frame(crosstable[["prop.col"]])
ggplot(crosstable1, aes(fill = crosstable1$x ,y = crosstable1$Freq , x =crosstable1$y)) + 
        geom_bar(position ="stack", stat = "identity")

##### third question - ¿if we control confounders, the relationship between vega and fruit
# and health still prevails?

# Income

brfss2013Foodless15 <- brfss2013Food %>% filter(X_incomg == "Less than $15,000") 
crosstable <- CrossTable(brfss2013Foodless15$X_rfhlth, brfss2013Foodless15$fruitVegeGroups)
crosstable1 <- as.data.frame(crosstable[["prop.col"]])
names(crosstable1) <- c("Health_Status", "y", "Freq")
p1 <- ggplot(crosstable1, aes(fill = Health_Status ,y = Freq , x = y)) + 
        geom_bar(position ="stack", stat = "identity") +
        ggtitle("Less than $15,000") + 
        ylab("Relative Frequency") + 
        scale_fill_manual(values=c("steelblue2", "indianred2")) + 
        theme(axis.text.x=element_text(size= 6.3), axis.title.x=element_blank())

brfss2013Food15to25 <- brfss2013Food %>% filter(X_incomg == "$15,000 to less than $25,000") 
crosstable <- CrossTable(brfss2013Food15to25$X_rfhlth, brfss2013Food15to25$fruitVegeGroups)
crosstable1 <- as.data.frame(crosstable[["prop.col"]])
names(crosstable1) <- c("Health_Status", "y", "Freq")
p2 <-ggplot(crosstable1, aes(fill = Health_Status ,y = Freq , x = y)) + 
        geom_bar(position ="stack", stat = "identity") +
        ggtitle("$15,000 to less than $25,000") +
        ylab("Relative Frequency") + 
        scale_fill_manual(values=c("steelblue2", "indianred2")) + 
        theme(axis.text.x=element_text(size=6.3), axis.title.x=element_blank())       

brfss2013Food125to35 <- brfss2013Food %>% filter(X_incomg == "$25,000 to less than $35,000") 
crosstable <- CrossTable(brfss2013Food125to35$X_rfhlth, brfss2013Food125to35$fruitVegeGroups)
crosstable1 <- as.data.frame(crosstable[["prop.col"]])
names(crosstable1) <- c("Health_Status", "y", "Freq")
p3 <-ggplot(crosstable1, aes(fill = Health_Status ,y = Freq , x = y)) + 
        geom_bar(position ="stack", stat = "identity") +
        ggtitle("$25,000 to less than $35,000") +
        ylab("Relative Frequency") + 
        scale_fill_manual(values=c("steelblue2", "indianred2")) + 
        theme(axis.text.x=element_text(size=6.3), axis.title.x=element_blank())          

brfss2013Food135to50 <- brfss2013Food %>% filter(X_incomg == "$35,000 to less than $50,000") 
crosstable <- CrossTable(brfss2013Food135to50$X_rfhlth, brfss2013Food135to50$fruitVegeGroups)
crosstable1 <- as.data.frame(crosstable[["prop.col"]])
names(crosstable1) <- c("Health_Status", "y", "Freq")
p4 <-ggplot(crosstable1, aes(fill = Health_Status ,y = Freq , x = y)) + 
        geom_bar(position ="stack", stat = "identity") +
        ggtitle("$35,000 to less than $50,000") +
        ylab("Relative Frequency") + 
        scale_fill_manual(values=c("steelblue2", "indianred2")) + 
        theme(axis.text.x=element_text(size=6.3), axis.title.x=element_blank())  

brfss2013Food50 <- brfss2013Food %>% filter(X_incomg == "$50,000 or more") 
crosstable <- CrossTable(brfss2013Food50$X_rfhlth, brfss2013Food50$fruitVegeGroups)
crosstable1 <- as.data.frame(crosstable[["prop.col"]])
names(crosstable1) <- c("Health_Status", "y", "Freq")
p5 <-ggplot(crosstable1, aes(fill = Health_Status ,y = Freq , x = y)) + 
        geom_bar(position ="stack", stat = "identity") +
        ggtitle("$50,000 or more") +
        ylab("Relative Frequency") + 
        scale_fill_manual(values=c("steelblue2", "indianred2")) + 
        theme(axis.text.x=element_text(size=6.3), axis.title.x=element_blank())  

grid.arrange(p1,p2,p3,p4,p5, ncol=2)

table(brfss2013Food$X_incomg)

# Income -- neh
p <- ggplot(brfss2013FoodExce, aes(x=ExerciPerMonth, y=averageDayFruitVeg, colour =X_rfhlth)) +
  geom_point( size = 1.5) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) 

