################################################################################
                        # Exploration
################################################################################

fileUrl <- "https://d3c33hcgiwev3.cloudfront.net/_6ee2a5c3100b9237616844a52883e240_intro_data_prob_project.Rmd?Expires=1614902400&Signature=iFkNCx-BPGyrbScCW6Oo93XrgZO0hlr44w8hQNjQANFeohzO0WlR4owFi04~vH7-hE~BMMjd6-y9oAYkV2uO2cuBd1ICCmvNVe893LAz023gDlkUO5SBVFhxU2zE8PW7HOueRnQs2vkAvCps0mNW9gzCCczt0VMPLruj5tdARL0_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
download.file( fileUrl, destfile = "Project.Rmd", method = "curl")

library(dplyr)
library(ggplot2)
library(gridExtra)

library(hrbrthemes)
library(gmodels)
library(gridExtra)
library(viridisLite)
library(viridis)

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

levels(brfss2013$X_incomg) <- c("< $15,000", 
                                    "$15,000 to $25,000",
                                    "$25,000 to $35,000",
                                    "$35,000 to $50,000",
                                    "$50,000 > " 
)

levels(brfss2013Food$exerany2) <- c("Physical activity in past 30 day", 
                                "No physical activity in past 30 day")

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

levels(brfss2013Food$fruitVegeGroups) <- c("> 1 fruit/vegetable per day", 
                                       "From 1 to 3 fruit/vegetable per day",
                                       "< 3 fruit/vegetable per day")



plot1 <- brfss2013Food %>%
        filter(!is.na(brfss2013Food$X_rfhlth), !is.na(brfss2013Food$fruitVegeGroups)) %>%
        group_by(fruitVegeGroups,X_rfhlth) %>%
        summarize(n = n()) %>%
        mutate(freq = n/sum(n),
               posn_pct = cumsum(freq)-0.5*freq)
names(plot1)[2] <- c("Health_Status")
tapply(plot1$freq, plot1$fruitVegeGroups, sum)

ggplot(plot1, aes(fill = Health_Status , y = freq , x = fruitVegeGroups)) + 
        geom_bar(position ="stack", stat = "identity", width = .8, color="black") +
        geom_text(aes(label= round(freq, digits = 2), y=1-posn_pct), color="white") +
        ggtitle("Probability of having good or poor healt given the daily 
             consumption of fruit and vegetable") +
        ylab("Relative frequency") + 
        scale_fill_manual(values=c("steelblue2", "indianred2")) + 
        theme(axis.text.x=element_text(size=10), axis.title.x=element_blank()) 


s <- brfss2013Food  %>%  filter(!is.na(brfss2013Food$fruitVegeGroups))  %>%
  group_by(fruitVegeGroups) %>%
  summarise(count = n()) %>% 
  mutate(mar_prob = count/sum(count))
install.packages("kableExtra")
library(kableExtra)

tapply(vector, index, function)
##### Second question - ¿There are possible coufounders to the relationship?

## income

# income vs vege and Fruit



plot2 <- brfss2013Food %>%
        filter(!is.na(brfss2013Food$fruitVegeGroups), !is.na(brfss2013Food$X_incomg)) %>%
        group_by(X_incomg,fruitVegeGroups) %>%
        summarize(n = n()) %>%
        mutate(freq = n/sum(n),
               posn_pct = cumsum(freq)-0.5*freq)
names(plot2)[2] <- c("Level_of_consumption")


p2 <- ggplot(plot2, aes(fill = Level_of_consumption , y = freq , x = X_incomg)) + 
            geom_bar(position ="stack", stat = "identity", width = .8, color="black") +
            geom_text(aes(label= round(freq, digits = 2), y=1-posn_pct), color="white") +
            ggtitle("Probability of having a specific level of fruit/vegetable consumption
            given the level of income") +
            ylab("Relative frequency") + 
            theme(axis.text.x=element_text(size=8), axis.title.x=element_blank(), 
                  plot.title = element_text(hjust = 0.5)) 


# income vs health


plot3 <- brfss2013 %>%
        filter(!is.na(brfss2013$X_rfhlth), !is.na(brfss2013$X_incomg)) %>%
        group_by(X_incomg,X_rfhlth) %>%
        summarize(n = n()) %>%
        mutate(freq = n/sum(n),
               posn_pct = cumsum(freq)-0.5*freq)
names(plot3)[2] <- c("Health_Status")


p3 <- ggplot(plot3, aes(fill = Health_Status , y = freq , x = X_incomg)) + 
                geom_bar(position ="stack", stat = "identity", width = .8, color="black") +
                geom_text(aes(label= round(freq, digits = 2), y=1-posn_pct), color="white") +
                ggtitle("Probability of having good or poor healt given
                         the level of income") +
                ylab("Relative frequency") + 
                scale_fill_manual(values=c("steelblue2", "indianred2")) + 
                theme(axis.text.x=element_text(size=8), axis.title.x=element_blank()) 
        
grid.arrange(p2, p3, ncol=1)


grid.arrange(arrangeGrob(p2, top = "Probability
                         of having a specific level of fruit/vegetable consumption given
                         the level of income"),
             arrangeGrob(p3, top = "Probability of having good or poor healt given
                         the level of income"))





## exercise

# exercise vs vege and fruit
brfss2013FoodExce <- brfss2013Food %>% 
        filter(exeroft1 %in% (201:299)) %>% 
        mutate(ExerciPerMonth = 
                       exeroft1 - 200 )
# exercise vs health
p4 <- ggplot(brfss2013FoodExce, aes(x=ExerciPerMonth, y=averageDayFruitVeg)) +
        geom_point(alpha = 1/5, size = 1.5) +
        geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) 




plot5 <- brfss2013Food %>%
        filter(!is.na(brfss2013Food$exerany2), 
               !is.na(brfss2013Food$totFruitVegetables),
               totFruitVegetables < 300)
        
M <- plot5 %>% group_by(exerany2) %>% summarise(means= mean(totFruitVegetables))

ggplot(plot5, aes(x=exerany2, y=totFruitVegetables)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", position=position_dodge(.9), show.legend=FALSE, 
               color="black", size= 5)  + 
  geom_text(data = M, aes(y = means + 5, label=round(means,2)))




p5 <- ggplot(plot3, aes(fill = Health_Status , y = freq , x = exerany2)) + 
      geom_bar(position ="stack", stat = "identity", width = .8, color="black") +
      geom_text(aes(label= round(freq, digits = 2), y=1-posn_pct), color="white") +
      ggtitle("Probability of having good or poor healt given
                             the level of income") +
      ylab("Relative frequency") + 
      scale_fill_manual(values=c("steelblue2", "indianred2")) + 
      theme(axis.text.x=element_text(size=8), axis.title.x=element_blank()) 

grid.arrange(p4, p5, ncol=1)

##### third question - ¿if we control confounders, the relationship between vega and fruit
# and health still prevails?


plot1 <- brfss2013Food %>%
        filter(!is.na(brfss2013Food$X_rfhlth), 
               !is.na(brfss2013Food$fruitVegeGroups),
               !is.na(brfss2013Food$X_incomg)) %>%
        group_by(X_incomg,fruitVegeGroups,X_rfhlth) %>%
        summarize(n = n()) %>%
        mutate(freq = n/sum(n), posn_pct = cumsum(freq)-0.5*freq)


names(plot1)[3] <- c("Health_Status")

ggplot(plot1, aes(fill = Health_Status , y = freq , x = fruitVegeGroups)) + 
      geom_bar(position ="stack", stat = "identity", width = .8, color="black") +
      geom_text(aes(label= round(freq, digits = 2), y=1-posn_pct), color="white") +
      facet_grid(.~ X_incomg)+
      ggtitle("Probability of having good or poor healt given the daily 
                 consumption of fruit and vegetable") +
      ylab("Relative frequency") + 
      scale_fill_manual(values=c("steelblue2", "indianred2")) + 
      theme(axis.text.x=element_text(size=10), axis.title.x=element_blank()) 


# exercise

levels(brfss2013Food$exerany2) <- c("Physical activity in past 30 day", 
                                    "No physical activity in past 30 day")

tapply(brfss2013Food$totFruitVegetables, brfss2013Food$exerany2, mean)

plot2 <- brfss2013Food %>% 
  filter(!is.na(brfss2013Food$totFruitVegetables), 
         !is.na(brfss2013Food$X_rfhlth),
         !is.na(brfss2013Food$exerany2), totFruitVegetables < 300)

names(plot2)[which(names(plot2) == "X_rfhlth")] <- c("Health_Status")

M <- plot2 %>% group_by(exerany2,X_rfhlth) %>% 
  summarise(means= mean(totFruitVegetables))

means <- M$means
ggplot(plot2, aes(x=exerany2, y=totFruitVegetables)) +
  geom_boxplot(aes(fill=X_rfhlth), position=position_dodge(.9)) +
  stat_summary(fun.y=mean, geom="point", aes(group=X_rfhlth), position=position_dodge(.9),show.legend=FALSE, 
               color="black", size= 10) +
  scale_fill_manual(values=c("steelblue2", "indianred2")) +  
  geom_text(data = M, aes(label = means, y = means + 0.08))


means <- aggregate(weight ~  group, PlantGrowth, mean)



