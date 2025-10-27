# Loading libraries
library("data.table")
library("plyr")
library("dplyr")
library("readr")                               
library("tibble")
library("ggplot2")
library("purrr")
library(tidyr)

# Read and lightly preprocess for Human Data

dta <- read.csv("/Experiment1//humanExp1_data.csv")

## Define cond, cat and ID as a factor
dta$cond = factor(dta$cond)
dta$cat = factor(dta$cat)
dta$ID = factor(dta$ID)
##Summarise human accuracy by subject number, category and duration
# Mean accuracy of categories and conditions for each participants
human_ID <- dta %>%
  group_by(ID, cat, cond) %>% dplyr::summarise(mean = mean(corr))
humanID_mean <- human_ID %>% select(ID, cond, mean)%>% group_by(ID, cond) %>% summarise(mean=mean(mean))

##Load BayesFactor package for anovaBF
library(BayesFactor)
library(MatrixModels)

##Run anovaBF analysis for Human data
bf <- anovaBF(formula = mean ~ cond*cat + ID,
              data = data.frame(human_ID), whichRandom = "ID")
bf

##summarise and group by cond and cat to see the direction 
human_ID %>% group_by(cond) %>% summarise(mean(mean))
human_ID %>% group_by(cat) %>% summarise(mean(mean))
human_ID %>% group_by(cat, cond) %>% summarise(mean(mean))

##Interaction between variables
bf[4] / bf[3]


# Density plot
humanID_mean %>% ggplot(aes(mean, colour=factor(cond))) + geom_density(aes(y=..scaled..)) 
splt <- humanID_mean %>% pivot_wider(names_from = cond, values_from = mean)

splt <- splt %>% 
  set_names(c("ID", "Human_50ms", "Human_200ms"))

sdiff <- splt %>% mutate(diff = Human_200ms - Human_50ms)

# Density graph
sdiff %>% ggplot(aes(diff)) +
  geom_density(aes(y=..scaled..)) +
  geom_vline(xintercept = 0, colour = 'red') +
  xlab("200ms - 50ms Mean Accuracy") +
  ylab("Scaled density")

# HUMAN VS COMPUTER

all_models <- read.csv("Experiment1/ft_models_data.csv", header = TRUE)
all_models <- all_models %>% group_by(Category, who) %>% summarise(mean = mean(accuracy))
coca_acc <-all_models[all_models$who == "CoCa_ViT_L14", ] %>% select(Category, mean, who)

human_ID <- human_ID%>% rename("Category" = "cat") 
human_split<- human_ID %>% mutate(who = case_when(cond == 50 ~ 'Human_50ms',
                                                  cond == 200 ~ 'Human_200ms'))

## Bayes Factor One sample t-test analysis
##between overall category accuracies of humans(50ms&200ms) and the DCNNs
hmean <- human_split %>% group_by(Category, who) %>% summarise(mean = mean(mean))
hmean 
all_model <-all_models %>% select(Category, mean, who)
all_model

objcompare <- function(this.cond, this.detector) {
  onetest <- hmean %>% filter(who == this.cond) 
  objnet <- all_model %>% filter(who == this.detector)
  dframe <- rbind(onetest, objnet)
  ttestBF(formula = mean ~ who, data = data.frame(dframe))
}


detectors <- unique(all_model$who)
detectors 
for(this.cond in c("Human_50ms", "Human_200ms")) {
  print(this.cond)
  for(this.detector in detectors) {
    print(this.detector)
    print(objcompare(this.cond, this.detector))
  }
}

## Effect size and t-test analysis  for humans and fine-tuned coca
human50 <- hmean %>% filter(who=="Human_50ms")
human200 <- hmean %>% filter(who=="Human_200ms")
best50 <- rbind(coca_acc, human50)
best200 <- rbind(coca_acc, human200)
library('effectsize')
cohens_d(mean ~ who,data = best50)
cohens_d(mean ~ who, data = best200)
t.test(best50$mean ~ best50$who)
t.test(best200$mean ~ best200$who)
best50 <- data.frame(best50)
best50$Category <- factor(best50$Category)
best50$who <- factor(best50$who)
best50$mean <-factor(best50$mean)
plot(mean ~ Category,data = best50)

## One sample t-test between category accuracies of each participant(50ms&200ms)
##and models
test_ID <- human_ID %>% filter(cond == "200")
objcompare <- function(this.detector, this.cat) {
  onetest <- test_ID %>% filter(Category == this.cat)
  objnet <- all_model %>% filter(who == this.detector) %>% filter(Category == this.cat)
  ttestBF(onetest$mean, mu = objnet$mean)
}
detectors <- unique(all_model$who)
cats <- unique(test_ID$Category)
for(this.detector in detectors) {
  print(this.detector)
  for(this.cat in cats) {
    print(this.cat)
    print(objcompare(this.detector, this.cat))
  }
}

