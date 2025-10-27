## Loading libraries
library("data.table")
library("plyr")
library("dplyr")
library("readr")                               
library("tibble")
library("ggplot2")
library("purrr")

## Human Data

# Make a list of csv files and with fread
#(it is more faster than read_csv and ignore different type of columns),
#read each of the files and bind the rows of the list
raw <- list.files(path = "Experiment1/Human_Rawdata", pattern = "*.csv",
                  full.names = TRUE) %>% 
  lapply(fread) %>%
  rbindlist(fill=TRUE)

# Raw data of the participants
#write.csv(raw, paste("humanExp1_rawdata.csv", sep = ""), row.names = FALSE)

# Select and rename the columns of human data
dta  <- raw %>% select(ID = subject_nr, block = List,
                       trial = count_fixation,cond = object_dur, cat = image_type,
                       resp = response, rt = average_response_time, corr = correct,
                       image_id = Image) 

# Filter training trials
dta <- dta[!(dta$block %in% c("training", "training ")),]

#write.csv(dta, paste("humanExp1_data.csv", sep = ""), row.names = FALSE)

## Calculating mean of categories and conditions
dta <- dta %>% rename("Category" = "cat") 

# Mean accuracy of categories and conditions for each participants
humanID_accuracy <- dta %>%
  group_by(ID, Category, cond) %>% dplyr::summarise(mean = mean(corr))
humanID_accuracy<- cbind(humanID_accuracy, who = "Human")

human1_50 <- humanID_accuracy[humanID_accuracy$cond == "50", ]
human1_200 <- humanID_accuracy[humanID_accuracy$cond == "200", ]
human1_200$who[human1_200$who == 'Human'] <-'Human 200ms'
human1_50$who[human1_50$who == 'Human'] <-'Human 50ms'

# Mean accuracies of categories and presentation times 
exp1 <-dta %>% group_by(cond, Category) %>% dplyr::summarise(mean = mean(corr))
exp1 <- tibble(exp1)
total_hum <-dta %>% group_by(cond) %>% dplyr::summarise(mean = mean(corr))
total_idhum <-humanID_accuracy %>% group_by(cond, ID) %>% dplyr::summarise(mean = mean(mean))
print(tibble(exp1))

# Mean accuracy for each image
human_image <- dta %>%
  group_by(image_id, Category, cond) %>% dplyr::summarise(id_mean = mean(corr))
human_image <- tibble(human_image)
human_image <- human_image[order(id_mean) , ]
print(human_image[order(-id_mean),])

#accuracy of human data as a csv document
#write.csv(humanID_accuracy, paste("exp1_human_accuracy.csv", sep = ""))

## Plot bar graph
library(ggthemes)
level_order <-c("Drill", "Plunger", "Safety pin", "TV", "Doormat", "Mug", "Helmet", "Basket", "Vase", "Plate")
exp1$cond <- as.character(exp1$cond)
# Plot bar graph for accuracies of categories and presentation times 
exp1_plot <- ggplot(exp1, aes(x =factor(Category, level=level_order), y = mean, fill=cond)) +
  geom_bar(stat="identity",width=0.7,  position=position_dodge(.7)) +
  geom_text(aes(label = paste0(round(mean * 100), '%')), size = 3,
            position = position_dodge(.7),
            vjust = -0.5) +
  scale_fill_manual(values=c("#D12600", "#3E71A8")) +
  theme_classic() + scale_y_continuous(labels=scales::percent) +
  labs(x='Categories', y='Accuracy%', title='Avg. Accuracy Scored by Categories & Conditions') +
  coord_cartesian(ylim = c(0,1)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + guides(x = guide_axis(angle = 30))

#Plot bar graph for test-retest
exp1_plot <- ggplot(total_hum, aes(x =reorder(cond, -mean), y = mean, fill = factor(cond))) +
  geom_bar(stat="identity",width=0.4,  position=position_dodge(.2)) +
  geom_text(aes(label = paste0(round(mean * 100), '%')), size = 4,
            position = position_dodge(.7),
            vjust = -0.5) +
  scale_fill_manual('cond', values=c('coral2', 'steelblue')) +
  theme_classic() + scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c('200ms', '50ms'))+
  labs(x='Stimuli Presentations Time', y='Accuracy%', title='Avg. Accuracy Scored by Categories & Conditions') +
  coord_cartesian(ylim = c(0,1)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14), axis.text.y=element_text(size = 12),
        axis.text.x = element_text(size = 12), axis.title = element_text(size = 16), strip.text.x = element_text(size = 14),
        axis.line =element_line(colour = "black" ), legend.position = "none")
print(exp1_plot)

## Model Simulation
## Importing the files for labels and category names of objectnet sample
# Finetuned Model Simulations (predictions out of 10 categories)
fresnet <- read.csv("model_predictions/ResNet152_torch_predictions.csv", header = TRUE)
fincp <- read.csv("model_predictions/InceptionV3_torch_predictions.csv", header = TRUE)
fvgg <- read.csv("model_predictions/VGG19_torch_predictions.csv", header = TRUE)
fvit8 <- read.csv("model_predictions/VIT_B8_torch_predictions.csv", header = TRUE)
fvitl16 <- read.csv("model_predictions/VIT_L16_torch_predictions.csv", header = TRUE)
fcoca <- read.csv("model_predictions/CoCa_ViT_L14_torch_predictions.csv", header = TRUE)


## Naming fist four columns of data frame
colnames(fresnet)[1:7] <- c("number", "actual_class", "image_id", "predicted_id",  "probability","who", "predicted_class")
colnames(fincp)[1:7] <-c("number", "actual_class", "image_id", "predicted_id",  "probability", "who", "predicted_class")
colnames(fvgg)[1:7] <- c("number", "actual_class", "image_id", "predicted_id",  "probability", "who", "predicted_class")
colnames(fvit8)[1:7] <- c("number", "actual_class", "image_id", "predicted_id",  "probability", "who", "predicted_class")
colnames(fvitl16)[1:7] <-c("number", "actual_class", "image_id", "predicted_id",  "probability","who", "predicted_class")
colnames(fcoca)[1:8] <-c("number", "actual_class", "image_id","predicted_id", "who", "predicted_class","correct_label", "correct_id" )

## If prediction and ID's match with each other=1 otherwise it is = 0 and making accuracy column
fresnet$accuracy <- ifelse(fresnet$actual_class == fresnet$predicted_class, 1, 0) 
fincp$accuracy <- ifelse(fincp$actual_class == fincp$predicted_class, 1, 0) 
fvgg$accuracy <- ifelse(fvgg$actual_class == fvgg$predicted_class, 1, 0) 
fvit8$accuracy <- ifelse(fvit8$actual_class == fvit8$predicted_class, 1, 0) 
fvitl16$accuracy <- ifelse(fvitl16$actual_class == fvitl16$predicted_class, 1, 0) 
fcoca$accuracy <- ifelse(fcoca$actual_class == fcoca$predicted_class, 1, 0)

## model dataframes
fresnet <- fresnet %>% select(Category = actual_class, image_id, who, predicted_class,accuracy)
fincp <- fincp %>%  select(Category = actual_class, image_id, who, predicted_class,accuracy)
fvgg <- fvgg %>%  select(Category = actual_class, image_id, who, predicted_class,accuracy)
fvit8 <- fvit8 %>%  select(Category = actual_class, image_id, who, predicted_class,accuracy)
fvitl16 <- fvitl16 %>%  select(Category = actual_class, image_id, who, predicted_class,accuracy)
fcoca <- fcoca %>% select(Category = actual_class, image_id, who, predicted_class,accuracy)
fsum <- rbind(fcoca, fvitl16, fvit8, fincp, fresnet, fvgg)
fsum<- fsum%>% mutate(Category = recode(Category,"drill"="Drill", "plunger" ="Plunger", "safety_pin"= "Safety pin", "tv" = "TV", "doormat"= "Doormat",
                                                "mug" = "Mug", "helmet" = "Helmet", "basket"="Basket", "vase" = "Vase", "plate" ="Plate"))
fsum<- fsum%>% mutate(predicted_class = recode(predicted_class,"drill"="Drill", "plunger" ="Plunger", "safety_pin"= "Safety pin", "tv" = "TV", "doormat"= "Doormat",
                                         "mug" = "Mug", "helmet" = "Helmet", "basket"="Basket", "vase" = "Vase", "plate" ="Plate"))

#write.csv(fsum, paste("ft_models_data.csv", sep = ""), row.names = FALSE)

## Calculating mean and sd of categories
fresnet <- data.table(fresnet)
fresnet_accuracy <- fresnet%>% group_by(Category, who) %>% dplyr::summarise(mean = mean(accuracy))
fvgg <- data.table(fvgg)
fvgg_accuracy <- fvgg %>% group_by(Category, who) %>% dplyr::summarise(mean = mean(accuracy))
finception <- data.table(fincp)
finception_accuracy <- finception%>% group_by(Category, who) %>% dplyr::summarise(mean = mean(accuracy))
fvit8 <- data.table(fvit8)
fvit8_accuracy <- fvit8 %>% group_by(Category, who) %>% dplyr::summarise(mean = mean(accuracy))
fvitl16 <- data.table(fvitl16)
fvitl16_accuracy <- fvitl16 %>% group_by(Category, who) %>% dplyr::summarise(mean = mean(accuracy))
fcoca <- data.table(fcoca)
fcoca_accuracy <-fcoca %>% group_by(Category, who) %>% dplyr::summarise(mean = mean(accuracy))

## Merge human and model dataframes
ftmodels <- rbind(fcoca_accuracy ,fvitl16_accuracy,fvit8_accuracy,
                  fresnet_accuracy, fvgg_accuracy, finception_accuracy)
ftmodels<- ftmodels%>% mutate(Category = recode(Category,"drill"="Drill", "plunger" ="Plunger", "safety_pin"= "Safety pin", "tv" = "TV", "doormat"= "Doormat",
            "mug" = "Mug", "helmet" = "Helmet", "basket"="Basket", "vase" = "Vase", "plate" ="Plate"))

humanID_accuracy <- rbind(human1_50, human1_200)
models_human <- rbind.fill(human1_50, human1_200,ftmodels)
models_human50 <- rbind.fill(human1_50,ftmodels)

level_order <- c("Drill", "Plunger", "Safety pin", "TV", "Doormat", "Mug", "Helmet", "Basket", "Vase", "Plate")
all_models_plot <- ggplot(ftmodels, aes(x = factor(Category, level=level_order), y = mean, group=who, color=who)) + geom_line() + geom_point() + labs(title = "Mean Accuracy of Models by Categories") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14)) + theme_classic()

print(all_models_plot)


library("patchwork")
library("ggplot2")
library("dplyr")
library("cowplot") # it allows you to save figures in .png file
library("ggbeeswarm")
library("jcolors")
library("lemon")
display_jcolors("pal12")

# Violin plot for the distribution of accuracy dot of each participants and models 
level_order <- c("Human 50ms", "Human 200ms", "CoCa_ViT_L14",
                 "VIT_B8","VIT_L16",
                 "ResNet152","InceptionV3", "VGG19")
level <- c("Drill", "Plunger", "Safety pin", "TV", "Doormat", "Mug", "Helmet", "Basket", "Vase", "Plate")

ftmeds <- ddply(models_human, .(who, Category), summarise, med = median(mean,na.rm=TRUE))

combine <- ggplot(data =models_human, mapping = aes(x = factor(who, levels = level_order), y = mean, fill = who))+
  geom_violin(width=1.25, size=0.5, trim=TRUE, alpha=0.4, scale='width',position="identity", show.legend = FALSE) + 
  geom_jitter(data = humanID_accuracy,aes(x = who, y = mean, fill = who),alpha=0.6, shape = 21, width=0.3,size= 2,show.legend = FALSE) +
  scale_fill_manual(name = "Human", values = c("Human 50ms" = "powderblue", "Human 200ms"= "palegoldenrod")) +
  geom_boxplot(data = humanID_accuracy,aes(x = who, y = mean, fill = who),width = 0.3, fill = "burlywood", color = "black")+ 
  geom_point(data = ftmodels, aes(x = who, y = mean, colour = who, shape = who), alpha=0.9, cex = 4,
             position = position_dodge(width=0.1),stroke = 1.25,show.legend = FALSE)+ 
  scale_color_manual(name = "Models", values = c("CoCa_ViT_L14" = "#D12600", 
                                                 "VIT_B8" = "#3CA437", "VIT_L16" = "#FDB462", "ResNet152"="deeppink3", 
                                                 "VGG19"="dodgerblue3", "InceptionV3" = "purple3"))+
  scale_shape_manual(name = "Models", values=c("CoCa_ViT_L14" = 11,"VIT_L16"=8, "VIT_B8" = 15,  "ResNet152"=19,
                                               "VGG19"=18, "InceptionV3" =17))+
  
  scale_x_discrete(expand=c(0.1, 0.1),labels = NULL)+
  facet_wrap( ~factor(Category, levels = level), nrow = 4,ncol = 3 ,strip.position = "bottom", scales = "free_x")+
  scale_y_continuous(labels=scales::percent) +
  coord_cartesian(expand = 0.1, clip="off") + # prevent cut off y and x axis limits so labels are visible
  labs(x='Object Categories', y='Accuracy %', title='Avg. Category Accuracies Scored by Models and Humans')+
  theme(panel.spacing = unit(1.15, units = "cm"), # removes space between panels
        strip.placement = "outside", panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',colour = "lightgray"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "lightgray"),
        strip.background = element_rect(fill = "white"))+# removes the background from the simulation names
  theme(plot.title = element_text(hjust = 0.5,size=20),
        axis.title.y = element_text(size =14, face ='bold',vjust = +2),
        axis.title.x = element_text(size =18, face ='bold',vjust = -0.8),
        axis.text.y =element_text(colour= "black",size=16),
        axis.text.x =element_text(colour= "black",size=16, hjust=0.85,vjust = 0.85,angle = 30),
        strip.text.x = element_text(size = 14),
        axis.line =element_line(colour = "black" ))+
  geom_label(data = ftmeds, aes(x = who, y = med, label = round(med,2)), fill="#d8e01b",
             size = 3, vjust = -0.85, col = "black",fontface='bold', label.padding = unit(0.20, 'lines'))

pdf(file = "violinplot_Experiment1_nolegend_20june2024.pdf", width = 10, height =10 )
print(combine)
dev.off()



# Plot overall accuracies of humans and models for all simulations 
pdf(file = "humanvsmodels_overall.pdf", width = 10, height =6 )
sum_models_human<- models_human %>% group_by(who) %>% dplyr::summarise(mean = mean(mean))
sum_models_human$who_f = factor(sum_models_human$who,
                                levels=c("Human 50ms", "Human 200ms", "CoCa_ViT_L14",
                                         "VIT_B8","VIT_L16",
                                         "ResNet152","InceptionV3",  "VGG19"))

plot <- ggplot(sum_models_human, aes(x =who_f, y = mean, fill=who_f)) +
  geom_bar(stat="identity",width=0.7,  position=position_dodge(.7),show.legend = FALSE) +
  scale_fill_manual(values=c("#D12600", "#DB6A00","#2E6657","#779d8d","#BBBE64", "#005B94", "#3E71A8",
                             "#610052" ,"#870E75"))+
  geom_text(aes(label = paste0(round(mean * 100), '%')), size = 4,
            position = position_dodge(.7),
            vjust = -0.5) +
  labs(x='Object Detectors', y='Accuracy%',
       title = " Overall Accuracy% of Human and Models", colour='Who') +
  theme_classic() + scale_y_continuous(labels=scales::percent, limits=c(0, 1))+
  theme(plot.title = element_text(hjust = 0.5, size = 14), axis.text.y=element_text(size = 12),
        axis.text.x = element_text(size = 12), axis.title = element_text(size = 14))+
  guides(x = guide_axis(angle = 20))
print(plot)
dev.off()

