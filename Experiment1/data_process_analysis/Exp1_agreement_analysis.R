# Install and load necessary packages
# Load required libraries
library("data.table")
library("plyr")
library("dplyr")
library("readr")                               
library("tibble")
library("ggplot2")
library("purrr")
library("readxl")
library("tidyverse")
library(psych)
library(ICC)
library("vICC")
library("irr")
library("psychTools")
library("mlmhelpr")
library(sjPlot)

df <- read.csv("Experiment1/humanExp1_data.csv")
df<- df %>%
  arrange(ID) %>%  # Ensure data is sorted by ID for consecutive numbering
  group_by(ID) %>%  # Group by ID
  mutate(id = cur_group_id()) %>%  # Assign a unique participant_id
  ungroup() %>% # Remove grouping information
  select(ID, id, everything())

df <- df%>%  mutate(cond = ifelse(cond %in% c( "50"), "human50ms", "human200ms")) %>%
  dplyr::mutate(Response = dplyr::recode(resp,'0'='Basket','1'='Doormat', '2'='Drill', 
                                         '3'='Helmet', '4'='Mug', '5'='Plate', '6'='Plunger',
                                         '7'='Safety pin', '8'='TV', '9'='Vase'))                                
# Reshape the data for kappa calculation
df <- df  %>% 
  rename("Subject" = "id",
         "Condition" ="cond" ,
         "Category" = "cat",
         "Accuracy" = "corr")
data <- data.frame(df) %>% select(ID, Subject, Condition,Category, Response, image_id)

# Transform data to wide format by assigning responses under the time columns
data <- data %>% group_by(Condition,Category, Subject, image_id) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from=c(Condition), values_from=c(Response)) %>%
  select(-row)



# EACH INDIVIUAL DATA BY THEMSELVES AT 50MS AND 200MS 
# Initialize a list to store 50ms-200ms kappa results for each paerticipants
results_list <- list()

# Loop through the common IDs in both datasets
ids <- unique(data$Subject)
length(unique(data$Subject))

for (subj_id in ids) {
  # Subset data for the current ID
  subset_data <- data[data$Subject == subj_id, ]
  k_data <- data.frame(subset_data$human50ms, subset_data$human200ms)
  
  # Perform kappa2 reliability analysis
  kappa_result <- kappa2(k_data, weight = "unweighted")
  print(kappa_result, all = TRUE)
  
  # Calculate lower and upper bounds using cohen.kappa
  cohen_kappa_result <- cohen.kappa(k_data,alpha=.05)
  cohen_kappa_result
  lower_bound <- cohen_kappa_result$confid[1]
  lower_bound 
  upper_bound <- cohen_kappa_result$confid[5]
  
  # Store the ID, Kappa value, lower bound, and upper bound in the results list
  results_list[[as.character(subj_id)]] <- list(
    Subject = subj_id,
    kappa_value = kappa_result$value,
    lower_bound = lower_bound,
    upper_bound = upper_bound
  )
}

# Print the results
print(results_list)

### Scatter plot kappa reliability results
# Extract values from results_list
subjects <- unlist(lapply(results_list, function(x) x$Subject))
kappa_values <- unlist(lapply(results_list, function(x) x$kappa_value))
lower_bounds <- unlist(lapply(results_list, function(x) x$lower_bound))
upper_bounds <- unlist(lapply(results_list, function(x) x$upper_bound))

# Create a data frame for plotting
plot_data <- data.frame(
  Subject = factor(subjects, levels = subjects[order(kappa_values)]),
  Kappa = kappa_values,
  Lower_Bound = lower_bounds,
  Upper_Bound = upper_bounds
)

# HUMANS WITHIN AT 50MS AND 200MS SEPARETLY
### Calculate the kappa values for all possible unique combination pair of participants data within each
# presentation time separetly
calculate_kappa <- function(data, time_condition) {
  # Ensure the time_condition is either "human200ms" or "human50ms"
  if (!time_condition %in% c("human200ms", "human50ms")) {
    stop("Invalid time condition. Use 'human200ms' or 'human50ms'.")
  }
  
  unique_subjects <- unique(data$Subject)
  kappa_values <- list()
  
  # Generate combinations of unique subjects
  for (i in seq(unique_subjects)) {
    for (j in seq(unique_subjects)) {
      if (i < j) {
        id <- unique_subjects[i]
        sub <- unique_subjects[j]
        
        # Filter data for the current subject pair
        subset_data1 <- data[data$Subject == id, c("Category", "image_id", time_condition)]
        subset_data2 <- data[data$Subject == sub, c("Category", "image_id", time_condition)]
        
        # Merge data based on Category and image_id
        merged_data <- merge(subset_data1, subset_data2, by = c("Category", "image_id"), all = TRUE)
        
        # Create a data frame with the responses for kappa calculation
        k_data <- data.frame(merged_data[[paste0(time_condition, ".x")]], 
                             merged_data[[paste0(time_condition, ".y")]])
        
        # Calculate kappa (Unweighted Kappa for categorical data without a logical order)
        kappa_result <- kappa2(k_data, weight = "unweighted")
        
        # Calculate lower and upper bounds using cohen.kappa
        cohen_kappa_result <- cohen.kappa(k_data, alpha = 0.05)
        lower_bound <- cohen_kappa_result$confid[1]
        upper_bound <- cohen_kappa_result$confid[5]
        
        # Store the subject pair and Kappa value in the results list
        kappa_values[[paste(id, sub, sep = "&")]] <- list(
          Subject = paste(id, sub, sep = "&"),
          Kappa_Value = kappa_result$value,
          Lower_Bound = lower_bound,
          Upper_Bound = upper_bound
        )
      }
    }
  }
  
  return(kappa_values)
}

# Calculate kappa values for 200ms condition
kappa_200ms <- calculate_kappa(data, "human200ms")

# Initialize vectors to store values
subjects <- vector("list", length = length(kappa_200ms))
kappa_values <- numeric(length = length(kappa_200ms))
lower_bounds <- numeric(length = length(kappa_200ms))
upper_bounds <- numeric(length = length(kappa_200ms))


# Extract values from kappa_results list
for (i in seq_along(kappa_200ms)) {
  subjects[[i]] <-kappa_200ms[[i]]$Subject
  kappa_values[i] <- kappa_200ms[[i]]$Kappa_Value
  lower_bounds[i] <- kappa_200ms[[i]]$Lower_Bound
  upper_bounds[i] <- kappa_200ms[[i]]$Upper_Bound
}
# Create a data frame for plotting
plot_data_200ms <- data.frame(
  Subjects = factor(unlist(subjects), levels = unlist(subjects)),
  Kappa = kappa_values,
  Lower_Bound = lower_bounds,
  Upper_Bound = upper_bounds
)


human200 <- plot_data_200ms %>% select(Kappa)

# Calculate kappa values for 50ms condition
kappa_50ms <- calculate_kappa(data, "human50ms")

# Initialize vectors to store values
subjects <- vector("list", length = length(kappa_50ms))
kappa_values <- numeric(length = length(kappa_50ms))
lower_bounds <- numeric(length = length(kappa_50ms))
upper_bounds <- numeric(length = length(kappa_50ms))


# Extract values from kappa_results list
for (i in seq_along(kappa_50ms)) {
  subjects[[i]] <-kappa_50ms[[i]]$Subject
  kappa_values[i] <- kappa_50ms[[i]]$Kappa_Value
  lower_bounds[i] <- kappa_50ms[[i]]$Lower_Bound
  upper_bounds[i] <- kappa_50ms[[i]]$Upper_Bound
}
# Create a data frame for plotting
plot_data_50ms <- data.frame(
  Subjects = factor(unlist(subjects), levels = unlist(subjects)),
  Kappa = kappa_values,
  Lower_Bound = lower_bounds,
  Upper_Bound = upper_bounds
)

human50 <- plot_data_50ms %>% select(Kappa)


# MACHINE VS MACHINE
# Read your data
machine_data <- fread("Experiment1/ft_models_data.csv")

# Remove accuracy column
machine_data <- select(machine_data, -5)

# List of machine models
machine_models <- c("CoCa_ViT_L14", "VIT_L16", "VIT_B8", "InceptionV3", "ResNet152","VGG19" )

### kappa value between models
# Initialize a list to store results
results_model <- list()

# Loop through each participant in machine data
for (i in machine_models) {
  # Subset data for the current participant
  model1 <- machine_data[machine_data$who == i, ]
  # Loop through each machine model
  for (k in machine_models) {
    if (i != k) {
      model2 <- machine_data[machine_data$who == k, ]
      mach <- merge(model1,model2, by =c("Category","image_id"), all = TRUE)
      k_data <- data.frame(mach$predicted_class.x, mach$predicted_class.y)
      
      # Perform kappa2 reliability analysis
      kappa_result <- kappa2(k_data, weight = "unweighted")
      
      # Store the model and kappa value in the list
      results_model[[paste0(i, "&", k)]] <- kappa_result$value
    }
  }
}

# Combine all participant dataframes into a single dataframe
Kappa_values <- do.call(rbind, results_model)
model_data <- data.frame(Kappa_values)
# Add participant ID as a column
model_data$Who <- rownames(model_data )
#write.csv(model_data, paste("Exp1_models_kappa.csv", sep = ""))


# HUMAN VS MACHINE
# Loop through the common IDs in both datasets
data <- data %>% select(Category, image_id, Subject, human200ms, human50ms)
ids <- unique(data$Subject)
data <- data.table(data)

length(unique(data$Subject))

calculate_kappa_human_model <- function(human_data, machine_data, time_condition, machine_models) {
  # Ensure the time_condition is either "human200ms" or "human50ms"
  if (!time_condition %in% c("human200ms", "human50ms")) {
    stop("Invalid time condition. Use 'human200ms' or 'human50ms'.")
  }
  
  results_list <- list()
  
  # Loop through each participant in the human data
  for (subj_id in unique(human_data$Subject)) {
    # Subset data for the current participant
    human_participant_data <- human_data[human_data$Subject == subj_id, ]
    
    kappa_values_list <- list()
    
    # Loop through each machine model
    for (model in machine_models) {
      # Subset data for the current machine model based on image_id
      machine_participant_data <- machine_data[machine_data$who == model, ]
      
      # Merge human and machine data based on Category and image_id
      human_machine <- merge(human_participant_data, machine_participant_data, by = c("Category", "image_id"), all = TRUE)
      
      # Extract relevant columns for kappa analysis
      kappa_data <- data.frame(
        human = as.factor(human_machine[[time_condition]]),
        machine = as.factor(human_machine$predicted_class)
      )
      
      # Perform kappa2 reliability analysis
      kappa_result <- kappa2(kappa_data, weight = "unweighted")
      
      # Store the model and kappa value in the list
      kappa_values_list[[paste0(model, "_", time_condition)]] <- kappa_result$value
    }
    
    # Combine all the kappa values into a dataframe for the current participant
    participant_df <- as.data.frame(kappa_values_list)
    
    # Store the participant ID and kappa values in the results list
    results_list[[as.character(subj_id)]] <- participant_df
  }
  
  return(results_list)
}


# Calculate kappa values for 200ms condition
results_200ms <- calculate_kappa_human_model(human_data = data, machine_data = machine_data,
                                             time_condition = "human200ms", machine_models = machine_models)

# Calculate kappa values for 50ms condition
results_50ms <- calculate_kappa_human_model(human_data = data, machine_data = machine_data,
                                            time_condition = "human50ms", machine_models = machine_models)

# Combine all participant dataframes into a single dataframe
results_df200 <- do.call(rbind, results_200ms)

# Combine all participant dataframes into a single dataframe
results_df50 <- do.call(rbind, results_50ms)

#Humans at 50ms
human50_human200 <- plot_data$Kappa
results_df50$human50_human200  <- human50_human200
human50_human50<- human50 %>% rename("human50_human50" = "Kappa")
all_results_df50 <- rbind.fill(results_df50,human50_human50)

results50 <- pivot_longer(data= all_results_df50,
                        cols = c("CoCa_ViT_L14_human50ms":"VGG19_human50ms", "human50_human200",
                                 "human50_human50"),
                        values_to = "Kappa_values",
                        names_to = "Who")
results50 <- na.omit(results50)
all50<- results50 %>% select(Kappa_values, Who)
unique(all50$Who)

#Humans at 200ms
human200_human200 <- human200 %>% rename("human200_human200" = "Kappa")
all_results_df200 <- rbind.fill(results_df200,human200_human200)


results200 <- pivot_longer(data= all_results_df200,
                        cols = c("CoCa_ViT_L14_human200ms":"VGG19_human200ms", "human200_human200"),
                        values_to = "Kappa_values",
                        names_to = "Who")
results200 <- na.omit(results200)
all200 <- results200 %>% select(Kappa_values, Who)
unique(all200$Who)
#write.csv(all50, paste("Exp1_kappa50_06_aug_2024.csv", sep = ""))
#write.csv(all200 , paste("Exp1_kappa200_06_aug_2024.csv", sep = ""))

all50 <- read.csv("Experiment1/Exp1_kappa50_06_aug_2024.csv")
all200 <- read.csv("Experiment1/Exp1_kappa200_06_aug_2024.csv")
results_meds50 <- ddply(all50, .(Who), summarise, med = median(Kappa_values,na.rm=TRUE))
results_meds200 <- ddply(all200, .(Who), summarise, med = median(Kappa_values,na.rm=TRUE))
all50 <- cbind(all50, cond='Human50')
all200 <- cbind(all200, cond='Human200')
models <- read.csv("Experiment1/Exp1_models_kappa.csv")
all <- rbind(all50,all200)
results_meds <- ddply(all, .(cond, Who), summarise, med = median(Kappa_values,na.rm=TRUE))

length(unique(all$Who))

allcolor <- c( "human50_human200" = "#BEBADA",  "human50_human50"="#FCCDE5", "CoCa_ViT_L14_human50ms"= "#00AEDB",
               "VIT_B8_human50ms"= "#8DD3C7", "VIT_L16_human50ms"=  "#B3DE69",
               "ResNet152_human50ms"= "#FFFFB3","InceptionV3_human50ms"= "#FB8072", "VGG19_human50ms"= "#FDB462",
               "human200_human200"= "#FCCDE5","CoCa_ViT_L14_human200ms"= "#00AEDB", "VIT_B8_human200ms"="#8DD3C7",
               "VIT_L16_human200ms"= "#B3DE69","ResNet152_human200ms"= "#FFFFB3",
               "InceptionV3_human200ms" ="#FB8072" , "VGG19_human200ms" ="#FDB462"  )

level <- c('Human50','Human200')
level_order<-  c( "human50_human200",  "human50_human50", "CoCa_ViT_L14_human50ms", "VIT_B8_human50ms", "VIT_L16_human50ms",
                  "ResNet152_human50ms","InceptionV3_human50ms", "VGG19_human50ms", "human200_human200",
                  "CoCa_ViT_L14_human200ms", "VIT_B8_human200ms", "VIT_L16_human200ms",
                  "ResNet152_human200ms","InceptionV3_human200ms", "VGG19_human200ms" )

frsa <- ggplot(data =all, mapping = aes(x = factor(Who, levels = level_order), y = Kappa_values, fill = Who))+
  geom_violin(size=0.5, trim=TRUE, alpha=0.4, scale='width',position = position_dodge(0.9)) +  
  scale_fill_manual(values = allcolor)+
  geom_jitter(data = all,aes(x = Who, y = Kappa_values, fill =Who),alpha=0.6, shape = 21, width=0.3,size= 2) +
  geom_boxplot(data = all,aes(x = Who, y = Kappa_values, fill = Who),width = 0.2, fill = "burlywood", color = "black")+ 
  scale_x_discrete(expand=c(0.1, 0.15))+
  coord_cartesian(expand = 0.1, clip="off") + # prevent cut off y and x axis limits so labels are visible
  facet_wrap( ~factor(cond, levels = level), nrow = 2,ncol = 1 ,strip.position = "bottom", scales = "free_x")+
  labs(x='Agreement Between', y='Kappa value',
       title='Distribution of Kappa Agreement Across Models and Humans')+
  theme(panel.spacing = unit(1.15, units = "cm"), # removes space between panels
        strip.placement = "outside", panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid',colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',colour = "lightgray"),
        strip.background = element_rect(fill = "white"))+# removes the background from the simulation names
  theme(plot.title = element_text(hjust = 0.5,size=20),
        axis.title.y = element_text(size =14, face ='bold',vjust = +2),
        axis.title.x = element_text(size =18, face ='bold',vjust = -0.8),
        axis.text.y =element_text(colour= "black",size=16),
        axis.text.x =element_text(colour= "black",size=16, hjust=0.85,vjust = 0.85,angle = 30),
        strip.text.x = element_text(size = 14),
        legend.position = "none", 
        axis.line =element_line(colour = "black" ))+
  geom_label(data = results_meds, aes(x = Who, y = med, label = round(med,2)), fill="#d8e01b",
             size = 3.2, vjust = -0.85, col = "black",fontface='bold',
             label.padding = unit(0.3, 'lines'), inherit.aes = FALSE)

pdf(file = "all_kappa_violin2_06_aug_2024.pdf", width = 8, height = 12)
print(frsa)
dev.off()












