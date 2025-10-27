# Clear workspace
rm(list = ls())

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

# FUNCTIONS
## PROCESS THE HUMAN DATA 
prepare_human_data <- function(file_path, crop_name = "data2_crop", full_name = "data2_full") {
  # Read the data from the specified file path
  dta <- read.csv(file_path, header = TRUE)
  dta <- tibble(dta)
  
  # Recode 'cond' and clean 'cat' variable
  dta$cond <- car::recode(dta$cond, "c('crop')='Crop';c('full')='Full'")
  dta$cat[dta$cat == "Tv"] <- "TV"
  
  # Arrange data, group by ID, and create a unique identifier for each participant
  df2 <- dta %>%
    arrange(ID) %>%  # Ensure data is sorted by ID for consecutive numbering
    group_by(ID) %>%  # Group by ID
    mutate(id = cur_group_id()) %>%  # Assign a unique participant_id
    ungroup() %>% # Remove grouping information
    select(ID, id, everything())
  
  # Rename columns for kappa calculation
  df2 <- df2 %>%
    rename(
      "Subject" = "id",
      "Condition" = "cond",
      "Category" = "cat",
      "Accuracy" = "corr",
      "Response" = "resp"
    )
  
  # Select the relevant columns
  data2 <- data.frame(df2) %>% 
    select(ID, Subject, Condition, Category, Response, image_id)
  
  # Split and reshape the data for "Crop" and "Full" conditions
  crop_data <- data2 %>%
    filter(Condition == "Crop") %>%
    group_by(Condition, Category, Subject, image_id) %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = c(Condition), values_from = c(Response)) %>%
    select(-row)
  
  full_data <- data2 %>%
    filter(Condition == "Full") %>%
    group_by(Condition, Category, Subject, image_id) %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = c(Condition), values_from = c(Response)) %>%
    select(-row)
  
  # Assign the resulting data frames to the specified variable names in the global environment
  assign(crop_name, crop_data, envir = .GlobalEnv)
  assign(full_name, full_data, envir = .GlobalEnv)
}

## HUMANS WITHIN CROP AND FULL SEPARETLY
### Calculate the kappa values for all possible unique combination pair of participants data within each
# image manipulation condition separetly
calculate_kappa <- function(data, Condition) {
  # Ensure the time_condition is either "crop" or "full"
  if (!Condition %in% c("Crop", "Full")) {
    stop("Invalid time condition. Use 'Crop' or 'Full'.")
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
        subset_data1 <- data[data$Subject == id, c("Category", "image_id", Condition)]
        subset_data2 <- data[data$Subject == sub, c("Category", "image_id", Condition)]
        
        # Merge data based on Category and image_id
        merged_data <- merge(subset_data1, subset_data2, by = c("Category", "image_id"), all = TRUE)
        
        # Create a data frame with the responses for kappa calculation
        k_data <- data.frame(merged_data[[paste0(Condition, ".x")]], 
                             merged_data[[paste0(Condition, ".y")]])
        
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

## cALCULATE KAPPA AND MAKE A DATAFRAME 
calculate_kappa_data <- function(data, condition) {
  # Calculate kappa values for the specified condition
  kappa_values <- calculate_kappa(data, condition)
  
  # Initialize vectors to store extracted values
  subjects <- vector("list", length = length(kappa_values))
  kappa_vals <- numeric(length = length(kappa_values))
  lower_bounds <- numeric(length = length(kappa_values))
  upper_bounds <- numeric(length = length(kappa_values))
  
  # Extract values from the kappa results list
  for (i in seq_along(kappa_values)) {
    subjects[[i]] <- kappa_values[[i]]$Subject
    kappa_vals[i] <- kappa_values[[i]]$Kappa_Value
    lower_bounds[i] <- kappa_values[[i]]$Lower_Bound
    upper_bounds[i] <- kappa_values[[i]]$Upper_Bound
  }
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    Subjects = factor(unlist(subjects), levels = unlist(subjects)),
    Kappa = kappa_vals,
    Lower_Bound = lower_bounds,
    Upper_Bound = upper_bounds
  )
  
  return(plot_data)
}

# PROCESS MODELS DATA AND CALCULATE KAPPA FOR MACHINE VS MACHINE
# Define the function to process machine data and calculate kappa values
process_mach_data <- function(file_path) {
  # Read the machine data from the provided file path
  machine_data <- fread(file_path)
  
  # Select only the relevant columns from the data
  machine_data <- machine_data %>% 
    select(actual_class, image_id, who, cat, predicted_class2, cond)
  

    mdata <- machine_data %>%  rename(
      "Subject" = "who",
      "Condition" = "cond",
      "Category" = "cat",
      "Response" = "predicted_class2"
    )
     
    # Select the relevant columns
    data <- data.frame(mdata) %>% 
      select(Subject, Condition, Category, Response, image_id)
    
    # Split and reshape the data for "Crop" and "Full" conditions
    crop_data <- data %>%
      filter(Condition == "Crop") %>%
      group_by(Condition, Category, Subject, image_id) %>%
      mutate(row = row_number()) %>%
      pivot_wider(names_from = c(Condition), values_from = c(Response)) %>%
      select(-row)
    
    full_data <- data %>%
      filter(Condition == "Full") %>%
      group_by(Condition, Category, Subject, image_id) %>%
      mutate(row = row_number()) %>%
      pivot_wider(names_from = c(Condition), values_from = c(Response)) %>%
      select(-row)
    
    # Return a list containing the reshaped data for "Crop" and "Full"
    return(list(crop = crop_data, full = full_data))
}

# DEFINE FUNCTION TO CALCULATE KAPPA BETWEEN MACHINES AND HUMANS
calculate_kappa_human_model <- function(human_data, machine_data, Condition) {
  # Ensure the time_condition is either "human200ms" or "human50ms"
  if (!Condition %in% c("Crop", "Full")) {
    stop("Invalid time condition. Use 'Crop' or 'Full'.")
  }
  
  results_list <- list()
  
  # Loop through each participant in the human data
  for (subj_id in unique(human_data$Subject)) {
    # Subset data for the current participant
    human_participant_data <- human_data[human_data$Subject == subj_id,  ]
    
    kappa_values_list <- list()
    machine_data$image_id <- gsub("\\.png$", ".jpg", machine_data$image_id)
    # Loop through each machine model
    for (model in unique(machine_data$Subject)) {
      # Subset data for the current machine model based on image_id
      machine_participant_data <- machine_data[machine_data$Subject == model, ]
      
      # Merge human and machine data based on Category and image_id
      human_machine <- merge(human_participant_data, machine_participant_data, by = c("Category", "image_id"), all = TRUE)
      
      # Create a data frame with the responses for kappa calculation
      k_data <- data.frame(human_machine[[paste0(Condition, ".x")]], 
                           human_machine[[paste0(Condition, ".y")]])
      
      # Calculate kappa (Unweighted Kappa for categorical data without a logical order)
      kappa_result <- kappa2(k_data, weight = "unweighted")
      
      # Store the model and kappa value in the list
      kappa_values_list[[paste0(model, "_", "human", "_", Condition)]] <- kappa_result$value
    }
    
    # Combine all the kappa values into a dataframe for the current participant
    participant_df <- as.data.frame(kappa_values_list)
    
    # Store the participant ID and kappa values in the results list
    results_list[[as.character(subj_id)]] <- participant_df
  }
  
  return(results_list)
}




# Read humans data from Context Experiments

## Human 50ms data

### Use the function to process human 50ms data 
file_path2 <- "Experiment2/human50Exp2_data.csv"

### Use the function to process human 50ms data with custom output names
prepare_human_data(file_path2,crop_name = "data2_crop", full_name = "data2_full")  # create data2_crop and data2_full
kappa50_crop <- calculate_kappa_data(data2_crop, "Crop")
kappa50_full <- calculate_kappa_data(data2_full, "Full")
data2_crop <- data.table(data2_crop) %>% select(Subject, Category, image_id,Crop) 
data2_full <- data.table(data2_full) %>% select(Subject, Category, image_id,Full)
human50_crop <- kappa50_crop %>% select(Kappa)
human50_full <- kappa50_full %>% select(Kappa)


## Human self-paced data 
file_path3 <- "Experiment3/humanselfExp2_data.csv"

### Use the function to process human self-paced data with custom output names
prepare_human_data(file_path3,crop_name = "data3_crop", full_name = "data3_full")  # create data2_crop and data2_full
kappa_sp_crop <- calculate_kappa_data(data3_crop, "Crop")
kappa_sp_full <- calculate_kappa_data(data3_full, "Full")
data3_crop <- data.table(data3_crop) %>% select(Subject, Category, image_id,Crop)
data3_full <- data.table(data3_full) %>% select(Subject, Category, image_id,Full)
human_sp_crop <- kappa_sp_crop %>% select(Kappa)
human_sp_full <- kappa_sp_full %>% select(Kappa)




# MACHINE VS MACHINE
# Read your data
file_path_models <-"Experiment2/Exp2_models_data_for_kappa.csv"
machine_data <-process_mach_data(file_path_models)
# Access the reshaped data
machine_crop <-machine_data$crop %>% mutate(Crop = tolower(Crop))
machine_full <- machine_data$full %>% mutate(Full = tolower(Full))

kmachine_crop <- calculate_kappa_data(machine_crop, "Crop")
kmachine_full <- calculate_kappa_data(machine_full , "Full")
model_data_crop <- data.frame(kmachine_crop)
model_data_full <- data.frame(kmachine_full)
# Add participant ID as a column
model_data_crop <-  cbind(model_data_crop, cond='Crop')
model_data_full <-  cbind(model_data_full, cond='Full')
model_data <-rbind(model_data_crop, model_data_full)
#write.csv(model_data, paste("Exp2_models_kappa.csv", sep = ""))



# HUMAN VS MACHINE

# Calculate kappa values between self-paced group and machines
results_sp_crop <- calculate_kappa_human_model(human_data =data3_crop, machine_data = machine_crop,
                                             Condition = "Crop")
cresults_sp <- do.call(rbind, results_sp_crop)

# merge human-human kappa for crop
chuman_sp_human_sp <- human_sp_crop %>% rename("chuman_sp_human_sp" = "Kappa")
call_results_sp <- rbind.fill(cresults_sp,chuman_sp_human_sp)
call_results_sp <- pivot_longer(data= call_results_sp,
                          cols = c("CoCa_ViT_L14_human_Crop":"VGG19_human_Crop", "chuman_sp_human_sp"),
                          values_to = "Kappa_values",
                          names_to = "Who")
call_results_sp <- na.omit(call_results_sp)
call_sp <- call_results_sp %>% select(Kappa_values, Who)
unique(call_sp$Who)


results_sp_full <- calculate_kappa_human_model(human_data = data3_full, machine_data = machine_full,
                                               Condition = "Full")
fresults_sp <- do.call(rbind, results_sp_full)

# merge human-human kappa for full
fhuman_sp_human_sp<- human_sp_full %>% rename("fhuman_sp_human_sp" = "Kappa")
fall_results_sp <- rbind.fill(fresults_sp,fhuman_sp_human_sp)
fall_results_sp <- pivot_longer(data= fall_results_sp,
                                cols = c("CoCa_ViT_L14_human_Full":"VGG19_human_Full", "fhuman_sp_human_sp"),
                                values_to = "Kappa_values",
                                names_to = "Who")
fall_results_sp <- na.omit(fall_results_sp)
fall_sp <- fall_results_sp %>% select(Kappa_values, Who)


# Calculate kappa values between 50ms group and machines
results_50_crop <- calculate_kappa_human_model(human_data =data2_crop, machine_data = machine_crop,
                                               Condition = "Crop")
cresults_50 <- do.call(rbind, results_50_crop)
# merge human-human in 50ms kappa for crop
chuman50_human50<- human50_crop %>% rename("chuman50_human50" = "Kappa")
call_results_50 <- rbind.fill(cresults_50,chuman50_human50)
call_results_50 <- pivot_longer(data= call_results_50,
                                cols = c("CoCa_ViT_L14_human_Crop":"VGG19_human_Crop", "chuman50_human50"),
                                values_to = "Kappa_values",
                                names_to = "Who")
call_results_50 <- na.omit(call_results_50)
call_50 <- call_results_50 %>% select(Kappa_values, Who)

# merge human-human in 50ms kappa for full
results_50_full <- calculate_kappa_human_model(human_data = data2_full, machine_data = machine_full,
                                               Condition = "Full")
fresults_50 <- do.call(rbind, results_50_full)

fhuman50_human50<- human50_full %>% rename("fhuman50_human50" = "Kappa")
fall_results_50 <- rbind.fill(fresults_50,fhuman50_human50)
fall_results_50 <- pivot_longer(data= fall_results_50,
                                cols = c("CoCa_ViT_L14_human_Full":"VGG19_human_Full", "fhuman50_human50"),
                                values_to = "Kappa_values",
                                names_to = "Who")
fall_results_50 <- na.omit(fall_results_50)
fall_50 <- fall_results_50 %>% select(Kappa_values, Who)

call_50 <- cbind(call_50, cond='Human50_crop')
fall_50 <- cbind(fall_50, cond='Human50_full')

all_50 <- rbind(call_50,fall_50)
results_meds50 <- ddply(all_50, .(cond, Who), summarise, med = median(Kappa_values,na.rm=TRUE))

length(unique(all_50$Who))

allcolor <- c( "chuman50_human50"="#FCCDE5", "CoCa_ViT_L14_human_Crop"= "#00AEDB",
               "VIT_B8_human_Crop"= "#8DD3C7", "VIT_L16_human_Crop"=  "#B3DE69",
               "ResNet152_human_Crop"= "#FFFFB3","InceptionV3_human_Crop"= "#FB8072", "VGG19_human_Crop"= "#FDB462",
               "fhuman50_human50"= "#FCCDE5","CoCa_ViT_L14_human_Full"= "#00AEDB", "VIT_B8_human_Full"="#8DD3C7",
               "VIT_L16_human_Full"= "#B3DE69","ResNet152_human_Full"= "#FFFFB3",
               "InceptionV3_human_Full" ="#FB8072" , "VGG19_human_Full" ="#FDB462"  )

level <- c('Human50_crop','Human50_full')
level_order<-  c( "chuman50_human50", "CoCa_ViT_L14_human_Crop", "VIT_B8_human_Crop", "VIT_L16_human_Crop",
                  "ResNet152_human_Crop","InceptionV3_human_Crop", "VGG19_human_Crop", "fhuman50_human50",
                  "CoCa_ViT_L14_human_Full", "VIT_B8_human_Full", "VIT_L16_human_Full",
                  "ResNet152_human_Full","InceptionV3_human_Full", "VGG19_human_Full" )

rsa50 <- ggplot(data =all_50, mapping = aes(x = factor(Who, levels = level_order), y = Kappa_values, fill = Who))+
  geom_violin(size=0.5, trim=TRUE, alpha=0.4, scale='width',position = position_dodge(0.9)) +  
  scale_fill_manual(values = allcolor)+
  geom_jitter(data = all_50,aes(x = Who, y = Kappa_values, fill =Who),alpha=0.6, shape = 21, width=0.3,size= 2) +
  geom_boxplot(data = all_50,aes(x = Who, y = Kappa_values, fill = Who),width = 0.2, fill = "burlywood", color = "black")+ 
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
  geom_label(data = results_meds50, aes(x = Who, y = med, label = round(med,2)), fill="#d8e01b",
             size = 3.6, vjust = -0.85, col = "black",fontface='bold',
             label.padding = unit(0.5, 'lines'), inherit.aes = FALSE)

pdf(file = "all_kappa50_violin2.pdf", width = 8, height = 12)
print(rsa50)
dev.off()

## violin plot for human self-paced
call_sp <- cbind(call_sp, cond='Humansp_crop')
fall_sp <- cbind(fall_sp, cond='Humansp_full')

all_sp <- rbind(call_sp,fall_sp)
results_meds_sp <- ddply(all_sp, .(cond, Who), summarise, med = median(Kappa_values,na.rm=TRUE))

length(unique(all_sp$Who))

allcolor <- c( "chuman_sp_human_sp"="#FCCDE5", "CoCa_ViT_L14_human_Crop"= "#00AEDB",
               "VIT_B8_human_Crop"= "#8DD3C7", "VIT_L16_human_Crop"=  "#B3DE69",
               "ResNet152_human_Crop"= "#FFFFB3","InceptionV3_human_Crop"= "#FB8072", "VGG19_human_Crop"= "#FDB462",
               "fhuman_sp_human_sp"= "#FCCDE5","CoCa_ViT_L14_human_Full"= "#00AEDB", "VIT_B8_human_Full"="#8DD3C7",
               "VIT_L16_human_Full"= "#B3DE69","ResNet152_human_Full"= "#FFFFB3",
               "InceptionV3_human_Full" ="#FB8072" , "VGG19_human_Full" ="#FDB462"  )

level <- c('Humansp_crop','Humansp_full')
level_order<-  c( "chuman_sp_human_sp", "CoCa_ViT_L14_human_Crop", "VIT_B8_human_Crop", "VIT_L16_human_Crop",
                  "ResNet152_human_Crop","InceptionV3_human_Crop", "VGG19_human_Crop", "fhuman_sp_human_sp",
                  "CoCa_ViT_L14_human_Full", "VIT_B8_human_Full", "VIT_L16_human_Full",
                  "ResNet152_human_Full","InceptionV3_human_Full", "VGG19_human_Full" )

rsa_sp <- ggplot(data =all_sp, mapping = aes(x = factor(Who, levels = level_order), y = Kappa_values, fill = Who))+
  geom_violin(size=0.5, trim=TRUE, alpha=0.4, scale='width',position = position_dodge(0.9)) +  
  scale_fill_manual(values = allcolor)+
  geom_jitter(data = all_sp,aes(x = Who, y = Kappa_values, fill =Who),alpha=0.6, shape = 21, width=0.3,size= 2) +
  geom_boxplot(data = all_sp,aes(x = Who, y = Kappa_values, fill = Who),width = 0.2, fill = "burlywood", color = "black")+ 
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
  geom_label(data = results_meds_sp, aes(x = Who, y = med, label = round(med,2)), fill="#d8e01b",
             size = 3.2, vjust = -0.85, col = "black",fontface='bold',
             label.padding = unit(0.3, 'lines'), inherit.aes = FALSE)

pdf(file = "all_kappa_sp_violin2_.pdf", width = 8, height = 12)
print(rsa_sp)
dev.off()
