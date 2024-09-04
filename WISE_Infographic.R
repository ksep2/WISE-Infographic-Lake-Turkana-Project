
##################################################
##                                              ##
##        Water Insecurity Experiences          ##    
##                (WISE) Scales                 ##
##                                              ##
##          PDF Infographic Generator           ##
##                                              ##
##                                              ##
##            Author: Scott Miller              ##
##        scott.miller@charitywater.org         ##
##                                              ##
##           Last Updated: 7/16/2024            ##
##                                              ##
##################################################

# Copyright (c) 2024 Scott M. Miller 
# This code is licensed under the MIT License.
# For full license text, see the LICENSE file in the repository root.


# ----------------------------------
# Install relevant packages
# ----------------------------------

install.packages(c("readxl", "dplyr", "data.table", "ggplot2", "grid"))        #Note: Remove the '#' at the start of this line to install packages
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(grid)


# ----------------------------------
# Set Directory & Import Data
# ----------------------------------

# Set directory: this needs to match the folder where WISE data is stored
setwd("/Users/katie/Documents/")

# Import Data: this needs to match the data set stored in the directory folder
data <- read_excel("IWISE_Lake_Turkana.xlsx")


# ----------------------------------
# Define Background Details for Automatic Text Generation
# ----------------------------------

text_scale <- "Individual (IWISE)"   # Enter the WISE scale (e.g. HWISE, iWISE, etc.)
text_country <- "Kenya"    # Enter country name
text_survey <- "Baseline Survey"    # Enter brief survey description
text_collectedby <- "Strathmore University"   # Enter name of organization that collected this data
text_timing <- "August 2024"  # Enter the timing of data collection 
text_geography <- "Project implementation areas within Turkana County"     # Describe the geographic coverage of the surveys


# ----------------------------------
# Data Cleaning & Naming
# ----------------------------------

# create an object with all WISE scale variable names
names <- c("WISE_Worry", "WISE_Angry", "WISE_Interrupt", "WISE_Clothes", "WISE_Plans",
           "WISE_Food", "WISE_Hands", "WISE_Body", "WISE_Drink", "WISE_Sleep",
           "WISE_None", "WISE_Shame")

# create a new variable `WISE_[item]_n' for each variable based on numerical codes
for (name in names) {
    data <- data %>%
        mutate(!!paste0(name, "_n") := case_when(
            .data[[name]] == "Never" ~ 0,
            .data[[name]] == "Rarely (1–2 times in the last 4 weeks)" ~ 1,
            .data[[name]] == "Sometimes (3–10 times in the last 4 weeks)" ~ 2,
            .data[[name]] == "Often (10-20 times in last 4 weeks)" ~ 3,
            .data[[name]] == "Always (More than 20 times in last 4 weeks)" ~ 3,
            TRUE ~ NA_real_
        ))
}


# calculate each observation's overall WISE score
data$WISE_Score <- rowSums(data[, grep("_n$", names(data))], na.rm = F)

# create 'Insecurity' variables for binary (>=12) and level cutoffs
data <- data %>%
    mutate(
        Insecurity_binary = case_when(
            !is.na(WISE_Score) & WISE_Score <= 11 ~ "Not Water Insecure",
            !is.na(WISE_Score) & WISE_Score >= 12 ~ "Water Insecure",
            TRUE ~ as.character(NA)
        ),
        Insecurity_level = case_when(
            !is.na(WISE_Score) & WISE_Score <= 2 ~ "Low-to-no",
            !is.na(WISE_Score) & WISE_Score <= 11 ~ "Mild",
            !is.na(WISE_Score) & WISE_Score <= 23 ~ "Moderate",
            !is.na(WISE_Score) & WISE_Score >= 24 ~ "Severe",
            TRUE ~ as.character(NA)
        )
    )

# ----------------------------------
# WISE Analysis
# ----------------------------------

#-----------------------
## HWISE Classification
#-----------------------

# create a table with the % of HHs classified as water insecure (HWISE Score < 12)
prop.table(table(as.character(data$Insecurity_binary))) * 100

# save the percentage of insecure responses
pct_insecure <- paste(round(prop.table(table(as.character(data$Insecurity_binary)))[2]*100, 1), "%", sep = "")

# create a table with the % of HHs in each HWISE grouping (Low-to-no, mild, moderate, severe water insecurity)
prop.table(table(as.character(data$Insecurity_level))) * 100


# Calculate percentage of observations >= 1 for each variable
percentages <- colMeans(data[, grep("_n$", names(data))] > 0, na.rm = TRUE) * 100

# Convert to data.table in long format
long_data <- data.table(variable = names(percentages), percentage = percentages) 

# Create and store bar graph
Item_Prevalence <- ggplot(long_data, aes(x = variable, y = percentage)) +
    geom_bar(stat = "identity", fill = "#2074bc") +  # Set all bars to the same custom color
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100)) +  # Set y-axis limits
    coord_flip() +
    labs(x = "", y = "", title = "") +
    theme(
        legend.position = "none"  # Remove legend
    ) +
    geom_text(aes(label = paste(round(percentage, 1), "%", sep = "")),  # Add this line to annotate bars with their percentage values
              position = position_dodge(width = 0.9), hjust = 1.3, color = "white") +
    scale_x_discrete(labels = c("WISE_Worry_n" = "Worry about water", 
                                "WISE_Angry_n" = "Felt angry about water",
                                "WISE_Interrupt_n" = "Had supply interuptions",
                                "WISE_Clothes_n" = "Could not wash clothes",
                                "WISE_Plans_n" = "Had to change plans",
                                "WISE_Food_n" = "Had to eat differently",
                                "WISE_Hands_n" = "Could not wash hands",
                                "WISE_Body_n" = "Could not wash body",
                                "WISE_Drink_n" = "Had no water to drink",
                                "WISE_Sleep_n" = "Went to bed thirsty",
                                "WISE_None_n" = "Had no water at all",
                                "WISE_Shame_n" = "Felt shame about water"
    ))



#-----------------------
## Household Size Decomposition
#-----------------------

# Group household size into categories
data$HHsize_group <- cut(data$`Household Size`, breaks = c(0, 5, 7, 10, Inf),
                    labels = c("<5", "5-7", "8-10", "11+"),
                    right = FALSE)

# Calculate the mean of Y for each X_group
hhsize_data <- data %>%
    group_by(HHsize_group) %>%
    summarize(pct_insecure = mean(Insecurity_binary == "Water Insecure", na.rm = TRUE) * 100)

# Create and store bar graph
Insecurity_by_HHsize <- ggplot(hhsize_data, aes(x = HHsize_group, y = pct_insecure)) +
    geom_bar(stat = "identity", fill = "#2074bc") +
    labs(x = "Household Size", y = "Insecurity") +
    geom_text(aes(label = paste(round(pct_insecure, 1), "%", sep = "")),  # Add this line to annotate bars with their percentage values
              position = position_dodge(width = 0.9), vjust = 1.3, color = "white") +
    theme_minimal()


#-----------------------
## District Decomposition
#-----------------------

# Calculate the mean of Y for each X_group
district_data <- data %>%
    group_by(District) %>%
    summarize(pct_insecure = mean(Insecurity_binary == "Water Insecure", na.rm = TRUE) * 100)

# Create the ggplot bar graph
Insecurity_by_District <- ggplot(district_data, aes(x = District, y = pct_insecure)) +
    geom_bar(stat = "identity", fill = "#2074bc") +
    labs(x = "District", y = "Insecurity") +
    geom_text(aes(label = paste(round(pct_insecure, 1), "%", sep = "")),  # Add this line to annotate bars with their percentage values
              position = position_dodge(width = 0.9), vjust = 1.3, color = "white") +
    theme_minimal()




# ------------------------------------------------------------------------------
# Generate Infographic in PDF format
# ------------------------------------------------------------------------------


#-----------------------
# Create PDF & grid layout
#-----------------------

pdf("Infographic.pdf", width = 10, height = 14)

grid.newpage() 
pushViewport(viewport(layout = grid.layout(9, 4)))
grid.rect(gp = gpar(fill = "#E2E2E3", col = "#E2E2E3"))


#-----------------------
# Header - grey background with Title
#-----------------------

grid.rect(gp = gpar(fill = "grey", col = "grey", alpha = 0.6), 
          x = unit(0.5, "npc"), 
          y = unit(0.95, "npc"), 
          width = unit(1, "npc"), 
          height = unit(0.2285, "npc"))

grid.text("Water Insecurity Experiences (WISE) Scales", 
          y = unit(0.98, "npc"), 
          x = unit(0.5, "npc"), 
          vjust = 3, hjust = .5, 
          gp = gpar(col = "#2074bc", cex = 2.4, alpha = 1))

grid.text(paste(text_country, text_survey, sep = " "), 
          y = unit(0.88, "npc"), 
          gp = gpar(col = "#2074bc", cex = 2.2))


#-----------------------
# Subheader - Blue background with data details
#-----------------------

grid.rect(gp = gpar(fill = "#2074bc", col = "#2074bc", alpha = 0.6), 
          x = unit(0.5, "npc"), 
          y = unit(0.78, "npc"), 
          width = unit(1, "npc"), 
          height = unit(0.11, "npc"))

grid.text("", 
          y = unit(0.78, "npc"), 
          x = unit(0.5, "npc"), 
          vjust = .5, hjust = .5, 
          gp = gpar(col = "#CA8B01", cex = 13, alpha = 0.3))

grid.text(paste(
    "",
    "Scale:",
    "Collected by:",
    "Collection Timing:",
    "Geographic Coverage:",
    "",
    "", sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.05, "npc"), 
    y = unit(0.72, "npc"), 
    gp = gpar(col = "Black", cex = 1, fontface = "bold"))

grid.text(paste(
    "",
    text_scale,
    text_collectedby,
    text_timing,
    text_geography,
    "",
    "",sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.27, "npc"), 
    y = unit(0.72, "npc"), 
    gp = gpar(col = "Black", cex = 1))


#-----------------------
# Top left paragraph (needs to be manually updated)
#-----------------------

grid.text(paste(
    "Northwestern University, charity: water,",
    "& others",
    sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.05, "npc"), 
    y = unit(0.66, "npc"), 
    gp = gpar(col = "Black", cex = 1.3, fontface = "bold"))


grid.text(paste(
    "                     have partnered to estimate experiences",
    "with water access and use around the world. Currently, 58",
    "organizations across 22 countries in Africa and Asia are",
    "measuring water insecurity in their program areas through",
    "charity: water's monitoring & evaluation framework.",
    "",
    sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.05, "npc"), 
    y = unit(0.575, "npc"), 
    gp = gpar(col = "Black", cex = 1))


#-----------------------
# Center left - paragraph
#-----------------------

grid.text(paste(
    paste("Who is water insecure in ",text_country, "?", sep = ""),
    "", sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.05, "npc"), 
    y = unit(0.53, "npc"), 
    gp = gpar(col = "Black", cex = 1.3, fontface = "bold"))


grid.text(paste(
    pct_insecure,
    "", sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.05, "npc"), 
    y = unit(0.485, "npc"), 
    gp = gpar(col = "#2074bc", cex = 1.7, fontface = "bold"))

grid.text(paste(
    "                   of households participating in the survey",
    "experienced moderate-to-high water insecurity in 2023.",
    "", sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.05, "npc"), 
    y = unit(0.48, "npc"), 
    gp = gpar(col = "Black", cex = 1))

#-----------------------
# Center left - Insecurity by HH size graph
#-----------------------

pushViewport(viewport(layout.pos.row = 6:7, layout.pos.col = 1:2))
print(Insecurity_by_HHsize, newpage = FALSE)
popViewport()


#-----------------------
# Bottom left - Insecurity by District graph
#-----------------------

pushViewport(viewport(layout.pos.row = 8:9, layout.pos.col = 1:2))
print(Insecurity_by_District, newpage = FALSE)
popViewport()


#-----------------------
# Top right paragraph
#-----------------------

grid.text(paste(
    "How did we measure water insecurity?",
    "", sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.53, "npc"), 
    y = unit(0.66, "npc"), 
    gp = gpar(col = "Black", cex = 1.3, fontface = "bold"))

grid.text(paste(
    "Most indicators measure water availability or",
    "infrastructure. These don’t tell us about people’s",
    "ability to reliably access or use water or how water",
    "insecurity varies by gender, age, etc. Which means we",
    "haven’t known exactly who is left behind… until now.",
    sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.53, "npc"), 
    y = unit(0.592, "npc"), 
    gp = gpar(col = "Black", cex = 1))


#-----------------------
# Center right paragraph
#-----------------------

grid.text(paste(
    "How does water insecurity manifest",
    "in this program area?", 
    sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.53, "npc"), 
    y = unit(0.53, "npc"), 
    gp = gpar(col = "Black", cex = 1.3, fontface = "bold"))

grid.text(paste(
    "We used the Household Water InSecurity Experiences",
    "(HWISE) Scale to measure individual experiences with",
    "water access and use. Respondents had the following",
    "negative experiences due to water problems in the",
    "last month.",
    sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.53, "npc"), 
    y = unit(0.44, "npc"), 
    gp = gpar(col = "Black", cex = 1))


#-----------------------
# Center right - Item Prevalence Graph
#-----------------------

pushViewport(viewport(layout.pos.row = 6:8, layout.pos.col = 3:4))
print(Item_Prevalence, newpage = FALSE)
popViewport()



grid.text(paste(
    "These data provide insights on prevalence and severity",
    "of water insecurity that can guide policymaking,",
    "including resource allocation. The WISE Scales will also",
    "be used to measure the impact of interventions, and",
    "monitor progress and accountability.",
    sep = "\n"), vjust = 0, hjust = 0, 
    x = unit(0.53, "npc"), 
    y = unit(0.025, "npc"), 
    gp = gpar(col = "Black", cex = 1))

dev.off()


