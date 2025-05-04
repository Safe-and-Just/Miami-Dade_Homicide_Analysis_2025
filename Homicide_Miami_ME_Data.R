###############################################
#######Code written by A.D.S.##################
#######Bend, OR, USA, May 2025#################
###############################################

rm(list = ls())
graphics.off()
library(tidyverse)
library(ggpubr)
library(readr)
library(dplyr)
library(viridis)

#load homicide by zipcode data
df<- read.csv("Miami_Dade_Homicide_Zipcode_2020_to_2024.csv")

#rename columns
colnames(df) <- c("Zipcode", "2020",
                  "2021", "2022", 
                  "2023", "2024")


# Transform the data from wide to long format
df_long <- df %>%
  pivot_longer(cols = `2020`:`2024`, 
               names_to = "year", 
               values_to = "value") %>%
  mutate(year = as.integer(year))  # Convert year to numeric for plotting


total_by_year <- df_long %>%
  group_by(year) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(zipcode = "TOTAL")  #"total" line

total_by_year <- total_by_year[, c("zipcode", 
                                   "year",
                                   "value")]

colnames(total_by_year) <- c("Zipcode", 
                             "year",
                             "value")

df_long$Zipcode <- as.character(df_long$Zipcode)

df_combined <- bind_rows(df_long, total_by_year)


#Plot county and zipcode homcide counts
p1 <-ggplot(df_combined, aes(x = year, y = value, group = Zipcode)) +
  # Plot lines: if TOTAL, make it black; else, use color by ZIP
  geom_line(data = subset(df_combined, Zipcode == "TOTAL"),
            color = "black", size = 1) +
  geom_line(data = subset(df_combined, Zipcode != "TOTAL"),
            aes(color = factor(Zipcode)), size = 1) +
  scale_color_viridis_d() +
  labs(
    title = "Homicide counts in M-D by county and by zipcodes",
    x = "Year",
    y = ""
  ) +
  theme_classic() +
  theme(legend.position = "none")

####
##Circle of Brotherhood's primary zipcode##

highlight_zips <- c("33147", "33142")

label_df <- df_long %>%
  filter(Zipcode %in% highlight_zips) %>%
  group_by(Zipcode) %>%
  arrange(year) %>%
  slice(1) %>%
  ungroup()

# Plot zipcode-level homicide counts and highlight COB primary zipcodes

p2<- ggplot(df_long, aes(x = year, y = value, group = Zipcode)) +
  # All ZIP lines
  geom_line(aes(color = factor(Zipcode)), size = 0.3) +
  # Highlighted ZIP lines, thicker
  geom_line(data = subset(df_long, Zipcode %in% highlight_zips),
            aes(color = factor(Zipcode)), size = 1.2) +
  # Labels at start of the line, slightly above the point
  geom_text(data = label_df,
            aes(label = Zipcode),
            vjust = 2,  # Move text slightly above the point
            hjust = 0,     # Center the label horizontally on the x position
            nudge_x = 0.2, # Shift the label to the right by 0.2 units
            nudge_y = 2, # Shift the label to the right by 0.2 units
            size = 4) +
  scale_color_viridis_d() +
  labs(
    title = "Homicide Counts by Zipcode",
    x = "Year",
    y = ""
  ) +
  theme_classic() +
  theme(legend.position = "none")
#####

#Write the figure as a .png image 
png("Homicide_Miami_Dade.png", width = 5, height = 8, units="in", res = 400)
ggarrange(p1, p2, ncol=1)
dev.off()

#Calculate total number of homicides per year 
sum(df$`2023`) #[1] 154
sum(df$`2022`) #[1] 154
sum(df$`2021`) #[1] 183
sum(df$`2020`) #[1] 221

#Use the formula below to arrive at percentage decreases based on count data
100*(1 - (5/31)) #change between 2020 to 2024 in zipcode 33147 ## 83.87% 
100*(1 - (8/20)) #change between 2020 to 2024 in zipcode 33142 ## 60 7%
100*(1 - (5/18)) #change between 2023 to 2024 in zipcode 33147 ## 72.27%
