

# Purpose: Plot lean mass per leg over time and volume conditions ####





## Load packages, Read the data #####

library(tidyverse)
library(exscidata)




## Plot using ggplot2

leg_volume <- exscidata::dxadata |>
  select(participant, single, multiple) |>
  
  pivot_longer(names_to = "volume", 
               values_to = "leg", 
               cols = single:multiple) |>
  
  
  distinct(participant, leg, volume) %>%
  
  
  print()



lean_mass <- exscidata::dxadata |>
  select(participant, include, time, 
         starts_with("lean.") & contains("_leg")) |>
  
  
  
  pivot_longer(names_to = "leg", 
               values_to = "leanmass", 
               cols = starts_with("lean")) |>
  
  
  mutate(leg = if_else(leg == "lean.left_leg", "L", "R")) |>
  print()



lean_mass <- full_join(leg_volume, lean_mass)



lean_mass_sum <- lean_mass |>
  filter(include == "incl") |>
  
  summarise(Median = median(leanmass), 
            Mean = mean(leanmass), 
            SD = sd(leanmass),
            q25 = quantile(leanmass, 0.25), 
            q75 = quantile(leanmass, 0.75), 
            Min = min(leanmass),
            Max = max(leanmass),
            
            .by = c(time, volume)) |>
  
  print()




p <- lean_mass_sum |>
  mutate(Time = if_else(time == "pre", 1, 2)) |>
  ggplot( aes(Time, Median, fill = volume)) +
  
  
  geom_errorbar(aes(ymin = Min, ymax = Max), 
                position = position_dodge(width = 0.4), 
                width = 0) + 
  
  
  geom_rect(aes(xmin = Time - 0.18, 
                xmax = Time + 0.18, 
                ymin = Median, 
                ymax = q75), 
            color = "black", 
            position = position_dodge(width = 0.4)) + 
  
  geom_rect(aes(xmin = Time - 0.18, 
                xmax = Time + 0.18, 
                ymin = q25, 
                ymax = Median), 
            color = "black", 
            position = position_dodge(width = 0.4)) 







p <- lean_mass |>
  filter(include == "incl") |>
  
  mutate(time = factor(time, levels = c("pre", "post"))) |>
  
  ggplot(aes(time, leanmass, fill = volume)) + geom_boxplot() +
  
  labs(x = "Time", 
       y = "Leg lean mass (g)", 
       fill = "Volume condition") +
  
  scale_x_discrete(labels = c("Pre\nintervention", 
                              "Post\nintervention")) +
  
  theme(legend.position = "top") +
  
  scale_y_continuous(limits = c(0, 14000))
  
 
  



## Save the plot as a pdf file

ggsave("figures/lean_mass.pdf", 
       p, 
       width = 8.9, 
       height = 8.9, 
       units = "cm")







