library(dplyr)
library(tidyverse)
Mecha <- read.csv("MechaCar_mpg.csv")
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, Mecha))

Coil <- read.csv("Suspension_Coil.csv")
total_summary <- Coil %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')
lot_summary <- Coil %>% group_by (Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

t.test(log10(Coil$PSI),mu=1500)
t.test(log10(Coil$PSI),mu=lot_summary$"Mean"[1])
t.test(log10(Coil$PSI),mu=lot_summary$"Mean"[2])
t.test(log10(Coil$PSI),mu=lot_summary$"Mean"[3])
