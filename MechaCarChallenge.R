library(jsonlite)
library(tidyverse)

#deliverable 1
mpgdf <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
lm(mpg  ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mpgdf)
summary(lm(mpg  ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mpgdf))

#deliverable 2
suscoil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
total_summary <- suscoil%>% summarize(Mean=mean(PSI),median=median(PSI),variance=var(PSI),stdev = sd(PSI), .groups = 'keep')
lot_summary <- suscoil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),median=median(PSI),variance=var(PSI),stdev = sd(PSI), .groups = 'keep')

#deliverable 3
t.test(log10(lot_summary$Mean),mu=mean(log10(total_summary$Mean)))

t.test(log10(subset(lot_summary, Manufacturing_Lot == "Lot1", Mean)),mu=mean(log10(total_summary$Mean)))
t.test(log10(subset(lot_summary, Manufacturing_Lot == "Lot2", Mean)),mu=mean(log10(total_summary$Mean)))
t.test(log10(subset(lot_summary, Manufacturing_Lot == "Lot3", Mean)),mu=mean(log10(total_summary$Mean)))

t.test(log10(1500),mu=mean(log10(1500)))




tony <- lot_summary[1,2]





