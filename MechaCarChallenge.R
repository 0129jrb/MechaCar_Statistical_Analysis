#deliverable 1 
#import data 
Mecharcar_mpg <- read.csv(file ='MechaCar_mpg.csv', check.names =F, stringsAsFactors = F)
lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars) #generate multiple linear regression model
summary(lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars)) #generate summary statistics

#deliverable 2 
#import data 
Suspension_Coil <- read.csv(file ='Suspension_Coil.csv', check.names =F, stringsAsFactors = F)
total_summary <- Suspension_Coil %>% summarize(Mean=mean(PSI),Median=median(PSI),Varience = var(PSI), SD = sd(PSI)) #create summary table with multiple columns
lot_summary <- Suspension_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Varience = var(PSI), SD = sd(PSI))

#deliverable 3 
t.test(Suspension_Coil$PSI, mu=1500)
t.test(subset(Suspension_Coil, Manufacturing_Lot == "Lot1")$PSI, mu=1500)

t.test(subset(Suspension_Coil, Manufacturing_lot == "Lot2")$PSI, mu=1500)

t.test(subset(Suspension_Coil, Manufacturing_lot == "Lot3")$PSI, mu=1500)

       
