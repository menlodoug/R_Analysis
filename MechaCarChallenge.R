library(tidyverse)
library(jsonlite)

mecha_car <- read.csv( 'MechaCar_mpg.csv' ,stringsAsFactors = F)
head(mecha_car)

mecha_matrix <- as.matrix(mecha_car[,c("vehicle.length","vehicle.weight","spoiler.angle","ground.clearance","AWD","mpg")]) #convert data frame into numeric matrix
cor(mecha_matrix)

lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=mecha_car) #generate multiple linear regression model
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance +AWD, data=mecha_car)) #generate summary statistics

plt <- ggplot(mecha_car,aes(x=log10(mpg))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

sample_table <- mecha_car %>% sample_n(20) #randomly sample 20 data points
plt <- ggplot(sample_table,aes(x=log10(mpg))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

t.test(log10(sample_table$mpg),mu=mean(log10(mecha_car$mpg))) #compare sample versus population means

sample_table <- mecha_car %>% sample_n(20) #generate 20 randomly sampled data points
sample_table2 <- mecha_car %>% sample_n(20) #generate another 20 randomly sampled data points

t.test(log10(sample_table$mpg),log10(sample_table2$mpg)) #compare means of two samples

coil <- read.csv( 'Suspension_Coil.csv' ,stringsAsFactors = F)
head(coil)

table(coil$Manufacturing_Lot, coil$PSI)
tbl <- table(coil$Manufacturing_Lot, coil$PSI) #generate contingency table
chisq.test(tbl) #compare categorical distributions

lm(PSI ~ Manufacturing_Lot,coil)
summary(lm(PSI ~ Manufacturing_Lot,coil)) #summarize linear model

model <- lm(PSI ~ Manufacturing_Lot,coil) #create linear model
yvals <- model$coefficients['Manufacturing_Lot']*coil$PSI + model$coefficients['(Intercept)'] #determine y-axis values from linear model
plt <- ggplot(coil,aes(x=Manufacturing_Lot,y=PSI)) #import dataset into ggplot2
plt + geom_point()#plot scatter and linear model

plt <- ggplot(coil,aes(y=PSI)) #import dataset into ggplot2
plt + geom_boxplot() #add boxplot

shapiro.test(coil$PSI)

summary (coil$PSI)  ## summary data for PSI

var(coil$PSI)  ## variance of PSI

sd(coil$PSI)  ## standard deviation of PSI

summarize_coil <- coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), St_Dev=sd(PSI), Min_PSI=min(PSI), Max_PSI=max(PSI)) #create summary table

install.packages("ggpubr")
library(ggpubr)
ggboxplot(coil, x = "Manufacturing_Lot", y = "PSI", 
          color = "Manufacturing_Lot", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Lot1", "Lot2", "Lot3"),
          ylab = "PSI", xlab = "")

ggline(coil, x = "Manufacturing_Lot", y = "PSI", 
       add = c("mean_se", "jitter"), 
       order = c("Lot1", "Lot2", "Lot3"),
       ylab = "PSI", xlab = "")


install.packages("gplots")
library(gplots)
# Box plot
boxplot(PSI ~ Manufacturing_Lot, data = coil,
        xlab = "", ylab = "PSI",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))
# plotmeans
library("gplots")
plotmeans(PSI ~ Manufacturing_Lot, data = coil, frame = FALSE,
          xlab = "", ylab = "PSI",
          main="Mean Plot with 95% CI")

# Compute the analysis of variance
res.aov <- aov(PSI ~ Manufacturing_Lot, data = coil)
# Summary of the analysis
summary(res.aov)

TukeyHSD(res.aov)

install.packages("multcomp")  ## Multiple comparisons using multcomp package
library(multcomp)
coil2 <- coil
coil2$Manufacturing_Lot <- as.factor(coil2$Manufacturing_Lot)
res.aov <- aov(PSI ~ Manufacturing_Lot, data = coil2)
summary(glht(res.aov, linfct = mcp(Manufacturing_Lot = "Tukey")))


summary.lm(res.aov)

sample_coil <- coil %>% sample_n(20) #randomly sample 20 data points
sample_coil2 <- coil %>% sample_n(20) #randomly sample 20 data points

t.test(log10(sample_coil$PSI),mu=mean(log10(coil$PSI))) #compare sample versus population means

t.test(log10(sample_coil$PSI),log10(sample_coil2$PSI)) #compare means of two samples

coil_lot1 <- coil %>% filter(Manufacturing_Lot=="Lot1") #select only data points where the Lot is Lot1
coil_lot2 <- coil %>% filter(Manufacturing_Lot=="Lot2") #select only data points where the Lot is Lot2
coil_lot3 <- coil %>% filter(Manufacturing_Lot=="Lot3") #select only data points where the Lot is Lot3
t.test(coil_lot1$PSI, coil_lot2$PSI,paired = T) #compare the mean difference between two samples
t.test(coil_lot1$PSI, coil_lot3$PSI,paired = T) #compare the mean difference between two samples
t.test(coil_lot2$PSI, coil_lot3$PSI,paired = T) #compare the mean difference between two samples

pairwise.t.test(coil$PSI, coil$Manufacturing_Lot, p.adjust.method = "BH")  ## Pairwise t-test
