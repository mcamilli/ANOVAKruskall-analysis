### Survival Graph ###

install.packages("tidyverse")
install.packages("janitor")
install.packages("tidyquant")
install.packages("patchwork")
install.packages("survival")
install.packages("survminer")
remove.packages("rlang")
install.packages("rlang")
install.packages("dplyr")


library(tidyverse)
library(janitor)
library(tidyquant)
library(patchwork)
library(survival)
library(survminer)
library(dplyr)
library(rlang)


#1 Load the data

survivalData <- survivalData 

#2 Fit and summarize the data

fit1 <- survfit(Surv(as.numeric(time,event)) ~ 1, data = survivalData)

summary(fit1)

#3 Plot all the survival data

ggsurvplot(fit1, data = survivalData)

kmsurviv <- ggsurvplot(fit1, data = survivalData)


#4 Plot for all treatments

fit2 <- survfit(Surv(as.numeric(time,event)) ~ treatment, data = survivalData)


ggsurvplot(fit2, data = survivalData)


kmTreatment <- ggsurvplot(fit2, data = survivalData,
                          xlim= c(1,20),
                          break.x.by= 2, xlab = "Days", ylab= c ("Percent Survival"),
                          legend.title = "",
                          legend.labs = c("3%", "6%", "9%", "0"),
                          palette = c("darkorange","blue", "red", "darkgreen"),
                          pval = F,
                          risk.table = F,
                          conf.int = T,
                          tables.theme = theme_cleantable())

print(kmTreatment)

#5 for each treatment x control group

fitc3 <- survfit(Surv(as.numeric(time,event)) ~ treatment , data = control_3)

fitc6 <- survfit(Surv(as.numeric(time,event)) ~ treatment , data = control_6)

fitc9 <- survfit(Surv(as.numeric(time,event)) ~ treatment , data = control_9)



ggsurvplot(fitc3, data = control_3,
           legend.title = "treatments",
           legend.labs = c("3%", "control"),
           pval = T,
           risk.table = T,
           conf.int = T,
           tables.theme = theme_cleantable())


ggsurvplot(fitc6, data = control_6,
           legend.title = "treatments",
           legend.labs = c("6%", "control"),
           pval = T,
           risk.table = T,
           conf.int = T,
           tables.theme = theme_cleantable())



ggsurvplot(fitc9, data = control_9,
           legend.title = "treatments",
           legend.labs = c("9%", "control"),
           pval = T,
           risk.table = T,
           conf.int = T,
           tables.theme = theme_cleantable())