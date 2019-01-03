#---------------------------------------------------Header----------------------------------------------
#AUTHOR: Nate Kell
#PROJECT: Term Paper
#DUE: End of Semester

#--------------------------------------------Links and References---------------------------------------

#Project Website: https://www.analyticsvidhya.com/blog/2018/05/24-ultimate-data-science-projects-to-boost-your-knowledge-and-skills/
#Data Description: http://archive.ics.uci.edu/ml/datasets/YearPredictionMSD
#Nearest Neighbors: http://www-personal.umich.edu/~yjli/content/projectreport.pdf
#Various Methods: https://cseweb.ucsd.edu/classes/wi17/cse258-a/reports/a028.pdf

#-------------------------------------Import Libraries and Setup Environment-----------------------------

#install.packages("data.table")
#install.packages("hexbin")
#install.packages("rstudioapi")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("MuMIn")
#install.packages("randomForest")

library(randomForest)
library(data.table)
library(hexbin)
library(rstudioapi)
library(ggplot2)
library(GGally)
library(MuMIn)

par(mfrow = c(1, 1))
cat("\014")
setwd(dirname(getActiveDocumentContext()$path))

#----------------------------------------Read Data and Organize Subsets-----------------------------------
system.time(dt <- fread("data.txt"))
attach(dt)

subset <- round(runif(20000, 0, 463715), 0)
dt.train <- dt[subset,]       #smaller subset to test faster
dt.test <- dt[463716:515345,] #test data

#----------------------------------------------Visualize Data--------------------------------------------
head(dt.train)
str(dt.train)
hist(dt.train$V1, breaks=50, main="Release Years in Dataset", xlab="Year")
abline(v = mean(dt.train$V1),
       col = "royalblue",
       lwd = 2)

abline(v = median(dt.train$V1),
       col = "red",
       lwd = 2)

legend(x = "topleft", # location of legend within plot area
       c("Mean", "Median"),
       col = c("royalblue", "red"),
       lwd = c(2, 2))

#-------------------------------------------Attempt basic models-----------------------------------------
#Mean/Median Model----------------------------------------------------------
mn <- mean(dt.train$V1)
errors = c()
mn

for(i in 1:20000){
  means[i] = mn
}

errors = abs(means - dt.train$V1)
mean(errors)
hist(errors, breaks=20, main=expression(paste("Error distribution for ", hat(y) == bar(y))))

dt.train.mm <- lm(dt.train$V1 ~ means)

dt.train.mm.summary <- summary(dt.train.mm)
dt.train.mm.anova <- summary(aov(dt.train.mm))

op <- par(mfrow = c(2,1))

dt.train.mm.summary
dt.train.mm.anova
plot(dt.train.mm, which = c(2, 1), main=expression(paste("Regression Plots for ", hat(y) == bar(y))))

#Basic Linear Model of first 12 Columns-----------------------------------------------------------------
dt.train.lin.small <- lm(dt.train$V1 ~ dt.train$V2 + dt.train$V3 + dt.train$V4 + dt.train$V5 + dt.train$V6 + dt.train$V7 + dt.train$V8 + dt.train$V9 + dt.train$V10 + dt.train$V11 + dt.train$V12)
dt.train.lin.small.summary <- summary(dt.train.lin.small)
dt.train.lin.small.anova <- summary(aov(dt.train.lin.small))

dt.train.lin.small.summary
dt.train.lin.small.anova
plot(dt.train.lin.small, which = c(2, 1))

#Basic Linear Model of all 90 Columns----------------------------------------------------------------------
dt.train.lin <- lm(dt.train$V1 ~ dt.train$V2 + dt.train$V3 + dt.train$V4 + dt.train$V5 + dt.train$V6 + dt.train$V7 + dt.train$V8 + dt.train$V9 + dt.train$V10 + dt.train$V11 + dt.train$V12 + dt.train$V13 + dt.train$V14 + dt.train$V15 + dt.train$V16 + dt.train$V17 + dt.train$V18 + dt.train$V19 + dt.train$V20 + dt.train$V21 + dt.train$V22 + dt.train$V23 + dt.train$V24 + dt.train$V25 + dt.train$V26 + dt.train$V27 + dt.train$V28 + dt.train$V29 + dt.train$V30 + dt.train$V31 + dt.train$V32 + dt.train$V33 + dt.train$V34 + dt.train$V35 + dt.train$V36 + dt.train$V37 + dt.train$V38 + dt.train$V39 + dt.train$V40 + dt.train$V41 + dt.train$V42 + dt.train$V43 + dt.train$V44 + dt.train$V45 + dt.train$V46 + dt.train$V47 + dt.train$V48 + dt.train$V49 + dt.train$V50 + dt.train$V51 + dt.train$V52 + dt.train$V53 + dt.train$V54 + dt.train$V55 + dt.train$V56 + dt.train$V57 + dt.train$V58 + dt.train$V59 + dt.train$V60 + dt.train$V61 + dt.train$V62 + dt.train$V63 + dt.train$V64 + dt.train$V65 + dt.train$V66 + dt.train$V67 + dt.train$V68 + dt.train$V69 + dt.train$V70 + dt.train$V71 + dt.train$V72 + dt.train$V73 + dt.train$V74 + dt.train$V75 + dt.train$V76 + dt.train$V77 + dt.train$V78 + dt.train$V79 + dt.train$V80 + dt.train$V81 + dt.train$V82 + dt.train$V83 + dt.train$V84 + dt.train$V85 + dt.train$V86 + dt.train$V87 + dt.train$V88 + dt.train$V89 + dt.train$V90 + dt.train$V91)

dt.train.lin.summary <- summary(dt.train.lin)
dt.train.lin.anova <- summary(aov(dt.train.lin))

dt.train.lin.summary
dt.train.lin.anova
plot(dt.train.lin, which = c(2, 1), main="Regression Plots with all 90 predictors")

#Basic Linear Model of first 12 Columns--------------------------------------
dt.train.lin.small <- lm(dt.train$V1 ~ dt.train$V2 + dt.train$V3 + dt.train$V4 + dt.train$V5 + dt.train$V6 + dt.train$V7 + dt.train$V8 + dt.train$V9 + dt.train$V10 + dt.train$V11 + dt.train$V12 + dt.train$V13 + dt.train$V14 + dt.train$V15 + dt.train$V16 + dt.train$V17 + dt.train$V18 + dt.train$V19 + dt.train$V20 + dt.train$V21 + dt.train$V22 + dt.train$V23 + dt.train$V24 + dt.train$V25 + dt.train$V26 + dt.train$V27 + dt.train$V28 + dt.train$V29 + dt.train$V30 + dt.train$V31 + dt.train$V32 + dt.train$V33 + dt.train$V34 + dt.train$V35 + dt.train$V36 + dt.train$V37 + dt.train$V38 + dt.train$V39 + dt.train$V40 + dt.train$V41 + dt.train$V42)
dt.train.lin.small.summary <- summary(dt.train.lin.small)
dt.train.lin.small.anova <- summary(aov(dt.train.lin.small))

dt.train.lin.small.summary
dt.train.lin.small.anova
plot(dt.train.lin.small, which = c(2, 1))

#Forest Attempt-----------------------------------------------------------------------
small <- dt.train[1:10000,]
rf <- randomForest(small$V1 ~ small$V2 + small$V3 + small$V4 + small$V5, data = small, ntree = 500, proximity = TRUE)
table(predict(rf), small$V1)
print(rf)

#Decade Forest Attempt----------------------------------------------------------------------
decades = small$V1 - small$V1 %% 10

rf <- randomForest(decades ~ small$V2 + small$V3 + small$V4 + small$V5 + small$V6 + small$V7 + small$V8 + small$V9 + small$V10 + small$V11 + small$V12 + small$V13 + small$V14 + small$V15 + small$V16 + small$V17 + small$V18 + small$V19 + small$V20 + small$V21 + small$V22 + small$V23 + small$V24 + small$V25 + small$V26 + small$V27 + small$V28 + small$V29 + small$V30 + small$V31 + small$V32 + small$V33 + small$V34 + small$V35 + small$V36 + small$V37 + small$V38 + small$V39 + small$V40 + small$V41 + small$V42 + small$V43 + small$V44 + small$V45 + small$V46 + small$V47 + small$V48 + small$V49 + small$V50 + small$V51 + small$V52 + small$V53 + small$V54 + small$V55 + small$V56 + small$V57 + small$V58 + small$V59 + small$V60 + small$V61 + small$V62 + small$V63 + small$V64 + small$V65 + small$V66 + small$V67 + small$V68 + small$V69 + small$V70 + small$V71 + small$V72 + small$V73 + small$V74 + small$V75 + small$V76 + small$V77 + small$V78 + small$V79 + small$V80 + small$V81 + small$V82 + small$V83 + small$V84 + small$V85 + small$V86 + small$V87 + small$V88 + small$V89 + small$V90 + small$V91, data = small, ntree = 200, proximity = TRUE)
table(predict(rf), decades)
print(rf)

#--------------------------------------------Document Teardown-------------------------------------------
par(mfrow = c(1, 1))
detach(dt)