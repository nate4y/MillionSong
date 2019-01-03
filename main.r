#-------------------------------------------------------------------------------------------------------
#AUTHOR: Nate Kell
#PROJECT: Term Paper
#DUE: End of Semester

#Links and References-----------------------------------------------------------------------------------

#Project Website: https://www.analyticsvidhya.com/blog/2018/05/24-ultimate-data-science-projects-to-boost-your-knowledge-and-skills/
#Data Description: http://archive.ics.uci.edu/ml/datasets/YearPredictionMSD
#Nearest Neighbors: http://www-personal.umich.edu/~yjli/content/projectreport.pdf
#Various Methods: https://cseweb.ucsd.edu/classes/wi17/cse258-a/reports/a028.pdf

#Import Libraries and Setup Environment-----------------------------------------------------------------

#install.packages("data.table")
#install.packages("hexbin")
#install.packages("rstudioapi")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("MuMIn")

library(data.table) #reads table of 500,000+ obs of 91 vars in a few seconds using fread
library(hexbin)
library(rstudioapi)
library(ggplot2)
library(GGally)
library(MuMIn)

cat("\014")
setwd(dirname(getActiveDocumentContext()$path))


#Read Data and Organize Subsets-------------------------------------------------------------------------
system.time(dt <- fread("data.txt"))
attach(dt)

#dt.train <- dt[1:463715, 1:12]   #subset of full data that is training data
subset <- round(runif(30000, 0, 463715), 0)
dt.train <- dt[subset,]      #smaller subset to test faster
df <- data.frame(dt.train)
head(df)
str(df)

#Analyze Relationships between factors (Colin. Matrix)--------------------------------------------------
#ggpairs(data=dt.train, columns=2:13, title="Training Data (first 500 rows)")

#Create Model and Analyze-------------------------------------------------------------------------------
dt.train.model <- lm(dt.train$V1 ~ dt.train$V2 + dt.train$V3 + dt.train$V4 + dt.train$V5 + dt.train$V6 + dt.train$V7 + dt.train$V8 + dt.train$V9 + dt.train$V10 + dt.train$V11 + dt.train$V12 + dt.train$V13 + dt.train$V14 + dt.train$V15 + dt.train$V16 + dt.train$V17 + dt.train$V18 + dt.train$V19 + dt.train$V20 + dt.train$V21 + dt.train$V22 + dt.train$V23 + dt.train$V24 + dt.train$V25 + dt.train$V26 + dt.train$V27 + dt.train$V28 + dt.train$V29 + dt.train$V30 + dt.train$V31 + dt.train$V32 + dt.train$V33 + dt.train$V34 + dt.train$V35 + dt.train$V36 + dt.train$V37 + dt.train$V38 + dt.train$V39 + dt.train$V40 + dt.train$V41 + dt.train$V42 + dt.train$V43 + dt.train$V44 + dt.train$V45 + dt.train$V46 + dt.train$V47 + dt.train$V48 + dt.train$V49 + dt.train$V50 + dt.train$V51 + dt.train$V52 + dt.train$V53 + dt.train$V54 + dt.train$V55 + dt.train$V56 + dt.train$V57 + dt.train$V58 + dt.train$V59 + dt.train$V60 + dt.train$V61 + dt.train$V62 + dt.train$V63 + dt.train$V64 + dt.train$V65 + dt.train$V66 + dt.train$V67 + dt.train$V68 + dt.train$V69 + dt.train$V70 + dt.train$V71 + dt.train$V72 + dt.train$V73 + dt.train$V74 + dt.train$V75 + dt.train$V76 + dt.train$V77 + dt.train$V78 + dt.train$V79 + dt.train$V80 + dt.train$V81 + dt.train$V82 + dt.train$V83 + dt.train$V84 + dt.train$V85 + dt.train$V86 + dt.train$V87 + dt.train$V88 + dt.train$V89 + dt.train$V90 + dt.train$V91)
dt.train.small <- glm(dt.train$V1 ~ dt.train$V2 + dt.train$V3 + dt.train$V4 + dt.train$V5 + dt.train$V6, family=poisson())
#decades = dt.train$V1 - dt.train$V1 %% 10
#head(decades)
#dt.train.model2 <- lm(decades ~ dt.train$V2 + dt.train$V3 + dt.train$V4 + dt.train$V5 + dt.train$V6 + dt.train$V7 + dt.train$V8 + dt.train$V9 + dt.train$V10 + dt.train$V11 + dt.train$V12 + dt.train$V13 + dt.train$V14 + dt.train$V15 + dt.train$V16 + dt.train$V17 + dt.train$V18 + dt.train$V19 + dt.train$V20 + dt.train$V21 + dt.train$V22 + dt.train$V23 + dt.train$V24 + dt.train$V25 + dt.train$V26 + dt.train$V27 + dt.train$V28 + dt.train$V29 + dt.train$V30 + dt.train$V31 + dt.train$V32 + dt.train$V33 + dt.train$V34 + dt.train$V35 + dt.train$V36 + dt.train$V37 + dt.train$V38 + dt.train$V39 + dt.train$V40 + dt.train$V41 + dt.train$V42 + dt.train$V43 + dt.train$V44 + dt.train$V45 + dt.train$V46 + dt.train$V47 + dt.train$V48 + dt.train$V49 + dt.train$V50 + dt.train$V51 + dt.train$V52 + dt.train$V53 + dt.train$V54 + dt.train$V55 + dt.train$V56 + dt.train$V57 + dt.train$V58 + dt.train$V59 + dt.train$V60 + dt.train$V61 + dt.train$V62 + dt.train$V63 + dt.train$V64 + dt.train$V65 + dt.train$V66 + dt.train$V67 + dt.train$V68 + dt.train$V69 + dt.train$V70 + dt.train$V71 + dt.train$V72 + dt.train$V73 + dt.train$V74 + dt.train$V75 + dt.train$V76 + dt.train$V77 + dt.train$V78 + dt.train$V79 + dt.train$V80 + dt.train$V81 + dt.train$V82 + dt.train$V83 + dt.train$V84 + dt.train$V85 + dt.train$V86 + dt.train$V87 + dt.train$V88 + dt.train$V89 + dt.train$V90 + dt.train$V91)

#plot(dt.train$V1, dt.train.model$fitted.values)

data.train.summary2 <- summary(dt.train.model)
data.train.anova2 <- summary(aov(dt.train.model))

op <- par(mfrow = c(2,1),
          oma = c(5,4,0,0) + 0.25,
          mar = c(0,0,1,1) + 0.25)

data.train.summary
data.train.anova
plot(dt.train.model, which = c(2, 1))

par(op)

detach(dt)
