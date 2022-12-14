#determine the correlation between the hormone chemicals
cor.test(DATA$Estradiol, DATA$Progesterone, method="pearson", use = "complete.obs")
#according to the output there is not a correlation between estradiol and progesterone at all
cor.test(DATA$Estradiol, DATA$Testosterone, method="pearson", use = "complete.obs")
#according to the output there is a significant negative correlation between estradiol and testosterone (the p value is < .05)
#however the correlation is very low at about 33%
cor.test(DATA$Testosterone, DATA$Progesterone, method="pearson", use = "complete.obs")
#according to the output there is not a correlation between testosterone and progesterone at all
#create a correlation visual
#subset the data first to only include the hormone chemicals
hormones <- DATA[, c(3,4,5)]
library("PerformanceAnalytics")
chart.Correlation(hormones, histogram=FALSE, method="pearson")
#create a slightly more visually pleasing correlation matrix
corr_matrix <- cor(hormones)
install.packages("corrplot")
library("corrplot")
corrplot(corr_matrix, type="upper", order="hclust", p.mat = corr_matrix, sig.level = 0.01, insig="blank")

#I now want to compare the means of all three hormone chemicals of both the control group (those who didn't have acne) and the 
#group with acne
#subset dataset into two datasets, one for control group and one for other group
library("dplyr")
library("tidyverse")
controlGroup <- filter(DATA, Group == 0)
acneGroup <- filter(DATA, Group == 1)
View(controlGroup)
View(acneGroup)
mean(controlGroup$Progesterone)
mean(controlGroup$Estradiol)
mean(controlGroup$Testosterone)
mean(acneGroup$Progesterone)
mean(acneGroup$Estradiol)
mean(acneGroup$Testosterone)
#means of control group: Progesterone = 0.4982971, Estradiol=370.9429, Testosterone=38.37229
#means of acne group: Progesterone=0.6017143, Estradiol=323.1472, Testosterone=55.66509
#the group of study participants with acne had (on average) a highler level of progesterone, lower level of estradiol,
#and higher level of testosterone

#make a bar graph comparing control group and acne group
#progesterone levels were converted to a smaller unit better visualization on the graph
values <- matrix(c(49.82971, 370.9429, 38.37229, 60.17143, 323.1472, 55.66509), 
                 nrow = 2, ncol = 3, byrow = TRUE)
colors = c("green", "red")
hormones2 <- c("Progesterone", "Estradiol", "Testosterone")
regions <- c("Group without acne", "Group with acne")
barplot(values, main = "Hormone Levels of Control Group VS Acne Presenting Group", names.arg = hormones2, 
        xlab = "Hormone", ylab = "Mean Level", 
        col = colors, beside = TRUE)
legend("right", regions, cex = 0.6, fill = colors)

DATA[DATA$Testosterone < 15 & DATA$Testosterone>70, ]

#I'm going to create a data set with only participants who have normal levels of all three hormone chemicals and then
#compare how many people in that dataset have acne to the original data
#according to healthline the standard range of testosterone is 15-70 ng/dL for women
#according to rochester medical school a normal estradiol level is 30 to 400 pg/mL, this study measured estradiol in pmol/L
#in this unit (pmol/L) a normal level is 110.13-1468.9
#according to healthline progesterone is normal unless above 24ng/mL for nonpregnant women
data1 <- subset(DATA, Testosterone >15 & Testosterone <70 & Estradiol >110.13 & Estradiol <1468.9 & Progesterone >0.89 & Progesterone <24)
View(data1)
#get value count of acne present column
table(data1$AcnePresent)
table(DATA$AcnePresent)
#only including participants with normal levels of all three hormone chemicals drastically decressed the number of people
#who have acne, in the original dataset 175 had acne, in the new dataset on 22 people had acne that also had normal levels
#of all three chemical hormones

#now I would like to see which hormone chemical decreases the amount of people with acne the most, starting with testosterone
testosterone <- subset(DATA, Testosterone >15 & Testosterone <70)
table(testosterone$AcnePresent)
table(DATA$AcnePresent)
#only including participants with normal levels of testerone decreased the amount of people with acne from 175 to 137

#estradiol
estradiol <- subset(DATA, Estradiol >110.13 & Estradiol <1468.9)
table(estradiol$AcnePresent)
#only including participants with normal levels of estradiol did not affect the original data at all

#Progesterone
Progesterone <- subset(DATA, Progesterone <24)
table(Progesterone$AcnePresent)
#only including participants with normal levels of estradiol did not affect the original data at all