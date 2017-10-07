#'---
#'author: File
#'---

setwd("~/Desktop/zstuff")

ema.data<-read.csv("EMATM351.csv")
attach(ema.data)


numberLeave<-length(which(LeaveRemain > 1))
numberRemain<-length(which(LeaveRemain < 1))

#'### Graphs
#'
par(mfrow=c(1,2))
plot(LeaveRemain~LowProf,
     col= c("darkred","blue")[Vote],
     pch= 1,
     xlab="Routine Profession",
     ylab = "Vote")
abline(lm(LeaveRemain~LowProf), col= "red")
legend("topleft", 
       legend = c("Leave","Remain"), 
       bty = "n", 
       pch=21, 
       pt.bg = c("darkred","blue"))
plot(LeaveRemain~HighProfession,
     ylab = "Vote",
     col= c("darkred","blue")[Vote],
     pch= 1,
     xlab="High Profession")
abline(lm(LeaveRemain~HighProfession), col= "red")
legend("topright", 
       legend = c("Leave","Remain"), 
       bty = "n", 
       pch=21, 
       pt.bg = c("darkred","blue"))
par(mfrow=c(1,1))

plot(LowProf~HighProfession,
     col= c("darkred","blue")[Vote],
     xlab= "Percentage of High Professions",
     ylab = "Percentage of Low Professions")
abline(lm(LowProf~HighProfession), col= "red")
legend("topright", 
       legend = c("Leave","Remain"), 
       bty = "n", 
       pch=21, 
       pt.bg = c("darkred","blue"))


par(mfrow= c(1,2))
Unemployedgraph<-plot(LeaveRemain~Unemployed,
                      xlab= "Percent Unemployed",
                      ylab= "Voting",
                      col= c("darkred","blue")[Vote])
abline(lm(LeaveRemain~Unemployed), col = "red") #regression line
legend("topright", 
       legend = c("Leave","Remain"), 
       bty = "n", 
       pch=21, 
       pt.bg = c("darkred","blue"))

studentgraph<-plot(LeaveRemain~student,
                   xlab=" Percentage of Students By District",
                   ylab= "Vote",
                   col= c("darkred","blue")[Vote])
abline(lm(LeaveRemain~student), col= "red")
legend("topright", 
       legend = c("Leave","Remain"), 
       bty = "n", 
       pch=21, 
       pt.bg = c("darkred","blue"))
par(mfrow= c(1,1))

whitegraph<- plot(LeaveRemain~White,
                  main= "Votes by percentage of white population",
                  xlab=" Percentage of White Ethnicity in Population",
                  ylab= " Vote",
                  col= c("darkred","blue")[Vote]
)
abline(lm(LeaveRemain~White), col="red")
legend("top", 
       legend = c("Leave","Remain"), 
       bty = "n", 
       pch=21, 
       pt.bg = c("darkred","blue"))

hist(LeaveRemain)



#'### Models
model1<- glm(LeaveRemain~ LowProf + HighProfession + White + student + Unemployed + LowProf*HighProfession + LowProf*White + Unemployed*White + HighProfession*White)
drop1(model1, test= "Chisq")
model2<- glm(LeaveRemain~ LowProf + HighProfession + White + student + Unemployed + LowProf*HighProfession)
drop1(model2, test= "Chisq")
summary(model1)
summary(model2)

#'## pearson correlation
cor.test(HighProfession, LowProf, method = "pearson")



## residuals to show normal distribution
resid.model<-residuals(model2)
hist.final<-hist(resid.model)
m.final <- hist.final$counts / hist.final$density
c.final <- seq(min(resid.model), 
               max(resid.model), 
               length.out= 100)

norm.final<- dnorm(x = c.final, 
                   mean = mean(resid.model), 
                   sd = sd(resid.model))
lines(c.final, 
      norm.final * m.final[1], 
      col = "blue", lwd = 2)



#'### K-NN 

str(ema.data)

round(prop.table(table(Vote)) * 100, digits = 1)

sub.data<-subset(ema.data, select=c("Vote", "White", "student", "Unemployed", "LowProf", "HighProfession"))
##normalize function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
sub_n<- as.data.frame(lapply(sub.data[2:6], normalize)) #normalize data besides first column
summary(sub_n$White)
sub_train <- sub_n[1:90,]
sub_test <- sub_n[91:346,]

sub_train_labels <- sub.data[1:90, 1]
sub_test_labels <- sub.data[91:346, 1]
library(class)
sub_test_pred <- knn(train = sub_train, test = sub_test,cl = sub_train_labels, k=3)

library(gmodels)
CrossTable(x = sub_test_labels, y= sub_test_pred, prop.chisq = FALSE)
183+32
print(215/246*100)
