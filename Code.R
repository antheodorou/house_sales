#Q1
require(foreign)
house <- read.table("usdata", header = T)
str(house)
#Q2
house$PRICE <- as.numeric(house$PRICE)
house$SQFT <- as.numeric(house$SQFT)
house$AGE <- as.numeric(house$AGE)
house$FEATS <- as.numeric(house$FEATS)
house$NE <- as.factor(house$NE)
house$COR <- as.factor(house$COR)
#Q3
summary(house)
library(psych)
#Numeric
numeric.only <- sapply(house, class) == "numeric"
housenum <- house[, numeric.only]
round(t(describe(housenum)), 2)
par(mfrow=c(2,3))
hist(housenum[,1], main=names(housenum)[1], xlab = names(housenum[1]))
hist(housenum[,2], main=names(housenum)[2], xlab = "SQUARE FEET")
hist(housenum[,3], main=names(housenum)[3], xlab = names(housenum[3]))
plot(table(housenum[,3])/nrow(housenum), type='h', xlim=range(housenum[,3])+c(-1,1), main=names(housenum)[3], ylab='Relative frequency')
hist(housenum[,4], main=names(housenum)[4], xlab = "FEATURES")
plot(table(housenum[,4])/nrow(housenum), type='h', xlim=range(housenum[,4])+c(-1,1), main=names(housenum)[4], ylab='Relative frequency')
#Factors
housefac <- house[ , !numeric.only]
par(mfrow=c(1,1))
par(mai=c(2.0,1.5,0.5,0.5))
barplot(sapply(housefac,table)/nrow(housefac), horiz=T, las=1, col=2:3, ylim=c(0,8), cex.names=1.5)
legend('top', fil=2:3, legend=c("No","Yes"), ncol=2, bty='n',cex=1.5)
#Q4
#Pairs of numerical variables
pairs(housenum)
require(corrplot)
corrplot(cor(housenum), method = "number")
#More focused pairwise correlation
par(mfrow=c(1,3))
plot(housenum[,2], housenum[,1], xlab=names(housenum)[2], ylab='Price',cex.lab=1.5)
abline(lm(housenum[,1]~housenum[,2]))
for(j in 3:4){
  boxplot(housenum[,1]~housenum[,j], xlab=names(housenum)[j], ylab='Price',cex.lab=1.5)
  abline(lm(housenum[,1]~housenum[,j]),col=2)
}
#Pairs of price and factor variables 
par(mfrow=c(1,2))
for(j in 1:2){
  boxplot(housenum[,1]~housefac[,j], xlab=names(housefac)[j], ylab='Price',cex.lab=2.0)
}
#Q5
model <- lm(house$PRICE ~., data = house)
summary(model)
#Q6
modelfin <- step(model, direction = "both")
#Q7
summary(modelfin)
#Q8
#Normality of the residuals
res <- modelfin$residuals
par(mfrow=c(1,2), mai = c(0.5,0.5,0.5,0.5) )
mt <- 'Residuals'
hist(res, probability=T, main=mt)
x0<-seq(min(res), max(res),length.out=100)
y0<-dnorm(x0, mean(res),sd(res))
lines(x0,y0, col=2, lty=2)
qqnorm(res, main=mt)
qqline(res)

par(mfrow=c(1,2))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2)
abline(h=4, col=2, lty=2)

library(nortest)
lillie.test(res)
shapiro.test(res)
library(car)
ncvTest(modelfin)
#Non linearity
library(car)
par(mfrow=c(1,1))
residualPlot(modelfin, type='rstudent')
residualPlots(modelfin, plot=F, type = "rstudent")
#Independence
library(randtests)
runs.test(modelfin$res)
library(car)
durbinWatsonTest(modelfin)
#Q9 - LASSO
require(glmnet)
X <- model.matrix(model)[,-1]
lasso <- glmnet(X, house$PRICE)
plot(lasso, xvar = "lambda", label = T)
lasso1 <- cv.glmnet(X, house$PRICE, alpha = 1)
plot(lasso1)
coef(lasso1, s = "lambda.1se")
plot(lasso1$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso1$lambda.min, lasso1$lambda.1se)), lty =2)