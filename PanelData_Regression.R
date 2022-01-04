pdata <- read.csv(file = 'PanelData.csv',fileEncoding="UTF-8-BOM")
names(pdata)

nrow(pdata)

install.packages(dplyr)
require(dplyr)


pdata %>% count(I)

nrow(pdata)

NROW(na.omit(pdata))

summary(pdata)


corrl<-cor(pdata)
corrl

install.packages("corrplot")
library(corrplot)
corrplot::corrplot(corrl)

library(gplots)
plotmeans(C~I , main = 'Heterogeneity Across Airline', data = pdata)
plotmeans(C~T , main = 'Heterogeneity Across Year', data = pdata)

library(plm)
ols <-plm(C ~ Q+PF+LF, data =pdata, model = "pooling")
summary(ols)

library(ols)
fixed.dum <-lm(C ~ Q+PF+LF+factor(I)-1, data =pdata)
fixed.dum
yhat<- fixed.dum$fitted

airline<-pdata$I
scatterplot(yhat~ pdata$T| airline, xlab= "I", ylab ="yhat", boxplots = FALSE, smooth = FALSE)
abline(lm(pdata$C~pdata$T),lwd=3,col="red")


library(plm)
fixed <- plm(C ~ Q+PF+LF,data =pdata, index = c("I","T"),model ="within")
summary(fixed)

fixef(fixed)

pFtest(fixed,ols)

random <- plm(C ~ Q+PF+LF,data =pdata, index = c("I","T"),model ="random")
summary(random)

phtest(fixed, random)

plmtest(fixed,c("time"),type = "bp")

library(lmtest)
bptest(C ~ Q+PF+LF+factor(I)+factor(T), data=pdata, studentize=F)

coeftest(fixed)

coeftest(fixed, vcovHC)