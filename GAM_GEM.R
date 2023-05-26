library(readxl)
library(xts)
library(caret)


GAM<-read_excel("E:/Arctic/Data.xlsx")
R
#build model
require(mgcv)

gamobj<-gam(GEM~s(Openwater)+s(CO)+s(WS)+s(T,k=7)+s(Traj48h)+s(P,k=6),family=gaussian(link=identity),data=GAM)
summary(gamobj)


pdf("GAM_Hg.pdf")
plot(gamobj,se=T,resid=T,pch=1,shade=TRUE,cex=0.12,cex.axis=1.5,font=2)
dev.off()

gam.check(gamobj)
AIC(gamobj)
Fit <- fitted.values(gamobj)
write.csv(Fit, file = "C:/Users/Lenovo/Desktop/GAM.csv")
write.csv(GAM$GEM, file = "C:/Users/Lenovo/Desktop/Obs.csv")


Pre<-as.numeric()
Obs<-as.numeric()

set.seed(1)
#5-cv
folds <-createMultiFolds(y=GAM$GEM,k=5)
for(i in 1:5){
  train<- GAM[ folds[[i]],] 
  test <- GAM[-folds[[i]],] 
  gamobj<-gam(GEM~s(Openwater)+s(CO)+s(WS)+s(T,k=7)+s(Traj48h)+s(P,k=6),family=gaussian(link=identity),data=train)
  testpre<-predict(gamobj,type='response', newdata=test)
  Pre<- append(Pre,testpre)
  Obs<- append(Obs,test$GEM)
}

write.csv(Pre, file = "C:/Users/Lenovo/Desktop/testpre.csv")
write.csv(Obs, file = "C:/Users/Lenovo/Desktop/test.csv")