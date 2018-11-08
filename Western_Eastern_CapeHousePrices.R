#According to Impulscentrum.be (2000) ''53% of non-urban females (all races) and 31% of non-urban males in the Eastern Cape fall within the lowest quintile. For the Free State the corresponding figures are respectively 60% and 47%. In the Western Cape only 8% of non-urban females and 9% of urban males fall in the lowest quintile, while for Gauteng the corresponding figures are respectively 21% and 9%.''well this does not apply for females only,it also apply for males as well .Western Cape has the highest earners, irrespective to any aspect compared to Eastern Cape, this report is to check if the financial characteristics of the families (households) living in these provinces based on the main income (HEADINC) of the household as the response and its relationship with age, gender, race, education.





library(readxl)
#reading the data
WC_EC_pop <- read_excel("C:/Users/Desmond Gukwe/Downloads/WC_EC_pop.xlsx")
View(WC_EC_pop)
head(WC_EC_pop)
nrow(WC_EC_pop)
ncol(WC_EC_pop)
#Separating the population according o the provinces 
WC_pop<-WC_EC_pop[which(WC_EC_pop$PROVINCE=="WC"),]
nrow(WC_pop)
EC_pop<-WC_EC_pop[which(WC_EC_pop$PROVINCE=="EC"),]
nrow(EC_pop)
#Sampling 
WCSam<- WC_pop[sample(1:nrow(WC_pop),20,replace=F),]                                   
nrow(WCSam)
ECSam<- EC_pop[sample(1:nrow(EC_pop),30,replace=F),]                                   
nrow(ECSam)
#Compining the sample
WC_EC_Sam <- rbind(WCSam,ECSam)
nrow(WC_EC_Sam)
#A population of 504 individuals from Western Cape (240) and Eastern Cape (264) was selected, 20 and 30 individual where randomly selected from Western Cape and Eastern Cape respectively. To construct a sample of 50 people
#Saving it as a csv
write.csv(WC_EC_Sam, file="C:/Users/Desmond Gukwe/Downloads/WC_EC_Sam.csv")  

#Making plots QQ plots & box&whiskers
par(mfrow=c(1,2))
boxplot(WCSam$HEADINC,col='blue',main='Box plot for Western Cape')
boxplot(ECSam$HEADINC,col='brown',main='Box plot for Estern Cape')
#Interpretation 
#Looking at figure 8 at Western Cape, the data is skewed to the left , since the median is not on the middle ,there are no extreme outlies on the Western Cape ,sample ,still on the same figure ,focusing on Eastern cape the data is almost symmetric which mean it might be normally distributed ,but there is extreme outlies .At figure 9 both of the Qq plots show that these two sample are normally distributed since the data follow the fitted line. The coefficient of variation is 10% and 16% respectively.


#Scatterplots
par(mfrow=c(1,2))
plot(WCSam$HHINCOME~WCSam$AGE ,col='red' ,main='Scatter plot for Western Cape')
plot(ECSam$HHINCOME~ECSam$AGE ,col='red' ,main='Scatter plot for Eastern Cape')
#The plot for Western cape shows that there no much of a relationship between HHincome and Age , so HHINCOME is not depended to the age in any manner and there is extreme outliers .
#The scatter plot for Eastern Cape kind of linear relationship between HHICOME and Age ,but the relationship is weak ,and there is extreme outliers 


#Checking Normality 
par(mfrow=c(1,2))
qqnorm(WC_pop$HEADINC,col='blue',main='QQ plot for Western Cape ')
qqline(WC_pop$HEADINC,lwd=3,col='red')
qqnorm(EC_pop$HEADINC,col='blue',main='QQ plot for Eastern Cape ')
qqline(EC_pop$HEADINC,lwd=3,col='red')
#Looking at fitted data of the populations, on both plots look like its normally distributed since it follows the fitted lines, but both are not symmetric but preferably the Western Cape Population is a little more symmetric than the  Eastern Cape population  .Feather test may be required to verify this assumption .

#Histograms
par(mfrow=c(1,2))
hist(EC_pop$HEADINC,col='brown',main = 'Histogram of Eastern Cape')
hist(WC_pop$HEADINC,col='brown',main = 'Histogram of Western Cape')

#These two histograms clearly show that there is a difference in the variances of the populations, also as the skewness of the distributions is deferent. To support this with a variance test the p value is = 1.648e-05 which is less that alpha, the variances are not the same.

#test
var.test(WC_pop$HHINCOME,EC_pop$HHINCOME)

t.test(WCSam$PERCY)

#building models
WC_EC_Sam$GENDER2 <- as.numeric(factor(WC_EC_Sam$GENDER , levels=c("MALE" ,"Female")))
par(mfrow=c(1,1))
plot(WC_EC_Sam$HEADINC~WC_EC_Sam$GENDER2,col='green',main='Headin vs gender')
mean.crim=mean(WC_EC_Sam$HEADINC)
abline(h=mean.crim)
Model_1=lm(WC_EC_Sam$HEADINC~WC_EC_Sam$GENDER2)
abline(Model_1,col='red')
summary(Model_1)

#Diagonostics
par(mfrow=c(2,2))
plot(Model_1,main= 'Diagonostics plot for headnic vs gender')

#Correlaton Tests
cor.test(WC_EC_Sam$HEADINC,WC_EC_Sam$GENDER2)
cor.test( WC_EC_Sam$HEADINC,WC_EC_Sam$RACE)
cor.test( WC_EC_Sam$HEADINC,WC_EC_Sam$AGE)
cor.test( WC_EC_Sam$HEADINC,WC_EC_Sam$EDUC_CAT)
cor.test( WC_EC_Sam$HEADINC,WC_EC_Sam$GENDER2)

#Model selection using backward selection using Adj  R Square
Full_Model=lm(WC_EC_Sam$HEADINC~WC_EC_Sam$GENDER2+WC_EC_Sam$EDUC_CAT+WC_EC_Sam$AGE+WC_EC_Sam$RACE)
summary(Full_Model)
mini_Model2=lm(WC_EC_Sam$HEADINC~WC_EC_Sam$GENDER2+WC_EC_Sam$EDUC_CAT+WC_EC_Sam$RACE)
summary(mini_Model2)
Model2=lm(WC_EC_Sam$HEADINC~WC_EC_Sam$GENDER2+WC_EC_Sam$EDUC_CAT+WC_EC_Sam$AGE)
summary(Model2)
mini_Model2=lm(WC_EC_Sam$HEADINC~WC_EC_Sam$GENDER2+WC_EC_Sam$EDUC_CAT)
summary(mini_Model2)
mini_Model2=lm(WC_EC_Sam$HEADINC~WC_EC_Sam$GENDER2+WC_EC_Sam$RACE)
summary(mini_Model2)
mini_Model33=lm(WC_EC_Sam$HEADINC~WC_EC_Sam$GENDER2+WC_EC_Sam$RACE)
summary(mini_Model3)
mini_Model44=lm(WC_EC_Sam$HEADINC~WC_EC_Sam$AGE+WC_EC_Sam$RACE)
summary(mini_Model44)
mini_Model7=lm(WC_EC_Sam$HEADINC~WC_EC_Sam$AGE+WC_EC_Sam$EDUC_CAT)
summary(mini_Model7)
#Final model
mini_Model2=lm(WC_EC_Sam$HEADINC~WC_EC_Sam$GENDER2+WC_EC_Sam$EDUC_CAT+WC_EC_Sam$RACE) #with adjuted r square of 0.4365

par(mfrow=c(2,2))
plot(mini_Model2)




