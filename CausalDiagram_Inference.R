library(naivebayes)
library(dagitty)
library(bnlearn)
#library(lavaan)

set.seed(5)
d.original=read.table("data/student-por.csv",sep=";",header=TRUE)

##We want to use only columns:
##2, 3, 4, 5, 6, 7, 8, 11, 14, 18, 19, 21, 22, 24, 25, 26, 27, 28, 29, 30
d.filtered <- d.original[c(2:8, 11, 14, 18, 19, 21, 22, 24:30)]
head(d.filtered)

# Merge Medu/Fedu into Pedu and Walc/Dalc into alc
d.filtered$alc <- rowMeans(cbind(d.filtered$Walc, d.filtered$Dalc), na.rm=TRUE)
d.filtered$Pedu <- rowMeans(cbind(d.filtered$Medu, d.filtered$Fedu), na.rm=TRUE)
d.merged <- d.filtered[, -which(names(d.filtered) %in% c("Walc", "Dalc", "Medu", "Fedu", "reason"))]

##We can use unique(d.ordering$varname) to discover the used values
d.ordering <- d.merged

##Specify ordering for categorical variables that represent continues variables
d.ordering$famsize <- as.numeric(ordered( d.ordering$famsize, levels=c("LE3", "GT3") ))
d.ordering$studytime <- as.numeric(ordered( d.ordering$studytime, levels=c("1", "2", "3", "4") ))
d.ordering$famrel <- as.numeric(ordered( d.ordering$famrel, levels=c("1", "2", "3", "4", "5") ))
d.ordering$freetime <- as.numeric(ordered( d.ordering$freetime, levels=c("1", "2", "3", "4", "5") ))
d.ordering$goout <- as.numeric(ordered( d.ordering$goout, levels=c("1", "2", "3", "4", "5") ))
d.ordering$health <- as.numeric(ordered( d.ordering$health, levels=c("1", "2", "3", "4", "5") ))
d.ordering$alc <- as.numeric(ordered( d.ordering$alc, levels=c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0)))
d.ordering$Pedu <- as.numeric(ordered( d.ordering$Pedu, levels=c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0)))

##What to do with age?
d.ordering$age <- as.numeric((d.ordering$age))
d.ordering$absences <- as.numeric((d.ordering$absences))

##Specify (trivial) ordering for binary variables
d.ordering$address <- as.numeric( ordered( d.ordering$address, c("U", "R")))
d.ordering$Pstatus <- as.numeric( ordered( d.ordering$Pstatus, c("A", "T")))
d.ordering$paid <- as.numeric( ordered( d.ordering$paid, c("no", "yes")))
d.ordering$higher <- as.numeric( ordered( d.ordering$higher, c("no", "yes")))
d.ordering$internet <- as.numeric( ordered( d.ordering$internet, c("no", "yes")))
d.ordering$sex <- as.numeric( ordered ( d.ordering$sex, c("F", "M")))
d.ordering$activities <- as.numeric( ordered (d.ordering$activities, c("yes", "no")))

##That was all the pre-processing that had to happen.
d<- d.ordering

##Train/test split currently 70:30
dt = sort(sample(nrow(d), nrow(d)*.7))
train <- d[dt,]
test <- d[-dt,]


##Now we’re prepared to process the entire dataset into a correlation matrix
## as if it were fully continuous.
# Extract polychoric correlation matrix
M <- lavCor(train)

g <- dagitty('dag {
bb="-4.293,-6.345,4.769,5.066"
Pedu [pos="3.221,-5.140"]
Pstatus [pos="1.985,-5.132"]
absences [pos="0.290,0.863"]
activities [pos="2.685,-2.492"]
address [pos="3.818,-4.017"]
age [pos="0.843,-5.125"]
alc [pos="-1.535,-1.599"]
famrel [pos="1.466,-3.548"]
famsize [pos="2.823,-3.994"]
freetime [pos="0.947,-0.476"]
goout [pos="0.091,-2.417"]
health [pos="-0.774,1.414"]
higher [pos="1.466,1.562"]
internet [pos="3.610,-3.317"]
paid [pos="3.368,-2.507"]
sex [pos="-0.021,-5.118"]
studytime [pos="-0.246,-0.491"]
Pedu -> activities
Pedu -> address
Pedu -> famsize
Pedu -> higher
Pedu -> internet
Pedu -> paid
Pedu -> studytime
Pstatus -> absences
Pstatus -> alc
Pstatus -> famrel
Pstatus -> famsize
absences -> higher
activities -> higher
activities -> studytime
address -> activities
address -> internet
age -> absences
age -> alc
age -> goout
age -> higher
alc -> absences
alc -> health
alc -> higher
alc -> studytime
famrel -> activities
famrel -> alc
famrel -> goout
famrel -> health
famsize -> famrel
famsize -> freetime
famsize -> studytime
freetime -> goout
freetime -> health
goout -> absences
goout -> alc
goout -> higher
internet -> activities
internet -> goout
internet -> higher
paid -> higher
paid -> studytime
sex -> activities
sex -> alc
sex -> goout
sex -> health
sex -> higher
sex -> studytime
studytime -> absences
freetime -> studytime
studytime -> health
studytime -> higher
}
')
##removed studytime -> freetime

localTests( g, sample.cov=M, sample.nobs=nrow(train) )

plot(g)

net1 <- model2network(toString(g, "bnlearn"))
fit1 <- bn.fit( net1, train)

#fit <- sem(toString(g,"lavaan"), sample.cov=M, sample.nobs=nrow(train))

head(train)

fit <- bn.fit(net1, train)
summary(fit)
fit

predict(fit,node="higher", data=test,  method="bayes-lw")

predicted.higher <- predict(fit,node="higher",data=test,method="bayes-lw")

plot(test[,"higher"],predicted.higher)


#lvsem <- toString(g, "lavaan")
#lvsem.fit <- sem(lvsem, train)
#summary(lvsem.fit)



#fg <- lavaanToGraph(fit, digits=2)
#coordinates(fg) <- coordinates(g)

#plot(fg, show.coefficients=TRUE)
#class(fit)

class_1 <- predicted.higher[test[,"higher"]==1]
class_2 <- predicted.higher[test[,"higher"]==2]

shapiro.test(class_1) ## p-value large enough to assume normal distr
shapiro.test(class_2) ## p-value large enough to assume normal distr

# Create a sequence of numbers between -10 and 10 incrementing by 0.1.
x <- seq(0, 3, by = .01)

# Choose the mean as 2.5 and standard deviation as 0.5.
y1 <- dnorm(x, mean = mean(class_1), sd = sd(class_1))
y2 <- dnorm(x, mean = mean(class_2), sd = sd(class_2))

plot(x,y2,type='l',col="red")
lines(x,y1,col="green")
points(1.7649, 2.1385, pch=16, cex=2, col="blue") ## used wolfram-alpha to find the intersection

predicted.higher[predicted.higher < 1.7649] <- 1
predicted.higher[predicted.higher > 1.7649] <- 2

print(predicted.higher)
print(test[, "higher"])

predicted.TP = length(predicted.higher[test[,"higher"]==2 & predicted.higher==2])
predicted.TN = length(predicted.higher[test[,"higher"]==1 & predicted.higher==1])
predicted.FP = length(predicted.higher[test[,"higher"]==1 & predicted.higher==2])                      
predicted.FN = length(predicted.higher[test[,"higher"]==2 & predicted.higher==1])

print(predicted.TP)
print(predicted.TN)
print(predicted.FP)
print(predicted.FN)

mcc <- (predicted.TP*predicted.TN - predicted.FP*predicted.FN) / sqrt( (predicted.TP + predicted.FP)*(predicted.TP + predicted.FN)*(predicted.TN + predicted.FP)*(predicted.TN + predicted.FN) )
print(mcc)
