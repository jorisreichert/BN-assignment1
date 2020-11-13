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
Pedu -> activities [beta="-0.038"]
Pedu -> address [beta="-0.21"]
Pedu -> famsize [beta="0.066"]
Pedu -> higher [beta="0.18"]
Pedu -> internet [beta="0.2"]
Pedu -> paid [beta="0.13"]
Pedu -> studytime [beta="0.079"]
Pstatus -> absences [beta="-0.1"]
Pstatus -> alc [beta="0.023"]
Pstatus -> famrel [beta="0.044"]
Pstatus -> famsize [beta="0.29"]
absences -> higher [beta="-0.034"]
activities -> higher [beta="-0.02"]
activities -> studytime [beta="-0.12"]
address -> activities [beta="-0.015"]
address -> internet [beta="-0.13"]
age -> absences [beta="0.18"]
age -> alc [beta="0.083"]
age -> goout [beta="0.12"]
age -> higher [beta="-0.26"]
alc -> absences [beta="0.14"]
alc -> health [beta="0.085"]
alc -> higher [beta="0.0076"]
alc -> studytime [beta="-0.18"]
famrel -> activities [beta="-0.052"]
famrel -> alc [beta="-0.19"]
famrel -> goout [beta="0.039"]
famrel -> health [beta="0.14"]
famsize -> famrel [beta="0.061"]
famsize -> freetime [beta="0.043"]
famsize -> studytime [beta="-0.077"]
freetime -> goout [beta="0.33"]
freetime -> health [beta="0.025"]
freetime -> studytime [beta="-0.099"]
goout -> absences [beta="-0.021"]
goout -> alc [beta="0.36"]
goout -> higher [beta="-0.044"]
internet -> activities [beta="-0.043"]
internet -> goout [beta="0.065"]
internet -> higher [beta="0.072"]
paid -> higher [beta="0.0027"]
paid -> studytime [beta="-0.00075"]
sex -> activities [beta="-0.15"]
sex -> alc [beta="0.35"]
sex -> goout [beta="0.018"]
sex -> health [beta="0.12"]
sex -> higher [beta="-0.043"]
sex -> studytime [beta="-0.18"]
studytime -> absences [beta="-0.17"]
studytime -> health [beta="0.017"]
studytime -> higher [beta="0.2"]
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
