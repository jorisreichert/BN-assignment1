##In this file we check how well the naive diagram would perform
library(naivebayes)
library(dagitty)
library(lavaan)

## Ik moet toegeven dat ik niet zeker weet hoe R werkt met variable names,
## het lijkt me fijn om na de pre-processing gewoon met d te werken maar
## lines als
## d <- d[c(2:8, 11, 14, 18, 21, 22, 24:30)]
## lijken me onlogisch/slecht leesbaar? -Joris 

d.original=read.table("data/student-por.csv",sep=";",header=TRUE)

##We want to use only columns:
##2, 3, 4, 5, 6, 7, 8, 11, 14, 18, 21, 22, 24, 25, 26, 27, 28, 29, 30
d.filtered <- d.original[c(2:8, 11, 14, 18, 21, 22, 24:30)]
head(d.filtered)


##TODO merge Medu&Fedu, merge Dalc&Walc
d.merged <- d.filtered


##TODO finish the polychoric correlation for the to-be-merged-variables
##We can use unique(d.ordering$varname) to discover the used values
d.ordering <- d.merged

##Specify ordering for categorical variables that represent continues variables
##TODO check if age needs adapting. I think it should go here as it's binned in years
d.ordering$famsize <- ordered( d.ordering$famsize, levels=c("LE3", "GT3") )
##TODO check merge of Medu/Fedu, it will go in this category.
d.ordering$studytime <- ordered( d.ordering$studytime, levels=c("1", "2", "3", "4") )
d.ordering$famrel <- ordered( d.ordering$famrel, levels=c("1", "2", "3", "4", "5") )
d.ordering$freetime <- ordered( d.ordering$freetime, levels=c("1", "2", "3", "4", "5") )
d.ordering$goout <- ordered( d.ordering$goout, levels=c("1", "2", "3", "4", "5") )
##TODO alcohol when merged, it will go in this category.
d.ordering$health <- ordered( d.ordering$health, levels=c("1", "2", "3", "4", "5") )


##Specify (trivial) ordering for binary variables
d.ordering$address <- as.numeric( ordered( d.ordering$address, c("U", "R")))
d.ordering$Pstatus <- as.numeric( ordered( d.ordering$Pstatus, c("A", "T")))
d.ordering$paid <- as.numeric( ordered( d.ordering$paid, c("no", "yes")))
d.ordering$higher <- as.numeric( ordered( d.ordering$higher, c("no", "yes")))
d.ordering$internet <- as.numeric( ordered( d.ordering$internet, c("no", "yes")))


##Specify solutions for non-binary unorderable variables
##TODO find solution for d.ordering$reason, perhaps leave it out.
unique(d.ordering$reason)



##That was all the pre-processing that had to happen.
d<- d.ordering
##Now we’re prepared to process the entire dataset into a correlation matrix
## as if it were fully continuous.
# Extract polychoric correlation matrix
M <- lavCor(d)


## build a DAG for the dataset
##TODO check whether the DAG nodes have names matching to the variables
g <- dagitty('
             dag {
             sex
             age
             Pstatus
             Pedu
             famsize
             address
             internet
             famrel
             paid
             freetime
             studytime
             activities
             goout
             alc
             health
             absences
             higher
             sex <- higher
             age <- higher
             Pstatus <- higher
             Pedu <- higher
             famsize <- higher
             address <- higher
             internet <- higher
             famrel <- higher
             paid <- higher
             freetime <- higher
             studytime <- higher
             activities <- higher
             goout <- higher
             alc <- higher
             health <- higher
             absences <- higher
             }
             ')

plot(g)

# Test model using polychoric correlation matrix
localTests( g, sample.cov=M, sample.nobs=nrow(d) )


##localTests(g,d, type="cis.chisq")