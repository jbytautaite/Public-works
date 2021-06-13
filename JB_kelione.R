#### VERSLO PROBLEMOS APIBREZIMAS ####
# 1. Imone, teikianti pavezejimo paslaugas. Pritaikyti 3 klasifikavimo modelius, kurie galetu prognozuoti, ar kelione bus vertinama "gerai" ar "tobulintinai" (Y). 


#### R APLINKOS ISVALYMAS

rm(list=ls()) 


#### DUOMENYS ####

# github repozitorija
#URL: https://raw.githubusercontent.com/jbytautaite/demo_BD_9/main/Keliones_registras.csv

# Duomenu ikelimas
setwd ('C:/Users/Povilas/Desktop/R_Kelione') 
d <-read.csv('Keliones_registras.csv', sep = ";", dec = ".") 


#### REIKALINGU R BIBLIOTEKU IKELIMAS ####
install.packages('lubridate') 
install.packages('ggplot2')  
install.packages('caTools') 
install.packages('class')  
install.packages('e1071') 
install.packages('caret') 


#### DUOMENU APRASYMAS ####


## DUOMENU STRUKTURA ##
str(d)
summary(d)


## DUOMENU PARUOSIMAS ##

# Datos vertimas i "date" formatq

library('lubridate')
d$isvykimo_data
savaite <- week(as_date(d$isvykimo_data))
class(savaite)


# Naujo formato duomenu iterpimas i data.frame
d1 <- d                        
d1 <- cbind(d, keliones_savaite = savaite) 
d1$isvykimo_data <- NULL

str(d1)
summary(d1)


# Kintamojo "laikas" valymas

d1$isvykimo_laikas <- NULL


# Keliones reitingo - y - kurimas

table(d1$keliones_reitingas)

0 -> d1$y
1 -> d1$y[d1$keliones_reitingas %in% c(4, 5)]

table(d1$y)
table(d1$y, d1$keliones_reitingas)
class(d1$y)


d1$keliones_reitingas <- NULL
summary(d1)



#### DUOMENU ANALIZE ####

# Kintamojo x - suma - analize


h1 <- hist(x = d1$suma, main='Suma', xlab='Suma, EUR', xlim=c(0,40), ylim=c(0,210), col='lightblue')
text(h1$mids,h1$counts,labels=h1$counts, adj=c(0.5, -0.5))

summary(d1$suma)
sd(d1$suma)


# Kintamojo x - keliones_trukme - analize

h2 <- hist(x = d1$keliones_trukme_min..decimals., main='Keliones_trukme_min', xlab='Keliones_trukme, min', xlim=c(0,80), ylim=c(0,80), col='lightgreen')
text(h2$mids,h2$counts,labels=h2$counts, adj=c(0.5, -0.5))

summary(d1$keliones_trukme_min..decimals.)
sd(d1$keliones_trukme_min..decimals.)

# Kintamuju x - suma, keliones_trukme - analize

plot(x = d1$keliones_trukme_min..decimals., y = d1$suma)


# Kintamuju x - suma, keliones_savaite - analize

ggplot2::qplot(x = keliones_savaite, y = suma, data = d1, main = 'Keliones kaina, taikoma skirtinga isvykimo savaite')
cor(d1[ ,2:6])



#### MODELIU TAIKYMAS ####


## DUOMENU SKAIDYMAS I TRAIN SET IR TEST SET ##

library(caTools)
set.seed(777)
split = sample.split(d1$y, SplitRatio = 0.8)
train_d1 = subset(d1, split == TRUE)
test_d1 = subset(d1, split == FALSE)

# Train ir Test imciu aritmetiniai vidurkiai
mean(train_d1$y)
mean(test_d1$y)


## LOGISTINE REGRESIJA - LOGIT ##

# Modelio paruosimas su mokymo imtimi
d1_formula <- y ~ .

m1_logit <- glm(formula = d1_formula, family = binomial(link='logit'), data = train_d1)
print(m1_logit) 


# Modelio prognozes
pred_test <- predict(m1_logit, newdata = test_d1, type = 'response') 


pred_test_df <- data.frame('y_raw' = pred_test, 'y_pred' = 0)
1 -> pred_test_df$y_pred [pred_test_df$y_raw > 0.5]

table(pred_test_df$y_pred) #gautos prognoziu klases


# Modelio vertinimas - tikslumas

class(pred_test_df$y_pred)
class(test_d1$y)
library('caret')
cM <- caret::confusionMatrix(as.factor(test_d1$y), as.factor(pred_test_df$y_pred), mode = "prec_recall", positive = "1")
print(cM)



## K-ARTIMIAUSIO KAIMYNO METODAS - KNN ##  

# k eksperimentai

k_list = c(1:20)
d1_results = data.frame ()


# Modelio paruosimas su mokymo imtimi

for(d1_k in k_list) {
  print (d1_k)
  
  m2_knn <-
    class::knn(
      train = train_d1
    , test = test_d1
    , cl = train_d1$y
    , k = d1_k
    , l = 0
    , prob = FALSE
    , use.all = TRUE)
  
  
# Modelio prognozes: Lyginama modelis su test imtimi  

tested_prediction <- m2_knn == test_d1$y
positive_prediction <- length(tested_prediction[tested_prediction == TRUE])
negative_prediction <- length(tested_prediction[tested_prediction == FALSE])

results <- positive_prediction / length(tested_prediction)

new_row <- data.frame (k = d1_k, accuracy = results)

d1_results <- rbind(d1_results, new_row)

}


# Rezultatai 

plot(x = d1_results$k, y = d1_results$accuracy, type = 'l')
which(d1_results$accuracy [2:20] == max(d1_results$accuracy))
d1_results [2:20, ] [4,]  # esant k = 5, modelio tikslumas 63,86%




## NAIVE BAYES METODAS ##

# Modelio paruosimas su mokymo imtimi

library('e1071')
library('caret')
set.seed(777)
indexes = createDataPartition(d1$y, p = .9, list = F)
train_d1 = d1[indexes, ]
test_d1 = d1[-indexes, ]

m3_nb <- e1071::naiveBayes(y ~ ., data = train_d1)
print(m3_nb)


# Modelio prognozes

pred_test <- predict(m3_nb, newdata = test_d1, type = 'class')


# Modelio vertinimas - tikslumas

class(test_d1$y)     
class(pred_test)  

cM <- confusionMatrix(as.factor(test$quality), pred_test)

cM <- confusionMatrix(as.factor(test_d1$y), pred_test, mode = "prec_recall", positive = "1")
print(cM)
