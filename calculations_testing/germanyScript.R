data_germany <- read.csv("germanyData.csv")

colnames(data_germany)
data_germany_useful <- data_germany[1:1, c(7, 8, 13, 14, 21, 22)]
data_germany_useful_matrix <- data_germany[1:576, c(7, 8, 13, 14, 21, 22)]

forVariablesToLoop <- colnames(data_germany_useful)

min(data_germany$new_cases)
min(data_germany$new_cases_smoothed)
min(data_germany$new_cases_per_million)
min(data_germany$new_cases_smoothed_per_million)
#min(data_germany$hosp_patients, na.rm = TRUE)
#min(data_germany$hosp_patients_per_million, na.rm = TRUE)

max(data_germany$new_cases)
max(data_germany$new_cases_smoothed)
max(data_germany$new_cases_per_million)
max(data_germany$new_cases_smoothed_per_million)
#max(data_germany$hosp_patients, na.rm = TRUE)
#max(data_germany$hosp_patients_per_million, na.rm = TRUE)

mean(data_germany$new_cases)
mean(data_germany$new_cases_smoothed)
mean(data_germany$new_cases_per_million)
mean(data_germany$new_cases_smoothed_per_million)
#mean(data_germany$hosp_patients, na.rm = TRUE)
#mean(data_germany$hosp_patients_per_million, na.rm = TRUE)

median(data_germany$new_cases)
median(data_germany$new_cases_smoothed)
median(data_germany$new_cases_per_million)
median(data_germany$new_cases_smoothed_per_million)
#median(data_germany$hosp_patients, na.rm = TRUE)
#median(data_germany$hosp_patients_per_million, na.rm = TRUE)

sd(data_germany$new_cases)
sd(data_germany$new_cases_smoothed)
sd(data_germany$new_cases_per_million)
sd(data_germany$new_cases_smoothed_per_million)
#sd(data_germany$hosp_patients, na.rm = TRUE)
#sd(data_germany$hosp_patients_per_million, na.rm = TRUE)

var(data_germany$new_cases)
var(data_germany$new_cases_smoothed)
var(data_germany$new_cases_per_million)
var(data_germany$new_cases_smoothed_per_million)
#var(data_germany$hosp_patients, na.rm = TRUE)
#var(data_germany$hosp_patients_per_million, na.rm = TRUE)

quantile(data_germany$new_cases)
quantile(data_germany$new_cases_smoothed)
quantile(data_germany$new_cases_per_million)
quantile(data_germany$new_cases_smoothed_per_million)
#quantile(data_germany$hosp_patients, na.rm = TRUE)
#quantile(data_germany$hosp_patients_per_million, na.rm = TRUE)

library(e1071)
skewness(data_germany$new_cases)
skewness(data_germany$new_cases_smoothed)
skewness(data_germany$new_cases_per_million)
skewness(data_germany$new_cases_smoothed_per_million)
#skewness(data_germany$hosp_patients, na.rm = TRUE)
#skewness(data_germany$hosp_patients_per_million, na.rm = TRUE)

kurtosis(data_germany$new_cases)
kurtosis(data_germany$new_cases_smoothed)
kurtosis(data_germany$new_cases_per_million)
kurtosis(data_germany$new_cases_smoothed_per_million)
#kurtosis(data_germany$hosp_patients, na.rm = TRUE)
#kurtosis(data_germany$hosp_patients_per_million, na.rm = TRUE)

#https://cs.wikipedia.org/wiki/T-test#Jednov%C3%BDb%C4%9Brov%C3%BD_t-test

t.test(data_germany$new_cases, mu = 3300)
t.test(data_germany$new_cases_smoothed)
t.test(data_germany$new_cases_per_million)
t.test(data_germany$new_cases_smoothed_per_million)
#t.test(data_germany$hosp_patients)
#t.test(data_germany$hosp_patients_per_million)

#t.test(data_germany$new_cases_per_million, data_slovakia$new_cases_per_million, var.equal = TRUE)
#t.test(data_germany$new_cases_per_million, data_czech$new_cases_per_million, var.equal = TRUE)
#t.test(data_germany$new_cases_per_million, data_poland$new_cases_per_million, var.equal = TRUE)
#t.test(data_germany$new_cases_per_million, data_austria$new_cases_per_million, var.equal = TRUE)
