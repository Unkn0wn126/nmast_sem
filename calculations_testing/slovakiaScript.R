data_slovakia <- read.csv("slovakiaData.csv")

colnames(data_slovakia)
data_slovakia_useful <- data_slovakia[1:1, c(7, 8, 13, 14, 21, 22)]
data_slovakia_useful_matrix <- data_slovakia[1:576, c(7, 8, 13, 14, 21, 22)]

forVariablesToLoop <- colnames(data_slovakia_useful)

min(data_slovakia$new_cases)
min(data_slovakia$new_cases_smoothed, na.rm = TRUE)
min(data_slovakia$new_cases_per_million)
min(data_slovakia$new_cases_smoothed_per_million, na.rm = TRUE)
min(data_slovakia$hosp_patients, na.rm = TRUE)
min(data_slovakia$hosp_patients_per_million, na.rm = TRUE)

max(data_slovakia$new_cases)
max(data_slovakia$new_cases_smoothed, na.rm = TRUE)
max(data_slovakia$new_cases_per_million)
max(data_slovakia$new_cases_smoothed_per_million, na.rm = TRUE)
max(data_slovakia$hosp_patients, na.rm = TRUE)
max(data_slovakia$hosp_patients_per_million, na.rm = TRUE)

mean(data_slovakia$new_cases)
mean(data_slovakia$new_cases_smoothed, na.rm = TRUE)
mean(data_slovakia$new_cases_per_million)
mean(data_slovakia$new_cases_smoothed_per_million, na.rm = TRUE)
mean(data_slovakia$hosp_patients, na.rm = TRUE)
mean(data_slovakia$hosp_patients_per_million, na.rm = TRUE)

median(data_slovakia$new_cases)
median(data_slovakia$new_cases_smoothed, na.rm = TRUE)
median(data_slovakia$new_cases_per_million)
median(data_slovakia$new_cases_smoothed_per_million, na.rm = TRUE)
median(data_slovakia$hosp_patients, na.rm = TRUE)
median(data_slovakia$hosp_patients_per_million, na.rm = TRUE)

sd(data_slovakia$new_cases)
sd(data_slovakia$new_cases_smoothed, na.rm = TRUE)
sd(data_slovakia$new_cases_per_million)
sd(data_slovakia$new_cases_smoothed_per_million, na.rm = TRUE)
sd(data_slovakia$hosp_patients, na.rm = TRUE)
sd(data_slovakia$hosp_patients_per_million, na.rm = TRUE)

var(data_slovakia$new_cases)
var(data_slovakia$new_cases_smoothed, na.rm = TRUE)
var(data_slovakia$new_cases_per_million)
var(data_slovakia$new_cases_smoothed_per_million, na.rm = TRUE)
var(data_slovakia$hosp_patients, na.rm = TRUE)
var(data_slovakia$hosp_patients_per_million, na.rm = TRUE)

quantile(data_slovakia$new_cases)
quantile(data_slovakia$new_cases_smoothed, na.rm = TRUE)
quantile(data_slovakia$new_cases_per_million)
quantile(data_slovakia$new_cases_smoothed_per_million, na.rm = TRUE)
quantile(data_slovakia$hosp_patients, na.rm = TRUE)
quantile(data_slovakia$hosp_patients_per_million, na.rm = TRUE)

library(e1071)
skewness(data_slovakia$new_cases)
skewness(data_slovakia$new_cases_smoothed, na.rm = TRUE)
skewness(data_slovakia$new_cases_per_million)
skewness(data_slovakia$new_cases_smoothed_per_million, na.rm = TRUE)
skewness(data_slovakia$hosp_patients, na.rm = TRUE)
skewness(data_slovakia$hosp_patients_per_million, na.rm = TRUE)

kurtosis(data_slovakia$new_cases)
kurtosis(data_slovakia$new_cases_smoothed, na.rm = TRUE)
kurtosis(data_slovakia$new_cases_per_million)
kurtosis(data_slovakia$new_cases_smoothed_per_million, na.rm = TRUE)
kurtosis(data_slovakia$hosp_patients, na.rm = TRUE)
kurtosis(data_slovakia$hosp_patients_per_million, na.rm = TRUE)

#https://cs.wikipedia.org/wiki/T-test#Jednov%C3%BDb%C4%9Brov%C3%BD_t-test

t.test(data_slovakia$new_cases, mu = 3300)
t.test(data_slovakia$new_cases_smoothed)
t.test(data_slovakia$new_cases_per_million)
t.test(data_slovakia$new_cases_smoothed_per_million)
t.test(data_slovakia$hosp_patients)
t.test(data_slovakia$hosp_patients_per_million)

#t.test(data_slovakia$new_cases_per_million, data_slovakia$new_cases_per_million, var.equal = TRUE)
#t.test(data_slovakia$new_cases_per_million, data_czech$new_cases_per_million, var.equal = TRUE)
#t.test(data_slovakia$new_cases_per_million, data_poland$new_cases_per_million, var.equal = TRUE)
#t.test(data_slovakia$new_cases_per_million, data_austria$new_cases_per_million, var.equal = TRUE)
