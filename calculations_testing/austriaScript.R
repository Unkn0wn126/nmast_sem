data_austria <- read.csv("austriaData.csv")

colnames(data_austria)
data_austria_useful <- data_austria[1:1, c(7, 8, 13, 14, 21, 22)]
data_austria_useful_matrix <- data_austria[1:576, c(7, 8, 13, 14, 21, 22)]

forVariablesToLoop <- colnames(data_austria_useful)

min(data_austria$new_cases)
min(data_austria$new_cases_smoothed, na.rm = TRUE)
min(data_austria$new_cases_per_million)
min(data_austria$new_cases_smoothed_per_million, na.rm = TRUE)
min(data_austria$hosp_patients, na.rm = TRUE)
min(data_austria$hosp_patients_per_million, na.rm = TRUE)

max(data_austria$new_cases)
max(data_austria$new_cases_smoothed, na.rm = TRUE)
max(data_austria$new_cases_per_million)
max(data_austria$new_cases_smoothed_per_million, na.rm = TRUE)
max(data_austria$hosp_patients, na.rm = TRUE)
max(data_austria$hosp_patients_per_million, na.rm = TRUE)

mean(data_austria$new_cases)
mean(data_austria$new_cases_smoothed, na.rm = TRUE)
mean(data_austria$new_cases_per_million)
mean(data_austria$new_cases_smoothed_per_million, na.rm = TRUE)
mean(data_austria$hosp_patients, na.rm = TRUE)
mean(data_austria$hosp_patients_per_million, na.rm = TRUE)

median(data_austria$new_cases)
median(data_austria$new_cases_smoothed, na.rm = TRUE)
median(data_austria$new_cases_per_million)
median(data_austria$new_cases_smoothed_per_million, na.rm = TRUE)
median(data_austria$hosp_patients, na.rm = TRUE)
median(data_austria$hosp_patients_per_million, na.rm = TRUE)

sd(data_austria$new_cases)
sd(data_austria$new_cases_smoothed, na.rm = TRUE)
sd(data_austria$new_cases_per_million)
sd(data_austria$new_cases_smoothed_per_million, na.rm = TRUE)
sd(data_austria$hosp_patients, na.rm = TRUE)
sd(data_austria$hosp_patients_per_million, na.rm = TRUE)

var(data_austria$new_cases)
var(data_austria$new_cases_smoothed, na.rm = TRUE)
var(data_austria$new_cases_per_million)
var(data_austria$new_cases_smoothed_per_million, na.rm = TRUE)
var(data_austria$hosp_patients, na.rm = TRUE)
var(data_austria$hosp_patients_per_million, na.rm = TRUE)

quantile(data_austria$new_cases)
quantile(data_austria$new_cases_smoothed, na.rm = TRUE)
quantile(data_austria$new_cases_per_million)
quantile(data_austria$new_cases_smoothed_per_million, na.rm = TRUE)
quantile(data_austria$hosp_patients, na.rm = TRUE)
quantile(data_austria$hosp_patients_per_million, na.rm = TRUE)

library(e1071)
skewness(data_austria$new_cases)
skewness(data_austria$new_cases_smoothed, na.rm = TRUE)
skewness(data_austria$new_cases_per_million)
skewness(data_austria$new_cases_smoothed_per_million, na.rm = TRUE)
skewness(data_austria$hosp_patients, na.rm = TRUE)
skewness(data_austria$hosp_patients_per_million, na.rm = TRUE)

kurtosis(data_austria$new_cases)
kurtosis(data_austria$new_cases_smoothed, na.rm = TRUE)
kurtosis(data_austria$new_cases_per_million)
kurtosis(data_austria$new_cases_smoothed_per_million, na.rm = TRUE)
kurtosis(data_austria$hosp_patients, na.rm = TRUE)
kurtosis(data_austria$hosp_patients_per_million, na.rm = TRUE)

#https://cs.wikipedia.org/wiki/T-test#Jednov%C3%BDb%C4%9Brov%C3%BD_t-test

t.test(data_austria$new_cases, mu = 3300)
t.test(data_austria$new_cases_smoothed)
t.test(data_austria$new_cases_per_million)
t.test(data_austria$new_cases_smoothed_per_million)
t.test(data_austria$hosp_patients)
t.test(data_austria$hosp_patients_per_million)

#t.test(data_austria$new_cases_per_million, data_austria$new_cases_per_million, var.equal = TRUE)
#t.test(data_austria$new_cases_per_million, data_czech$new_cases_per_million, var.equal = TRUE)
#t.test(data_austria$new_cases_per_million, data_austria$new_cases_per_million, var.equal = TRUE)
#t.test(data_austria$new_cases_per_million, data_austria$new_cases_per_million, var.equal = TRUE)
