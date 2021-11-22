data_poland <- read.csv("polandData.csv")

colnames(data_poland)
data_poland_useful <- data_poland[1:1, c(7, 8, 13, 14, 21, 22)]
data_poland_useful_matrix <- data_poland[1:576, c(7, 8, 13, 14, 21, 22)]

forVariablesToLoop <- colnames(data_poland_useful)

min(data_poland$new_cases)
min(data_poland$new_cases_smoothed, na.rm = TRUE)
min(data_poland$new_cases_per_million)
min(data_poland$new_cases_smoothed_per_million, na.rm = TRUE)
min(data_poland$hosp_patients, na.rm = TRUE)
min(data_poland$hosp_patients_per_million, na.rm = TRUE)

max(data_poland$new_cases)
max(data_poland$new_cases_smoothed, na.rm = TRUE)
max(data_poland$new_cases_per_million)
max(data_poland$new_cases_smoothed_per_million, na.rm = TRUE)
max(data_poland$hosp_patients, na.rm = TRUE)
max(data_poland$hosp_patients_per_million, na.rm = TRUE)

mean(data_poland$new_cases)
mean(data_poland$new_cases_smoothed, na.rm = TRUE)
mean(data_poland$new_cases_per_million)
mean(data_poland$new_cases_smoothed_per_million, na.rm = TRUE)
mean(data_poland$hosp_patients, na.rm = TRUE)
mean(data_poland$hosp_patients_per_million, na.rm = TRUE)

median(data_poland$new_cases)
median(data_poland$new_cases_smoothed, na.rm = TRUE)
median(data_poland$new_cases_per_million)
median(data_poland$new_cases_smoothed_per_million, na.rm = TRUE)
median(data_poland$hosp_patients, na.rm = TRUE)
median(data_poland$hosp_patients_per_million, na.rm = TRUE)

sd(data_poland$new_cases)
sd(data_poland$new_cases_smoothed, na.rm = TRUE)
sd(data_poland$new_cases_per_million)
sd(data_poland$new_cases_smoothed_per_million, na.rm = TRUE)
sd(data_poland$hosp_patients, na.rm = TRUE)
sd(data_poland$hosp_patients_per_million, na.rm = TRUE)

var(data_poland$new_cases)
var(data_poland$new_cases_smoothed, na.rm = TRUE)
var(data_poland$new_cases_per_million)
var(data_poland$new_cases_smoothed_per_million, na.rm = TRUE)
var(data_poland$hosp_patients, na.rm = TRUE)
var(data_poland$hosp_patients_per_million, na.rm = TRUE)

quantile(data_poland$new_cases)
quantile(data_poland$new_cases_smoothed, na.rm = TRUE)
quantile(data_poland$new_cases_per_million)
quantile(data_poland$new_cases_smoothed_per_million, na.rm = TRUE)
quantile(data_poland$hosp_patients, na.rm = TRUE)
quantile(data_poland$hosp_patients_per_million, na.rm = TRUE)

library(e1071)
skewness(data_poland$new_cases)
skewness(data_poland$new_cases_smoothed, na.rm = TRUE)
skewness(data_poland$new_cases_per_million)
skewness(data_poland$new_cases_smoothed_per_million, na.rm = TRUE)
skewness(data_poland$hosp_patients, na.rm = TRUE)
skewness(data_poland$hosp_patients_per_million, na.rm = TRUE)

kurtosis(data_poland$new_cases)
kurtosis(data_poland$new_cases_smoothed, na.rm = TRUE)
kurtosis(data_poland$new_cases_per_million)
kurtosis(data_poland$new_cases_smoothed_per_million, na.rm = TRUE)
kurtosis(data_poland$hosp_patients, na.rm = TRUE)
kurtosis(data_poland$hosp_patients_per_million, na.rm = TRUE)

#https://cs.wikipedia.org/wiki/T-test#Jednov%C3%BDb%C4%9Brov%C3%BD_t-test

t.test(data_poland$new_cases, mu = 3300)
t.test(data_poland$new_cases_smoothed)
t.test(data_poland$new_cases_per_million)
t.test(data_poland$new_cases_smoothed_per_million)
t.test(data_poland$hosp_patients)
t.test(data_poland$hosp_patients_per_million)

#t.test(data_poland$new_cases_per_million, data_poland$new_cases_per_million, var.equal = TRUE)
#t.test(data_poland$new_cases_per_million, data_czech$new_cases_per_million, var.equal = TRUE)
#t.test(data_poland$new_cases_per_million, data_poland$new_cases_per_million, var.equal = TRUE)
#t.test(data_poland$new_cases_per_million, data_austria$new_cases_per_million, var.equal = TRUE)
