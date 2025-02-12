data_czech <- read.csv("czechData.csv")
data_poland <- read.csv("polandData.csv")
data_germany <- read.csv("germanyData.csv")
data_slovakia <- read.csv("slovakiaData.csv")
data_austria <- read.csv("austriaData.csv")

colnames(data_czech)
data_czech_useful <- data_czech[1:1, c(7, 8, 13, 14, 21, 22, 60)]
data_czech_useful_matrix <- data_czech[1:576, c(7, 8, 13, 14, 21, 22, 60)]

forVariablesToLoop <- colnames(data_czech_useful)

min(data_czech$new_cases)
min(data_czech$new_cases_smoothed)
min(data_czech$new_cases_per_million)
min(data_czech$new_cases_smoothed_per_million)
min(data_czech$hosp_patients, na.rm = TRUE)
min(data_czech$hosp_patients_per_million, na.rm = TRUE)

max(data_czech$new_cases)
max(data_czech$new_cases_smoothed)
max(data_czech$new_cases_per_million)
max(data_czech$new_cases_smoothed_per_million)
max(data_czech$hospital_beds_per_thousand)
max(data_czech$hosp_patients, na.rm = TRUE)
max(data_czech$hosp_patients_per_million, na.rm = TRUE)

mean(data_czech$new_cases)
mean(data_czech$new_cases_smoothed)
mean(data_czech$new_cases_per_million)
mean(data_czech$new_cases_smoothed_per_million)
mean(data_czech$hosp_patients, na.rm = TRUE)
mean(data_czech$hosp_patients_per_million, na.rm = TRUE)

median(data_czech$new_cases)
median(data_czech$new_cases_smoothed)
median(data_czech$new_cases_per_million)
median(data_czech$new_cases_smoothed_per_million)
median(data_czech$hosp_patients, na.rm = TRUE)
median(data_czech$hosp_patients_per_million, na.rm = TRUE)

sd(data_czech$new_cases)
sd(data_czech$new_cases_smoothed)
sd(data_czech$new_cases_per_million)
sd(data_czech$new_cases_smoothed_per_million)
sd(data_czech$hosp_patients, na.rm = TRUE)
sd(data_czech$hosp_patients_per_million, na.rm = TRUE)

var(data_czech$new_cases)
var(data_czech$new_cases_smoothed)
var(data_czech$new_cases_per_million)
var(data_czech$new_cases_smoothed_per_million)
var(data_czech$hosp_patients, na.rm = TRUE)
var(data_czech$hosp_patients_per_million, na.rm = TRUE)

quantile(data_czech$new_cases)
quantile(data_czech$new_cases_smoothed)
quantile(data_czech$new_cases_per_million)
quantile(data_czech$new_cases_smoothed_per_million)
quantile(data_czech$hosp_patients, na.rm = TRUE)
quantile(data_czech$hosp_patients_per_million, na.rm = TRUE)

library(e1071)
skewness(data_czech$new_cases)
skewness(data_czech$new_cases_smoothed)
skewness(data_czech$new_cases_per_million)
skewness(data_czech$new_cases_smoothed_per_million)
skewness(data_czech$hosp_patients, na.rm = TRUE)
skewness(data_czech$hosp_patients_per_million, na.rm = TRUE)

kurtosis(data_czech$new_cases)
kurtosis(data_czech$new_cases_smoothed)
kurtosis(data_czech$new_cases_per_million)
kurtosis(data_czech$new_cases_smoothed_per_million)
kurtosis(data_czech$hosp_patients, na.rm = TRUE)
kurtosis(data_czech$hosp_patients_per_million, na.rm = TRUE)

#https://cs.wikipedia.org/wiki/T-test#Jednov%C3%BDb%C4%9Brov%C3%BD_t-test

t.test(data_czech$new_cases, mu = 3300)
t.test(data_czech$new_cases_smoothed)
t.test(data_czech$new_cases_per_million)
t.test(data_czech$new_cases_smoothed_per_million)
t.test(data_czech$hosp_patients)
t.test(data_czech$hosp_patients_per_million)

t.test(data_czech$new_cases_per_million, data_slovakia$new_cases_per_million, var.equal = TRUE)
t.test(data_czech$new_cases_per_million, data_germany$new_cases_per_million, var.equal = TRUE)
t.test(data_czech$new_cases_per_million, data_poland$new_cases_per_million, var.equal = TRUE)
t.test(data_czech$new_cases_per_million, data_austria$new_cases_per_million, var.equal = TRUE)

#https://www.dummies.com/education/math/statistics/base-r-statistical-functions/
