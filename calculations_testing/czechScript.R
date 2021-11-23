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

#https://www.dummies.com/education/math/statistics/base-r-statistical-functions/

#HISTOGRAMY
hist(data_czech$new_cases_smoothed)
hist(data_czech$new_cases_per_million)
hist(data_czech$new_cases_smoothed_per_million)
hist(data_czech$hosp_patients_per_million)
hist(data_czech$new_cases, main = "Histogram: Hospitalizováni pacienti v ÈR",
     ,xlab = "Hospitalizovaní v CR", border = "red", col = "yellow",xlim = c(0,16000), las = 1, breaks = 8)

#Histogram novì nakažených
histNewTests <- hist(log1p(data_czech$new_tests),
                     main = "Nove testovani v Cesku",
                     xlab = "Logaritmus z poctu nove testovanych",
                     col = "yellow",
                     breaks = 10)

histNewCases <- hist(log1p(data_czech$new_cases),
                     main = "Nove pripady v Cesku",
                     xlab = "Logaritmus z poctu novych pripadu",
                     col = "yellow",
                     breaks = 10)

#histCzNewCases <- hist(log1p(data_czech$new_cases))
#histGerNewCases <- hist(log1p(data_germany$new_cases))
plot(histCzNewCases, col=rgb(0,0,1,1/4), xlim = c(7,15),
     main = "Nove pripady pro Cesko a Nemecko",
     xlab = "Logaritmus z novych pripadu")  # first histogram

plot(histGerNewCases,  col = rgb(1,0,0,1/4),xlim = c(7,15), add = TRUE)
legend("topright",
       legend = c("Cesko", "Nemecko"),
       fill = c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)),       # Color of the squares
       border = "black") # Color of the border of the squares

histCzNewCases <- hist(log1p(data_czech$new_tests),
                       main = "Cesko: nove testy",
                       col = "limegreen",
                       xlab = "Logaritmus z poctu novych testu")

histGerNewCases <- hist(log1p(data_austria$new_tests),
                        main = "Nemecko: nove testy",
                        col = "lightblue",
                        xlab = "Logaritmus z poctu novych testu")

plot(histCzNewCases, col = rgb(0,0,1,1/4), ylim = c(0,220), xlim = c(7,15))  # first histogram
plot(histGerNewCases,  col = rgb(1,0,0,1/4),ylim = c(0,220), xlim = c(7,15), add = TRUE)
legend("topright",
       legend = c("Cesko", "Nemecko"),
       fill = c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)),       # Color of the squares
       border = "black") # Color of the border of the squares

#PLOTY
b <- plot(log1p(data_czech$new_cases), pch = 4)
a <- plot(data_czech$new_tests, xlim = c(180,600),pch = 4)
plot(data_czech$reproduction_rate, pch = 4)
plot(data_czech$icu_patients, pch = 4)
plot(data_czech$hosp_patients, pch = 4)
plot(data_czech$weekly_icu_admissions, pch = 4)
plot(data_czech$weekly_hosp_admissions, pch = 4)
plot(data_czech$positive_rate, pch = 4)
plot(data_czech$new_vaccinations, pch = 4)
plot(data_czech$excess_mortality, pch = 4)
plot(log1p(data_czech$new_cases), pch = 4)

#BOXPLOTY
boxplot(data_czech$new_cases_per_million)
boxplot(data_czech$reproduction_rate)
boxplot(log1p(data_czech$new_deaths),
        main = "Cesko zlogaritmovane nove smrti",
)
#3D GRAFY
library("plot3D")
scatter3D(data_czech$new_cases, data_czech$new_tests, data_czech$total_cases)
scatter3D(log1p(data_czech$new_cases), log1p(data_czech$new_tests), log1p(data_czech$total_cases))

scatter3D(data_czech$new_cases, data_czech$new_tests, data_czech$new_vaccinations)
scatter3D(c(1:576), c(1:576), log1p(data_czech$total_cases))
scatter3D(c(1:576), c(1:576), (data_czech$reproduction_rate))

#HEXBIN
library(hexbin)
library(RColorBrewer)

bin <- hexbin(log1p(data_czech$new_cases), data_czech$new_deaths, xbins = 40)
my_colors = colorRampPalette(rev(brewer.pal(11,'Spectral')))
plot(bin, main = "" , colramp=my_colors , legend = F )

#CHERNOFF FACES
library(aplpack)
countries <- c("Czech", "Slovakia", "Germany", "Poland", "Austria")
countriesMeanNewCases <- c(mean(data_czech$new_cases),
                             mean(data_slovakia$new_cases),
                             mean(data_germany$new_cases),
                             mean(data_poland$new_cases),
                             mean(data_austria$new_cases))

countriesMeanTotalCases <- c(mean(data_czech$total_cases),
                             mean(data_slovakia$total_cases),
                             mean(data_germany$total_cases),
                             mean(data_poland$total_cases),
                             mean(data_austria$total_cases))

countriesPopulation <- c(mean(data_czech$population),
                             mean(data_slovakia$population),
                             mean(data_germany$population),
                             mean(data_poland$population),
                             mean(data_austria$population))

tabulka <- data.frame(countries, countriesMeanNewCases, countriesMeanTotalCases, countriesPopulation)

faces(tabulka[,-1], labels = tabulka[,1])

##QQPLOT
?qqplot()
qqplot(data_czech$new_cases,data_czech$new_deaths)
qqplot(data_czech$new_tests,data_czech$new_cases)

#TESTOVANI HYPOTEZ
#TESTY
#https://cs.wikipedia.org/wiki/T-test#Jednov%C3%BDb%C4%9Brov%C3%BD_t-test

#JEDNOVYBEROVY STUDENTUV TEST
t.test(data_czech$new_cases, mu = 3300)
t.test(data_czech$new_cases_smoothed)
t.test(data_czech$new_cases_per_million)
t.test(data_czech$new_cases_smoothed_per_million)
t.test(data_czech$hosp_patients)
t.test(data_czech$hosp_patients_per_million)

#DVOUVYBEROVY STUDENTUV TEST
p1 = data_czech$new_cases[1:288]
p2 = data_czech$new_cases[289:576]
t.test(p1,p2)
t.test(data_czech$new_cases_per_million, data_slovakia$new_cases_per_million, var.equal = FALSE)
t.test(data_czech$new_cases_per_million, data_germany$new_cases_per_million, var.equal = TRUE)
t.test(data_czech$new_cases_per_million, data_poland$new_cases_per_million, var.equal = TRUE)
t.test(data_czech$new_cases_per_million, data_austria$new_cases_per_million, var.equal = TRUE)

#WILCOX TEST
wilcox.test(data_czech$new_cases_per_million, data_slovakia$new_cases_per_million)
wilcox.test(data_czech$new_cases_per_million, data_germany$new_cases_per_million)

#FISHERUV TEST
var.test(data_czech$new_cases_per_million, data_slovakia$new_cases_per_million)
var.test(lm(data_czech$new_cases_per_million ~ 1), lm(data_slovakia$new_cases_per_million ~ 1)) #stejny jak to nad tim

#SHAPIRO WILK TEST
shapiro.test(data_czech$new_cases)
shapiro.test(data_czech$new_tests)

#ANOVA
anova <- aov(data_czech$new_tests~data_czech$new_cases+data_czech$new_deaths)
summary(anova)
par(mfrow = c(3,2))
plot(anova, 1)
plot(anova, 2)
plot(anova, 3)
plot(anova, 4)
plot(anova, 5)
plot(anova, 6)
