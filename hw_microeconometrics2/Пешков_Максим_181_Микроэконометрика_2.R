########Домашнее задание 2
####Пешков Максим БЭК181


###############  Подготовка к работе ###############



# Подключим необходимые библиотеки
library("titanic")                                               # пакет, содержащий датафрейм с
                                                                 # информацией о пассажирах Титаника
library("glmx")                                                  # пакет, позволяющий оценивать пробит                                                   # модель с гетероскдестичной 
                                                                 # случайной ошибкой
library("lmtest")                                                # дополнительные тесты

library("numDeriv")                                              # численное дифференцирование
library("sampleSelection")                                       # встроенный датафрейм для
                                                                 # выполнения домашнего задания
library("GJRM")                                                  # система бинарных уравнений

library("stargazer")


library("memisc")
library("dplyr")
library("psych")
library("foreign")
library("hexbin")
library("car")
library("sandwich")
library("tidyverse") 
library('estimatr')


library("kableExtra")

library("margins") 
library("plm")
library("xtable")
library("lmtest")
library("pbivnorm")

# Подключим дополнительные библиотеки
library("mvtnorm")                       # симуляции из многомерного
# нормального распределения

library("numDeriv")                      # численное дифференцирование

library("stringr")                       # работа со строками
library("tm")

library("AER")                           # tobit модель и тест на overdispersion 
library("VGAM")                          # Модель tobit, второй метод
library("crch")                          # Модель tobit, третий метод
library("truncreg")                      # Регрессия с усечением

library("hpa")                           # моменты усеченного 
# нормального распределения
library("readxl")  



# Отключим scientific notation
options(scipen = 999)


#######  Скачаем данные ########

# Подключим встроенный датафрейм, содержащий информацию
# о характеристиках американских женщин и их трудовом
# статусе в 1975-1976 годах
data(Mroz87)
# Для тех, у кого не считывается встроенный датафрейм
                                                                        # предварительно установите пакет
#Mroz87 <- read_excel("Desktop\\hw_microeconometrics2\\Mroz87.xlsx") # укажите путь к файлу                                                                                          # на своем компьютере
h <- na.omit(as.data.frame(Mroz87))


head(h)






###############  Часть 1 ###############


####### Описание данных #########

# Краткое описание:
help(Mroz87)


# lfp(employment_dummy) - бинарная переменная, принимающая значение 1
#       если иженщина работает и 0 - в противном случае
# hours - количество проработанных часов за год
# kids5 - количество детей младше 6 лет
# kids618 - количество несовершеннолетних детей
#           старше пяти лет
# age - возраст женщины
# educ - число лет, потраченных женщиной на
#        получение образования
# wage - почасовая зарплата женщины
# hushrs - количество часов, проработанных мужем женщины
# husage - возраст мужа женщины
# huseduc - число лет, потраченных муженм женщины на
#           получение образования
# huswage - зарплата мужа женщины
# faminc - доход семьи женщины
# mtr - налоговая нагрузка на женщину
# fatheduc - число лет, потраченных отцом женщины на
#            получение образования
# motheduc - число лет, потраченных отцом женщины на
#            получение образования
# unem - безработица в регионе проживания женщины
# city - бинарная переменная, принимающая значение 1
#        если женщина живет в городе и 0 - иначе
# exper - рабочий стаж женщины в годах
# nwifeinc - доход семьи женщины за вычетом ее дохода
# wifecoll - бинарная переменная, принимающая значение 1
#            если женщина посещала колледж и 0 - иначе
# huscoll - бинарная переменная, принимающая значение 1
#           если муж женщины посещал колледж и 0 - иначе


h$employment_dummy <- h$lfp #для большей понятности
h$husband_work_hours <- h$hushrs #для большей понятности
h$education <- h$educ #для большей понятности
h$experience <- h$exper #для большей понятности
h$unemployment_region <- h$unem #для большей понятности

head(h)


#### мои выбранные переменные


# employment_dummy ~ husband_work_hours+ kids5+unemployment_region
# wage~husband_work_hours+education+experience






###############  Часть 2 ###############


####### Задание 2.1-2.3

tr_left =0 

model_tobit <- crch(wage~husband_work_hours+education+experience,   # формула
                    data = h,                        # данные
                    left = tr_left                # нижнее (левое) усечение
                    )                
summary(model_tobit)                                 # посмотрим результат
est_tobit <- coef(model_tobit)                       # достанем оценки
coef_tobit <- est_tobit[-length(est_tobit)]          # оценки регрессионных
# коэффициентов
sigma_tobit <- exp(est_tobit[length(est_tobit)])     # достаем оценку 
# стандартного отклонения


summary(model_tobit)


####### Задание 2.4


# Создадим индивида
She <- data.frame("husband_work_hours" = 2000,
                    "education" = 15,
                    "experience" = 5)

#### 1


# Рассчитаем оценку безусловного математического 
# ожидания зависимой переменной, то есть E(y*)
wage_est <- predict(model_tobit, 
                      newdata = She) 

wage_est

#### 2
lambda_she <- dnorm(wage_est/sigma_tobit)/pnorm(wage_est/sigma_tobit)

(wage_est+lambda_she*sigma_tobit)*pnorm(wage_est/sigma_tobit) #то есть E(y)

#### 3

pnorm(wage_est/sigma_tobit) #то есть P(y>0)



####### Задание 2.5



#### 1 предельный эффект для E(y*)

coef_tobit[3]

#### 2 предельный эффект для E(y)

coef_tobit[3]*pnorm(wage_est/sigma_tobit)

#### 3 предельный эффект для P(y>0)

dnorm(wage_est/sigma_tobit)*coef_tobit[3]/sigma_tobit



####### Задание 2.6

tr_left =0 

model_tobit2 <- crch(wage~husband_work_hours+education+experience+I(education^2),   # формула
                    data = h,                        # данные
                    left = tr_left                # нижнее (левое) усечение
)                
summary(model_tobit2)                                 # посмотрим результат
est_tobit2 <- coef(model_tobit2)                       # достанем оценки
coef_tobit2 <- est_tobit2[-length(est_tobit2)]          # оценки регрессионных
# коэффициентов
sigma_tobit2 <- exp(est_tobit2[length(est_tobit2)])     # достаем оценку 
# стандартного отклонения

wage_est2 <- predict(model_tobit2, 
                    newdata = She) 

wage_est2
#### 1 предельный эффект для E(y*)

coef_tobit2[3] + 2*She$education*coef_tobit2[5]

#### 2 предельный эффект для E(y)

(coef_tobit2[3] + 2*She$education*coef_tobit2[5])*pnorm(wage_est2/sigma_tobit2)

#### 3 предельный эффект для P(y>0)

dnorm(wage_est2/sigma_tobit2)*(coef_tobit2[3] + 2*She$education*coef_tobit2[5])/sigma_tobit2


####### Задание 2.7

model_tobit_het <- crch(wage~husband_work_hours+education+experience|age+experience,   # формула
                    data = h,                        # данные
                    left = tr_left,                # нижнее (левое) усечение
                    link.scale = "log"
)                
summary(model_tobit_het)                                 # посмотрим результат

LR <- 2*(as.numeric(logLik(model_tobit_het))-as.numeric(logLik(model_tobit)))

p_value_LR <- 1 - pchisq(q = LR, df = 2) 

p_value_LR


####### Задание 2.8

She <- data.frame("husband_work_hours" = 2000,
                  "education" = 15,
                  "experience" = 5,
                  "age" = 45)


est_tobit_het <- coef(model_tobit_het)                       # достанем оценки
coef_tobit_het_sigma <- est_tobit_het[c(5,6,7)]   

coef_tobit_het <- est_tobit_het[c(1,2,3,4)] 

sigma_tobit_het <- exp(coef_tobit_het_sigma[1] + coef_tobit_het_sigma[2]*She$age+coef_tobit_het_sigma[3]*She$experience) 


wage_est_het <- coef_tobit_het[1] + coef_tobit_het[2]*She$husband_work_hours +coef_tobit_het[3]*She$education+coef_tobit_het[4]*She$experience


###предельные эффекты

#### 1 предельный эффект для E(y*)
est_tobit_het[4]

#### 2 предельный эффект для E(y)
coef_tobit_het[4]*pnorm(wage_est_het/sigma_tobit_het) + coef_tobit_het_sigma[3]*sigma_tobit_het*dnorm(wage_est_het/sigma_tobit_het)

#### 3 предельный эффект для P


dnorm(wage_est_het/sigma_tobit_het)*((est_tobit_het[4]/sigma_tobit_het) - wage_est_het*(coef_tobit_het_sigma[3]/sigma_tobit_het))




###############  Часть 3 ###############


####### Задание 3.1-3.2


model_mle <- selection(                              
  selection = employment_dummy ~ husband_work_hours + kids5+unemployment_region,                     # уравнение отбора
  outcome = wage ~ husband_work_hours + education+experience,                   # основное уравнение
  data = h,                                          # данные
  method = "ml")                                     # метод расчета ММП
summary(model_mle)                                   # результат оценивания
coef_mle <- coef(model_mle, part = "outcome")        # сохраним оценки коэффициентов
rho_mle <- model_mle$estimate["rho"]                 # оценка корреляции между
# случайными ошибками
sigma_mle <- model_mle$estimate["sigma"]             # стандартное отклонение
# случайной ошибки



####### Задание 3.3-3.4

# метода Хекмана, основанного на
# двухшаговой процедуре
model_2st <- selection(                              
  selection = employment_dummy ~ husband_work_hours + kids5+unemployment_region,                     # уравнение отбора
  outcome = wage ~ husband_work_hours + education+experience,                   # основное уравнение
  data = h,                                          
  method = "2step")                                  # метод расчета двухшаговая процедура
summary(model_2st)                                   # результат оценивания
coef_2st <- coef(model_2st, part = "outcome")        # сохраним оценки коэффициентов
coef_2st <- coef_2st[-length(coef_2st)]              # удалим лишний коэффициент
rho_2st <- model_2st$rho                             # оценка корреляции между
# случайными ошибками
# Сравним оценки и истинные значения
# регрессионные коэффициенты
data.frame(
           "Heckman MLE" = coef_mle,                 # оценки ММП Хекмана
           "Heckman 2step" = coef_2st)               # оценки двухшагового Хекмана
# корреляция случайных ошибок
data.frame(
           "Heckman MLE" = rho_mle,                  # оценка ММП Хекмана
           "Heckman 2step" = rho_2st)                # оценка двухшагового Хекмана


#OLS

model_ols <- lm(wage ~ husband_work_hours + education+experience,                   # основное уравнение
  data = h)                                 
summary(model_ols)  

coef_ols <- model_ols$coefficients


data.frame(
  "Heckman MLE" = coef_mle,                 # оценки ММП Хекмана
  "Heckman 2step" = coef_2st,
  "OLS" = coef_ols)


####### Задание 3.5


# Создадим индивида
She <- data.frame("husband_work_hours" = 2000,
                  "education" = 15,
                  "experience" = 5,
                  "kids5" = 1,
                  "unemployment_region" = 10,
                  "age" = 35)

# то есть E(y*|z)
cost_cond <- predict(model_mle, 
                     newdata = She, 
                     part = "outcome",                   # для основного уравнения
                     type = "conditional")               # условные предсказания 
cost_cond[1]                                             # E(y*|z = 0)
cost_cond[2]                                             # E(y*|z = 1)

cost_star <- predict(model_mle, 
                     newdata = She, 
                     part = "outcome",                   
                     type = "unconditional")             # безусловные предсказания   
cat_prob <- predict(model_mle, 
                    newdata = She, 
                    part = "selection",                  # для уравнения отбора
                    type = "response")                   # предсказываем вероятность
# оценим линейный индекс
cat_li <- predict(model_mle, 
                  newdata = She, 
                  part = "selection",                    # для уравнения отбора
                  type = "link")                         # предсказываем линейный индекс

# Оценим E(y*|z) вручную:
lambda_est_1 <- dnorm(cat_li) / pnorm(cat_li)            # оценка отношения Миллса
lambda_est_2 <- dnorm(cat_li) / pnorm(-cat_li)
cost_star + rho_mle * sigma_mle * lambda_est_1           # E(y*|z = 1)
cost_star - rho_mle * sigma_mle * lambda_est_2           # E(y*|z = 0)

##предельные эффекты

# E(y*|z = 1)
cat_li <- predict(model_mle, 
                  newdata = She, 
                  part = "selection",                    # для уравнения отбора
                  type = "link")                         # предсказываем линейный индекс


coef_s_est <- coef(model_mle)[1:4]

coef_mle["husband_work_hours"] - rho_mle * sigma_mle *(cat_li * lambda_est_1 +lambda_est_1 ^ 2)*coef_s_est["husband_work_hours"]

# E(y*|z = 0)
coef_mle["husband_work_hours"] + rho_mle * sigma_mle *(cat_li * lambda_est_2 +lambda_est_2 ^ 2)*coef_s_est["husband_work_hours"]




