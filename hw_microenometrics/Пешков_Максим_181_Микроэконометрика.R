########Домашнее задание 1
####Пешков Максим БЭК181
 

###############  Подготовка к работе ###############

### Установим все нужные пакеты

library("memisc")
library("dplyr")
library("psych")
library("foreign")
library("hexbin")
library("car")
library("sandwich")
library("tidyverse") 
library('estimatr')

library("sgof")
library("ggplot2")
library("foreign")
library("pander")

library("devtools")
library("pander")
library("shiny")
library("rmarkdown")
library("texreg")

library("stargazer")

library("knitr")
knitr::opts_chunk$set(echo = FALSE, fig.width = 16, fig.height = 8)

library("gridExtra")
library("grid")
library("ggcorrplot")
library("interplot")

library("DescTools")
library("pROC")  
library("numDeriv") 

library("robust")
library("robustbase")

library("margins") 
library("glmx")   
library("plm")
library("xtable")
library("lmtest")

library("mvtnorm")
library("GJRM")                    
library("pbivnorm")

library("readxl")
library("xlsx")


library("GJRM")                                         # оценивание систем
# бинарных уравнений
library("pbivnorm")

set.seed(777)  

###  Скачаем данные и введем переменные

df <- read.xlsx('homework.xlsx', sheetName = 'Sheet1')
df <- mutate(df, age2 = age^2) #добавим возраст в квадрате

head(df)



###############  Часть 1 ###############



###############  Задания 1.1-1.2

#Посмотрим на гистограммы переменных (из интереса для предложения гипотез)

histogram(df[,'sub'])
histogram(log(df[,'income'])) #доход как всегда лог-нормальный
histogram(df[,'income'])
histogram(df[,'internet']) #равномерное
histogram(df[,'age']) #равномерное
histogram(df[,'TV']) #дамми 40/60 разделение
histogram(df[,'series']) #равномерное но как бы категориальная не супер непрерывная

corr <- round(cor(df[,c(2, 3, 4, 5, 7, 8, 12,13)]), 2)
ggcorrplot(corr, hc.order = TRUE, lab = TRUE)

#берем время в интернете, частоту просмотра TV, возраст




###############  Часть 2 ###############

####### Задание 2.1-2.2

# линейно-вероятностная модель
model.linear <- lm(sub~series+internet+TV+age+I(age^2)+internet:TV,
                   data = df)

summary(model.linear)

AIC(model.linear)

stargazer(model.linear)


####### Задание 2.3

## MME в линейно-вероятностная модель
ME.linear <- margins(model.linear, 
                    data = df)
texreg(ME.linear)

coefficients(model.linear)[2] #series
coefficients(model.linear)[3]+coefficients(model.linear)[7]*max(df[,'TV']) #internet - всегда положительно
coefficients(model.linear)[3]+coefficients(model.linear)[7]*min(df[,'TV']) #internet - всегда положительно
coefficients(model.linear)[3]+coefficients(model.linear)[7]*mean(df[,'TV']) #internet - всегда положительно

coefficients(model.linear)[4]+coefficients(model.linear)[7]*mean(df[,'internet']) #TV - всегда отрицательно
coefficients(model.linear)[4]+coefficients(model.linear)[7]*min(df[,'internet']) #TV - всегда отрицательно
coefficients(model.linear)[4]+coefficients(model.linear)[7]*max(df[,'internet']) #TV - всегда отрицательно


coefficients(model.linear)[5]+2*coefficients(model.linear)[6]*mean(df[,'age']) #age
coefficients(model.linear)[5]+2*coefficients(model.linear)[6]*min(df[,'age']) #age
coefficients(model.linear)[5]+2*coefficients(model.linear)[6]*max(df[,'age']) #age

-(coefficients(model.linear)[5])/(2*coefficients(model.linear)[6])

coefficients(model.linear)[5]+2*coefficients(model.linear)[6]*50 #age

# если больше 95, то отрицательный. меньше - положительный




###############  Часть 3 ###############

######## Задания 3.1-3.2

# probit
model.probit <- glm(sub~series+internet+TV+age+I(age^2)+internet:TV,
                   data = df,
                   family=binomial(link = "probit"))
stargazer(model.probit)

summary(model.probit)


######## Задание 3.3

Maks <- data.frame(series = 4,                             # мои характеристики
                   internet = 0.7,
                   TV = 0,
                   age = 21)

prob.Maks <- predict(model.probit, newdata = Maks,    # оценка вероятности 
                      type = "response")    
prob.Maks

pnorm(coefficients(model.probit)[1]+coefficients(model.probit)[2]*4+coefficients(model.probit)[3]*0.7+coefficients(model.probit)[5]*21+
coefficients(model.probit)[6]*21*21)

######## Задания 3.4

#MME probit

mean_ind <- data.frame(series = mean(df[,'series']),                          
                       internet = mean(df[,'internet']),
                       TV = mean(df[,'TV']),
                       age = mean(df[,'age']))
prob.mean <- predict(model.probit, newdata = mean_ind,    # оценка вероятности 
                     type = "response")  

ME.probit <- margins(model.probit, variables = NULL,  
                     data = df)
summary(ME.probit)

den <- dnorm(prob.mean) 

den*(coefficients(model.probit)[5]+2*coefficients(model.probit)[6]*mean(df[,'age'])) #age

p1 <- pnorm(coefficients(model.probit)[1]
      +coefficients(model.probit)[2]*mean(df[,'series'])
      +coefficients(model.probit)[3]*mean(df[,'internet'])
      +coefficients(model.probit)[4]*1
      +coefficients(model.probit)[5]*mean(df[,'age'])+
        coefficients(model.probit)[6]*mean(df[,'age'])*mean(df[,'age'])
      +coefficients(model.probit)[7]*mean(df[,'internet'])*1) 


p0 <- pnorm(coefficients(model.probit)[1]
        +coefficients(model.probit)[2]*mean(df[,'series'])
        +coefficients(model.probit)[3]*mean(df[,'internet'])
        +coefficients(model.probit)[4]*0
        +coefficients(model.probit)[5]*mean(df[,'age'])+
          coefficients(model.probit)[6]*mean(df[,'age'])*mean(df[,'age'])
        +coefficients(model.probit)[7]*mean(df[,'internet'])*0)
p1-p0 #TV



######## Задание 3.5

# Сравним прогностическую точность
# пробит модель
probs.probit <- predict(model.probit,  
                        type = "response",
                        newdata = df)                  # вероятность
def.probit <- as.numeric(probs.probit >= 0.5)
correct.probit <- mean(def.probit == df$sub)           # доля верных прогнозов
# наивная модель
def.p <- mean(df$sub)                                  # доля
correct.naive <- max(def.p, 1 - def.p)                     # доля верных прогнозов
# линейная
probs.linear <- predict(model.linear,  
                        type = "response",
                        newdata = df)                  # вероятность
def.linear <- as.numeric(probs.linear >= 0.5)
correct.linear <- mean(def.linear == df$sub)

# сравнение
rbind(probit = correct.probit,                             # пробит модель
      naive = correct.naive,# наивная модель
      linear = correct.linear)  #линейная                             



######## Задания 3.6-3.7

#значимость предельных эффектов есть в выводе margins
summary(ME.probit)





###############  Часть 4 ###############


######## Задание 4.1

#тестирование на нормальность ошибок

model.probit <- glm(sub~series+internet+TV+age+I(age^2)+I(internet*TV),
                    data = df,
                    family=binomial(link = "probit"))

X.mat <- as.matrix(model.frame(model.probit))            # достаем датафрейм с регрессорами и
X.mat[, 1] <- 1                                          # первращаем его в матрицу, а также
colnames(X.mat)[1] <- "Intercept"                        # заменяем зависимую переменную на константу
head(X.mat, 6)
# Достанем датафрейм, содержащий
# переменные модели
d <- model.frame(model.probit)                           # все переменные

# Рассчитаем предварительные величины
y.li.est <- predict(model.probit)                        # оценка линейного индекса                                     
F.est <- pnorm(y.li.est)                                 # функции от линейного           
f.est <- dnorm(y.li.est)                                 # индекса

# Вычислим обобщенные остатки
gr <- ((d[, 1] - F.est) /                                # обобщенный остаток
         (F.est * (1 - F.est))) * f.est

# Считаем производные по коэффициентам
d_beta <- apply(X.mat, 2, function(x)                    # производные по
{                              # регресcионным коэффициентам
  x * gr
})
d_t1 <- (gr * y.li.est ^ 2)                              # производная по t1
d_t2 <- (gr * y.li.est ^ 3)                              # производная по t2

# Проводим LM тест
n <- nrow(d)                                             # число наблюдений
LM_df <- data.frame("my_ones" = rep(1, n),               # вектор из единиц 
                    "d_" = d_beta,
                    d_t1, d_t2)          
head(LM_df, 5)
ones_regression <- summary(lm(my_ones ~. + 0,            # регрессия на вектор единиц без константы
                              data = LM_df))       
R2 <- ones_regression$r.squared                          # коэффициент детерминации регрессии
LM_value_2 <- R2 * n                                     # LM статистика
p.value_2 <- 1 - pchisq(q = LM_value_2, 
                        df = 2)

p.value_2


######## Задание 4.2

###гетероскедастичность

model.probit <- glm(sub~series+internet+TV+age+I(age^2)+I(internet*TV),
                    data = df,
                    family=binomial(link = "probit"))

model.hetprobit <- hetglm(sub~series+internet+TV+age+I(age^2)+I(internet*TV)|
                            income+series,
                          data = df,
                          family=binomial(link = "probit"),
                          link.scale = "log")

lrtest(model.probit, model.hetprobit)



######## Задание 4.3
#предельный эффект series


# Достанем полученные оценки
beta.est <- model.hetprobit$coefficients$mean              # оценки коэффициентов при переменных
# основного уравнения
tau.est <- model.hetprobit$coefficients$scale              # оценки коэффициентов при переменных
# в уравнении дисперсии
sigma.est <- predict(model.hetprobit, type = "scale")

mean_ind_het <- data.frame(series = mean(df[,'series']),                          
                       internet = mean(df[,'internet']),
                       TV = mean(df[,'TV']),
                       age = mean(df[,'age']),
                       income = mean(df[,'income']))

prob.meanhet <- predict(model.hetprobit, newdata = mean_ind_het,    # оценка вероятности 
                      type = "response")                   
li.adj <- predict(model.hetprobit, newdata = mean_ind_het,  # оценка отношения линейного
                        type = "link")                     
# отклонению случайно ошибки
sigma.meanhet <- predict(model.hetprobit, newdata = mean_ind_het,   # оценка стандартного
                       type = "scale")                     # отклонения случайной

li.meanhet <- li.adj * sigma.meanhet     # оценка линейного# индекса 

# Считаем предельный эффект аналитически   
ME.series.het <- dnorm(li.meanhet, sd = sigma.meanhet) * 
  (beta.est["series"] - 
     li.meanhet * tau.est["series"])

ME.series.het

delta <- 1e-6                                              # приращение                                                 
mean_ind_het.delta <- mean_ind_het
mean_ind_het.delta$series <- mean_ind_het$series + delta          
prob.mean_ind_het.delta <- predict(model.hetprobit, 
                            newdata = mean_ind_het.delta,
                            type = "response")
ME.series.het.2 <- (prob.mean_ind_het.delta - prob.meanhet) / delta

ME.series.het.2

ME.series.het.sd <- 2*tau.est["series"]*exp(mean_ind_het["series"]*tau.est["series"]+
                                            +mean_ind_het["income"]*tau.est["income"])

ME.series.het.sd



######## Задание 4.4

#lr tests
#### 1) 
model.probit.R <- glm(formula = sub~series+internet+TV+I(age^2)+I(internet*TV),           # на возраст
                      data = df,                                  
                      family = binomial(link = "probit")) 
summary(model.probit.R)

lrtest(model.probit, model.probit.R)   

#### 2) 
model.probit.R.2 <- glm(formula = sub~series+internet+TV+I(internet*TV),           # на возраст
                      data = df,                                  
                      family = binomial(link = "probit")) 
summary(model.probit.R.2)

lrtest(model.probit, model.probit.R.2)

#### 3) 
model.probit.R.3 <- glm(formula = sub~series+internet+TV+I(internet*TV)
                        + I(age+10*age^2),           # на возраст
                        data = df,                                  
                        family = binomial(link = "probit")) 
summary(model.probit.R.3)
lrtest(model.probit, model.probit.R.3)

#### 4) 
model.probit.R.4 <- glm(formula = sub~series+internet+I(internet*TV)
                        + I(age+10*age^2),
                      offset = I(-0.5 * TV),           
                      data = df,                                  
                      family = binomial(link = "probit")) 
summary(model.probit.R.4)

# Проверим гипотезу LR тестом
lrtest(model.probit, model.probit.R.4)


######## Задание 4.5


# Проверим, можно ли оценивать совместную модели
# для мужчин и женщин
# H0: коэффициенты в моделях не различаются

# Оценим ограниченную модель
model.probit.R.male <- glm(sub~ series+internet+TV+age+I(age^2)+I(internet*TV)+male,
                      data = df,
                      family = binomial(link = "probit"))

# Оценим полную модель как комбинацию двух моделей
model.probit.F1 <- glm(sub~series+internet+TV+age+I(age^2)+I(internet*TV),
                       data = df[df$male == 1, ],
                       family = binomial(link = "probit"))

model.probit.F0 <- glm(sub~ series+internet+TV+age+I(age^2)+I(internet*TV),
                       data = df[df$male == 0, ],
                       family = binomial(link = "probit"))

# Считаем логарифмы правдоподобия 
# полной и ограниченной моделей
lnL.F <- logLik(model.probit.F1) + logLik(model.probit.F0) # логарифм правдоподобия 
# полной модели
lnL.R <- logLik(model.probit.R.male)                            # логарифм правдоподобия 
# ограниченной модели

# Тестируем гипотезу
r <- length(model.probit.R.male$coefficients) - 2               # число ограничений
t <- 2 * (lnL.F - lnL.R)                                   # статистика теста
p.value <- as.numeric(1 - pchisq(t, df = r))               # p-value теста

p.value

######## Задание 4.6

# Проверим, можно ли оценивать совместную модели
# для разных населенных пунктов
# H0: коэффициенты в моделях не различаются

# Оценим ограниченную модель
model.probit.R.res <- glm(sub~ series+internet+TV+age+I(age^2)+I(internet*TV)+residence,
                           data = df,
                           family = binomial(link = "probit"))
summary(model.probit.R.res)

# Оценим полную модель как комбинацию трех моделей
model.probit.F1.res <- glm(sub~series+internet+TV+age+I(age^2)+I(internet*TV),
                       data = df[df$residence == "City", ],
                       family = binomial(link = "probit"))

model.probit.F2.res <- glm(sub~series+internet+TV+age+I(age^2)+I(internet*TV),
                           data = df[df$residence == "Capital", ],
                           family = binomial(link = "probit"))

model.probit.F3.res <- glm(sub~series+internet+TV+age+I(age^2)+I(internet*TV),
                           data = df[df$residence == "Village", ],
                           family = binomial(link = "probit"))

# Считаем логарифмы правдоподобия 
# полной и ограниченной моделей
lnL.F <- logLik(model.probit.F1.res) + logLik(model.probit.F2.res)+logLik(model.probit.F3.res) # логарифм правдоподобия 
# полной модели
lnL.R <- logLik(model.probit.R.res)                            # логарифм правдоподобия 
# ограниченной модели

# Тестируем гипотезу
r <- length(model.probit.R.res$coefficients) - 2               # число ограничений
t <- 2 * (lnL.F - lnL.R)                                   # статистика теста
p.value <- as.numeric(1 - pchisq(t, df = r))               # p-value теста

p.value



###############  Часть 5 ###############


######## Задание 5.1


model.logit <- glm(sub~series+internet+TV+age+I(age^2)+I(internet*TV),
                    data = df,
                    family=binomial(link = "logit"))
stargazer(model.logit)




######## Задание 5.2


# Достанем оценки коэффициентов
coef.logit <- coef(model.logit)
# Отношение шансов - (вероятность успеха) / (вероятность неудачи)
#                    P(y = 1) / P(y = 0)                     
step <- 1                                                 # приращение для переменных

# [P(y = 1 | xk + step) / P(y = 0 | xk + step)] /
# [P(y = 1 | xk) / P(y = 0 | xk)]

# Оценим, во сколько раз, при прочих равных,
# изменится отношение шансов при
OR.series <- exp(coef.logit["series"] * step)    



######## Задание 5.3

#age
step <- 1
OR.age <- exp(coef.logit["age"] * step +                    # изменении возраста на step
                coef.logit["I(age^2)"] * step ^ 2 +
                2 * coef.logit["I(age^2)"] * 
                Maks$age * step)
OR.age

#internet
step <- 0.1
OR.internet <- exp(coef.logit["internet"] * step + coef.logit["I(internet * TV)"] * 
                Maks$TV * step)

OR.internet

#tv
step <- 1
OR.TV <- exp(coef.logit["TV"] * step + coef.logit["I(internet * TV)"] * 
                     Maks$internet * step)
OR.TV





###############  Часть 6 ###############

######## Задание 6.1

default_formula <- sub ~ series + internet+income              # для каждого из уравнений системы
stable_formula <- TV ~ age + internet                   
model_bp <- gjrm(formula = list(default_formula,        # задаем лист, состоящий из 
                                stable_formula),        # обеих формул
                 data = df,
                 Model = "B",                           # указываем тип модели как систему
                 # бинанрных уравнений
                 margins = c("probit", "probit"),       # задаем маржинальные распределения
                 # случайных ошибок уравнений
                 BivD = "N")                            # задаем тип совместного распределения
# случайных ошибок (копулу)
summary(model_bp)                                       # посмотрим на результат



######## Задание 6.2

rho_est <- model_bp$theta                               # оценка корреляции между случайными ошибками

rho_est


######## Задание 6.3

model_bp <- gjrm(formula = list(default_formula,        # задаем лист, состоящий из 
                                stable_formula),        # обеих формул
                 data = df,
                 Model = "B",                           # указываем тип модели как систему
                 # бинанрных уравнений
                 margins = c("probit", "probit"),       # задаем маржинальные распределения
                 # случайных ошибок уравнений
                 BivD = "N",
                 )   


cov_est <- solve(model_bp$fit$hessian)                  # оценка асимптотической ковариационной матрицы
std_rho <- sqrt(cov_est["theta.star", "theta.star"])    # оценка стандартной ошибки оценки корреляции
p_value_rho <- 2 * min(pnorm(rho_est / std_rho),        # p-value теста о равенстве корреляции между
                       1- pnorm(rho_est / std_rho))

p_value_rho

######## Задание 6.4

individ <- data.frame(series = 4,                             # мои характеристики
                   internet = 0.7,
                   income=50000,
                   age = 21)

#### 1)
p_1 <-pnorm(coefficients(model_bp, eq = 1)[1]+coefficients(model_bp, eq = 1)[2]*individ$series+
+coefficients(model_bp, eq = 1)[3]*individ$internet+ coefficients(model_bp, eq = 1)[4]*individ$income)

#### 2)
p_2 <-pnorm(coefficients(model_bp, eq = 2)[5]+coefficients(model_bp, eq = 2)[6]*individ$age+
        +coefficients(model_bp, eq = 2)[7]*individ$internet)


default_li <- predict(model_bp, eq = 1)                 
stable_li <- predict(model_bp, eq = 2)   

#### 3)
p_1_2 <- pbivnorm(x = cbind(coefficients(model_bp, eq = 1)[5]+coefficients(model_bp, eq = 1)[6]*individ$age+
                              +coefficients(model_bp, eq = 1)[7]*individ$internet, 
                            coefficients(model_bp, eq = 2)[5]+coefficients(model_bp, eq = 2)[6]*individ$age+
                              +coefficients(model_bp, eq = 2)[7]*individ$internet),  
                  rho = rho_est) 
p_1_2

#### 4)
p_1_n2 <- pbivnorm(x = cbind(coefficients(model_bp, eq = 1)[5]+coefficients(model_bp, eq = 1)[6]*individ$age+
                              +coefficients(model_bp, eq = 1)[7]*individ$internet, 
                            -(coefficients(model_bp, eq = 2)[5]+coefficients(model_bp, eq = 2)[6]*individ$age+
                              +coefficients(model_bp, eq = 2)[7]*individ$internet)),  
                  rho = -rho_est) 

p_1_cond_2 <- p_1_n2 / (1- p_2) 

p_1_cond_2




###############  Часть 7 ###############

######## Задание 7.1

# логит модель
probs.logit <- predict(model.logit,  type = "response")    # вероятность
sub.logit <- as.numeric(probs.logit >= 0.5)          # дефолт
correct.logit <- mean(sub.logit == df$sub)

#система
probs.sys <- predict(model_bp, eq = 1)    # вероятность
sub.sys <- as.numeric(probs.sys >= 0.5)          # дефолт
correct.sys <- mean(sub.sys == df$sub)

rbind(probit = correct.probit,                             # пробит модель
      naive = correct.naive,# наивная модель
      linear = correct.linear,#линейная 
      logit= correct.logit,
    sys = correct.sys
      )                              


######## Задание 7.2

model.linear1 <- lm(default_formula, data=df)
model.linear2 <- lm(stable_formula, data=df)
aic.linear <- AIC(model.linear1)+AIC(model.linear2)
bic.linear <- BIC(model.linear1)+BIC(model.linear2)

model.probit1 <- glm(default_formula, data=df, family=binomial(link = "probit"))
model.probit2 <- glm(stable_formula, data=df,family=binomial(link = "probit"))
aic.probit <- AIC(model.probit1)+AIC(model.probit2)
bic.probit <- BIC(model.probit1)+BIC(model.probit2)

model.logit1 <- glm(default_formula, data=df, family=binomial(link = "logit"))
model.logit2 <- glm(stable_formula, data=df,family=binomial(link = "logit"))
aic.logit <- AIC(model.logit1)+AIC(model.logit2)
bic.logit <- BIC(model.logit1)+BIC(model.logit2)

rbind(probit = aic.probit,                             # пробит модель
      linear = aic.linear,#линейная 
      logit= aic.logit,
      sys = AIC(model_bp)
)  

rbind(probit = bic.probit,                             # пробит модель
      linear = bic.linear,#линейная 
      logit= bic.logit,
      sys = BIC(model_bp)
)  


###############  Часть 8 ###############

set.seed(123)

######## Задание 8.1-8.2

n <- 5000                                           # число индивидов в выборке
h <- data.frame(income = exp(rnorm(n, 10, 0.7)))     # доход
h$age = round(runif(n, 20, 100))                     # возраст
h$сigarette <- rbinom(n, 1, 0.4)                      # дамми на курильщиков

eps <- rt(n, 13) 
beta <- c(1, 0.5, -0.09, 0.2) 


cofe_li <- beta[1] +                                        # линейный индекс,
  beta[2] * log(h$inc) +                                # отражающий вклад наблюдаемых факторов в вероятность частого употребления кофе
  beta[3] * h$age +
  beta[4] * h$сigarette                         

cofe_star <- cofe_li + eps                                   # латентная переменная,
# отражающая склонность
# к дефолту
h$coffee <- as.numeric(cofe_star >= 0)                         # наблюдаемое значение переменной
mean(h$coffee)    
#60 процентов пьет часто кофе

# Итоговые данные
head(h, 10)



######## Задание 8.3

# Будем оценивать регрессию не
# предполагая заранее наличие константы
modelLnL <- function(beta, y, X)                          # функция правдоподобия
{
  beta <- matrix(beta, ncol = 1)                           # вектор оптимизируемых параметров, то есть
  # регрессионных коэффициентов,
  # переводим в матрицу с одним столбцом
  y_est <- X %*% beta                                      # оценка математического ожидания 
  # латентной переменной
  
  n_obs <- nrow(X)                                         # количество наблюдений
  
  L_vec <- matrix(NA, nrow = n_obs,                        # вектор столбец вкладов наблюдений
                  ncol = 1)                                # в функцию правдоподобия
  
  is_y_0 <- y == 0                                         # вектор условий y = 0
  is_y_1 <- y == 1                                         # вектор условий y = 1
  
  L_vec[is_y_1] <- pt(y_est[is_y_1], 13)                    # вклад наблюдений для которых yi = 1
  L_vec[is_y_0] <- 1 - pt(y_est[is_y_0], 13)                # вклад наблюдений для которых yi = 0
  
  lnL <- sum(log(L_vec))                                   # логарифм функции правдоподобия
  
  return(lnL)
}

#регрессия
student_model <- function(formula,                                # формула
                   data)                                   # датафрейм                
{
  d <- model.frame(formula, data)                          # извлекаем переменные согласно формуле
  y <- as.matrix(d[, 1], ncol = 1)                         # зависимая переменная как первый
  # столбец в d
  X <- as.matrix(d[, -1])                                  # независимые переменные как все переменные
  # из data кроме зависимой
  X <- cbind(1, X)                                         # добавим константу
  
  x0_n <- ncol(X)                                          # число оцениваемых параметров
  
  X_names <- names(d)[-1]                                  # имена независимых переменных
  X_names <- c("Intercept", X_names)                       # добавляем имя константе
  
  result <- optim(par = rep(0, x0_n),                      # в качестве начальных точек возьмем нули
                  method = "BFGS",                         # численный метод оптимизации
                  fn = modelLnL,                          # максимизируемая функция правдоподобия
                  control = list(maxit = 10000,            # чтобы минимизационную задачу превратить
                                 fnscale = -1,             # в максимизационную умножаем функцию на -1
                                 reltol = 1e-10),          # установим достаточно высокую точность          
                  hessian = TRUE,                          # вернем Гессиан функции
                  X = X, y = y)                            # аргументы оптимизируемой функции 
  
  
  beta.est <- result$par                                   # оценки коэффициентов
  names(beta.est) <- X_names                               # сопоставляем имена для оценок коэффициентов
  
  as.cov.est <- solve(-result$hessian)                     # оценка асимптотической ковариационной
  colnames(as.cov.est) <- X_names                          # матрицы полученных оценок
  rownames(as.cov.est) <- X_names                          # сопоставляем имена
  
  return_list <- list("beta" = beta.est,                   # возвращаем оценки коэффициентов и
                      "cov" = as.cov.est,                  # асимптотической ковариационной матрицы
                      "data" = data,                       # возвращаем использовавшийся датафрейм
                      "lnL" = result$value,                # возвращаем логарифм функции правдоподобия
                      "X" = X,                             # возвращаем матрицу регрессоров
                      "y" = y,                             # возвращаем зависимую переменную     
                      "formula" = formula)                 # возвращаем использовавшуюся формулу                          
  return(return_list)                                      # возвращаем результат                               
}

# Воспользуемся созданной функцией
model <- student_model(coffee ~ log(income) + age+сigarette, 
                data = h)                                    
beta.est <- model$beta                                     # получаем оценки коэффициентов
beta.cov.est <- model$cov                                  # получаем оценку асимптотической
# ковариационной матрицы оценок
# коэффициентов

as_std_est <- sqrt(diag(beta.cov.est))                      # вычисляем оценки асимптотических

z <- beta.est / as_std_est                               # считаем тестовые статистики
p_values <- 2 * pmin(pnorm(z), 1 - pnorm(z)) 

z
p_values
