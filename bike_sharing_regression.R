
#Título: Bike Sharing - Prediction using Multiple Regression
#Authors: Carla M. Lemos e Hélder P. Silva
#Course: Data Mining I
#Date: 20/11/2020


# Download de Livrarias
# install.packages("tidyverse")
# install.packages("psych")
# install.packages("gridExtra")
# install.packages("caret")
# install.packages("glmnet")
# install.packages("caTools")

#Importar Livrarias
library("tidyverse") # Funcionalidades: modelar, transformar e visualizar dados
library("psych") # Estatística descritiva
library("caret") # Seleção de variáveis
library("gridExtra") # Visualização de dados
library("tibble") # Tabelas
library("glmnet") # Lasso Model
library("caTools") # Lasso Model
options(tibble.width = Inf)

# Defnir diretório de trabalho
# setwd("/Volumes/SSD/Education/Pos_Graduacao/Course_Work/Data Mining I/bike_sharing_regression")

# Importar dataset Bike_Sharing
bike_share <- read_csv("day.csv")

# Verificar se dataset foi importado corretamente
head(bike_share)

# Visualizar labels e fazer devidas correções
names(bike_share)

names(bike_share) <- c("id", "date", "season", "year", "month", "holiday", "weekday", "workingday", 
                       "weathersit", "temp", "atemp", "hum", "windspeed", "casual", "registered", "cnt")


# Verficar codificação das variáveis
str(bike_share)

# Correção do tipo de variável

bike_share$date <- as.Date(bike_share$date, "%d/%m/%Y")

variaveis_numericas <- c("temp","atemp","hum","windspeed","registered", "casual")
bike_share[,variaveis_numericas] <- lapply(bike_share[,variaveis_numericas], as.numeric)

variaveis_categoricas <- c("season", "year", "month", "holiday", "weekday","workingday", "weathersit")
bike_share[,variaveis_categoricas] <- lapply(bike_share[,variaveis_categoricas] , as.factor)



# Codificação das etiquetas das variáveis categóricas

bike_share$weekday <- factor(format(bike_share$weekday, format = "%A"),
                             levels = c(0,1,2,3,4,5,6),
                             labels = c("Sunday", "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

bike_share$year <- factor(format(bike_share$year, format = "%A"),
                          levels = c(0,1),
                          labels = c(2011,2012))

bike_share$season <- factor(format(bike_share$season, format = "%A"),
                            levels = c(1,2,3,4),
                            labels = c("Spring","Summer", "Autumn", "Winter"))

bike_share$weathersit <- factor(format(bike_share$weathersit, format = "%A"),
                                levels = c(1,2,3),
                                labels = c("Good","Cloudy", "Bad Weather"))



# Desnormalização das variáveis

temp_real <- function(min, max, vector){
  temp_real <- vector * (max-min) + min
  return (temp_real)
}

bike_share$real_temp <- temp_real(-8,39, bike_share$temp)
bike_share$real_atemp <-temp_real(-16,50, bike_share$atemp) 
bike_share$real_hum <- bike_share$hum*100
bike_share$real_windspeed <- bike_share$windspeed*67

rm(temp_real)



# Frequencias para variáveis categóricas

for (categorica in variaveis_categoricas){
  print(categorica)
  print(table(bike_share[categorica]))
}

rm(categorica)


# Análise de valores omissos

valores_omissos<-data.frame(apply(bike_share,2,function(x){sum(is.na(x))}))
names(valores_omissos)[1]='Valores omissos'

valores_omissos


# Identificar observações com outliers 

for (numerica in c(variaveis_numericas)){
  print(numerica)
  print(length(boxplot(bike_share[numerica])$out))
}

rm(numerica)


# Boxplots das variáveis numéricas

which(bike_share$windspeed %in% boxplot(bike_share$windspeed)$out)
which(bike_share$temp %in% boxplot(bike_share$temp)$out)
which(bike_share$hum %in% boxplot(bike_share$hum)$out)
which(bike_share$atemp %in% boxplot(bike_share$atemp)$out)
which(bike_share$registered %in% boxplot(bike_share$registered)$out)
which(bike_share$casual %in% boxplot(bike_share$casual)$out)


# Selecionar observações com outliers

obs_outliers <- unique(
                c(which(bike_share$windspeed %in%
                          boxplot(bike_share$windspeed)$out), 
                  which(bike_share$hum %in% 
                          boxplot(bike_share$hum)$out),
                  which(bike_share$casual %in% 
                          boxplot(bike_share$casual)$out))
                )

bike_share_outliers <- bike_share[obs_outliers,]
rm(obs_outliers)

# Remoção de outliers (substituição pela média ponderada)

bike_share$hum <- ifelse((bike_share$hum %in% boxplot(bike_share$hum)$out),
                         ave(bike_share$hum, bike_share$season, FUN = function(x) mean(x, na.rm = TRUE)),
                         bike_share$hum)

bike_share$windspeed <- ifelse((bike_share$windspeed %in% boxplot(bike_share$windspeed)$out),
                               ave(bike_share$windspeed, bike_share$season, FUN = function(x) mean(x, na.rm = TRUE)),
                               bike_share$windspeed)

bike_share$casual <- ifelse((bike_share$casual %in% boxplot(bike_share$casual)$out),
                               ave(bike_share$casual, bike_share$weathersit, bike_share$workingday, FUN = function(x) mean(x, na.rm = TRUE)),
                               bike_share$casual)


# Verificar boxplots corrigidos

boxplot(bike_share$windspeed)
boxplot(bike_share$hum)
boxplot(bike_share$casual)


# Estatísticas descritivas

est_descritiva <- describe(bike_share, omit = T, interp = F, IQR = T, quant=c(.25,.75))
est_descritiva %>% 
  select("max", "min","mean", "median", "sd", "skew", "kurtosis", "Q0.25", "Q0.75")


hist_temp_real <- ggplot(bike_share, aes(bike_share$real_temp)) + 
  geom_histogram(col="white", fill= "#c9c9c9")+
  xlab("Temperatura")+
  theme_light()

hist_atemp_real <- ggplot(bike_share, aes(bike_share$real_atemp)) + 
  geom_histogram(col="white", fill= "#c9c9c9")+
  xlab("Sensação Térmica")+
  theme_light()

hist_hum_real <- ggplot(bike_share, aes(bike_share$real_hum)) + 
  geom_histogram(col="white", fill= "#c9c9c9")+
  xlab("Humidade")+
  theme_light()

hist_windspeed_real <- ggplot(bike_share, aes(bike_share$real_windspeed)) + 
  geom_histogram(col="white", fill= "#c9c9c9")+
  xlab("Velocidade Vento")+
  theme_light()

grid.arrange(hist_temp_real, hist_atemp_real, hist_hum_real, hist_windspeed_real,ncol=2, nrow=2)

rm(hist_temp_real, hist_atemp_real, hist_hum_real, hist_windspeed_real)


# ANÁLISE GRÁFICA

# Distribuição de utilizadores por ano (Boxplot)

casual_by_year <- ggplot(bike_share, aes(x=year, y=casual))+
  geom_boxplot()+
  xlab("Anos")+
  ylab("Distribuição utilizadores")+
  ggtitle("Utilizadores Casuais")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))

registered_by_year <- ggplot(bike_share, aes(x=year, y=registered))+
  geom_boxplot()+
  xlab("Anos")+
  ylab("Distribuição utilizadores")+
  ggtitle("Utilizadores Registados")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(casual_by_year, registered_by_year, ncol=2)

rm(casual_by_year, registered_by_year)


# Variação do número de alugueres em função da estação do ano e situação meteorológica (Bar Chart)

season_count <- ggplot(bike_share, aes(x=season, y = cnt))+
  stat_summary(fun.y=sum, aes(group=1), geom="bar", fill = "#b8b8b8")+
  xlab("Estação do ano")+
  ylab("Numero de Utilizadores")+
  scale_y_continuous(limits=c(0, 2250000))+
  theme_light()

weather_count <- ggplot(bike_share, aes(x=weathersit, y = cnt))+
  stat_summary(fun.y=sum, aes(group=1), geom="bar", fill="#b8b8b8")+
  xlab("Situação meteorológica")+
  ylab("")+
  theme_light()

grid.arrange(season_count, weather_count, ncol=2)

rm(season_count, weather_count)


# Variação do número de alugueres ao longo do ano (Boxplot)

ggplot(bike_share, aes(x=month, y=cnt))+
  geom_boxplot()+
  xlab("Meses do ano")+
  ylab("Contagem utilizadores")+
  ggtitle("Distibuição de utilizadores por mês")+
  geom_vline(xintercept = 3, linetype="dashed", 
               color = "blue", size=.3)+
  geom_vline(xintercept = 6, linetype="dashed", 
             color = "blue", size=.3)+
  geom_vline(xintercept = 9, linetype="dashed", 
             color = "blue", size=.3)+
  geom_vline(xintercept = 12, linetype="dashed", 
             color = "blue", size=.3)+
  theme_light()+
  theme(plot.title = element_text(hjust = .5))


# Contagem de utilizadores reg + casuais por semana. (Bar Chart)

regist_users_by_week <- ggplot(bike_share, aes(x=weekday, y=registered, fill=workingday))+
  geom_col()+
  xlab("Dias da Semana")+
  ylab("Contagem total utilizadores")+
  ggtitle("Utililizadores registados por dia")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Não", "Sim"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))


casual_users_by_week <- ggplot(bike_share, aes(x=weekday, y=casual, fill=workingday))+
  geom_col()+
  xlab("Dias da Semana")+
  ylab("Contagem total utilizadores")+
  ggtitle("Utililizadores casuais por dia")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Não", "Sim"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))


grid.arrange(casual_users_by_week, regist_users_by_week, ncol=2)

rm(casual_users_by_week, regist_users_by_week)


# Temperatura  em função das estação ano (Table)

temp_season <- summarise(group_by(bike_share, season), min_temp = round(min(real_temp),2),
                         max_temp=round(max(real_temp),2),
                         median_temp=round(median(real_temp),2),
                         mean_temp = round(mean(real_temp),2), 
                         stdv_temp = round(sd(real_temp),2), 
                         count=n())

temp_season

rm(temp_season)

# Temperatura em função da estação do ano (Boxplot).

ggplot(bike_share, aes(x=season, y = real_temp))+
  geom_boxplot()+
  xlab("Estação do ano")+
  ylab("Temperatura (ºC)")+
  ggtitle("Temperatura em função da estação do ano")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))


# Alugueres e temperatura por estação (Scatterplot)

ggplot(bike_share, aes(real_temp, cnt)) +
  geom_jitter(width = 0.25, alpha = 0.5, color = "grey30") +
  labs(y="Contagem de aluguer", 
       x="Temperatura (ºC)", 
       title = "Aluguer e temperatura por estação do ano") +
  facet_grid(season~.)+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5))



# Distribuição variável alugueres (Hist)

ggplot(bike_share, aes(cnt)) +
  geom_histogram(aes(y = stat(density)), col="white", fill= "#c9c9c9") +
  geom_density(col="grey50")+
  xlab("Distribuição do número de alugueres")+
  ylab("Densidade")+
  theme_light()


# Matrix de Correlações

matrix_correlação <-data.frame(round(cor(bike_share[,c(10:16)]),2))

matrix_correlação

# Pairplot

pairs(bike_share[,c(10:16)], col = c("grey80", "grey50"), 
              main='Pairs Pairplot')


# Procedimento de treino e teste do modelo (80/20)
# Definir seed para que os resultados sejam reproduzíveis
set.seed(12)

# Divisão em treino e teste

n <- nrow(bike_share)
trainIndex = sample(1:n, size = round(0.80*n), replace=FALSE)
train = bike_share[trainIndex ,]
test = bike_share[-trainIndex ,]

dim(train)
dim(test)

rm (n, trainIndex)

# Visualizar os dados de treino e teste

head(train)
head(test)

# Selecionar as features que serão incluídas no modelo.

train_selecao <- subset(train, select = c("season", "year",
                                          "month", "holiday", "weekday", 
                                          "workingday", "weathersit", 
                                          "temp", "atemp", "hum",
                                          "windspeed", "cnt"))

train_selecao_numericas <- train_selecao[,c("temp", "atemp", "hum", "windspeed", "cnt")]


test_selecao <- subset(test, select = c("season", "year",
                                          "month", "holiday", "weekday", 
                                          "workingday", "weathersit", 
                                          "temp", "atemp", "hum",
                                          "windspeed", "cnt"))



elevada_correlacao <- findCorrelation(cor(train_selecao_numericas), cutoff=0.7, names = T)
which(colnames(train_selecao) %in% elevada_correlacao)


# Removida variável "atemp" - elevada correlação com temp.

train_selecao <- train_selecao [, -which(colnames(train_selecao) %in% 
                                          elevada_correlacao)]

test_selecao <- test_selecao [, -which(colnames(test_selecao) %in% 
                                          elevada_correlacao)]

rm(elevada_correlacao, train_selecao_numericas, train, test)



# Testar correlações entre variaveis categoricas (Qui-Quadrado)


workingday_holiday <- table(train_selecao$workingday, train_selecao$holiday)
workingday_holiday

workingday_weekday <- table(train_selecao$workingday, train_selecao$weekday)
workingday_weekday

weekday_holiday <- table(train_selecao$weekday, train_selecao$holiday)
weekday_holiday

workingday_holiday_qui<-chisq.test(workingday_holiday)
workingday_holiday_qui

workingday_weekday_qui <- chisq.test(workingday_weekday)
workingday_weekday_qui

weekday_holiday_qui<-chisq.test(weekday_holiday)
weekday_holiday_qui




rm(workingday_holiday, workingday_weekday, weekday_holiday, workingday_holiday_qui, workingday_weekday_qui, weekday_holiday_qui)


# Removidas variáveis holiday e workingday

train <- train_selecao
test <- test_selecao

train_selecao <- train_selecao [, -c(4,6)]
test_selecao <- test_selecao [, -c(4,6)]


# Distância de Cook - remover observações problemáticas

modelo = lm(formula = cnt ~ ., data = train_selecao)

dist_cook <- cooks.distance(modelo)
plot(dist_cook, main = "Observações com potencial de distúrbio")
abline(h=3*mean(dist_cook, na.rm = T), col="red")

length(which(dist_cook>3*mean(dist_cook, na.rm = T))) # 41 observações

train_selecao <- train_selecao[-which(dist_cook>3*mean(dist_cook, na.rm = T)),]


rm(modelo,dist_cook)

# ---------- Modelo 1 ----------------------------------------------------------

m1 = lm(formula = cnt ~ ., data = train_selecao)

m1

summary(m1)
plot(m1)


# Avaliar normalidade dos resíduos.
ks.test(m1$residuals, "pnorm", 0, sd(m1$residuals))

# Avaliar independência dos erros.
Box.test(m1$residuals, type="Ljung")

# Avaliar variância constante dos erros (Homocedasticidade)
lmtest::bptest(m1) # Se H0 rejeitado -> transformar usando log(y) ou sqrt(y) ou BoxCox


# ---------- Modelo 2 ----------------------------------------------------------

m2 <- lm(formula = log(cnt) ~., data = train_selecao)

m2

summary(m2)
plot(m2)

ks.test(m2$residuals, "pnorm", 0, sd(m2$residuals))
Box.test(m2$residuals, type="Ljung")
lmtest::bptest(m2)



# ---------- Modelo 3 ----------------------------------------------------------

m3 <- lm(formula = sqrt(cnt) ~., data = train_selecao)

m3

summary(m3)
plot(m3)

ks.test(m3$residuals, "pnorm", 0, sd(m3$residuals))
Box.test(m3$residuals, type="Ljung")
lmtest::bptest(m3)



# ---------- Modelo 4 ----------------------------------------------------------

# Modelo Lasso

variaveis_independentes <- model.matrix(cnt~., bike_share[,c(3:13,16)])[,-1]
variavel_dependente <- bike_share$cnt

set.seed(6)

# Divisão em treino e teste

trainIndex = sample(2, nrow(bike_share), replace = T, prob = c(0.8,0.20))

variaveis_independentes_train <- subset(variaveis_independentes, trainIndex==1)
variaveis_independentes_test <- subset(variaveis_independentes, trainIndex==2)

variavel_dependente_train <- subset(variavel_dependente, trainIndex==1)
variavel_dependente_test <- subset(variavel_dependente, trainIndex==2)


# Cross validation 

cross_validation <- cv.glmnet(variaveis_independentes_train, variavel_dependente_train, alpha = 1)

# Modelo lasso

lasso_model = glmnet(variaveis_independentes_train, variavel_dependente_train, alpha=1)

plot(cross_validation)
plot(lasso_model,xvar = "lambda",label = T)

# Verificar o melhor lambda

cross_validation$lambda.min

plot(lasso_model,xvar = "lambda",label = T)
abline(v=log(cross_validation$lambda.min), col="red", lty="dashed")

# Coeficientes do modelo

predict(lasso_model, s = cross_validation$lambda.min, type = "coefficients")[1:30,]



# Previsões dos modelos.

y_pred_1 <- predict(m1, newdata = test_selecao)
y_pred_2 <- exp(predict(m2, newdata = test_selecao))
y_pred_3 <- (predict(m3, newdata = test_selecao)^2)
y_pred_4 <- predict(lasso_model, s=cross_validation$lambda.min, newx = variaveis_independentes_test)
y_pred_4 <- as.vector(y_pred_4)

# Análise dos erros dos modelos.

ME_1 <- mean(test_selecao$cnt-y_pred_1)
MSE_1 <- mean((test_selecao$cnt-y_pred_1)^2)
MAPE_1 <- mean(abs(test_selecao$cnt - y_pred_1)/test_selecao$cnt)


ME_2 <- mean(test_selecao$cnt-y_pred_2)
MSE_2 <- mean((test_selecao$cnt-y_pred_2)^2)
MAPE_2 <- mean(abs(test_selecao$cnt - y_pred_2)/test_selecao$cnt)


ME_3 <- mean(test_selecao$cnt-y_pred_3)
MSE_3 <- mean((test_selecao$cnt-y_pred_3)^2)
MAPE_3 <- mean(abs(test_selecao$cnt - y_pred_3)/test_selecao$cnt)


ME_4 <- mean(variavel_dependente_test-y_pred_4)
MSE_4 <- mean((variavel_dependente_test-y_pred_4)^2)
MAPE_4 <- mean(abs(variavel_dependente_test - y_pred_4)/variavel_dependente_test)



# Quadro comparativo do erro

data.frame("Métrica" = c("ME", "MSE", "MAPE"), 
           "lm"=round(c(ME_1,MSE_1,MAPE_1),2), 
           "lm(log)"=round(c(ME_2,MSE_2,MAPE_2),2),
           "lm(sqrt)"= round(c(ME_3,MSE_3,MAPE_3),2),
           "Lasso"=round(c(ME_4,MSE_4,MAPE_4),2))
