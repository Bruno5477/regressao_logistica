library(mlbench)
library(tidyverse)
library(caret)
library(glmnet)
library(sAIC)
library(ggcorrplot)
library(GGally)

base = read_csv('Heart_Disease_Prediction.csv')

# Criando os fatores
base$Sex = factor(base$Sex, levels = c(0, 1), labels = c('Feminino', 'Masculino'))
base$`Chest pain type` = factor(base$`Chest pain type`, levels = c(1,2,3,4), labels = c('angina', 'angina at?pica','n?o anginosa','assintom?tica'))
base$`FBS over 120` = factor(base$`FBS over 120`, levels = c(0, 1), labels = c('N?o', 'Sim'))
base$`EKG results` = factor(base$`EKG results`, levels = c(0, 1 ,2), labels = c('normal', 'anormal','ventricular'))
base$`Exercise angina` = factor(base$`Exercise angina`, levels = c(0, 1), labels = c('N?o', 'Sim'))
base$`Slope of ST` = factor(base$`Slope of ST`, levels = c(1, 2 ,3), labels = c('ascendente', 'plana','declive'))
base$Thallium = factor(base$Thallium,levels = c(3,6,7),labels = c('normal', 'corrigido','revers?vel'))
base$`Heart Disease` = factor(base$`Heart Disease`,labels = c('N?o', 'Sim'))

## Gr?ficos ----
# Variaveis categoricas

g1 = ggplot(base) +
  geom_bar(aes(x = `Exercise angina`, fill = `Heart Disease`), 
           position="fill", width = 0.25) +
  theme_bw() + coord_flip() +
  labs(y = 'Propor??o', x = 'Angina induzida por exerc?cio') +
  scale_fill_manual(name = 'Doen?a card?aca', values = c( '#8B0000', '#00008B')) +
  ggtitle('Propor??o de Doen?a card?aca por angina')

g2 = ggplot(base) +
  geom_bar(aes(x = `FBS over 120`, fill = `Heart Disease`), 
           position="fill", width = 0.25) +
  theme_bw() + coord_flip() +
  labs(y = 'Propor??o', x = 'Diabetes') +
  scale_fill_manual(name = 'Doen?a card?aca', values = c( '#8B0000', '#00008B')) +
  ggtitle('Propor??o de Doen?a card?aca entre diab?ticos e n?o diab?ticos')

g3 = ggplot(base) +
  geom_bar(aes(x = `Slope of ST`, fill = `Heart Disease`), 
           position="fill", width = 0.25) +
  theme_bw() + coord_flip() +
  labs(y = 'Propor??o', x = 'Inclina??o do segmento ST') +
  scale_fill_manual(name = 'Doen?a card?aca', values = c( '#8B0000', '#00008B')) +
  ggtitle('Propor??o de Doen?a card?aca por do segmento ST')

g4 = ggplot(base) +
  geom_bar(aes(x = `EKG results`, fill = `Heart Disease`), 
           position="fill", width = 0.25) +
  theme_bw() + coord_flip() +
  labs(y = 'Propor??o', x = 'Resultado do eletrocardiogr?fico') +
  scale_fill_manual(name = 'Doen?a card?aca', values = c( '#8B0000', '#00008B')) +
  ggtitle('Propor??o de Doen?a card?aca por eletrocardiogr?fico')

g5 = ggplot(base) +
  geom_bar(aes(x = Sex, fill = `Heart Disease`), 
           position="fill", width = 0.25) +
  theme_bw() + coord_flip() +
  labs(y = 'Propor??o', x = 'Sexo') +
  scale_fill_manual(name = 'Doen?a card?aca', values = c( '#8B0000', '#00008B')) +
  ggtitle('Propor??o de Doen?a card?aca por sexo')

g6 = ggplot(base) +
  geom_bar(aes(x = Thallium , fill = `Heart Disease`), 
           position="fill", width = 0.25) +
  theme_bw() + coord_flip() +
  labs(y = 'Propor??o', x = 'Talassemia') +
  scale_fill_manual(name = 'Doen?a card?aca', values = c( '#8B0000', '#00008B')) +
  ggtitle('Propor??o de Doen?a card?aca por tipo de Talassemia')

g7 = ggplot(base) +
  geom_bar(aes(x = `Chest pain type`, fill = `Heart Disease`), 
           position="fill", width = 0.25) +
  theme_bw() + coord_flip() +
  labs(y = 'Propor??o', x = 'Tipo de dor no peito') +
  scale_fill_manual(name = 'Doen?a card?aca', values = c( '#8B0000', '#00008B')) +
  ggtitle('Propor??o de Doen?a card?aca por tipo de dor no peito')

G1 = gridExtra::grid.arrange(g1,g2,g3)

#ggsave(filename = "an?lise_1.png", plot = G1, device = png(width = 850, height = 750))

# Variaveis quantitativas
g8 = ggplot(base) +
  geom_boxplot(aes(x = Age, y = `Heart Disease`,fill = `Heart Disease`)) + 
  scale_fill_manual(values = c( '#8B0000', '#00008B')) +
  guides(fill="none")+theme_bw() +
  labs(x = 'Idade', y="Doen?a card?aca") + 
  ggtitle('Idade dos indiv?duos por Doen?a card?aca')

g9 =ggplot(base) +
  geom_boxplot(aes(x = Cholesterol, y = `Heart Disease`,fill = `Heart Disease`)) + 
  scale_fill_manual(values = c( '#8B0000', '#00008B')) +
  guides(fill="none")+theme_bw() +
  labs(x = 'Colesterol (mg/dL)', y="Doen?a card?aca") + 
  ggtitle('Colesterol dos indiv?duos por Doen?a card?aca')

g10 = ggplot(base) +
  geom_boxplot(aes(x =`ST depression`, y = `Heart Disease`,fill = `Heart Disease`)) + 
  scale_fill_manual(values = c( '#8B0000', '#00008B')) +
  guides(fill="none")+theme_bw() +
  labs(x = 'Depress?o do segmento ST', y="Doen?a card?aca") + 
  ggtitle('Depress?o do segmento ST dos indiv?duos por Doen?a card?aca')

g11 =ggplot(base) +
  geom_boxplot(aes(x = `Max HR`, y = `Heart Disease`,fill = `Heart Disease`)) + 
  scale_fill_manual(values = c( '#8B0000', '#00008B')) +
  guides(fill="none")+theme_bw() +
  labs(x = 'Frequ?ncia card?aca m?xima alcan?ada', y="Doen?a card?aca") + 
  ggtitle('Frequ?ncia card?aca m?xima dos indiv?duos por Doen?a card?aca')

g12 = ggplot(base) +
  geom_boxplot(aes(x = `Number of vessels fluro`, y = `Heart Disease`,fill = `Heart Disease`)) + 
  scale_fill_manual(values = c( '#8B0000', '#00008B')) +
  guides(fill="none")+theme_bw() +
  labs(x = 'N?mero de vasos principais coloridos por fluoroscopia', y="Doen?a card?aca") + 
  ggtitle('N?mero de vasos principais dos indiv?duos por Doen?a card?aca')


g13 = ggplot(base) +
  geom_boxplot(aes(x = BP, y = `Heart Disease`,fill = `Heart Disease`)) + 
  scale_fill_manual(values = c( '#8B0000', '#00008B')) +
  guides(fill="none")+theme_bw() +
  labs(x = 'Press?o arterial em repouso (mmHg)', y="Doen?a card?aca") + 
  ggtitle('Press?o arterial dos indiv?duos por Doen?a card?aca')

G2 = gridExtra::grid.arrange(g8,g9,g10,g11,g12,g13)

#ggsave(filename = "quantitativas.png", plot = G2, device = png(width = 860, height = 750))

## Dividindo aleatoriamente os dados em conjunto de treinamento (75% para 
## construir o modelo) e conjunto de teste (25% para avaliar a capacidade 
## preditiva do modelo).

set.seed(100)
amostras <- base$`Heart Disease` %>% 
  createDataPartition(p = 0.75, list = FALSE)

treino <- base[amostras, ]
teste <- base[-amostras, ]

# Organizando os dados (essa fun??o j? coloca vari?veis categ?ricas como dummies)
x <- model.matrix(`Heart Disease`~., treino)[,-1]

# Transformando as categorias de y em classes num?ricas
y <- ifelse(treino$`Heart Disease` == "Sim", 1, 0)

# Encontrando o melhor lambda via cross-validation

set.seed(20211)
(cv.lasso <- cv.glmnet(x, y, family = "binomial", type.measure = "class"))
mod.lasso <- glmnet(x , y , family = "binomial", lambda = cv.lasso$lambda.min)
coef(mod.lasso)       

(D.lasso <- deviance(mod.lasso))
prob.lasso <- predict(mod.lasso, type = "response", newx = x)
ri.lasso <- (y - prob.lasso)/sqrt(prob.lasso*(1-prob.lasso)); (X2.lasso <- sum(ri.lasso^2))
(AIC.lasso <- sAIC::sAIC(x , y , beta=coef(mod.lasso), family="binomial"))

LL.lasso <- 14 - AIC.lasso[[1]]/2
BIC.lasso <- -2*LL.lasso + log(203)*14

x_teste <- model.matrix(`Heart Disease` ~ .,data = teste)
y_teste <- ifelse(teste$`Heart Disease` == "Sim", 1, 0)
pred.lasso <- predict(mod.lasso, type = "response", newx = x_teste[,-1])
class.lasso <- ifelse(pred.lasso > 0.5, 1, 0)
cm_3 = caret::confusionMatrix(data = factor(class.lasso), reference = factor(y_teste))

cm_3$overall[1]

# modelo completo

mod.completo <- glm( `Heart Disease`~ .,data = treino, family = binomial(link = "logit"))
summary(mod.completo)

AIC.1 = mod.completo$aic
LL <- 14 - mod.completo$aic/2
BIC_1 <- -2*LL + log(203)*14

di <- resid(mod.completo, type = "deviance"); (D_1 <- sum(di^2))
ri <- resid(mod.completo, type = "pearson"); (X2_1 <- sum(ri^2))

pred <- predict(mod.completo, type = "response", newdata = teste)
class <- ifelse(pred > 0.5, 1, 0)
cm_1 = caret::confusionMatrix(data = factor(class), reference = factor(y_teste))

cm_1$overall[1]

# Modelo s? com as vari?veis significativas

mod2 = glm( `Heart Disease`~ Sex + `Chest pain type` +`Number of vessels fluro`  + `Exercise angina`  
            + Thallium, data = treino, family = binomial(link = 'logit'))

summary(mod2)
AIC.2 = mod2$aic
LL <- 9 - mod2$aic/2
BIC_2 <- -2*LL + log(203)*9

di <- resid(mod2, type = "deviance"); (D_2 <- sum(di^2))
ri <- resid(mod2, type = "pearson"); (X2_2 <- sum(ri^2))

pred <- predict(mod2, type = "response", newdata = teste)
class <- ifelse(pred > 0.5, 1, 0)
cm_2 = caret::confusionMatrix(data = factor(class), reference = factor(y_teste))

cm_2$overall[1]

################## Tabela com o diagn?stico dos modelos

diag_mod = data.frame(
  Modelo = c('Modelo completo', 'Modelo reduzido', 'Modelo com LASSO'),
  AIC = c(mod.completo$aic, mod2$aic, AIC.lasso$AIC),
  BIC = c(BIC_1, BIC_2, BIC.lasso),
  Deviance = c(D_1, D_2, D.lasso),
  Pearson = c(X2_1, X2_2, X2.lasso),
  Acur?cia = c(cm_1$overall[1], cm_2$overall[1], cm_3$overall[1]) 
)


raz?o = round(exp(cbind(OR = coef(mod2), confint(mod2, level=0.95))),3)
coeficientes =round(coef(mod2),3)
################# Tabela com as estimativas pontuais e raz?o de chance (do modelo final)

est = data.frame(Vari?vel= c('Intercepto','Angina induzida por exerc?cio' , 'Dor no peito assimtom?tica', 'N?mero de vasos principais',
                             'Sexo Masculino','Talassemia revers?vel'),
                 Estimativa =c(-4.486,1.758, 1.865, 1.791, 1.211, 2.487 ), 
                 pvalor = c(0.00005,0.001,0.031,0.00005,0.037,0.00005 ),
                 chance = c(NA,5.802,6.456,5.995,3.357,12.021),
                 IC1 = c(NA,2.025,1.277,3.142,1.106,4.283),
                 IC2 = c(NA,17.990,40.324,12.905,10.982,37.612))


est2 = est[-1,]
est2$Vari?vel = factor(est2$Vari?vel)

ggplot(est2) + 
  geom_linerange(aes(x = Vari?vel, ymin = IC1, ymax = IC2), size = 2.5, color = '#5D86EC') +
  coord_flip() + theme_light() + ylim(0, 41) +
  geom_hline(yintercept = 1, linetype = 'dashed', size = 1)


save(est, diag_mod, file="modelos.Rdata")

base |> 
ggplot(mapping = aes(x = `Heart Disease`)) +
  geom_bar(fill = "Red")
exp(1.758)
