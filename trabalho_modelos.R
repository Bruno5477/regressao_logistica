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
base$`Chest pain type` = factor(base$`Chest pain type`, levels = c(1,2,3,4), labels = c('angina', 'angina atípica','não anginosa','assintomática'))
base$`FBS over 120` = factor(base$`FBS over 120`, levels = c(0, 1), labels = c('Não', 'Sim'))
base$`EKG results` = factor(base$`EKG results`, levels = c(0, 1 ,2), labels = c('normal', 'anormal','ventricular'))
base$`Exercise angina` = factor(base$`Exercise angina`, levels = c(0, 1), labels = c('Não', 'Sim'))
base$`Slope of ST` = factor(base$`Slope of ST`, levels = c(1, 2 ,3), labels = c('ascendente', 'plana','declive'))
base$Thallium = factor(base$Thallium,levels = c(3,6,7),labels = c('normal', 'corrigido','reversível'))
base$`Heart Disease` = factor(base$`Heart Disease`,labels = c('Não', 'Sim'))

## Gráficos ----
# Variaveis categoricas

g1 = ggplot(base) +
  geom_bar(aes(x = `Exercise angina`, fill = `Heart Disease`), 
           position="fill", width = 0.25) +
  theme_bw() + coord_flip() +
  labs(y = 'Proporção', x = 'Angina induzida por exercício') +
  scale_fill_manual(name = 'Doença cardíaca', values = c( '#8B0000', '#00008B')) +
  ggtitle('Proporção de Doença cardíaca por angina')

g2 = ggplot(base) +
  geom_bar(aes(x = `FBS over 120`, fill = `Heart Disease`), 
           position="fill", width = 0.25) +
  theme_bw() + coord_flip() +
  labs(y = 'Proporção', x = 'Diabetes') +
  scale_fill_manual(name = 'Doença cardíaca', values = c( '#8B0000', '#00008B')) +
  ggtitle('Proporção de Doença cardíaca entre diabéticos e não diabéticos')

g3 = ggplot(base) +
  geom_bar(aes(x = `Slope of ST`, fill = `Heart Disease`), 
           position="fill", width = 0.25) +
  theme_bw() + coord_flip() +
  labs(y = 'Proporção', x = 'Inclinação do segmento ST') +
  scale_fill_manual(name = 'Doença cardíaca', values = c( '#8B0000', '#00008B')) +
  ggtitle('Proporção de Doença cardíaca por do segmento ST')

g4 = ggplot(base) +
  geom_bar(aes(x = `EKG results`, fill = `Heart Disease`), 
           position="fill", width = 0.25) +
  theme_bw() + coord_flip() +
  labs(y = 'Proporção', x = 'Resultado do eletrocardiográfico') +
  scale_fill_manual(name = 'Doença cardíaca', values = c( '#8B0000', '#00008B')) +
  ggtitle('Proporção de Doença cardíaca por eletrocardiográfico')

g5 = ggplot(base) +
  geom_bar(aes(x = Sex, fill = `Heart Disease`), 
           position="fill", width = 0.25) +
  theme_bw() + coord_flip() +
  labs(y = 'Proporção', x = 'Sexo') +
  scale_fill_manual(name = 'Doença cardíaca', values = c( '#8B0000', '#00008B')) +
  ggtitle('Proporção de Doença cardíaca por sexo')

g6 = ggplot(base) +
  geom_bar(aes(x = Thallium , fill = `Heart Disease`), 
           position="fill", width = 0.25) +
  theme_bw() + coord_flip() +
  labs(y = 'Proporção', x = 'Talassemia') +
  scale_fill_manual(name = 'Doença cardíaca', values = c( '#8B0000', '#00008B')) +
  ggtitle('Proporção de Doença cardíaca por tipo de Talassemia')

g7 = ggplot(base) +
  geom_bar(aes(x = `Chest pain type`, fill = `Heart Disease`), 
           position="fill", width = 0.25) +
  theme_bw() + coord_flip() +
  labs(y = 'Proporção', x = 'Tipo de dor no peito') +
  scale_fill_manual(name = 'Doença cardíaca', values = c( '#8B0000', '#00008B')) +
  ggtitle('Proporção de Doença cardíaca por tipo de dor no peito')

G1 = gridExtra::grid.arrange(g1,g2,g3)

#ggsave(filename = "análise_1.png", plot = G1, device = png(width = 850, height = 750))

# Variaveis quantitativas
g8 = ggplot(base) +
  geom_boxplot(aes(x = Age, y = `Heart Disease`,fill = `Heart Disease`)) + 
  scale_fill_manual(values = c( '#8B0000', '#00008B')) +
  guides(fill="none")+theme_bw() +
  labs(x = 'Idade', y="Doença cardíaca") + 
  ggtitle('Idade dos indivíduos por Doença cardíaca')

g9 =ggplot(base) +
  geom_boxplot(aes(x = Cholesterol, y = `Heart Disease`,fill = `Heart Disease`)) + 
  scale_fill_manual(values = c( '#8B0000', '#00008B')) +
  guides(fill="none")+theme_bw() +
  labs(x = 'Colesterol (mg/dL)', y="Doença cardíaca") + 
  ggtitle('Colesterol dos indivíduos por Doença cardíaca')

g10 = ggplot(base) +
  geom_boxplot(aes(x =`ST depression`, y = `Heart Disease`,fill = `Heart Disease`)) + 
  scale_fill_manual(values = c( '#8B0000', '#00008B')) +
  guides(fill="none")+theme_bw() +
  labs(x = 'Depressão do segmento ST', y="Doença cardíaca") + 
  ggtitle('Depressão do segmento ST dos indivíduos por Doença cardíaca')

g11 =ggplot(base) +
  geom_boxplot(aes(x = `Max HR`, y = `Heart Disease`,fill = `Heart Disease`)) + 
  scale_fill_manual(values = c( '#8B0000', '#00008B')) +
  guides(fill="none")+theme_bw() +
  labs(x = 'Frequência cardíaca máxima alcançada', y="Doença cardíaca") + 
  ggtitle('Frequência cardíaca máxima dos indivíduos por Doença cardíaca')

g12 = ggplot(base) +
  geom_boxplot(aes(x = `Number of vessels fluro`, y = `Heart Disease`,fill = `Heart Disease`)) + 
  scale_fill_manual(values = c( '#8B0000', '#00008B')) +
  guides(fill="none")+theme_bw() +
  labs(x = 'Número de vasos principais coloridos por fluoroscopia', y="Doença cardíaca") + 
  ggtitle('Número de vasos principais dos indivíduos por Doença cardíaca')


g13 = ggplot(base) +
  geom_boxplot(aes(x = BP, y = `Heart Disease`,fill = `Heart Disease`)) + 
  scale_fill_manual(values = c( '#8B0000', '#00008B')) +
  guides(fill="none")+theme_bw() +
  labs(x = 'Pressão arterial em repouso (mmHg)', y="Doença cardíaca") + 
  ggtitle('Pressão arterial dos indivíduos por Doença cardíaca')

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

# Organizando os dados (essa função já coloca variáveis categóricas como dummies)
x <- model.matrix(`Heart Disease`~., treino)[,-1]

# Transformando as categorias de y em classes numéricas
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

# Modelo só com as variáveis significativas

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

################## Tabela com o diagnóstico dos modelos

diag_mod = data.frame(
  Modelo = c('Modelo completo', 'Modelo reduzido', 'Modelo com LASSO'),
  AIC = c(mod.completo$aic, mod2$aic, AIC.lasso$AIC),
  BIC = c(BIC_1, BIC_2, BIC.lasso),
  Deviance = c(D_1, D_2, D.lasso),
  Pearson = c(X2_1, X2_2, X2.lasso),
  Acurácia = c(cm_1$overall[1], cm_2$overall[1], cm_3$overall[1]) 
)


razão = round(exp(cbind(OR = coef(mod2), confint(mod2, level=0.95))),3)
coeficientes =round(coef(mod2),3)
################# Tabela com as estimativas pontuais e razão de chance (do modelo final)

est = data.frame(Variável= c('Intercepto','Angina induzida por exercício' , 'Dor no peito assimtomática', 'Número de vasos principais',
                             'Sexo Masculino','Talassemia reversível'),
                 Estimativa =c(-4.486,1.758, 1.865, 1.791, 1.211, 2.487 ), 
                 pvalor = c(0.00005,0.001,0.031,0.00005,0.037,0.00005 ),
                 chance = c(NA,5.802,6.456,5.995,3.357,12.021),
                 IC1 = c(NA,2.025,1.277,3.142,1.106,4.283),
                 IC2 = c(NA,17.990,40.324,12.905,10.982,37.612))


est2 = est[-1,]
est2$Variável = factor(est2$Variável)

ggplot(est2) + 
  geom_linerange(aes(x = Variável, ymin = IC1, ymax = IC2), size = 2.5, color = '#5D86EC') +
  coord_flip() + theme_light() + ylim(0, 41) +
  geom_hline(yintercept = 1, linetype = 'dashed', size = 1)


save(est, diag_mod, file="modelos.Rdata")

base |> 
ggplot(mapping = aes(x = `Heart Disease`)) +
  geom_bar(fill = "Red")
exp(1.758)
