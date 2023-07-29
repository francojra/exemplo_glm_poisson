library(vegan)

# Carregar dados

data(dune) ## 30 espécies de plantas em 20 locais
data(dune.env) ## Características ambientais 

# Visualizar dados

View(dune)
View(dune.env)

# Ver estrutura dos dados

head(dune)
head(dune.env)
str(dune)
str(dune.env)

riq <- specnumber(dune) # Para contar as espécies de cada sítio
riq
View(riq)

mod1 <- glm(riq ~ A1 + Moisture + Manure, family = poisson(link = "log"), 
            data = dune.env)
summary(mod1)

## O residual deviance está maior que os graus de liberdade
## Isso pode indicar que os dados estão sobredispersos.

6.8246 / 11 # Dividir residual deviance pelos graus de liberdade
## Resultado: 0.6204182

## O resultado da divisão foi menor que 1, então não ocorre 
## sobredispersão, parece haver underdispersion

# Diagnose básica dos resíduos do modelo

plot(mod1) 

# Diagnose avançada

library(DHARMa)

simulationOutput <- simulateResiduals(fittedModel = mod1, plot = TRUE)

## O plot claramente indica que há problema com overdispersion, 
## mas não em termos de desvios de normalidade (KS test) 

testDispersion(mod1)

## Aqui temos um gráfico e o resultado novamente do teste de 
## overdispersion (que já aparecia no gráfico anterior) mostrando que de 
## fato há overdispersion: perceba que o valor de P é significativo. 
## O gráfico nos mostra em cinza a distribuição dos resíduos aleatorizados
## e a linha vermelha o valor observado da estatística. Se a linha está 
## bem à direita da distribuição, isso indica overdispersion, 
## se estiver à esquerda seria o caso de underdispersion.

# Inflação de zeros - DHARMa

testZeroInflation(mod1)

## Tanto a função do DHARMa quanto do performance conseguiram 
## detectar que o modelo tem problemas com overdispersion, mas 
## isso não é causado pelo excesso de zeros.

# Calculando o R2 do modelo



# Teste formal da ANOVA

anova(mod1, test = "Chisq")
