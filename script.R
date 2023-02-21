#### CONTROLE ESTATÍSTICO DO PROCESSO ####
install.packages("qcc")
library("qcc")
help(qcc)
##eedfffff
data(circuit)
attach(circuit)
qcc(x[trial], sizes=size[trial], type="c")
# remove out-of-control points (see help(circuit) for the reasons)
inc <- setdiff(which(trial), c(6,20))
qcc(x[inc], sizes=size[inc], type="c", labels=inc)
qcc(x[inc], sizes=size[inc], type="c", labels=inc, 
    newdata=x[!trial], newsizes=size[!trial], newlabels=which(!trial))
qcc(x[inc], sizes=size[inc], type="u", labels=inc, 
    newdata=x[!trial], newsizes=size[!trial], newlabels=which(!trial))
circuit
detach(circuit)

data(pcmanufact)
attach(pcmanufact)
qcc(x, sizes=size, type="u")
detach(pcmanufact)

data(dyedcloth)
attach(dyedcloth)
qcc(x, sizes=size, type="u")
# standardized control chart
q <- qcc(x, sizes=size, type="u", plot=FALSE)
z <- (q$statistics - q$center)/sqrt(q$center/q$size)
plot(z,  type="o", ylim=range(z,3,-3), pch=16)
abline(h=0, lty=2)
abline(h=c(-3,3), lty=2)
detach(dyedcloth)

##
##  Continuous one-at-time data 
##

# viscosity data (Montgomery, pag. 242)
x <- c(33.75, 33.05, 34, 33.81, 33.46, 34.02, 33.68, 33.27, 33.49, 33.20,
       33.62, 33.00, 33.54, 33.12, 33.84)
qcc(x, type="xbar.one")
qcc(x, type="xbar.one", std.dev = "SD")


#############################
#Por atributo
data("orangejuice")
orangejuice

data(orangejuice)
orangejuice <- transform(orangejuice, d = D/size)
describe(orangejuice, by = trial)
boxplot(d ~ trial, data = orangejuice)
plot(d ~ sample, data = orangejuice, type = "b", pch = ifelse(trial, 1, 19)) 




data(orangejuice)
attach(orangejuice)
qcc(D[trial], sizes=size[trial], type="p")

# remove out-of-control points (see help(orangejuice) for the reasons)
inc <- setdiff(which(trial), c(15,23))
q1 <- qcc(D[inc], sizes=size[inc], type="p")
qcc(D[inc], sizes=size[inc], type="p", newdata=D[!trial], newsizes=size[!trial]) 
detach(orangejuice)

data(orangejuice2)
attach(orangejuice2)
names(D) <- sample
qcc(D[trial], sizes=size[trial], type="p")
q2 <- qcc(D[trial], sizes=size[trial], type="p", newdata=D[!trial], newsizes=size[!trial])
detach(orangejuice2)


# put on the same graph the two orange juice samples
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(1,2), mar=c(5,5,3,0))
plot(q1, title="First samples", ylim=c(0,0.5), add.stats=FALSE, restore.par=FALSE)
par("mar"=c(5,0,3,3), yaxt="n")
plot(q2, title="Second samples", add.stats=FALSE, ylim=c(0,0.5))
par(oldpar)


#example
data(orangejuice2)
orangejuice2 <- transform(orangejuice2, d = D/size)
describe(orangejuice2, by = trial)
boxplot(d ~ trial, data = orangejuice2)
plot(d ~ sample, data = orangejuice2, type = "b", pch = ifelse(trial, 1, 19))






############################
data(pistonrings) #Dados sobre aneis de pistal - 200 observações, 3 colunas (diametro, amostra, julgamento), são 5 medições por amostra
pistonrings

#Qual o diâmetro médio dos anéis de pistão para cada amostra?
aggregate(diameter~sample, data=pistonrings, mean) #média de cada amostra

#formatar o conjunto de dados
attach(pistonrings)
diameter<-qcc.groups(diameter,sample) #formatar o dataset
aggregate(diameter~sample, data=pistonrings, mean)


# Interpretação Gráfico
qcc(diameter, type="xbar", std.dev= "UWAVE-SD") # XBAR sd
# podemos observar que, em geral, o gráfico de barrass X parece ser bom, mas depois começa a
# ficar fora de controle, parece que está começando a aumentar sistemicamente após o subgrupo
# (amostra) 28, deve ser investigado.
# Temos o Centro= 74.0036 diâmetro, Limite inferior = 73.99014 e superior = 74.01707 e o 
# nº de informações além do limite = 2(vermelho) e o numero de corridas violadoras = 1(amarelo)
#a partir do subgrupo 33 temos 7 valores sucessivos acima da média e isso será uma 
# execução violadora

qcc(diameter, type="xbar", std.dev= "UWAVE-R") # XBAR R
# podemos observar que, em geral, o gráfico de barrass X parece ser bom, mas depois começa a
# ficar fora de controle, parece que está começando a aumentar sistemicamente após o subgrupo
# (amostra) 28, deve ser investigado.
#Os limites de controle mudam um pouco em relação ao gráfico anterior, a média permanece
# a mesma, mas ainda existe alguns pontos fora de controle que precisam ser controlados
# Temos o Centro= 74.0036 diâmetro, Limite inferior = 73.99009 e superior = 74.01712 e o 
# nº de informações além do limite = 2(vermelho) e o numero de corridas violadoras = 1(amarelo)



qcc(diameter, type="S") # chart S, medições da variabilidade interna comparando a variabilidade interna
qcc(diameter, type="R") # chart R








