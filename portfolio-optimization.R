#limpa memoria
rm(list=ls())

#Aponta para o diretorio com as quotes
diretorioquote = "D:/portfolio-optimization/Quotes";
setwd(diretorioquote);

#Junta em um único arquivo as quotações de todos os ativos, o critério de agrupamento é a data.
f = NULL
files = 
  c("ABEV3.SA.csv","B3SA3.SA.csv","BBAS3.SA.csv","BBDC3.SA.csv","BBDC4.SA.csv","BBSE3.SA.csv","BRAP4.SA.csv","BRFS3.SA.csv","BRKM5.SA.csv","BRML3.SA.csv","BTOW3.SA.csv","CCRO3.SA.csv","CIEL3.SA.csv","CMIG4.SA.csv","CSAN3.SA.csv","CSNA3.SA.csv","ELET3.SA.csv","ELET6.SA.csv","EMBR3.SA.csv","ESTC3.SA.csv","GGBR4.SA.csv","GOAU4.SA.csv","GOLL4.SA.csv","ITSA4.SA.csv","ITUB4.SA.csv","JBSS3.SA.csv","KROT3.SA.csv","LAME4.SA.csv","LREN3.SA.csv","MGLU3.SA.csv","MULT3.SA.csv","NATU3.SA.csv","PCAR4.SA.csv","PETR3.SA.csv","PETR4.SA.csv","RADL3.SA.csv","RAIL3.SA.csv","RENT3.SA.csv","SBSP3.SA.csv","TIMP3.SA.csv","UGPA3.SA.csv","USIM5.SA.csv","VALE3.SA.csv","VIVT4.SA.csv","VVAR3.SA.csv","WEGE3.SA.csv")

for (i in 1:length(files)) {
  csv = read.csv(files[i])
  csv = csv[,c("Date","Close")]
  names(csv) = c("Date",files[i])
  if (i == 1) f = csv
  else f = merge(f,csv)
}

#Calcular os retornos

for (i in 2:ncol(f)) {
  # Serie de tempo para os preços
  prices = f[,i] 
  
  # cria o Pt-1
  prices_prev = c(NA,prices[1:(length(prices)-1)]) 
  
  # Serie de tempos para retorno
  returns = (prices-prices_prev)/prices_prev 
  
  # substitui a inesima coluna com os retornos
  f[,i] = returns 
}
# Remove a primeira linha e a coluna de data
asset_returns = f[2:nrow(f),2:ncol(f)]

#Define o retorno do portifolio
portfolio_returns = function(x) {
  port.returns = 0
  
  # Multiplica o inesimo ativo pelo inesimo peso do ativo
  for (i in 1:length(x)) {
    port.returns = port.returns + asset_returns[,i] * x[i]
  }
  
  return (port.returns)
}

#Funçãoo objetiva e a penalidade (sharp ratio x restrição)

sharpe = function(x) {
  port.returns = portfolio_returns(x)
  
  return (mean(port.returns)/sqrt(var(port.returns)))
  
}

#Define a restrição Long

constraint = function(x) {
  boundary_constr = (sum(x)-1)**2   # restrição do peso do portifolio =1
  
  for (i in 1:length(x)) {
    boundary_constr = boundary_constr + 
      max(c(0,x[i]-1))**2 +  # "x <= 1" restrição
      max(c(0,-x[i]))**2     # "x >= 0" restrição
  }
  
  return (boundary_constr)
}

#Define o problema de otimização

obj = function(x) {
  # Para maximizar o Sharpe ratio, nos multiplicamos ele por -1 como um problema de otimização
  
  return (-sharpe(x)+100*constraint(x))
}

library("GA")
ga_res = ga(
  # define os pesoas no portifolio com valores reais
  type="real-valued", 
  
  # GA faz uma maximização, por isso multiplicamos a função objeto por -1
  function(x){-obj(x)}, 
  
  # x_i >= 0
  lower = rep(0,ncol(asset_returns)), 
  
  # x_i <= 1
  upper = rep(1,ncol(asset_returns)), 
  
  # Número maximo de intereções 
  maxiter = 500000, 
  
  # Se o fit maximo se mativer por 6000 intereções consecutivas para
  run=6000, 
  
  # Usar todos os nucleos do processador
  parallel=TRUE,
  
  # Olhar os resultados na tela
  monitor=TRUE,
  
  # Seed para replicabilidade
  seed=1
)
sol = as.vector(summary(ga_res)$solution)
  
cbind(names(asset_returns),sol)

optimal_returns = portfolio_returns(sol)
plot(cumsum(optimal_returns),type="l",lwd=5)

lines(cumsum(asset_returns[,1]),col="blue")
lines(cumsum(asset_returns[,2]),col="blue")
lines(cumsum(asset_returns[,3]),col="blue")
lines(cumprod(asset_returns[,4]),col="blue")
lines(cumsum(asset_returns[,5]),col="blue")
lines(cumsum(asset_returns[,6]),col="blue")
lines(cumsum(asset_returns[,7]),col="blue")
lines(cumsum(asset_returns[,8]),col="blue")
lines(cumsum(asset_returns[,9]),col="blue")
lines(cumsum(asset_returns[,10]),col="blue")
lines(cumsum(asset_returns[,11]),col="blue")
lines(cumsum(asset_returns[,12]),col="blue")
lines(cumsum(asset_returns[,13]),col="blue")
lines(cumsum(asset_returns[,14]),col="blue")
lines(cumsum(asset_returns[,15]),col="blue")
lines(cumsum(asset_returns[,16]),col="blue")
lines(cumsum(asset_returns[,17]),col="blue")
lines(cumsum(asset_returns[,18]),col="blue")
lines(cumsum(asset_returns[,19]),col="blue")
lines(cumsum(asset_returns[,20]),col="blue")
lines(cumsum(asset_returns[,21]),col="blue")
lines(cumsum(asset_returns[,22]),col="blue")
lines(cumsum(asset_returns[,23]),col="blue")
lines(cumsum(asset_returns[,24]),col="blue")
lines(cumsum(asset_returns[,25]),col="blue")
lines(cumsum(asset_returns[,26]),col="blue")
lines(cumsum(asset_returns[,27]),col="blue")
lines(cumsum(asset_returns[,28]),col="blue")
lines(cumsum(asset_returns[,29]),col="blue")
lines(cumsum(asset_returns[,30]),col="blue")
lines(cumsum(asset_returns[,31]),col="blue")
lines(cumsum(asset_returns[,32]),col="blue")
lines(cumsum(asset_returns[,33]),col="blue")
lines(cumsum(asset_returns[,34]),col="blue")

legend(-5.5,2.5,legend=c("Weighted portfolio",names(asset_returns)),
       col = c("black","blue","red","green","violet","peru","pink"),lty=1)
