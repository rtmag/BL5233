data = read.table("GlobalAmphAnalysis.txt",header=T)

dataThreat0 = data[!is.na(data$IUCN06),]

dataThreat = dataThreat0[dataThreat0$IUCN06!=2,]

dataThreat$threatRisk = dataThreat$IUCN06
dataThreat$threatRisk[dataThreat$threatRisk==3] = 1

dataThreat$habit = factor(dataThreat$habit)
dataThreat$repros = factor(dataThreat$repros)
dataThreat$reproc = factor(dataThreat$reproc)
dataThreat$fert = factor(dataThreat$fert)
dataThreat$egg.site = factor(dataThreat$egg.site)
dataThreat$larv.site = factor(dataThreat$larv.site)
dataThreat$parcar = factor(dataThreat$parcar)
dataThreat$larvae = factor(dataThreat$larvae)
dataThreat$sizeLog = log(dataThreat$size)
dataThreat$rangeLog = log(dataThreat$range)

TV=(temp.max –temp.min)^0.5
PV=(prec.max –prec.min)^0.5

dataThreat$RG = scale(dataThreat$rangeLog);
dataThreat$BS = scale(dataThreat$sizeLog);
dataThreat$TM = scale(dataThreat$temp.mn);
dataThreat$TV = scale((dataThreat$temp.max - dataThreat$temp.min)^0.5);#calculates the temperature seasonality
dataThreat$PM = scale(dataThreat$prec.mn);
dataThreat$PV = scale((dataThreat$prec.max - dataThreat$prec.min)^0.5);#calculates the precipitation seasonality
dataThreat$HD = scale(dataThreat$hum.D);#human density
dataThreat$HL = scale(dataThreat$pc.r.loss);#proportional habitat loss

dataThreat$RC = dataThreat$reproc;
dataThreat$RM = dataThreat$repros;
dataThreat$PC = dataThreat$parcar; 
dataThreat$FE = dataThreat$fert;
dataThreat$SP = dataThreat$egg.site;
dataThreat$HB = dataThreat$habit;
dataThreat$LA = dataThreat$larvae;
dataThreat$LS = dataThreat$larv.site;

