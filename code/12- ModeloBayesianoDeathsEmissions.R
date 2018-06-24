## -------------------------------------------------------------------------
## Modelo Naive Bayes
## -------------------------------------------------------------------------
## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

if (!require("e1071")){
  install.packages("e1071")
  library("e1071")
}

if (!require("ROCR")){
  install.packages("ROCR")
  library("ROCR")
}

if (!require("glmnet")){
  install.packages("glmnet") 
  library("glmnet")
}

if (!require("caTools")){
  install.packages("caTools") 
  library(caTools)
}


## -------------------------------------------------------------------------

##### 2 Bloque de carga de datos #####

files <- (Sys.glob("../data/csv/DeathsEmissions_final_*.csv"))

if (exists("DeathsEmissions")){rm(DeathsEmissions)}

for (file in files){
  print(paste("Procesando fichero: ",file,sep=";"))
  data=read.csv2(file,stringsAsFactors = FALSE, fileEncoding = "windows-1252", sep=";")
  if (exists("DeathsEmissions")){
    data = data[data$AnioDefuncion == 2015,]
    DeathsEmissions=rbind(DeathsEmissions,data)
  }else{
    DeathsEmissions=data
  }
}


rm(data)

## -------------------------------------------------------------------------

##### 3. Bloque de revisión basica del dataset #####

str(DeathsEmissions)
head(DeathsEmissions)
summary(DeathsEmissions)

## -------------------------------------------------------------------------

##### 4. Bloque de formateo de variables #####
DfDeaEmiReduc <- subset( DeathsEmissions, select = -c(X, 
                                                        ProvinciaInscri:AnioNacimiento,
                                                        MesDefuncion:PaisResidencia,
                                                        MesesCumplidos:TamanioPaisNdad,
                                                        CausaMuertebas2:CausaMortaInfan,
                                                        PoblaciÃ.n:Id,Ocupacion,
                                                        LatitudE,LongitudE))
str(DfDeaEmiReduc)

rm(DeathsEmissions)

DfDeaEmiReduc=DfDeaEmiReduc[!is.na(DfDeaEmiReduc$AnioCumplidos),]

DfDeaEmiReduc[is.na(DfDeaEmiReduc)] <- 0
DfDeaEmiReduc$ProvinciaReside = as.factor(DfDeaEmiReduc$ProvinciaReside)
DfDeaEmiReduc$MunicipioReside=as.factor(DfDeaEmiReduc$MunicipioReside)
DfDeaEmiReduc$Sexo=as.factor(DfDeaEmiReduc$Sexo)
DfDeaEmiReduc$EstadoCivil=as.factor(DfDeaEmiReduc$EstadoCivil)
DfDeaEmiReduc$TotalAniosId=as.factor(DfDeaEmiReduc$TotalAniosId)
DfDeaEmiReduc$CausaMuertebas1=as.factor(DfDeaEmiReduc$CausaMuertebas1)
DfDeaEmiReduc$AnioCumplidos=as.factor(DfDeaEmiReduc$AnioCumplidos)
DfDeaEmiReduc$Contaminante=as.factor(DfDeaEmiReduc$Contaminante)
DfDeaEmiReduc$NivelEstudios=as.factor(DfDeaEmiReduc$NivelEstudios)
DfDeaEmiReduc$CantidadTotalkg=as.factor(DfDeaEmiReduc$CantidadTotalkg)

DfDeaEmiReduc = DfDeaEmiReduc[DfDeaEmiReduc$AnioCumplidos != 0,]

str(DfDeaEmiReduc)
head(DfDeaEmiReduc)
summary(DfDeaEmiReduc)


##### 5. Bloque de creación de variables auxiliares #####
table(DfDeaEmiReduc$CausaMuertebas1)
#DfDeaEmiReduc$CausaMuertebasB = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'B')
#DfDeaEmiReduc$CausaMuertebasI = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'I')
#DfDeaEmiReduc$CausaMuertebasD = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'D')
#DfDeaEmiReduc$CausaMuertebasE = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'E')
#DfDeaEmiReduc$CausaMuertebasF = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'F')
#DfDeaEmiReduc$CausaMuertebasG = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'G')
#DfDeaEmiReduc$CausaMuertebasH = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'H')
DfDeaEmiReduc$CausaMuertebasC = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'C')
DfDeaEmiReduc$CausaMuertebasC = as.factor(DfDeaEmiReduc$CausaMuertebasC)
#DfDeaEmiReduc$CausaMuertebasJ = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'J')
#DfDeaEmiReduc$CausaMuertebasK = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'K')
#DfDeaEmiReduc$CausaMuertebasL = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'L')
#DfDeaEmiReduc$CausaMuertebasM = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'M')
#DfDeaEmiReduc$CausaMuertebasN = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'N')
#DfDeaEmiReduc$CausaMuertebasO = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'O')
#DfDeaEmiReduc$CausaMuertebasP = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'P')
#DfDeaEmiReduc$CausaMuertebasQ = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'Q')
#DfDeaEmiReduc$CausaMuertebasR = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'R')
#DfDeaEmiReduc$CausaMuertebasV = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'V')
#DfDeaEmiReduc$CausaMuertebasW = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'W')
#DfDeaEmiReduc$CausaMuertebasX = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'X')
#DfDeaEmiReduc$CausaMuertebasY = as.numeric(DfDeaEmiReduc$CausaMuertebas1 == 'Y')


head(DfDeaEmiReduc)
summary(DfDeaEmiReduc)

str(DfDeaEmiReduc)

## -------------------------------------------------------------------------

##### 6. Bloque de creación de conjuntos de entrenamiento y test #####

set.seed(1234) 
SAMPLE = sample.split(DfDeaEmiReduc$CausaMuertebasC, SplitRatio = .75)
DeaEmiTrain = subset(DfDeaEmiReduc, SAMPLE == TRUE)
DeaEmiTest = subset(DfDeaEmiReduc, SAMPLE == FALSE)

## -------------------------------------------------------------------------

##### 7. Bloque de modelo Naive Bayes #####

modeloBayesTrain=naiveBayes(CausaMuertebasC~
                              ProvinciaReside+
                              MunicipioReside+
                              Sexo+
                              EstadoCivil+
                              AnioCumplidos+
                              NivelEstudios+
                              Contaminante+
                              TotalAniosId+
                              CantidadTotalkg, 
                            data=DeaEmiTrain,family=binomial(link="Bayes"))

modeloBayesTrain

str(modeloBayesTrain)
## -------------------------------------------------------------------------

##### 8. Bloque de evaluación de los modelos #####

DeaEmiTrain$predDefecto <- predict(modeloBayesTrain,DeaEmiTrain)
DeaEmiTrain$prediccion <- predict(modeloBayesTrain,DeaEmiTrain,type="raw")[,2]
Predauxiliar= prediction(DeaEmiTrain$prediccion, 
                         DeaEmiTrain$CausaMuertebasC, 
                         label.ordering = NULL)
auc.tmp = performance(Predauxiliar, "auc");
aucModeloBayestrain = as.numeric(auc.tmp@y.values)
aucModeloBayestrain

CurvaRocModeloBayesTrain <- performance(Predauxiliar,"tpr","fpr")
plot(CurvaRocModeloBayesTrain,colorize=TRUE)
abline(a=0,b=1)

## Indice de GINI
GINItrain=2*aucModeloBayestrain-1


DeaEmiTest$predDefecto <- predict(modeloBayesTrain,DeaEmiTest)
DeaEmiTest$prediccion <- predict(modeloBayesTrain,DeaEmiTest,type="raw")[,2]
Predauxiliar = prediction(DeaEmiTest$prediccion, DeaEmiTest$CausaMuertebasC,
                          label.ordering = NULL)
auc.tmp = performance(Predauxiliar, "auc");
aucModeloBayestest = as.numeric(auc.tmp@y.values)
aucModeloBayestest

CurvaRocModeloBayesTest <- performance(Predauxiliar,"tpr","fpr")
plot(CurvaRocModeloBayesTest,colorize=TRUE)
abline(a=0,b=1)

## Indice de GINI
GINItest=2*aucModeloBayestest-1

## Capacidad del Modelo
mean(as.numeric(DeaEmiTest$CausaMuertebasC)-1)
aggregate(DeaEmiTest$prediccion~DeaEmiTest$CausaMuertebasC,FUN=mean)

## -------------------------------------------------------------------------
##       PARTE 3: MODELO DE CLASIFICACIÓN: PUESTA EN VALOR DEL MODELO
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 9. Bloque de puesta en valor de un modelo: Fijación del Threshold #####
ALPHA=0.5
ConfusionTest=table(DeaEmiTest$CausaMuertebasC,DeaEmiTest$prediccion>=ALPHA)
AccuracyTest= (sum(DeaEmiTest$CausaMuertebasC==1 & 
                  DeaEmiTest$prediccion>=ALPHA)+
                  sum(DeaEmiTest$CausaMuertebasC==0 & 
                  DeaEmiTest$prediccion<ALPHA))/length(DeaEmiTest$y)
PrecisionTest=sum(DeaEmiTest$CausaMuertebasC==1 & 
                  DeaEmiTest$prediccion>=ALPHA)/sum(DeaEmiTest$prediccion>=ALPHA)
CoberturaTest=sum(DeaEmiTest$CausaMuertebasC==1 & 
                  DeaEmiTest$prediccion>=ALPHA)/sum(DeaEmiTest$CausaMuertebasC==1)
ConfusionTest
AccuracyTest
PrecisionTest
CoberturaTest

ALPHA=0.2
ConfusionTest=table(DeaEmiTest$CausaMuertebasC,DeaEmiTest$prediccion>=ALPHA)
AccuracyTest= (sum(DeaEmiTest$CausaMuertebasC==1 & 
                     DeaEmiTest$prediccion>=ALPHA)+
                 sum(DeaEmiTest$CausaMuertebasC==0 & 
                       DeaEmiTest$prediccion<ALPHA))/length(DeaEmiTest$y)
PrecisionTest=sum(DeaEmiTest$CausaMuertebasC==1 & 
                    DeaEmiTest$prediccion>=ALPHA)/sum(DeaEmiTest$prediccion>=ALPHA)
CoberturaTest=sum(DeaEmiTest$CausaMuertebasC==1 & 
                    DeaEmiTest$prediccion>=ALPHA)/sum(DeaEmiTest$CausaMuertebasC==1)
ConfusionTest
AccuracyTest
PrecisionTest
CoberturaTest

ALPHA=0.8
ConfusionTest=table(DeaEmiTest$CausaMuertebasC,DeaEmiTest$prediccion>=ALPHA)
AccuracyTest= (sum(DeaEmiTest$CausaMuertebasC==1 & 
                     DeaEmiTest$prediccion>=ALPHA)+
                 sum(DeaEmiTest$CausaMuertebasC==0 & 
                       DeaEmiTest$prediccion<ALPHA))/length(DeaEmiTest$y)
PrecisionTest=sum(DeaEmiTest$CausaMuertebasC==1 & 
                    DeaEmiTest$prediccion>=ALPHA)/sum(DeaEmiTest$prediccion>=ALPHA)
CoberturaTest=sum(DeaEmiTest$CausaMuertebasC==1 & 
                    DeaEmiTest$prediccion>=ALPHA)/sum(DeaEmiTest$CausaMuertebasC==1)
ConfusionTest
AccuracyTest
PrecisionTest
CoberturaTest


## -------------------------------------------------------------------------

##### 10. KS y punto de máxima separación #####

DeaEmiKS=DeaEmiTest[order(DeaEmiTest$prediccion, decreasing=TRUE),c("CausaMuertebasC","prediccion")]
DeaEmiKS$N=1:length(DeaEmiKS$CausaMuertebasC)
DeaEmiKS$EXITOSACUM=cumsum(as.numeric(DeaEmiKS$CausaMuertebasC)-1)
DeaEmiKS$FRACASOSACUM=DeaEmiKS$N-DeaEmiKS$EXITOSACUM
DeaEmiKS$EXITOSTOT=sum(DeaEmiKS$CausaMuertebasC==1)
DeaEmiKS$FRACASOSTOT=sum(DeaEmiKS$CausaMuertebasC==0)
DeaEmiKS$TOTAL=DeaEmiKS$EXITOSTOT+DeaEmiKS$FRACASOSTOT
DeaEmiKS$TPR=DeaEmiKS$EXITOSACUM/DeaEmiKS$EXITOSTOT
DeaEmiKS$FPR=DeaEmiKS$FRACASOSACUM/DeaEmiKS$FRACASOSTOT
DeaEmiKS$DIFF=DeaEmiKS$TPR-DeaEmiKS$FPR
plot(DeaEmiKS$DIFF)
max(DeaEmiKS$DIFF)
which(DeaEmiKS$DIFF==max(DeaEmiKS$DIFF))
DeaEmiKS[2764,]

plot(DeaEmiKS$prediccion*1000,1-DeaEmiKS$TPR,xlab="SCORE",ylab="Porcentaje acumulado",main="Distribuciones por Score (rojo malos, azul buenos)",type="l",col="blue")
lines(DeaEmiKS$prediccion*1000,1-DeaEmiKS$FPR,col="red")

## -------------------------------------------------------------------------

##### 11. F1Score y punto óptimo estadístico #####

DeaEmiKS$Accuracy=(DeaEmiKS$EXITOSACUM+DeaEmiKS$FRACASOSTOT-DeaEmiKS$FRACASOSACUM)/DeaEmiKS$TOTAL
DeaEmiKS$Precision=DeaEmiKS$EXITOSACUM/DeaEmiKS$N
DeaEmiKS$Cobertura=DeaEmiKS$EXITOSACUM/DeaEmiKS$EXITOSTOT
DeaEmiKS$F1Score=2*(DeaEmiKS$Precision*DeaEmiKS$Cobertura)/(DeaEmiKS$Precision+DeaEmiKS$Cobertura)
plot(DeaEmiKS$F1Score)
max(DeaEmiKS$F1Score,na.rm=TRUE)
which(DeaEmiKS$F1Score==max(DeaEmiKS$F1Score,na.rm=TRUE))
DeaEmiKS[1475,]

ALPHAS=seq(0,1,0.05)
Accuracy=c()
Precision=c()
Cobertura=c()
F1Score=c()
for (i in 1:length(ALPHAS)){
  ALPHA=ALPHAS[i]
  Confusion=table(DeaEmiKS$CausaMuertebasC,DeaEmiKS$prediccion>=ALPHA)
  Accuracy=c(Accuracy,(sum(DeaEmiKS$CausaMuertebasC==1 & DeaEmiKS$prediccion>=ALPHA)+sum(DeaEmiKS$CausaMuertebasC==0 & DeaEmiKS$prediccion<ALPHA))/length(DeaEmiKS$CausaMuertebasC))
  Precision=c(Precision,sum(DeaEmiKS$CausaMuertebasC==1 & DeaEmiKS$prediccion>=ALPHA)/sum(DeaEmiKS$prediccion>=ALPHA))
  Cobertura=c(Cobertura,sum(DeaEmiKS$CausaMuertebasC==1 & DeaEmiKS$prediccion>=ALPHA)/sum(DeaEmiKS$CausaMuertebasC==1))
}
F1Score=2*(Precision*Cobertura)/(Precision+Cobertura)
DFF1=data.frame(ALPHAS,Accuracy,Precision,Cobertura,F1Score)

DFF1

