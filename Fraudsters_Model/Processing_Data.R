library(xgboost)
X_test<- read.csv("/home/salvador/Documents/Rappi_MX/fernandopaulin-rpmx_ds_challenge-9c6f6f0dd29e/X_test.csv")

Model=xgb.load("/home/salvador/Documents/Rappi_MX/fernandopaulin-rpmx_ds_challenge-9c6f6f0dd29e/Fraudsters_Model.json")
Modelo<- Model[[2]]

library(reticulate)
Modelo<-py_load_object("/home/salvador/Documents/Rappi_MX/fernandopaulin-rpmx_ds_challenge-9c6f6f0dd29e/finalized_model.sav")
table(Modelo$predict(X_test))


### Make the back code to do Feature engineering
## The idea is to take the raw data as an Input
## and export a Prediction for each Customer
setwd("/home/salvador/Documents/Rappi_MX/fernandopaulin-rpmx_ds_challenge-9c6f6f0dd29e")

TXN<- read.csv("ds_challenge_apr2021.csv")
TXN$TXN<- 1

Approved_TXN<- aggregate(TXN~ID_USER,TXN[TXN$status_txn=="Aceptada",],sum)
names(Approved_TXN)[2]<- "TXN_Aprobadas"
Declined_TXN<- aggregate(TXN~ID_USER,TXN[TXN$status_txn=="Rechazada",],sum)
names(Declined_TXN)[2]<- "TXN_Declinadas"
In_Process_TXN<- aggregate(TXN~ID_USER,TXN[TXN$status_txn=="En proceso",],sum)
names(In_Process_TXN)[2]<- "TXN_En_Proceso"

## Add the data
Customers<- merge(Approved_TXN,Declined_TXN,by="ID_USER",
                  all.x = TRUE,all.y = TRUE)
Customers<- merge(Customers,In_Process_TXN,by="ID_USER",
                  all.x = TRUE,all.y = TRUE)

Customers[,c( "TXN_Aprobadas" ,"TXN_Declinadas" ,"TXN_En_Proceso")]<-
  apply(Customers[,c( "TXN_Aprobadas" ,"TXN_Declinadas" ,"TXN_En_Proceso")],2,function(x) ifelse(is.na(x),0,x))

Categorical_Variables<- c("establecimiento","tipo_tc")

TXN$establecimiento<- as.character(TXN$establecimiento)
TXN$establecimiento<- ifelse(is.na(TXN$establecimiento),"",TXN$establecimiento)

TXN$establecimiento<- ifelse(TXN$establecimiento%in%c("N/A",""),"Desconocido",TXN$establecimiento)

## Obtaein the most frecuent stablishment for each user
## and his credit line
Establishment_TXN<-aggregate(establecimiento~ID_USER,TXN,function(x) names(which.max(table(x))))
Type_card_TXN<-aggregate(tipo_tc~ID_USER,TXN,function(x) names(which.max(table(x))))
Line_Card<- aggregate(linea_tc~ID_USER,TXN,function(x) names(which.max(table(x))))


Customers<- merge(Customers,Establishment_TXN,by="ID_USER",
                  all.x = TRUE)

Customers<- merge(Customers,Type_card_TXN,by="ID_USER",
                  all.x = TRUE)
Customers<- merge(Customers,Line_Card,by="ID_USER",
                  all.x = TRUE)

Categorical_Data<- Customers[,c("establecimiento","tipo_tc")]
library(mltools)
library(data.table)
library(reshape2)
library(caret)
newdata <- one_hot(as.data.table(Categorical_Data))


dummy <- dummyVars(" ~ .", data=Categorical_Data)
newdata <- data.frame(predict(dummy, newdata = Categorical_Data)) 
names(newdata)<- c("establecimiento_Abarrotes"  , "establecimiento_Desconocido",
                    "establecimiento_Farmacia"   , "establecimiento_MPago"  ,     "establecimiento_Restaurante",
                    "establecimiento_Super"     ,  "tipo_tc_Física"         ,     "tipo_tc_Virtual" )

Customers<- cbind(Customers,newdata)

Predictive_Variables<- c("TXN_Aprobadas"        ,       "TXN_Declinadas"       ,       "TXN_En_Proceso" ,
                         "linea_tc"              ,      "establecimiento_Abarrotes" , "establecimiento_Desconocido",
                         "establecimiento_Farmacia"   , "establecimiento_MPago"    ,   "establecimiento_Restaurante",
                         "establecimiento_Super"   ,    "tipo_tc_Física"          ,    "tipo_tc_Virtual")

X<-Customers[,Predictive_Variables] 
X<- as.data.frame(apply(X,2,as.numeric))
names(X)<- Predictive_Variables

Customers$Is_Fraudster<- Modelo$predict(X)


