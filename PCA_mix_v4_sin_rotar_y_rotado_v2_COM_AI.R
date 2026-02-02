
#----fin-----------------------------#

#https://www.youtube.com/watch?v=N6P2YD5t94o



#CARGAR PAQUETES
#install.packages("PCAmixdata")
library(tidyverse)
library(psych)
library(sna)
library(qgraph)
library(EGAnet)
library(readxl)
library(dplyr)
library(PCAmixdata)



#CAMBIO CAMBIO CAMBIO 1 NOMBRE DEL ARCHIVO   "C:\Users\ximen\OneDrive\Escritorio\Pruebas PCA\Demografía_PCA_V1.xlsx"
t_modulo<-read_excel("C:/Users/ximen/OneDrive/Escritorio/Pruebas PCA/COMPLETO/AI_COMPLETO_PCA.xlsx")
t_modulo<-data.frame(t_modulo)
#CAMBIO CAMBIO CAMBIO 1

###########Númeoro de columnas en el archivo#################
n_var=ncol(t_modulo)
############################################################


head(t_modulo)

#Se convierten todas las variables a categóricas
t_modulo <- t_modulo %>%
  mutate_at(vars(1:n_var,), as.factor)
head(t_modulo)

#Asegurarse de convertir las columnas numéricas 

#CAMBIO CAMBIO CAMBIO 2 VALIDAR TODAS LAS  NUMÉRICAS
#t_modulo$LLAVEMOD <- as.integer(t_modulo$LLAVEMOD)
#t_modulo$LLAVEVIV <- as.integer(t_modulo$LLAVEVIV)
#t_modulo$LLAVEHOG <- as.integer(t_modulo$LLAVEHOG)

t_modulo$EDAD <- as.integer(t_modulo$EDAD)
t_modulo$INGRESO <- as.integer(t_modulo$INGRESO)

#t_modulo$EST_DIS <- as.integer(t_modulo$EST_DIS)
#t_modulo$UPM_DIS <- as.integer(t_modulo$UPM_DIS)
#t_modulo$FAC_PER <- as.integer(t_modulo$FAC_PER)
#t_modulo$LLAVEHOG <- as.integer(t_modulo$LLAVEHOG)

#t_modulo$EDAD <- as.integer(t_modulo$EDAD)
#t_modulo$INGRESO <- as.integer(t_modulo$INGRESO)
head(t_modulo)
#CAMBIO CAMBIO CAMBIO 2


#Separamodim.1#Separamos las variables cualitativas de las cuantitativas
dtsep <- splitmix(t_modulo)
print(dtsep$col.quant)
print(dtsep$col.qual)

#Despues del PCAmix puede incrementar el númro de variables por las variables Dummy

######################CAMBIO CAMBIO CAMBIO 3 
res.pcamix <- PCAmix(X.quanti = dtsep$X.quanti,
                      X.quali=dtsep$X.quali,
                      rename.level=TRUE,
                      ndim=30, 
                      graph=FALSE,
                      )
######################CAMBIO CAMBIO CAMBIO 3 

res.pcamix$eig
res.pcamix$eig[,1] #Contiene los eigenvalores de los componentes principales
#Se deben elegir los valores mayores a 1
plot(res.pcamix$eig[,1], type="b", pch=20, col="blue")
abline(h=1)
#Se va a trabajar con 2 se alcanza el 59.8% y con 3 el 74.87 %

res.pcamix$eig #Trabajar cn 4 o 5 componentes



n_dim_significativas=0
for (i in res.pcamix$eig[,1] ){
  if (i>1){
    n_dim_significativas=n_dim_significativas+1
  }
    
}

print(paste("Dimensiones finales:",n_dim_significativas))

#Una vez determinadas las variables significativas determinamos 
#si es correcta la cantidad o es necesario agregar mas


################################################################################################
#CAMBIO CAMBIO CAMBIO 4 PROBAR CON VARIAS PCA's ENTRE 5-10

#El usuario debe elegir con cuantas dimensiones se queda
n_dim_significativas=10# SOLO ESTA VERSION, SIN EL SEGUNDO
#CAMBIO CAMBIO CAMBIO 4
################################################################################################



#Agrupación con componentes sin rotar
#Sqload nos muestra las correlaciones sin importar si son significativas
#se coloca como DataFrame



#Se analizan las contribuciones o correlaciones de cada una de las variables y componentes y se agregan a un DataFrame
ag<-res.pcamix$sqload[,1:n_dim_significativas]
ag<-data.frame(ag)
head(ag)
ag #Muestra los datos para todas las variables

#Se agrega una columna para mostrar en que PCA o dim contribuye más
ag$dim<-rep(0,n_var)

for(i in 1:n_var){
  ag$dim[i]<-which.max(ag[i,-1*(n_dim_significativas+1)])
}

head(ag)
ag

#Se agrega una columna para mostrar los nombres de las variables
ag$variables <- rownames(ag)
head(ag)

#Se genera una columna para indicar el componente en el que tiene mayor contribucion con la variable
#Se elimina la pos de la columna adicional dim_max
ag$cor.max <- rep(0,n_var)
for(i in 1:n_var){

  
    ag$cor.max[i] <- max(ag[i,-c(n_dim_significativas+1,n_dim_significativas+2)])
}

ag



#Ahora se mostrará un gráfico de barras que variable esta asociada con cada componente y se busca la mayor
#correlación

ggplot(ag,aes(x=reorder(variables,-dim),
              y=cor.max, fill=as.factor(dim) ))+
  geom_bar(stat ='identity') +
  coord_flip()

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


################################################################################################
#Los grupos en los se distribuyen las variables
#n_dim_significativas=7
################################################################################################
"""
#Rota las componentes
rot<-PCArot(res.pcamix,n_dim_significativas)


#Se analizan las contribuciones o correlaciones de cada una de las variables y componentes y se agregan a un DataFrame
ag<-rot$sqload[,1:n_dim_significativas]
ag<-data.frame(ag)
head(ag)
ag #Muestra los datos para todas las variables

#Se agrega una columna para mostrar en que PCA o dim contribuye más
ag$dim<-rep(0,n_var)

for(i in 1:n_var){
  ag$dim[i]<-which.max(ag[i,-1*(n_dim_significativas+1)])
}
head(ag)
ag


#Se agrega una columna para mostrar los nombres de las variables
ag$variables <- rownames(ag)
head(ag)

#Se genera una columna para indicar el componente en el que tiene mayor contribucion con la variable
#Se elimina la pos de la columna adicional dim_max
ag$cor.max <- rep(0,n_var)
for(i in 1:n_var){
    ag$cor.max[i] <- max(ag[i,-c(n_dim_significativas+1,n_dim_significativas+2)])
}

ag



ggplot(ag,aes(x=reorder(variables,-dim),
              y=cor.max, fill=as.factor(dim) ))+
  geom_bar(stat ='identity') +
  coord_flip()


"""
#$$$$$$$$$$$$$$$$$$$
#sco<-rot$scores[,1:4]
#sco<-data.frame(sco)
#names(sco)<-c("d1","d2","d3",,"d4")
#head(sco)






