#NOMBRE DEL ARCHIVO
t_modulo<-read_excel("C:/Users/ximen/OneDrive/Escritorio/Pruebas PCA/COMPLETO/AF_COMPLETO_PCA.xlsx")
t_modulo<-data.frame(t_modulo)

###########Número de columnas en el archivo#################
n_var=ncol(t_modulo)
############################################################
head(t_modulo)

#Se convierten todas las variables a categóricas
t_modulo <- t_modulo %>%
  mutate_at(vars(1:n_var,), as.factor)
head(t_modulo)

#Asegurarse de convertir las columnas numéricas 

t_modulo$EDAD <- as.integer(t_modulo$EDAD)
t_modulo$INGRESO <- as.integer(t_modulo$INGRESO) 
t_modulo$FRECUENCIA_USO_CTA_DEBITO <- as.integer(t_modulo$FRECUENCIA_USO_CTA_DEBITO) 

head(t_modulo)

dtsep <- splitmix(t_modulo)
print(dtsep$col.quant)
print(dtsep$col.qual)

res.pcamix <- PCAmix(X.quanti = dtsep$X.quanti,
                      X.quali=dtsep$X.quali,
                      rename.level=TRUE,
                      ndim=75, 
                      graph=FALSE,
                      )

res.pcamix$eig
res.pcamix$eig[,1] 
plot(res.pcamix$eig[,1], type="b", pch=20, col="blue")
abline(h=1)

res.pcamix$eig 


n_dim_significativas=0
for (i in res.pcamix$eig[,1] ){
  if (i>1){
    n_dim_significativas=n_dim_significativas+1
  }
    
}

print(paste("Dimensiones finales:",n_dim_significativas))

################################################################################################
n_dim_significativas=15
################################################################################################

ag<-res.pcamix$sqload[,1:n_dim_significativas]
ag<-data.frame(ag)
head(ag)
ag
ag$dim<-rep(0,n_var)

for(i in 1:n_var){
  ag$dim[i]<-which.max(ag[i,-1*(n_dim_significativas+1)])
}

head(ag)
ag

ag$variables <- rownames(ag)
head(ag)

ag$cor.max <- rep(0,n_var)
for(i in 1:n_var){

  
    ag$cor.max[i] <- max(ag[i,-c(n_dim_significativas+1,n_dim_significativas+2)])
}

ag



ggplot(ag,aes(x=reorder(variables,-dim),
              y=cor.max, fill=as.factor(dim) ))+
  geom_bar(stat ='identity') +
  coord_flip()

################################################################################################

################################################################################################

#Rota las componentes
rot<-PCArot(res.pcamix,n_dim_significativas)

ag<-rot$sqload[,1:n_dim_significativas]
ag<-data.frame(ag)
head(ag)
ag 
ag$dim<-rep(0,n_var)

for(i in 1:n_var){
  ag$dim[i]<-which.max(ag[i,-1*(n_dim_significativas+1)])
}
head(ag)
ag

ag$variables <- rownames(ag)
head(ag)

ag$cor.max <- rep(0,n_var)
for(i in 1:n_var){
    ag$cor.max[i] <- max(ag[i,-c(n_dim_significativas+1,n_dim_significativas+2)])
}

ag



ggplot(ag,aes(x=reorder(variables,-dim),
              y=cor.max, fill=as.factor(dim) ))+
  geom_bar(stat ='identity') +
  coord_flip()








