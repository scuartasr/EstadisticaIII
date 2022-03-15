#Código R para la lectura de lo datos mensuales sobre  productividad 
#en toneladas para chocolate (Australia), primera columna en cbe2.csv
#Serie mensual que inicia en enero de 1958

#Para leer primera columna de archivo cbe2.csv
choc=read.table(file.choose(),header=T,skip=2, sep=";",dec=",",
                 colClasses=c("numeric",rep("NULL",2)))

#Convirtiendo los datos en objeto serie de tiempo
choc=ts(choc,freq=12,start=c(1958,1))

#Determinando tipo de objeto R 
class(choc)

#Gráfica de la serie y de su logaritmo natural
#compare la variabilidad  ¿es aditiva o multiplicatica la serie?
plot(choc)

win.graph()
plot(log(choc))

#Análisis serie choc: como una serie multiplicativa
win.graph()
plot(decompose(choc,type="multiplicative")) #Descomp. multip. para choc
win.graph()
plot(decompose(log(choc),type="additive")) #Descomp. aditiva log(choc)

#Tendencia de log(choc)
Tt.log=decompose(log(choc),type="additive")$trend

#Otras gráficas para analizar patrones en log(choc)
#para tendencia
plot(Tt.log,ylim=c(min(log(choc)),max(log(choc))),
                        main="Tendencia log(choc)")

#para analizar componente estacional en la escala log: 
#efectos de los meses del año calendario en la media de log(choc) 
boxplot(log(choc)~cycle(log(choc)),names=month.abb)

#Comparación del patrón anual en los primeros cinco años
plot.ts(log(choc)[1:60],ylab=expression(log(choc)),
                main="log(choc) - primeros 60 meses")
abline(v=seq(1,60,by=12),col=2) #líneas de refencia colocadas en los diciembres

