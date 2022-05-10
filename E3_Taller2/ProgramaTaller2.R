rm(list=ls(all=TRUE))
library(forecast)

#----------------------------------------------------------------------------------------------------------------------------------------------------
##Cargando funciones de usuario necesarias: Ajuste ruta de archivos a la ruta donde descargue los archivos con las funciones de usuario de nombres 
#"Funciones-Criterios.Informacion-Calidad.Intervalos.R" y "Funcion-regexponencial.R"
#----------------------------------------------------------------------------------------------------------------------------------------------------

#Cargando archivo con las funciones exp.crit.inf.resid() para calcular AIC y BIC, y amplitud.cobertura() para calcular amplitud media y coberturas de los I.P
source("C:\\Users\\Nelfi_Gonzalez\\Documents\\ESTADISTICA III 3009137\\CLASES2022\\Talleres de clase\\Funciones de usuario\\Funciones-Criterios.Informacion-Calidad.Intervalos.R")

#Cargando archivo con la función regexponencial() para hacer la regresion exponencial polinomial
source("C:\\Users\\Nelfi_Gonzalez\\Documents\\ESTADISTICA III 3009137\\CLASES2022\\Talleres de clase\\Funciones de usuario\\Funcion-regexponencial.R")

#----------------------------------------------------------------------------------------------------------------------------------------------------
#o bien, desde repositorio en github
#----------------------------------------------------------------------------------------------------------------------------------------------------
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funciones-Criterios.Informacion-Calidad.Intervalos.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-regexponencial.R")


#----------------------------------------------------------------------------------------------------------------------------------------------------
#Leyendo datos del archivo IndProductividad.txt y haciendo gráficos descriptivos usados en este taller
#----------------------------------------------------------------------------------------------------------------------------------------------------
#La siguiente linea permite leer los datos guardados en archivo
#de texto en disco local y crear un objeto serie de tiempo. file.choose() abre ventana de windows para
#explorar y ubicar el archivo. header=T para indicar que los datos
#están encabezados por nombre de la columna; si no hay encabezado, se toma header=F
datos.indpro=ts(scan(file.choose(),dec = "."),frequency=12,start=c(1950,1))
datos.indpro

#Gráfica de la serie
win.graph()
plot(datos.indpro)

#Gráfica del logaritmo natural de la serie
win.graph()
plot(log(datos.indpro))

#Gráfica de la descomposición aditiva de la serie
win.graph()
plot(decompose(datos.indpro,type="additive"))

#Gráfica de la tendencia filtrada por filtro de descomposición aditiva de la serie
win.graph()
plot(decompose(datos.indpro,type="additive")$trend,ylim=c(min(datos.indpro),max(datos.indpro)),lwd=2)


#Gráfica de la tendencia filtrada por filtro de descomposición aditiva del log de la serie
win.graph()
plot(decompose(log(datos.indpro),type="additive")$trend,ylim=c(min(log(datos.indpro)),max(log(datos.indpro))),lwd=2)


#Boxplots de la distribución de los datos según periodos del año
win.graph(width=5,height=4,pointsize=8)
boxplot(datos.indpro~cycle(datos.indpro),names=month.abb)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Definiendo variables y objetos R necesarios en los ajustes y pronósticos
#----------------------------------------------------------------------------------------------------------------------------------------------------
#Creando índice t de tiempo para el ajuste con los primeros n=276 datos
n=length(datos.indpro)-12 #Tamaño muestra de ajuste
t=1:n
#Potencias del índice de tiempo en el ajuste
t2=t^2
t3=t^3
t4=t^4
t5=t^5
t6=t^6

#Definiendo las matrices de diseño para los ajustes
X1=data.frame(t,t2,t3) #para modelo 1
X2=data.frame(t,t2,t3,t4) #para modelos 2 
X3=data.frame(t,t2,t3,t4,t5,t6) #para modelos 3, 4 y 5 

yt=ts(datos.indpro[t],freq=12,start=c(1950,1)) #Valores de la serie para el ajuste


tnuevo=(n+1):length(datos.indpro) #índice de tiempo para pronósticos en la validación cruzada
#Potencias del índice de tiempo en los pronósticos
t2nuevo=tnuevo^2
t3nuevo=tnuevo^3
t4nuevo=tnuevo^4
t5nuevo=tnuevo^5
t6nuevo=tnuevo^6

#Definiendo valores de las matrices de diseño en los pronósticos
X1nuevo=data.frame(t=tnuevo,t2=t2nuevo,t3=t3nuevo) #para modelo 1
X2nuevo=data.frame(t=tnuevo,t2=t2nuevo,t3=t3nuevo,t4=t4nuevo) #para modelo 2
X3nuevo=data.frame(t=tnuevo,t2=t2nuevo,t3=t3nuevo,t4=t4nuevo,t5=t5nuevo,t6=t6nuevo) #para modelos 3, 4 y 5
ytf=ts(datos.indpro[tnuevo],freq=12,start=c(1973,1)) #valores de la serie en la validación cruzada

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Ajustes y pronósticos de los modelos
#----------------------------------------------------------------------------------------------------------------------------------------------------
#Ajuste modelo de tendencia cúbica
modelo1=lm(yt~.,data=X1)
summary(modelo1)
ythat1=ts(fitted(modelo1),frequency=12,start=start(yt))
predict1=predict(modelo1,newdata=X1nuevo,interval="prediction") #pronóstico puntual y por I.P modelo 1
predict1=ts(predict1,frequency=12,start=start(ytf))
predict1

#serie de tiempo de los pronósticos puntuales
ytpron1=predict1[,1]

#Ajuste modelo de tendencia polinomial grado p=4
modelo2=lm(yt~.,data=X2)
summary(modelo2)
ythat2=ts(fitted(modelo2),frequency=12,start=start(yt))
predict2=predict(modelo2,newdata=X2nuevo,interval="prediction") #pronóstico puntual y por I.P modelo 2
predict2=ts(predict2,frequency=12,start=start(ytf))
predict2

#serie de tiempo de los pronósticos puntuales
ytpron2=predict2[,2]

#Ajuste modelo de tendencia polinomial grado p=6
modelo3=lm(yt~.,data=X3)
summary(modelo3)
ythat3=ts(fitted(modelo3),frequency=12,start=start(yt))


predict3=predict(modelo3,newdata=X3nuevo,interval="prediction") #pronóstico puntual y por I.P modelo 3
predict3=ts(predict3,frequency=12,start=start(ytf))
predict3

#serie de tiempo de los pronósticos puntuales
ytpron3=predict3[,3]

#Ajuste y pronósticos modelo exponencial polinomial de grado p=6
#Crear vector con nombres de los parámetros a usar en la fórmula R del modelo exponencial
#Forma larga de hacerlo
parammod4=c("beta0","beta1","beta2","beta3","beta4","beta5","beta6")
parammod4
#Forma corta de hacerlo con la ayuda de la función paste()
parammod4=paste0("beta",0:6)
parammod4


modelo4=regexponencial(respuesta=yt,data=X3,names.param=parammod4) #Ajuste del modelo
summary(modelo4)
ythat4=ts(fitted(modelo4),frequency=12,start=start(yt))
predict4=predict(modelo4,newdata=X3nuevo,interval="prediction") #solo da pronóstico puntual 
predict4=ts(predict4,frequency=12,start=start(ytf))
predict4
#serie de tiempo de los pronósticos puntuales
ytpron4=predict4

#Ajuste modelo 5 log polinomial de grado p=6
modelo5=lm(log(yt)~.,data=X3)
summary(modelo5)
#Por ser modelo log polinomial es necesario destransformar y aplicar factor de corrección exp(MSE/2)
ythat5=ts(exp(fitted(modelo5))*exp(summary(modelo5)$sigma^2/2),freq=12,start=start(yt))

#Por ser modelo log polinomial es necesario destransformar y aplicar factor de corrección
predict5=exp(predict(modelo5,newdata=X3nuevo,interval="prediction"))*exp(summary(modelo5)$sigma^2/2) #pronóstico puntual y por I.P modelo 5
predict5=ts(predict5,frequency=12,start=start(ytf))
predict5

#serie de tiempo de los pronósticos puntuales
ytpron5=predict5[,1]

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Comparación de los ajustes
#----------------------------------------------------------------------------------------------------------------------------------------------------
#Gráficos de los ajustes#Gráficos de los ajustes en una misma ventana gráfica
win.graph(width=15,height=10)
layout(rbind(c(1:3),c(4:6)))
plot(datos.indpro,lwd=2)
lines(ythat1,col=2,lwd=2)
legend("topleft",legend=c("Serie real","Serie Ajustada modelo 1"),col=c(1,2),lty=1,lwd=2)
plot(datos.indpro,lwd=2)
lines(ythat2,col=2,lwd=2)
legend("topleft",legend=c("Serie real","Serie Ajustada modelo 2"),col=c(1,2),lty=1,lwd=2)
plot(datos.indpro,lwd=2)
lines(ythat3,col=2,lwd=2)
legend("topleft",legend=c("Serie real","Serie Ajustada modelo 3"),col=c(1,2),lty=1,lwd=2)
plot(datos.indpro,lwd=2)
lines(ythat4,col=2,lwd=2)
legend("topleft",legend=c("Serie real","Serie Ajustada modelo 4"),col=c(1,2),lty=1,lwd=2)
plot(datos.indpro,lwd=2)
lines(ythat5,col=2,lwd=2)
legend("topleft",legend=c("Real","Serie Ajustada modelo 5"),col=c(1,2),lty=1,lwd=2)

#Cálculo AIC y BIC versión exp(Cn*(p))
#cálculo número de parámetros en cada modelo
nparmod1=length(coef(modelo1)[coef(modelo1)!=0]);nparmod1
nparmod2=length(coef(modelo2)[coef(modelo2)!=0]);nparmod2
nparmod3=length(coef(modelo3)[coef(modelo3)!=0]);nparmod3
nparmod4=length(coef(modelo4)[coef(modelo4)!=0]);nparmod4
nparmod5=length(coef(modelo5)[coef(modelo5)!=0]);nparmod5

#NOTA: En modelos con ajuste sin transformar los datos, residuals(modelo) calcula Yt-Ythat, pero no en el modelo 5
Criterios1=exp.crit.inf.resid(residuales=residuals(modelo1),n.par=nparmod1);Criterios1
Criterios2=exp.crit.inf.resid(residuales=residuals(modelo2),n.par=nparmod2);Criterios2
Criterios3=exp.crit.inf.resid(residuales=residuals(modelo3),n.par=nparmod3);Criterios3
Criterios4=exp.crit.inf.resid(residuales=residuals(modelo4),n.par=nparmod4);Criterios4

#En el modelo 5, debido a transformación log se usan los seudo-residuos en lugar de los residuos de ajuste
seudores5=yt-ythat5 #Seudo-residuos modelo5
Criterios5=exp.crit.inf.resid(residuales=seudores5,n.par=nparmod5);Criterios5

#Organizando en una tabla los valores de AIC y BIC
criterios=rbind(Criterios1,Criterios2,Criterios3,Criterios4,Criterios5)
rownames(criterios) = c("modelo1","modelo2","modelo3","modelo4","modelo5")
criterios


#----------------------------------------------------------------------------------------------------------------------------------------------------
#Comparación de residuales de todos los modelos
#----------------------------------------------------------------------------------------------------------------------------------------------------
#Gráfico de residuales comunes  versus tiempo, en una misma ventana gráfica
win.graph(width=15,height=10)
layout(rbind(c(1:3),c(4:6)))
plot.ts(residuals(modelo1),ylim=c(min(residuals(modelo1),-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),max(residuals(modelo1),-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma)))
abline(h=c(-2*summary(modelo1)$sigma,0,2*summary(modelo1)$sigma),col=2)
legend("bottomright",legend=c("Modelo 1"),lty=1,col=1,lwd=2)

plot.ts(residuals(modelo2),ylim=c(min(residuals(modelo2),-2*summary(modelo2)$sigma,2*summary(modelo2)$sigma),max(residuals(modelo2),-2*summary(modelo2)$sigma,2*summary(modelo2)$sigma)))
abline(h=c(-2*summary(modelo2)$sigma,0,2*summary(modelo2)$sigma),col=2)
legend("bottomright",legend=c("Modelo 2"),lty=1,col=1,lwd=2)

plot.ts(residuals(modelo3),ylim=c(min(residuals(modelo3),-2*summary(modelo3)$sigma,2*summary(modelo3)$sigma),max(residuals(modelo3),-2*summary(modelo3)$sigma,2*summary(modelo3)$sigma)))
abline(h=c(-2*summary(modelo3)$sigma,0,2*summary(modelo3)$sigma),col=2)
legend("topright",legend=c("Modelo 3"),lty=1,col=1,lwd=2)

plot.ts(residuals(modelo4),ylim=c(min(residuals(modelo4),-2*summary(modelo4)$sigma,2*summary(modelo4)$sigma),max(residuals(modelo4),-2*summary(modelo4)$sigma,2*summary(modelo4)$sigma)))
abline(h=c(-2*summary(modelo4)$sigma,0,2*summary(modelo4)$sigma),col=2)
legend("topright",legend=c("Modelo 4"),lty=1,col=1,lwd=2)

plot.ts(residuals(modelo5),ylim=c(min(residuals(modelo5),-2*summary(modelo5)$sigma,2*summary(modelo5)$sigma),max(residuals(modelo5),-2*summary(modelo5)$sigma,2*summary(modelo5)$sigma)))
abline(h=c(-2*summary(modelo5)$sigma,0,2*summary(modelo5)$sigma),col=2)
legend("topright",legend=c("Modelo 5"),lty=1,col=1,lwd=2)


#Gráfico de residuales comunes  versus variable respuesta, en una misma ventana gráfica
win.graph(width=15,height=10)
layout(rbind(c(1:3),c(4:6)))
plot(fitted(modelo1),residuals(modelo1),cex=1.1,ylim=c(min(residuals(modelo1),-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),max(residuals(modelo1),-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma)))
abline(h=c(-2*summary(modelo1)$sigma,0,2*summary(modelo1)$sigma),col=2)
legend("bottomright",legend=c("Modelo 1"),lty=1,col=1,lwd=2)

plot(fitted(modelo2),residuals(modelo2),cex=1.1,ylim=c(min(residuals(modelo2),-2*summary(modelo2)$sigma,2*summary(modelo2)$sigma),max(residuals(modelo2),-2*summary(modelo2)$sigma,2*summary(modelo2)$sigma)))
abline(h=c(-2*summary(modelo2)$sigma,0,2*summary(modelo2)$sigma),col=2)
legend("bottomright",legend=c("Modelo 2"),lty=1,col=1,lwd=2)

plot(fitted(modelo3),residuals(modelo3),cex=1.1,ylim=c(min(residuals(modelo3),-2*summary(modelo3)$sigma,2*summary(modelo3)$sigma),max(residuals(modelo3),-2*summary(modelo3)$sigma,2*summary(modelo3)$sigma)))
abline(h=c(-2*summary(modelo3)$sigma,0,2*summary(modelo3)$sigma),col=2)
legend("topright",legend=c("Modelo 3"),lty=1,col=1,lwd=2)

plot.ts(fitted(modelo4),residuals(modelo4),cex=1.1,ylim=c(min(residuals(modelo4),-2*summary(modelo4)$sigma,2*summary(modelo4)$sigma),max(residuals(modelo4),-2*summary(modelo4)$sigma,2*summary(modelo4)$sigma)))
abline(h=c(-2*summary(modelo4)$sigma,0,2*summary(modelo4)$sigma),col=2)
legend("topright",legend=c("Modelo 4"),lty=1,col=1,lwd=2)

plot(fitted(modelo5),residuals(modelo5),cex=1.1,ylim=c(min(residuals(modelo5),-2*summary(modelo5)$sigma,2*summary(modelo5)$sigma),max(residuals(modelo5),-2*summary(modelo5)$sigma,2*summary(modelo5)$sigma)))
abline(h=c(-2*summary(modelo5)$sigma,0,2*summary(modelo5)$sigma),col=2)
legend("topright",legend=c("Modelo 5"),lty=1,col=1,lwd=2)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Comparación de los pronósticos
#----------------------------------------------------------------------------------------------------------------------------------------------------
#Gráfico comparativo de pronósticos de modelos 1, 2, 3, 4, 5
plot(ytf,type="b",col=1,pch=19,ylim=c(min(ytf,ytpron1,ytpron2,ytpron3,ytpron4,ytpron5),max(ytf,ytpron1,ytpron2,ytpron3,ytpron4,ytpron5)),lwd=2,ylab="Ventas en dólares nominales",xaxt="n")
axis(1,at=time(ytf),labels=c("73.1","73.2","73.3","73.4","73.5","73.6","73.7","73.8","73.9","73.10","73.11","73.12"),cex.axis=0.8)
lines(ytpron1,col=2,pch=1,type="b",lwd=2)
lines(ytpron2,col=3,pch=2,type="b",lwd=2)
lines(ytpron3,col=4,pch=3,type="b",lwd=2)
lines(ytpron4,col=5,pch=4,type="b",lwd=2)
lines(ytpron5,col=6,pch=5,type="b",lwd=2)

legend("topleft",legend=c("Real","Modelo 1","Modelo 2","Modelo 3","Modelo 4","Modelo 5"),pch=c(19,1:5),col=c(1:6),lwd=2)

#precisión de los pronósticos por I.P del 95% de confianza
amplcob1=amplitud.cobertura(real=ytf,LIP=predict1[,2],LSP=predict1[,3]);amplcob1
amplcob2=amplitud.cobertura(real=ytf,LIP=predict2[,2],LSP=predict2[,3]);amplcob2
amplcob3=amplitud.cobertura(real=ytf,LIP=predict3[,2],LSP=predict3[,3]);amplcob3
amplcob5=amplitud.cobertura(real=ytf,LIP=predict5[,2],LSP=predict5[,3]);amplcob5

#Tabla de medidas de precisión de pronósticos
comp=rbind(accuracy(ytpron1,ytf),accuracy(ytpron2,ytf),accuracy(ytpron3,ytf),accuracy(ytpron4,ytf),accuracy(ytpron5,ytf))[,c(2,3,5)]
otros=rbind(amplcob1,amplcob2,amplcob3,c(NA,NA),amplcob5)

tablaprecis=cbind(comp,otros)
rownames(tablaprecis)=c("Modelo 1","Modelo 2","Modelo 3","Modelo 4","Modelo 5")
tablaprecis


#----------------------------------------------------------------------------------------------------------------------------------------------------
#Parte 2: Ajustes usando sólo los datos desde enero de 1955
#----------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))

#----------------------------------------------------------------------------------------------------------------------------------------------------
##Cargando funciones de usuario necesarias: Ajuste ruta de archivos a la ruta donde descargue los archivos con las funciones de usuario de nombres 
#"Funciones-Criterios.Informacion-Calidad.Intervalos.R" y "Funcion-regexponencial.R"
#----------------------------------------------------------------------------------------------------------------------------------------------------

#Cargando archivo con las funciones exp.crit.inf.resid() para calcular AIC y BIC, y amplitud.cobertura() para calcular amplitud media y coberturas de los I.P
source("C:\\Users\\Nelfi_Gonzalez\\Documents\\ESTADISTICA III 3009137\\CLASES2022\\Talleres de clase\\Funciones de usuario\\Funciones-Criterios.Informacion-Calidad.Intervalos.R")

#Cargando archivo con la función regexponencial() para hacer la regresion exponencial polinomial
source("C:\\Users\\Nelfi_Gonzalez\\Documents\\ESTADISTICA III 3009137\\CLASES2022\\Talleres de clase\\Funciones de usuario\\Funcion-regexponencial.R")

#----------------------------------------------------------------------------------------------------------------------------------------------------
#o bien, desde repositorio en github
#----------------------------------------------------------------------------------------------------------------------------------------------------
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funciones-Criterios.Informacion-Calidad.Intervalos.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-regexponencial.R")

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Leyendo datos del archivo IndProductividad.txt excluyendo los cinco primeros años y haciendo gráficos descriptivos usados en este taller
#----------------------------------------------------------------------------------------------------------------------------------------------------
datos.indpro=ts(scan(file.choose(),dec = ".")[-c(1:60)],frequency=12,start=c(1955,1))
datos.indpro

#Gráfica de la serie
win.graph()
plot(datos.indpro)

#Gráfica del logaritmo natural de la serie
win.graph()
plot(log(datos.indpro))

#Gráfica de la descomposición aditiva de la serie
win.graph()
plot(decompose(datos.indpro,type="additive"))


#Gráfica de la tendencia filtrada por filtro de descomposición aditiva de la serie
win.graph()
plot(decompose(datos.indpro,type="additive")$trend,ylim=c(min(datos.indpro),max(datos.indpro)),lwd=2)

#Gráfica de la tendencia filtrada por filtro de descomposición aditiva del log de la serie
win.graph()
plot(decompose(log(datos.indpro),type="additive")$trend,ylim=c(min(log(datos.indpro)),max(log(datos.indpro))),lwd=2)

#Boxplots de la distribución de los datos según periodos del año
win.graph(width=5,height=4,pointsize=8)
boxplot(datos.indpro~cycle(datos.indpro),names=month.abb)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Definiendo variables y objetos R necesarios en los ajustes y pronósticos
#----------------------------------------------------------------------------------------------------------------------------------------------------
#Creando índice t de tiempo para el ajuste con los primeros 276 datos
n=length(datos.indpro)-12
t=1:n
#Potencias del índice de tiempo en el ajuste
t2=t^2
t3=t^3
t4=t^4
t5=t^5
t6=t^6

#Definiendo las matrices de diseño para los ajustes
X1=data.frame(t,t2,t3) #para modelo 1
X2=data.frame(t,t2,t3,t4) #para modelos 2 
X3=data.frame(t,t2,t3,t4,t5,t6) #para modelos 3, 4 y 5 

yt=ts(datos.indpro[t],freq=12,start=c(1955,1)) #Valores de la serie para el ajuste


tnuevo=(n+1):length(datos.indpro) #índice de tiempo para pronósticos en la validación cruzada
#Potencias del índice de tiempo en los pronósticos
t2nuevo=tnuevo^2
t3nuevo=tnuevo^3
t4nuevo=tnuevo^4
t5nuevo=tnuevo^5
t6nuevo=tnuevo^6

#Definiendo valores de las matrices de diseño en los pronósticos
X1nuevo=data.frame(t=tnuevo,t2=t2nuevo,t3=t3nuevo) #para modelo 1
X2nuevo=data.frame(t=tnuevo,t2=t2nuevo,t3=t3nuevo,t4=t4nuevo) #para modelo 2
X3nuevo=data.frame(t=tnuevo,t2=t2nuevo,t3=t3nuevo,t4=t4nuevo,t5=t5nuevo,t6=t6nuevo) #para modelos 3, 4 y 5

ytf=ts(datos.indpro[tnuevo],freq=12,start=c(1973,1)) #valores de la serie en la validación cruzada

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Ajustes y pronósticos de los modelos
#----------------------------------------------------------------------------------------------------------------------------------------------------
#Ajuste modelo de tendencia cúbica
modelo1=lm(yt~.,data=X1)
summary(modelo1)
ythat1=ts(fitted(modelo1),frequency=12,start=start(yt))
predict1=predict(modelo1,newdata=X1nuevo,interval="prediction") #pronóstico puntual y por I.P modelo 1
predict1=ts(predict1,frequency=12,start=start(ytf))
predict1

#serie de tiempo de los pronósticos puntuales
ytpron1=predict1[,1]

#Ajuste modelo de tendencia polinomial grado p=4
modelo2=lm(yt~.,data=X2)
summary(modelo2)
ythat2=ts(fitted(modelo2),frequency=12,start=start(yt))
predict2=predict(modelo2,newdata=X2nuevo,interval="prediction") #pronóstico puntual y por I.P modelo 2
predict2=ts(predict2,frequency=12,start=start(ytf))
predict2

#serie de tiempo de los pronósticos puntuales
ytpron2=predict2[,2]

#Ajuste modelo de tendencia polinomio p=6
modelo3=lm(yt~.,data=X3)
summary(modelo3)
ythat3=ts(fitted(modelo3),frequency=12,start=start(yt))


predict3=predict(modelo3,newdata=X3nuevo,interval="prediction") #pronóstico puntual y por I.P modelo 3
predict3=ts(predict3,frequency=12,start=start(ytf))
predict3

#serie de tiempo de los pronósticos puntuales
ytpron3=predict3[,3]

#Ajuste y pronósticos modelo exponencial polinomial de grado p=6
#Crear vector con nombres de los parámetros a usar en la fórmula R del modelo exponencial
#Forma larga de hacerlo
parammod4=c("beta0","beta1","beta2","beta3","beta4","beta5","beta6")
parammod4
#Forma corta de hacerlo con la ayuda de la función paste()
parammod4=paste("beta",0:6,sep="")
parammod4
modelo4=regexponencial(respuesta=yt,data=X3,names.param=parammod4) #Ajuste del modelo
summary(modelo4)
ythat4=ts(fitted(modelo4),frequency=12,start=start(yt))
predict4=predict(modelo4,newdata=X3nuevo,interval="prediction") #solo da pronóstico puntual 
predict4=ts(predict4,frequency=12,start=start(ytf))
predict4
#serie de tiempo de los pronósticos puntuales
ytpron4=predict4

#Ajuste modelo 5 log polinomial de grado p=6
modelo5=lm(log(yt)~.,data=X3)
summary(modelo5)
#Por ser modelo log polinomial es necesario destransformar y aplicar factor de corrección exp(MSE/2)
ythat5=ts(exp(fitted(modelo5))*exp(summary(modelo5)$sigma^2/2),freq=12,start=start(yt))

#Por ser modelo log polinomial es necesario destransformar y aplicar factor de corrección
predict5=exp(predict(modelo5,newdata=X3nuevo,interval="prediction"))*exp(summary(modelo5)$sigma^2/2) #pronóstico puntual y por I.P modelo 5
predict5=ts(predict5,frequency=12,start=start(ytf))
predict5

#serie de tiempo de los pronósticos puntuales
ytpron5=predict5[,1]

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Comparación de los ajustes en una misma ventana gráfica
#----------------------------------------------------------------------------------------------------------------------------------------------------
#Gráficos de los ajustes
win.graph(width=15,height=10)
layout(rbind(c(1:3),c(4:6)))
plot(datos.indpro,lwd=2)
lines(ythat1,col=2,lwd=2)
legend("topleft",legend=c("Serie real","Serie Ajustada modelo 1"),col=c(1,2),lty=1,lwd=2)
plot(datos.indpro,lwd=2)
lines(ythat2,col=2,lwd=2)
legend("topleft",legend=c("Serie real","Serie Ajustada modelo 2"),col=c(1,2),lty=1,lwd=2)
plot(datos.indpro,lwd=2)
lines(ythat3,col=2,lwd=2)
legend("topleft",legend=c("Serie real","Serie Ajustada modelo 3"),col=c(1,2),lty=1,lwd=2)
plot(datos.indpro,lwd=2)
lines(ythat4,col=2,lwd=2)
legend("topleft",legend=c("Serie real","Serie Ajustada modelo 4"),col=c(1,2),lty=1,lwd=2)
plot(datos.indpro,lwd=2)
lines(ythat5,col=2,lwd=2)
legend("topleft",legend=c("Real","Serie Ajustada modelo 5"),col=c(1,2),lty=1,lwd=2)

#Cálculo AIC y BIC versión exp(Cn*(p))
#cálculo número de parámetros en cada modelo
nparmod1=length(coef(modelo1)[coef(modelo1)!=0]);nparmod1
nparmod2=length(coef(modelo2)[coef(modelo2)!=0]);nparmod2
nparmod3=length(coef(modelo3)[coef(modelo3)!=0]);nparmod3
nparmod4=length(coef(modelo4)[coef(modelo4)!=0]);nparmod4
nparmod5=length(coef(modelo5)[coef(modelo5)!=0]);nparmod5

#NOTA: En modelos con ajuste sin transformar los datos, residuals(modelo) calcula Yt-Ythat pero no en el modelo5
Criterios1=exp.crit.inf.resid(residuales=residuals(modelo1),n.par=nparmod1);Criterios1
Criterios2=exp.crit.inf.resid(residuales=residuals(modelo2),n.par=nparmod2);Criterios2
Criterios3=exp.crit.inf.resid(residuales=residuals(modelo3),n.par=nparmod3);Criterios3
Criterios4=exp.crit.inf.resid(residuales=residuals(modelo4),n.par=nparmod4);Criterios4

#En el modelo 5, debido a transformación log se usan los seudo-residuos en lugar de los residuos de ajuste
seudores5=yt-ythat5 #Seudo-residuos modelo5
Criterios5=exp.crit.inf.resid(residuales=seudores5,n.par=nparmod5);Criterios5

#Organizando en una tabla los valores de AIC y BIC
criterios=rbind(Criterios1,Criterios2,Criterios3,Criterios4,Criterios5)
rownames(criterios) = c("modelo1","modelo2","modelo3","modelo4","modelo5")
criterios


#----------------------------------------------------------------------------------------------------------------------------------------------------
#Comparación de residuales de todos los modelos
#----------------------------------------------------------------------------------------------------------------------------------------------------
#Gráfico de residuales comunes  versus tiempo, en una misma ventana gráfica
win.graph(width=15,height=10)
layout(rbind(c(1:3),c(4:6)))
plot.ts(residuals(modelo1),ylim=c(min(residuals(modelo1),-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),max(residuals(modelo1),-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma)))
abline(h=c(-2*summary(modelo1)$sigma,0,2*summary(modelo1)$sigma),col=2)
legend("bottomright",legend=c("Modelo 1"),lty=1,col=1,lwd=2)

plot.ts(residuals(modelo2),ylim=c(min(residuals(modelo2),-2*summary(modelo2)$sigma,2*summary(modelo2)$sigma),max(residuals(modelo2),-2*summary(modelo2)$sigma,2*summary(modelo2)$sigma)))
abline(h=c(-2*summary(modelo2)$sigma,0,2*summary(modelo2)$sigma),col=2)
legend("bottomright",legend=c("Modelo 2"),lty=1,col=1,lwd=2)

plot.ts(residuals(modelo3),ylim=c(min(residuals(modelo3),-2*summary(modelo3)$sigma,2*summary(modelo3)$sigma),max(residuals(modelo3),-2*summary(modelo3)$sigma,2*summary(modelo3)$sigma)))
abline(h=c(-2*summary(modelo3)$sigma,0,2*summary(modelo3)$sigma),col=2)
legend("topright",legend=c("Modelo 3"),lty=1,col=1,lwd=2)

plot.ts(residuals(modelo4),ylim=c(min(residuals(modelo4),-2*summary(modelo4)$sigma,2*summary(modelo4)$sigma),max(residuals(modelo4),-2*summary(modelo4)$sigma,2*summary(modelo4)$sigma)))
abline(h=c(-2*summary(modelo4)$sigma,0,2*summary(modelo4)$sigma),col=2)
legend("topright",legend=c("Modelo 4"),lty=1,col=1,lwd=2)

plot.ts(residuals(modelo5),ylim=c(min(residuals(modelo5),-2*summary(modelo5)$sigma,2*summary(modelo5)$sigma),max(residuals(modelo5),-2*summary(modelo5)$sigma,2*summary(modelo5)$sigma)))
abline(h=c(-2*summary(modelo5)$sigma,0,2*summary(modelo5)$sigma),col=2)
legend("topright",legend=c("Modelo 5"),lty=1,col=1,lwd=2)


#Gráfico de residuales comunes  versus variable respuesta, en una misma ventana gráfica
win.graph(width=15,height=10)
layout(rbind(c(1:3),c(4:6)))
plot(fitted(modelo1),residuals(modelo1),cex=1.1,ylim=c(min(residuals(modelo1),-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),max(residuals(modelo1),-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma)))
abline(h=c(-2*summary(modelo1)$sigma,0,2*summary(modelo1)$sigma),col=2)
legend("bottomright",legend=c("Modelo 1"),lty=1,col=1,lwd=2)

plot(fitted(modelo2),residuals(modelo2),cex=1.1,ylim=c(min(residuals(modelo2),-2*summary(modelo2)$sigma,2*summary(modelo2)$sigma),max(residuals(modelo2),-2*summary(modelo2)$sigma,2*summary(modelo2)$sigma)))
abline(h=c(-2*summary(modelo2)$sigma,0,2*summary(modelo2)$sigma),col=2)
legend("bottomright",legend=c("Modelo 2"),lty=1,col=1,lwd=2)

plot(fitted(modelo3),residuals(modelo3),cex=1.1,ylim=c(min(residuals(modelo3),-2*summary(modelo3)$sigma,2*summary(modelo3)$sigma),max(residuals(modelo3),-2*summary(modelo3)$sigma,2*summary(modelo3)$sigma)))
abline(h=c(-2*summary(modelo3)$sigma,0,2*summary(modelo3)$sigma),col=2)
legend("topright",legend=c("Modelo 3"),lty=1,col=1,lwd=2)

plot.ts(fitted(modelo4),residuals(modelo4),cex=1.1,ylim=c(min(residuals(modelo4),-2*summary(modelo4)$sigma,2*summary(modelo4)$sigma),max(residuals(modelo4),-2*summary(modelo4)$sigma,2*summary(modelo4)$sigma)))
abline(h=c(-2*summary(modelo4)$sigma,0,2*summary(modelo4)$sigma),col=2)
legend("topright",legend=c("Modelo 4"),lty=1,col=1,lwd=2)

plot(fitted(modelo5),residuals(modelo5),cex=1.1,ylim=c(min(residuals(modelo5),-2*summary(modelo5)$sigma,2*summary(modelo5)$sigma),max(residuals(modelo5),-2*summary(modelo5)$sigma,2*summary(modelo5)$sigma)))
abline(h=c(-2*summary(modelo5)$sigma,0,2*summary(modelo5)$sigma),col=2)
legend("topright",legend=c("Modelo 5"),lty=1,col=1,lwd=2)

#----------------------------------------------------------------------------------------------------------------------------------------------------
#Comparación de los pronósticos
#----------------------------------------------------------------------------------------------------------------------------------------------------
#Gráfico comparativo de pronósticos de modelos 1, 2, 3, 4, 5
plot(ytf,type="b",col=1,pch=19,ylim=c(min(ytf,ytpron1,ytpron2,ytpron3,ytpron4,ytpron5),max(ytf,ytpron1,ytpron2,ytpron3,ytpron4,ytpron5)),lwd=2,ylab="Ventas en dólares nominales",xaxt="n")
axis(1,at=time(ytf),labels=c("73.1","73.2","73.3","73.4","73.5","73.6","73.7","73.8","73.9","73.10","73.11","73.12"),cex.axis=0.8)
lines(ytpron1,col=2,pch=1,type="b",lwd=2)
lines(ytpron2,col=3,pch=2,type="b",lwd=2)
lines(ytpron3,col=4,pch=3,type="b",lwd=2)
lines(ytpron4,col=5,pch=4,type="b",lwd=2)
lines(ytpron5,col=6,pch=5,type="b",lwd=2)

legend("topleft",legend=c("Real","Modelo 1","Modelo 2","Modelo 3","Modelo 4","Modelo 5"),pch=c(19,1:5),col=c(1:6),lwd=2)

#precisión de los pronósticos por I.P del 95% de confianza
amplcob1=amplitud.cobertura(real=ytf,LIP=predict1[,2],LSP=predict1[,3]);amplcob1
amplcob2=amplitud.cobertura(real=ytf,LIP=predict2[,2],LSP=predict2[,3]);amplcob2
amplcob3=amplitud.cobertura(real=ytf,LIP=predict3[,2],LSP=predict3[,3]);amplcob3
amplcob5=amplitud.cobertura(real=ytf,LIP=predict5[,2],LSP=predict5[,3]);amplcob5

#Tabla de medidas de precisión de pronósticos
comp=rbind(accuracy(ytpron1,ytf),accuracy(ytpron2,ytf),accuracy(ytpron3,ytf),accuracy(ytpron4,ytf),accuracy(ytpron5,ytf))[,c(2,3,5)]
otros=rbind(amplcob1,amplcob2,amplcob3,c(NA,NA),amplcob5)

tablaprecis=cbind(comp,otros)
rownames(tablaprecis)=c("Modelo 1","Modelo 2","Modelo 3","Modelo 4","Modelo 5")
tablaprecis


