setwd("C:/Users/USUARIO/Downloads")

library(raster)

nc <- brick('sst.mnmean (1).nc')

nc <- rotate(nc)
extent(nc)
e <- extent(-170,-120,-5,5)
#e <- extent(-169,-119,-4,4)
vals <- extract(nc , e , nl = nlayers(nc) )
vals <- colMeans( vals )
vals

str(vals)

total <- data.frame(vals)

Fecha <- seq.Date(as.Date("1854-01-01"),as.Date("2022-11-30"),by="month")
total <- data.frame(vals,Fecha)

total <- total[total$Fecha>="1936-01-01" & total$Fecha<="2020-12-31",]
View(total)
mes <- format(total$Fecha,'%m')
año <- format(total$Fecha,'%Y')

total <- data.frame(total,mes,año)

list <- list()
j<-1
for (i in seq(1936,1991,5)){
  list[[j]] <- total[total$año>=i & total$año<=i+29,] 
  j<- j+1
}



library(tidyverse)
list[[1]]
for (i in 1:length(list)){
  list[[i]] <- list[[i]] %>% group_by(mes) %>%
    summarise(clim = mean(vals))
}


data.frame(vals)

fin_d <- data.frame(vals)
View(list)
Fecha <- seq.Date(as.Date("1854-01-01"),as.Date("2022-11-30"),by="month")
fin_d<- data.frame(fin_d,Fecha)
fin_d<- fin_d[fin_d$Fecha>="1950-01-01",]
View(fin_d)
clima <-c()
for (i in 1:length(list)){
  clima <- rbind(clima,data.frame(climato=rep(c((list[[i]]$clim)),6)))
}


clima2 <- data.frame(climato=list[[12]]$clim[1:11])

clima <- rbind(clima,clima2)


fin_ct<- data.frame(fin_d,clima)
View(fin_ct2)



head(fin_ct)


fin_ct$anom <- fin_ct$vals-fin_ct$climato 

fin_ct2 <- fin_ct[,c(2,1,3,4)]

fin_ct$anom<- round(fin_ct$anom,2)



write.xlsx(
  fin_ct2,
  file="C:/Users/USUARIO/Desktop/TSM/anom.xlsx",
  col.names = T,
  row.names = F)


oni <-c()

for (i in 1:(length(fin_ct$anom)-2)){
  oni <- c(oni,((fin_ct$anom[i]+fin_ct$anom[i+1]+fin_ct$anom[i+2])/3))
}

#Falta el primer valor

oni <- c(mean(fin_ct$anom[1:2]),oni)

Fecha <- seq.Date(as.Date("1950-01-01"),as.Date("2022-10-30"),by="month")
Meses <- c("DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ")

fal<-length(oni)%/%12
ta <- length(oni)%%12

oni_fin <- data.frame(Fecha,Meses = c(rep(Meses,fal),Meses[1:ta]),oni)


View(oni_fin)
oni_fin$oni <- round(oni_fin$oni,2)

oni_fin

library(xlsx)
write.xlsx(
  oni_fin,
  file="C:/Users/USUARIO/Desktop/TSM/oni2.xlsx",
  col.names = T,
  row.names = F)

library(rsoi)       #Librería para descarga de índices
library(tidyverse)  #Manejo de datos

#Descarga indice oni
oni <- download_oni() 

#Método 1 : Grafica de barras
head(oni_fin)
oni

plot(oni$Date,oni$ONI,type="l",col="green")
lines(oni_fin$Fecha,oni_fin$oni,type="l",col="orange",add=T)


library(hrbrthemes)
#Nueva columna de datos
oni_fin$oni2<- oni_fin$oni
oni_fin$oni[oni_fin$oni > 0] <- 0
oni_fin$oni2[oni_fin$oni2 < 0] <- 0


ONI <-ggplot(oni_fin[oni_fin$Fecha>= "1980-01-01",], aes(Fecha,oni)) + 
  geom_ribbon(aes(ymin = 0, ymax = oni), fill = "dodgerblue2", alpha = .8)+geom_ribbon(aes(ymin = oni2, ymax = 0), fill = "orangered", alpha = .8)+
  labs(caption="Fuente: Elaboración propia con datos ERSST.v5 ",color="gray48")+theme_light()+
  theme (plot.title = element_text(hjust=0.5,family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3)))+
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray40", size=rel(1.3)))+
  ggtitle("Índice Niño Oceánico (ONI) \n 1950-2022")+xlab("Años")+geom_hline(yintercept = 0.5,colour="gray30",linetype=2)+
  geom_hline(yintercept = -0.5,colour="gray30",linetype=2)+ geom_text(aes(x = as.Date("2022-06-01"), y = -0.3,label = "La Niña"),stat = "unique",size=2.5)+
  geom_text(aes(x = as.Date("2022-06-01"), y = 0.7,label = "El Niño"),stat = "unique",size=2.5)+scale_x_date(date_labels = '%Y',date_breaks='4 years')+
  scale_y_continuous(breaks = seq(-2,2.5,0.5))+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )+ylab("ONI (ºC)")
