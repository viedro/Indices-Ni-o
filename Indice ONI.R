library(rsoi)       #Librería para descarga de índices
library(tidyverse)  #Manejo de datos

#Descarga indice oni
oni <- download_oni() 

#Método 1 : Grafica de barras

ggplot(data = oni, aes(x = Date, y = ONI, fill = phase),color=phase)+
#+geom_hline(yintercept = 1,linetype = 'dashed')+
  geom_col()+
  # ggsci::scale_fill_jama() +
  scale_fill_manual(values = c("dodgerblue2","aquamarine","orangered" ), name = "") +scale_x_date(date_labels = '%Y',date_breaks='4 years') +
  cowplot::theme_minimal_grid()+
  labs(x = "Años", y = "Índice ONI") +
  theme(legend.position = "top", panel.grid = element_line(linetype = "dotted"))+labs(caption="Fuente: Elaboración propia con datos de NOAA")+
  theme (plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3))) +
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray48", size=rel(1.3)))
#+geom_hline(yintercept = 1,linetype = 'dashed',color="gray48")
#+geom_text(aes(x = as.Date("2022-01-01"), y = 1.15,label = "El Niño"),size = 3.5, color = "gray48")


#Método 2 geom_ribbon

library(hrbrthemes)
#Nueva columna de datos
oni$ONI2<- oni$ONI
oni$ONI[oni$ONI > 0] <- 0
oni$ONI2[oni$ONI2 < 0] <- 0


ggplot(oni, aes(Date,ONI)) + 
  geom_ribbon(aes(ymin = 0, ymax = ONI), fill = "dodgerblue2", alpha = .8)+geom_ribbon(aes(ymin = ONI2, ymax = 0), fill = "orangered", alpha = .8)+
  labs(caption="Fuente: Elaboración propia con datos de la NOAA",color="gray48")+theme_ipsum()+
  theme (plot.title = element_text(hjust=0.5,family="Comic Sans MS",
                                   size=rel(1.5), 
                                   vjust=0.1, 
                                   face="bold.italic", 
                                   color="gray48", 
                                   lineheight=0.8))+theme(axis.title.x = element_text(face="plain", vjust=-0.1, colour="gray48", size=rel(1.3)))+
  theme(axis.title.y = element_text(face="plain", vjust=1.5, colour="gray40", size=rel(1.3)))+
  ggtitle("Índice Niño Oceánico (ONI) \n 1950-2022")+xlab("Años")+geom_hline(yintercept = 0.5,colour="gray30",linetype=2)+
  geom_hline(yintercept = -0.5,colour="gray30",linetype=2)+ geom_text(aes(x = as.Date("2022-05-01"), y = -0.3,label = "La Niña"),stat = "unique",size=2.5)+
  geom_text(aes(x = as.Date("2022-05-01"), y = 0.7,label = "El Niño"),stat = "unique",size=2.5)+scale_x_date(date_labels = '%Y',date_breaks='4 years')+
  scale_y_continuous(breaks = seq(-2,2.5,0.5))
