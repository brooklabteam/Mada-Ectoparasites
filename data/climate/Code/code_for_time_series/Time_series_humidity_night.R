library(ggplot2)
library(lubridate)
library(cowplot)

# I creat my home work directory 
homewd="D:/EKIPA_FANIHY/2023_Ectoparasite_works/Climate_data/Mada-Ectoparasites/"

# creat the working directory
setwd(paste0(homewd,"Climate-data/Climate_tables"))

# call the data from the working directory
Humnight<-read.csv("Humid_point_nighttime.csv")
names(Humnight)

# remove the useless columne and change the columne SITE_ID as name of the rows
Humnight<-Humnight[-1]
rownames(Humnight)<-Humnight$SITE_ID
view(Humnight)
# Transpose the data
A<-t(Humnight)
names(A)
View(A)

# and remove the first line (who become the names of each columne)
A<-A[-1,]

# save the new transposed data to a CSV files
write.csv(A,"Humnight_trasposed.csv")


# re-call the new data for the main work
Humnight<-read.csv("Humnight_trasposed.csv")
View(Humnight)
Humnight$date<-Humnight$X

unique(Humday$X)

Humnight$date<-sub("Humid_pt_nighttime_2","2",Humnight$date)


class(Humnight$date)
View(Humnight)
# change the class as date using the package lubridate
Humnight$date <-lubridate::ym(Humnight$date)
Humnight$month<-lubridate::month(Humnight$date)
Humnight$year<-lubridate::year(Humnight$date)


names(Humnight)
k<-ggplot(data = Humnight)+
  geom_point(aes(x=date,y=AngavoKely),size=2.5,alpha=.2)+
  geom_line(aes(x=date,y=AngavoKely))+
  geom_smooth(aes(x=date,y=AngavoKely))+
  ylab("Humidity")+xlab("Year")+ggtitle("Angavokely (smooth=loess)")+
  theme_bw()+theme(axis.title.x = element_blank());k
z<-ggplot(data = Humnight)+
  geom_point(aes(x=date,y=Maromizaha),size=2.5,alpha=.2)+
  geom_line(aes(x=date,y=Maromizaha))+
  geom_smooth(aes(x=date,y=Maromizaha))+
  ylab("Humidity")+xlab("Year")+ggtitle("Maromizaha (smooth=loess)")+
  theme_bw()+theme(axis.text = element_text());z

b<-ggplot(data = Humnight)+
  geom_point(aes(x=date,y=AngavoBe),size=2.5,alpha=.2)+
  geom_line(aes(x=date,y=AngavoBe))+
  geom_smooth(aes(x=date,y=AngavoBe))+
  ylab("Humidity")+xlab("Year")+ggtitle("AngavoBe (smooth=loess)")+
  theme_bw()+theme(axis.title.x = element_blank());b

cowplot::plot_grid(k,z,nrow = 2)

ggsave(paste0(homewd,"/Other-figures/Humnight_by_years.png"),
       units = "mm",height = 60,width = 90, dpi = 1000,scale = 3)




names(Humnight)
k1<-ggplot(data = Humnight)+
  geom_point(aes(x=month,y=AngavoKely),size=2.5,shape=16, alpha=.3,color="forestgreen")+
  geom_smooth(aes(x=month,y=AngavoKely))+
  xlab("Month of the year (2013 jan-2020 mar)")+ ylab("Humidity")+
  ggtitle("Angavokely(loess)")+
  theme_bw()+theme(panel.grid = element_blank(),
                   axis.title.x = element_blank())+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));k1
k2<-ggplot(data = Humnight)+
  geom_point(aes(x=month,y=AngavoBe),size=2.5,shape=16, alpha=.3,color="forestgreen")+
  geom_smooth(aes(x=month,y=AngavoBe))+
  xlab("Month of the year (2013 jan-2020 mar)")+ ylab("Humidity")+
  ggtitle("Angavobe(loess)")+
  xlim(c(1,12))+
  theme_bw()+theme(panel.grid = element_blank(),
                   axis.title.x = element_blank())+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));k2


k3<-ggplot(data = Humnight)+
  geom_point(aes(x=month,y=Maromizaha),size=2.5,shape=16, alpha=.3,color="forestgreen")+
  geom_smooth(aes(x=month,y=Maromizaha))+
  ggtitle("Maromizaha(loess")+
  xlab("Month of the year (2013 jan-2020 mar)")+ ylab("Humidity")+
  xlim(c(1,12))+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));k3


cowplot::plot_grid(k1,k3,nrow = 2)

ggsave(paste0(homewd,"/Other-figures/Humnight_by_month.png"),
       units = "mm",height = 60,width = 90, dpi = 1000,scale = 3)



cowplot::plot_grid(k,k1,z,k3,ncol=2,nrow = 2)

ggsave(paste0(homewd,"/Other-figures/Humnight_by_month.png"),
       units = "mm",height = 60,width = 90, dpi = 1000,scale = 3)
