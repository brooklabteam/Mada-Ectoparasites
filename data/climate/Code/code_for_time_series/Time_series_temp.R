library(ggplot2)
library(lubridate)
library(cowplot)

# I creat my home work directory 
homewd="D:/EKIPA_FANIHY/2023_Ectoparasite_works/Climate_data/Mada-Ectoparasites/"

# creat the working directory
setwd(paste0(homewd,"Climate-data/Climate_tables"))

# call the data from the working directory
temp<-read.csv("Temp_point_AFA.csv")
names(temp)

# remove the useless columne and change the columne SITE_ID as name of the rows
temp<-temp[-1]
rownames(temp)<-temp$SITE_ID
view(temp)
# Transpose the data
A<-t(temp)


# and remove the first line (who become the names of each columne)
A<-A[-1,]

# save the new transposed data to a CSV files
write.csv(A,"temp_trasposed.csv")


# re-call the new data for the main work
temp<-read.csv("temp_trasposed.csv")
View(temp)
temp$date<-temp$X

unique(temp$X)

temp$date<-sub("Temp_pt_.2","2",temp$date)


class(temp$date)
View(temp)
# change the class as date using the package lubridate
temp$date <-lubridate::ym(temp$date)
temp$month<-lubridate::month(temp$date)
temp$year<-lubridate::year(temp$date)

write.csv(temp,"temp_trasposed.csv")
temp<-read.csv("temp_trasposed.csv")

names(temp)
k<-ggplot(data = temp)+
  geom_point(aes(x=date,y=AngavoKely),size=2.5,alpha=.2)+
  geom_line(aes(x=date,y=AngavoKely))+
  geom_smooth(aes(x=date,y=AngavoKely))+
  ylab("Humidity")+xlab("Year")+ggtitle("Angavokely (smooth=loess)")+
  theme_bw()+theme(axis.title.x = element_blank());k
z<-ggplot(data = temp)+
  geom_point(aes(x=date,y=Maromizaha),size=2.5,alpha=.2)+
  geom_line(aes(x=date,y=Maromizaha))+
  geom_smooth(aes(x=date,y=Maromizaha))+
  ylab("Humidity")+xlab("Year")+ggtitle("Maromizaha (smooth=loess)")+
  theme_bw()+theme(axis.text = element_text());z

b<-ggplot(data = temp)+
  geom_point(aes(x=date,y=AngavoBe),size=2.5,alpha=.2)+
  geom_line(aes(x=date,y=AngavoBe))+
  geom_smooth(aes(x=date,y=AngavoBe))+
  ylab("Humidity")+xlab("Year")+ggtitle("AngavoBe (smooth=loess)")+
  theme_bw()+theme(axis.title.x = element_blank());b

cowplot::plot_grid(k,z,nrow = 2)

ggsave(paste0(homewd,"/Other-figures/temp_by_years.png"),
       units = "mm",height = 60,width = 90, dpi = 1000,scale = 3)




names(temp)
k1<-ggplot(data = temp)+
  geom_point(aes(x=month,y=AngavoKely),size=2.5,shape=16, alpha=.3,color="forestgreen")+
  geom_smooth(aes(x=month,y=AngavoKely))+
  xlab("Month of the year (2013 jan-2020 mar)")+ ylab("Humidity")+
  ggtitle("Angavokely(loess)")+
  theme_bw()+theme(panel.grid = element_blank(),
                   axis.title.x = element_blank())+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));k1
k2<-ggplot(data = temp)+
  geom_point(aes(x=month,y=AngavoBe),size=2.5,shape=16, alpha=.3,color="forestgreen")+
  geom_smooth(aes(x=month,y=AngavoBe))+
  xlab("Month of the year (2013 jan-2020 mar)")+ ylab("Humidity")+
  ggtitle("Angavobe(loess)")+
  xlim(c(1,12))+
  theme_bw()+theme(panel.grid = element_blank(),
                   axis.title.x = element_blank())+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));k2


k3<-ggplot(data = temp)+
  geom_point(aes(x=month,y=Maromizaha),size=2.5,shape=16, alpha=.3,color="forestgreen")+
  geom_smooth(aes(x=month,y=Maromizaha))+
  ggtitle("Maromizaha(loess)")+
  xlab("Month of the year (2013 jan-2020 mar)")+ ylab("Humidity")+
  xlim(c(1,12))+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_x_continuous(breaks = seq(1,12,2),
                     labels = c("jan","march","may","july","sept","nov"));k3


cowplot::plot_grid(k1,k3,nrow = 2)

ggsave(paste0(homewd,"/Other-figures/temp_by_month.png"),
       units = "mm",height = 60,width = 90, dpi = 1000,scale = 3)



cowplot::plot_grid(k,k1,z,k3,ncol=2,nrow = 2)

ggsave(paste0(homewd,"/Other-figures/temp_by_month.png"),
       units = "mm",height = 60,width = 90, dpi = 1000,scale = 3)

write.csv(temp,"Humnday_trasposed.csv")
