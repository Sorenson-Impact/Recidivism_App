library(animation)
library(ggplot2)
library(gganimate)
library(cowplot)
library(magick)
library(ggplot2)
library(gganimate)
library(tweenr)

ani.options(convert="c:program files/imagemagick/convert.exe")
sample<-read.csv("test3.csv")
splitSample<-split(sample,sample$month)
tween<-tween_states(splitSample,tweenlength = 2,statelength = 1 ,
                    ease= c('sine-in-out'),
                    nframes = 400)
p<-ggplot(tween,aes(x=x,y=y,frame=month,colour=Status))+
  geom_point()+labs(title="Month ", x="Months Free", y="Participants")+
  scale_color_manual(values =levels(tween$Status),labels=c("Free","In prison") )+
  guides(fill=guide_legend("State"))+
  geom_vline(xintercept = 0,linetype="dotdash")
m<-"Month:"
gg_animate(p,"output.mp4", interval=1/8, title_frame = ~ paste(m,round(., 0)))
