rm(list=ls(envir=globalenv()), envir=globalenv())
gc()
gc()

library(tidyverse)
library(ggthemes)
library(ggpubr)
library(scales)
library(openxlsx)
library(export)

getwd()
setwd("C:/Users/miyaj/OneDrive - Tokyo University of Agriculture and Technology/ドキュメント/R/2022analysis")

#光合成経時プロット####
load("input/LN")
load("C:/Users/miyaj/OneDrive - Tokyo University of Agriculture and Technology/ドキュメント/R/2022_PRPrediction/01results/result.22")

#KTHPだけでいいかな
#BILもわんちゃん
pr=results
pr=pr %>% filter(LineName %in% c("Koshihikari","Takanari","HP-a","HP-b"))

#グラフの色
col.kos="#9E4B35" #コシヒカリの色 赤　J08-50V
col.tak="#26809E" #タカナリの色 青　J72-40T
col.hpa="#3E9E4B" #HP-aの色 緑　J46-60T
col.hpb="#512E9E" #HP-bの色 紫　J89-40T

colors=c(col.kos, col.tak, col.hpa, col.hpb)

#出穂日
head=pr%>%
  dplyr::group_by(LineName) %>%
  dplyr::summarise(hd=mean(Head.D))

#プロットのテンプレート[p_tpl]
Xrange=as.POSIXct(c("2022-07-01 JST", "2022-09-30 JST"))
Xlab="Date"
Yrange=c(-3, 63)
Yscale=seq(0, 60, 20)
Ylab=bquote(italic(A)~" (µmol" ~CO[2]~ m^-2~s^-1*")")

p_tpl=ggplot()+
  scale_x_datetime(expand=c(0.05, 0.05), breaks=seq(as.POSIXct("2022-07-01 00:00:00 JST"), 
                                                    as.POSIXct("2022-09-30 00:00:00 JST"), 
                                                    "1 month"), limits=Xrange, 
                   labels=date_format(format="%b/1", tz="Asia/Tokyo"))+ #x軸の範囲と目盛りの数
  coord_cartesian(expand=c(0, 0), ylim=Yrange)+
  ggtitle("zoom")+
  theme(aspect.ratio=0.8, 
        axis.line=element_line(linetype="blank", size=0.5), 
        axis.ticks=element_line(colour="black", size=0.5), 
        panel.border=element_rect(fill=NA, size=1),
        panel.background=element_rect(fill="transparent", color=NA),
        panel.grid.major=element_line(linetype="blank"), 
        panel.grid.minor=element_line(linetype="blank"), 
        plot.title=element_text(colour="#5D311D", family="Helvetica", hjust=0.5, vjust=1, face="bold", size=20),
        axis.title=element_text(colour="black", family="Helvetica", face="bold",size=14), 
        axis.text=element_text(colour="black", family="Helvetica", size=12), 
        plot.background=element_rect(fill="transparent", color=NA),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"),  #上、右、下、左
        legend.title=element_text(colour="#5D311D", family="Helvetica", size=8),
        legend.text=element_text(colour="#5D311D", family="Helvetica", size=8),
        legend.key=element_rect(fill=NA),
        legend.background=element_rect(fill=NA),
        legend.key.width=unit(0.8, "line"),
        legend.direction="vertical",
        legend.spacing.y = unit(0.5, 'mm'),
        legend.key.size = unit(3, "mm"),
        legend.position = "",
        strip.placement="", 
        strip.background=element_blank(),
        strip.text=element_text(colour="#5D311D", face="bold", family="Helvetica", size=16, 
                                hjust=0.5, vjust=0.5, margin=margin(0.1, 0.1, 0.1, 0.1)), #margin(top, right, bottom, left)
        panel.spacing=unit(0.5, "lines"))


#PredictionPlot
#pdf("02output/01_FuJo_my.pdf", height=7, width=10)
for (i in 135:135) {
  pr_ <- bind_cols(pr[c(1:25)],pr[PRE[i]])
  colnames(pr_)[colnames(pr_)==PRE[i]] <- "Model"
  sum <- bind_cols(aggregate(data=pr_,PhotoRate~date+LineName,FUN = mean),
                   aggregate(data=pr_,PhotoRate~date+LineName,FUN = sd),
                   aggregate(data=pr_,Model~date+LineName,FUN = mean),
                   aggregate(data=pr_,Model~date+LineName,FUN = sd))
  colnames(sum) <- c("date","LineName","Mean","date2","LineName2","SD","date3","LineName3","prem","date4","LineName4","preul")
  sum$year <- format(sum$date,format = "%Y")
  pr_$LineName <- factor(pr_$LineName,levels = LN_KTHP)
  sum$LineName <- factor(sum$LineName,levels = LN_KTHP)
  head$LineName <- factor(head$LineName,levels = LN_KTHP)
  
  p=p_tpl+
    geom_vline(data=head, aes(xintercept=hd, color=LineName), linetype="dashed", size=0.3)+ #出穂日
    geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
    geom_line(data=sum, aes(x=date, y=Mean), color="black", alpha=0.8, size=0.3)+
    geom_point(data=pr_, aes(x=date, y=PhotoRate), color="gray40", size=0.5)+
    geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
    geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
    geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
    scale_color_manual(values=colors)+
    scale_fill_manual(values=colors)+
    theme(axis.text.x=element_text(hjust=0.5))+
    labs(x=Xlab, y=Ylab, title=PRE[i])+
    facet_wrap(. ~ LineName, ncol =2,scales = "free")
  print(p)
}

graph2ppt(p,file="output/PR_KTHP_lineplot",
          height=7,
          width=10)

# ggsave(filename = "output/01_PR_KTHP_pre.jpeg",
#        height = 7,
#        width = 10,
#        dpi = 300)
dev.off()

p=p_tpl+
  geom_vline(data=head, aes(xintercept=hd, color=LineName), linetype="dashed", size=0.3)+ #出穂日
  geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
  geom_line(data=sum, aes(x=date, y=Mean), color="gray10", alpha=0.8, size=0.8)+
  geom_point(data=pr_, aes(x=date, y=PhotoRate), color="gray30", size=0.8)+
  # geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
  # geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
  # geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
  scale_color_manual(values=colors)+
  scale_fill_manual(values=colors)+
  theme(axis.text.x=element_text(hjust=0.5))+
  labs(x=Xlab, y=Ylab, title=PRE[i])+
  facet_wrap(. ~ LineName, ncol =2,scales = "free")
print(p)
graph2ppt(p,file="output/PR_KTHP_lineplot2",
          height=7,
          width=10)




#MeasurePlot
for (i in 135:135) {
  pr_ <- bind_cols(pr[c(1:25)],pr[PRE[i]])
  colnames(pr_)[colnames(pr_)==PRE[i]] <- "Model"
  sum <- bind_cols(aggregate(data=pr_,PhotoRate~date+LineName,FUN = mean),
                   aggregate(data=pr_,PhotoRate~date+LineName,FUN = sd),
                   aggregate(data=pr_,Model~date+LineName,FUN = mean),
                   aggregate(data=pr_,Model~date+LineName,FUN = sd))
  colnames(sum) <- c("date","LineName","Mean","date2","LineName2","SD","date3","LineName3","prem","date4","LineName4","preul")
  sum$year <- format(sum$date,format = "%Y")
  pr_$LineName <- factor(pr_$LineName,levels = LN_KTHP)
  sum$LineName <- factor(sum$LineName,levels = LN_KTHP)
  head$LineName <- factor(head$LineName,levels = LN_KTHP)
  
  p=p_tpl+
    geom_vline(data=head, aes(xintercept=hd, color=LineName), linetype="dashed", size=0.3)+ #出穂日
    geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
    geom_line(data=sum, aes(x=date, y=Mean), color="black", alpha=0.8, size=0.3)+
    geom_point(data=pr_, aes(x=date, y=PhotoRate), color="black", size=0.7,alpha=0.9,shape=16)+
    # geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
    # geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
    # geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
    scale_color_manual(values=colors)+
    scale_fill_manual(values=colors)+
    theme(axis.text.x=element_text(hjust=0.5))+
    labs(x=Xlab, y=Ylab, title=PRE[i])+
    facet_wrap(. ~ LineName, ncol =4,scales = "free")
  print(p)
}
ggsave(filename = "output/01_PR_KTHP_meas.jpeg",
       height = 7,
       width = 10,
       dpi = 300)
dev.off()

#BILやるかあ
pr=results
pr=pr[grep("BIL",pr$LineName),]

#グラフの色
# col.kos="#ff4b00" #コシヒカリの色 赤　J08-50V
# col.tak="#005aff" #タカナリの色 青　J72-40T
# col.hpa="#03af7a" #HP-aの色 緑　J46-60T
# col.hpb="#960099" #HP-bの色 紫　J89-40T

# colors=c(col.kos, col.tak, col.hpa, col.hpb)

#出穂日
head=pr%>%
  dplyr::group_by(LineName) %>%
  dplyr::summarise(hd=mean(Head.D))

#プロットのテンプレート[p_tpl]
Xrange=as.POSIXct(c("2022-07-01 JST", "2022-09-30 JST"))
Xlab="Date"
Yrange=c(-3, 63)
Yscale=seq(0, 60, 20)
Ylab=bquote(italic(A)~" (µmol" ~CO[2]~ m^-2~s^-1*")")

p_tpl=ggplot()+
  scale_x_datetime(expand=c(0.05, 0.05), breaks=seq(as.POSIXct("2022-07-01 00:00:00 JST"), 
                                                    as.POSIXct("2022-09-30 00:00:00 JST"), 
                                                    "1 month"), limits=Xrange, 
                   labels=date_format(format="%b/1", tz="Asia/Tokyo"))+ #x軸の範囲と目盛りの数
  coord_cartesian(expand=c(0, 0), ylim=Yrange)+
  ggtitle("zoom")+
  theme(aspect.ratio=0.8, 
        axis.line=element_line(linetype="blank", size=0.5), 
        axis.ticks=element_line(colour="black", size=0.5), 
        panel.border=element_rect(fill=NA, size=1),
        panel.background=element_rect(fill="transparent", color=NA),
        panel.grid.major=element_line(linetype="blank"), 
        panel.grid.minor=element_line(linetype="blank"), 
        plot.title=element_text(colour="#5D311D", family="Helvetica", hjust=0.5, vjust=1, face="bold", size=20),
        axis.title=element_text(colour="black", family="Helvetica", face="bold",size=14), 
        axis.text=element_text(colour="black", family="Helvetica", size=12), 
        plot.background=element_rect(fill="transparent", color=NA),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"),  #上、右、下、左
        legend.title=element_text(colour="#5D311D", family="Helvetica", size=8),
        legend.text=element_text(colour="#5D311D", family="Helvetica", size=8),
        legend.key=element_rect(fill=NA),
        legend.background=element_rect(fill=NA),
        legend.key.width=unit(0.8, "line"),
        legend.direction="vertical",
        legend.spacing.y = unit(0.5, 'mm'),
        legend.key.size = unit(3, "mm"),
        legend.position = "",
        strip.placement="", 
        strip.background=element_blank(),
        strip.text=element_text(colour="#5D311D", face="bold", family="Helvetica", size=16, 
                                hjust=0.5, vjust=0.5, margin=margin(0.1, 0.1, 0.1, 0.1)), #margin(top, right, bottom, left)
        panel.spacing=unit(0.5, "lines"))


#PredictionPlot
#pdf("02output/01_FuJo_my.pdf", height=7, width=10)
for (i in 135:135) {
  pr_ <- bind_cols(pr[c(1:25)],pr[PRE[i]])
  colnames(pr_)[colnames(pr_)==PRE[i]] <- "Model"
  sum <- bind_cols(aggregate(data=pr_,PhotoRate~date+LineName,FUN = mean),
                   aggregate(data=pr_,PhotoRate~date+LineName,FUN = sd),
                   aggregate(data=pr_,Model~date+LineName,FUN = mean),
                   aggregate(data=pr_,Model~date+LineName,FUN = sd))
  colnames(sum) <- c("date","LineName","Mean","date2","LineName2","SD","date3","LineName3","prem","date4","LineName4","preul")
  sum$year <- format(sum$date,format = "%Y")
  # pr_$LineName <- factor(pr_$LineName,levels = LN_KTHP)
  # sum$LineName <- factor(sum$LineName,levels = LN_KTHP)
  # head$LineName <- factor(head$LineName,levels = LN_KTHP)
  
  p=p_tpl+
    geom_vline(data=head, aes(xintercept=hd, color=LineName), linetype="dashed", size=0.3)+ #出穂日
    geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
    geom_line(data=sum, aes(x=date, y=Mean), color="black", alpha=0.8, size=0.3)+
    geom_point(data=pr_, aes(x=date, y=PhotoRate), color="black", size=0.7,alpha=0.5,shape=16)+
    geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
    geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
    geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
    # scale_color_manual(values=colors)+
    # scale_fill_manual(values=colors)+
    scale_colour_brewer(palette="Paired")+
    scale_fill_brewer(palette="Paired")+
    theme(axis.text.x=element_text(hjust=0.5))+
    labs(x=Xlab, y=Ylab, title=PRE[i])+
    facet_wrap(. ~ LineName, nrow = 2)
  print(p)
}
ggsave(filename = "output/03_PR_BIL_pre.jpeg",
       height = 7,
       width = 7,
       dpi = 300)
dev.off()

colors = rep("#9E4B35",length(unique(pr$LineName)))
for (i in 135:135) {
  pr_ <- bind_cols(pr[c(1:25)],pr[PRE[i]])
  colnames(pr_)[colnames(pr_)==PRE[i]] <- "Model"
  sum <- bind_cols(aggregate(data=pr_,PhotoRate~date+LineName,FUN = mean),
                   aggregate(data=pr_,PhotoRate~date+LineName,FUN = sd),
                   aggregate(data=pr_,Model~date+LineName,FUN = mean),
                   aggregate(data=pr_,Model~date+LineName,FUN = sd))
  colnames(sum) <- c("date","LineName","Mean","date2","LineName2","SD","date3","LineName3","prem","date4","LineName4","preul")
  sum$year <- format(sum$date,format = "%Y")
  # pr_$LineName <- factor(pr_$LineName,levels = LN_KTHP)
  # sum$LineName <- factor(sum$LineName,levels = LN_KTHP)
  # head$LineName <- factor(head$LineName,levels = LN_KTHP)
  
  p=p_tpl+
    geom_vline(data=head, aes(xintercept=hd, color=LineName), linetype="dashed", size=0.3)+ #出穂日
    geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
    geom_line(data=sum, aes(x=date, y=Mean), color="black", alpha=0.8, size=0.3)+
    geom_point(data=pr_, aes(x=date, y=PhotoRate), color="gray40", size=0.5)+
    geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
    geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
    geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
    scale_color_manual(values=colors)+
    scale_fill_manual(values=colors)+
    theme(axis.text.x=element_text(hjust=0.5))+
    labs(x=Xlab, y=Ylab, title=PRE[i])+
    facet_wrap(. ~ LineName, ncol = 2)
  print(p)
}
graph2ppt(p,file="output/PR_BIL_lineplot",
          height=7,
          width=10)
# ggsave(filename = "output/04_PR_BIL_pre.jpeg",
#        height = 7,
#        width = 7,
#        dpi = 300)
dev.off()

#MeasurePlot
for (i in 135:135) {
  pr_ <- bind_cols(pr[c(1:25)],pr[PRE[i]])
  colnames(pr_)[colnames(pr_)==PRE[i]] <- "Model"
  sum <- bind_cols(aggregate(data=pr_,PhotoRate~date+LineName,FUN = mean),
                   aggregate(data=pr_,PhotoRate~date+LineName,FUN = sd),
                   aggregate(data=pr_,Model~date+LineName,FUN = mean),
                   aggregate(data=pr_,Model~date+LineName,FUN = sd))
  colnames(sum) <- c("date","LineName","Mean","date2","LineName2","SD","date3","LineName3","prem","date4","LineName4","preul")
  sum$year <- format(sum$date,format = "%Y")
  # pr_$LineName <- factor(pr_$LineName,levels = LN_KTHP)
  # sum$LineName <- factor(sum$LineName,levels = LN_KTHP)
  # head$LineName <- factor(head$LineName,levels = LN_KTHP)
  
  p=p_tpl+
    geom_vline(data=head, aes(xintercept=hd),color="#9E4B35", linetype="dashed", size=0.3)+ #出穂日
    geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
    geom_line(data=sum, aes(x=date, y=Mean), color="gray10", alpha=0.8, size=0.8)+
    geom_point(data=pr_, aes(x=date, y=PhotoRate), color="gray30", size=0.8)+
    # geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
    # geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
    # geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
    # scale_color_manual(values=colors)+
    # scale_fill_manual(values=colors)+
    scale_colour_brewer(palette="Paired")+
    scale_fill_brewer(palette="Paired")+
    theme(axis.text.x=element_text(hjust=0.5))+
    labs(x=Xlab, y=Ylab, title=PRE[i])+
    facet_wrap(. ~ LineName, ncol = 2)
  print(p)
}
graph2ppt(p,file="output/PR_BIL_lineplot2",
          height=7,
          width=10)
# ggsave(filename = "output/02_PR_BIL_meas.jpeg",
#        height = 7,
#        width = 7,
#        dpi = 300)
dev.off()


#草丈経時プロット####
load("input/LN")
load("C:/Users/miyaj/OneDrive - Tokyo University of Agriculture and Technology/ドキュメント/R/2022_HeightPrediction/01results/PredictedResults_Height_FieldGC")

#KTHPだけでいいかな
#BILもわんちゃん
pr=results
pr=pr %>% filter(LineName %in% c("Koshihikari","Takanari","HP-a","HP-b"))

#グラフの色
col.kos="#9E4B35" #コシヒカリの色 赤　J08-50V
col.tak="#26809E" #タカナリの色 青　J72-40T
col.hpa="#3E9E4B" #HP-aの色 緑　J46-60T
col.hpb="#512E9E" #HP-bの色 紫　J89-40T

colors=c(col.kos, col.tak, col.hpa, col.hpb)

#出穂日
head=pr%>%
  dplyr::group_by(LineName) %>%
  dplyr::summarise(hd=mean(Head.D))

#プロットのテンプレート[p_tpl]
Xrange=as.POSIXct(c("2022-05-15 JST", "2022-08-30 JST"))
Xlab="Date"
Yrange=c(-3, 168)
Yscale=seq(0, 150, 50)
Ylab="Plant Height (cm)"

p_tpl=ggplot()+
  scale_x_datetime(expand=c(0.05, 0.05), breaks=seq(as.POSIXct("2022-05-15 00:00:00 JST"), 
                                                    as.POSIXct("2022-08-30 00:00:00 JST"), 
                                                    "1 month"), limits=Xrange, 
                   labels=date_format(format="%b/1", tz="Asia/Tokyo"))+ #x軸の範囲と目盛りの数
  coord_cartesian(expand=c(0, 0), ylim=Yrange)+
  ggtitle("zoom")+
  theme(aspect.ratio=0.8, 
        axis.line=element_line(linetype="blank", size=0.5), 
        axis.ticks=element_line(colour="black", size=0.5), 
        panel.border=element_rect(fill=NA, size=1),
        panel.background=element_rect(fill="transparent", color=NA),
        panel.grid.major=element_line(linetype="blank"), 
        panel.grid.minor=element_line(linetype="blank"), 
        plot.title=element_text(colour="#5D311D", family="Helvetica", hjust=0.5, vjust=1, face="bold", size=20),
        axis.title=element_text(colour="black", family="Helvetica", face="bold",size=14), 
        axis.text=element_text(colour="black", family="Helvetica", size=12), 
        plot.background=element_rect(fill="transparent", color=NA),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"),  #上、右、下、左
        legend.title=element_text(colour="#5D311D", family="Helvetica", size=8),
        legend.text=element_text(colour="#5D311D", family="Helvetica", size=8),
        legend.key=element_rect(fill=NA),
        legend.background=element_rect(fill=NA),
        legend.key.width=unit(0.8, "line"),
        legend.direction="vertical",
        legend.spacing.y = unit(0.5, 'mm'),
        legend.key.size = unit(3, "mm"),
        legend.position = "",
        strip.placement="", 
        strip.background=element_blank(),
        strip.text=element_text(colour="#5D311D", face="bold", family="Helvetica", size=16, 
                                hjust=0.5, vjust=0.5, margin=margin(0.1, 0.1, 0.1, 0.1)), #margin(top, right, bottom, left)
        panel.spacing=unit(0.5, "lines"))


#PredictionPlot
#pdf("02output/01_FuJo_my.pdf" )
for (i in 21) {
  pr_ <- bind_cols(pr[c(1:25)],pr[PRE[i]])
  colnames(pr_)[colnames(pr_)==PRE[i]] <- "Model"
  sum <- bind_cols(aggregate(data=pr_,Height~date+LineName,FUN = mean),
                   aggregate(data=pr_,Height~date+LineName,FUN = sd),
                   aggregate(data=pr_,Model~date+LineName,FUN = mean),
                   aggregate(data=pr_,Model~date+LineName,FUN = sd))
  colnames(sum) <- c("date","LineName","Mean","date2","LineName2","SD","date3","LineName3","prem","date4","LineName4","preul")
  sum$year <- format(sum$date,format = "%Y")
  pr_$LineName <- factor(pr_$LineName,levels = LN_KTHP)
  sum$LineName <- factor(sum$LineName,levels = LN_KTHP)
  head$LineName <- factor(head$LineName,levels = LN_KTHP)
  
  p=p_tpl+
    geom_vline(data=head, aes(xintercept=hd, color=LineName), linetype="dashed", size=0.3)+ #出穂日
    geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
    geom_line(data=sum, aes(x=date, y=Mean), color="black", alpha=0.8, size=0.3)+
    geom_point(data=pr_, aes(x=date, y=Height), color="gray40", size=0.5)+
    geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
    geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=1)+
    geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
    scale_color_manual(values=colors)+
    scale_fill_manual(values=colors)+
    theme(axis.text.x=element_text(hjust=0.5))+
    labs(x=Xlab, y=Ylab, title=PRE[i])+
    facet_wrap(. ~ LineName, ncol =2,scales = "free")
  print(p)
}
graph2ppt(p,file="output/Height_KTHP_lineplot",
          height=7,
          width=10)


ggsave(filename = "output/01_Height_KTHP_pre.jpeg",
       height = 7,
       width = 10,
       dpi = 300)
dev.off()


#MeasurePlot
for (i in 21) {
  pr_ <- bind_cols(pr[c(1:25)],pr[PRE[i]])
  colnames(pr_)[colnames(pr_)==PRE[i]] <- "Model"
  sum <- bind_cols(aggregate(data=pr_,Height~date+LineName,FUN = mean),
                   aggregate(data=pr_,Height~date+LineName,FUN = sd),
                   aggregate(data=pr_,Model~date+LineName,FUN = mean),
                   aggregate(data=pr_,Model~date+LineName,FUN = sd))
  colnames(sum) <- c("date","LineName","Mean","date2","LineName2","SD","date3","LineName3","prem","date4","LineName4","preul")
  sum$year <- format(sum$date,format = "%Y")
  pr_$LineName <- factor(pr_$LineName,levels = LN_KTHP)
  sum$LineName <- factor(sum$LineName,levels = LN_KTHP)
  head$LineName <- factor(head$LineName,levels = LN_KTHP)
  
  p=p_tpl+
    geom_vline(data=head, aes(xintercept=hd, color=LineName), linetype="dashed", size=0.3)+ #出穂日
    geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
    geom_line(data=sum, aes(x=date, y=Mean), color="gray10", alpha=0.8, size=0.8)+
    geom_point(data=pr_, aes(x=date, y=Height), color="gray30", size=0.8)+
    # geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
    # geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
    # geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
    scale_color_manual(values=colors)+
    scale_fill_manual(values=colors)+
    theme(axis.text.x=element_text(hjust=0.5))+
    labs(x=Xlab, y=Ylab, title=PRE[i])+
    facet_wrap(. ~ LineName, ncol =2,scales = "free")
  print(p)
}

graph2ppt(p,file="output/Height_KTHP_lineplot2",
          height=7,
          width=10)

ggsave(filename = "output/01_Height_KTHP_meas.jpeg",
       height = 7,
       width = 10,
       dpi = 300)
dev.off()

#BILやるかあ
pr=results
pr=pr[grep("BIL",pr$LineName),]

#出穂日
head=pr%>%
  dplyr::group_by(LineName) %>%
  dplyr::summarise(hd=mean(Head.D))

#プロットのテンプレート[p_tpl]
Xrange=as.POSIXct(c("2022-05-15 JST", "2022-08-30 JST"))
Xlab="Date"
Yrange=c(-3, 168)
Yscale=seq(0, 150, 50)
Ylab="Plant Height (cm)"

p_tpl=ggplot()+
  scale_x_datetime(expand=c(0.05, 0.05), breaks=seq(as.POSIXct("2022-05-15 00:00:00 JST"), 
                                                    as.POSIXct("2022-08-30 00:00:00 JST"), 
                                                    "1 month"), limits=Xrange, 
                   labels=date_format(format="%b/1", tz="Asia/Tokyo"))+ #x軸の範囲と目盛りの数
  coord_cartesian(expand=c(0, 0), ylim=Yrange)+
  ggtitle("zoom")+
  theme(aspect.ratio=0.8, 
        axis.line=element_line(linetype="blank", size=0.5), 
        axis.ticks=element_line(colour="black", size=0.5), 
        panel.border=element_rect(fill=NA, size=1),
        panel.background=element_rect(fill="transparent", color=NA),
        panel.grid.major=element_line(linetype="blank"), 
        panel.grid.minor=element_line(linetype="blank"), 
        plot.title=element_text(colour="#5D311D", family="Helvetica", hjust=0.5, vjust=1, face="bold", size=20),
        axis.title=element_text(colour="black", family="Helvetica", face="bold",size=14), 
        axis.text=element_text(colour="black", family="Helvetica", size=12), 
        plot.background=element_rect(fill="transparent", color=NA),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"),  #上、右、下、左
        legend.title=element_text(colour="#5D311D", family="Helvetica", size=8),
        legend.text=element_text(colour="#5D311D", family="Helvetica", size=8),
        legend.key=element_rect(fill=NA),
        legend.background=element_rect(fill=NA),
        legend.key.width=unit(0.8, "line"),
        legend.direction="vertical",
        legend.spacing.y = unit(0.5, 'mm'),
        legend.key.size = unit(3, "mm"),
        legend.position = "",
        strip.placement="", 
        strip.background=element_blank(),
        strip.text=element_text(colour="#5D311D", face="bold", family="Helvetica", size=16, 
                                hjust=0.5, vjust=0.5, margin=margin(0.1, 0.1, 0.1, 0.1)), #margin(top, right, bottom, left)
        panel.spacing=unit(0.5, "lines"))


#PredictionPlot
#pdf("02output/01_FuJo_my.pdf", height=7, width=10)
for (i in 21) {
  pr_ <- bind_cols(pr[c(1:25)],pr[PRE[i]])
  colnames(pr_)[colnames(pr_)==PRE[i]] <- "Model"
  sum <- bind_cols(aggregate(data=pr_,Height~date+LineName,FUN = mean),
                   aggregate(data=pr_,Height~date+LineName,FUN = sd),
                   aggregate(data=pr_,Model~date+LineName,FUN = mean),
                   aggregate(data=pr_,Model~date+LineName,FUN = sd))
  colnames(sum) <- c("date","LineName","Mean","date2","LineName2","SD","date3","LineName3","prem","date4","LineName4","preul")
  sum$year <- format(sum$date,format = "%Y")
  # pr_$LineName <- factor(pr_$LineName,levels = LN_KTHP)
  # sum$LineName <- factor(sum$LineName,levels = LN_KTHP)
  # head$LineName <- factor(head$LineName,levels = LN_KTHP)
  
  p=p_tpl+
    geom_vline(data=head, aes(xintercept=hd, color=LineName), linetype="dashed", size=0.3)+ #出穂日
    geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
    geom_line(data=sum, aes(x=date, y=Mean), color="black", alpha=0.8, size=0.3)+
    geom_point(data=pr_, aes(x=date, y=Height), color="gray40", size=0.5)+
    geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
    geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
    geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
    # scale_color_manual(values=colors)+
    # scale_fill_manual(values=colors)+
    scale_colour_brewer(palette="Paired")+
    scale_fill_brewer(palette="Paired")+
    theme(axis.text.x=element_text(hjust=0.5))+
    labs(x=Xlab, y=Ylab, title=PRE[i])+
    facet_wrap(. ~ LineName, ncol = 2)
  print(p)
}
ggsave(filename = "output/03_Height_BIL_pre.jpeg",
       height = 7,
       width = 7,
       dpi = 300)
dev.off()

colors = rep("#9E4B35",length(unique(pr$LineName)))
for (i in 21) {
  pr_ <- bind_cols(pr[c(1:25)],pr[PRE[i]])
  colnames(pr_)[colnames(pr_)==PRE[i]] <- "Model"
  sum <- bind_cols(aggregate(data=pr_,Height~date+LineName,FUN = mean),
                   aggregate(data=pr_,Height~date+LineName,FUN = sd),
                   aggregate(data=pr_,Model~date+LineName,FUN = mean),
                   aggregate(data=pr_,Model~date+LineName,FUN = sd))
  colnames(sum) <- c("date","LineName","Mean","date2","LineName2","SD","date3","LineName3","prem","date4","LineName4","preul")
  sum$year <- format(sum$date,format = "%Y")
  # pr_$LineName <- factor(pr_$LineName,levels = LN_KTHP)
  # sum$LineName <- factor(sum$LineName,levels = LN_KTHP)
  # head$LineName <- factor(head$LineName,levels = LN_KTHP)
  
  p=p_tpl+
    geom_vline(data=head, aes(xintercept=hd, color=LineName), linetype="dashed", size=0.3)+ #出穂日
    geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
    geom_line(data=sum, aes(x=date, y=Mean), color="black", alpha=0.8, size=0.3)+
    geom_point(data=pr_, aes(x=date, y=Height), color="gray40", size=0.5)+
    geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
    geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
    geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
    scale_color_manual(values=colors)+
    scale_fill_manual(values=colors)+
    theme(axis.text.x=element_text(hjust=0.5))+
    labs(x=Xlab, y=Ylab, title=PRE[i])+
    facet_wrap(. ~ LineName, ncol = 2)
  print(p)
}
graph2ppt(p,file="output/Height_BIL_lineplot",
          height=7,
          width=10)

ggsave(filename = "output/04_Height_BIL_pre.jpeg",
       height = 7,
       width = 7,
       dpi = 300)
dev.off()

#MeasurePlot
for (i in 21) {
  pr_ <- bind_cols(pr[c(1:25)],pr[PRE[i]])
  colnames(pr_)[colnames(pr_)==PRE[i]] <- "Model"
  sum <- bind_cols(aggregate(data=pr_,Height~date+LineName,FUN = mean),
                   aggregate(data=pr_,Height~date+LineName,FUN = sd),
                   aggregate(data=pr_,Model~date+LineName,FUN = mean),
                   aggregate(data=pr_,Model~date+LineName,FUN = sd))
  colnames(sum) <- c("date","LineName","Mean","date2","LineName2","SD","date3","LineName3","prem","date4","LineName4","preul")
  sum$year <- format(sum$date,format = "%Y")
  # pr_$LineName <- factor(pr_$LineName,levels = LN_KTHP)
  # sum$LineName <- factor(sum$LineName,levels = LN_KTHP)
  # head$LineName <- factor(head$LineName,levels = LN_KTHP)
  
  p=p_tpl+
    geom_vline(data=head, aes(xintercept=hd),color="#9E4B35", linetype="dashed", size=0.3)+ #出穂日
    geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
    geom_line(data=sum, aes(x=date, y=Mean), color="black", alpha=0.8, size=0.3)+
    geom_point(data=pr_, aes(x=date, y=Height), color="gray30", size=0.8)+
    # geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
    # geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
    # geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
    # scale_color_manual(values=colors)+
    # scale_fill_manual(values=colors)+
    scale_colour_brewer(palette="Paired")+
    scale_fill_brewer(palette="Paired")+
    theme(axis.text.x=element_text(hjust=0.5))+
    labs(x=Xlab, y=Ylab, title=PRE[i])+
    facet_wrap(. ~ LineName, ncol = 2)
  print(p)
}
graph2ppt(p,file="output/Height_BIL_lineplot2",
          height=7,
          width=10)

ggsave(filename = "output/02_Height_BIL_meas.jpeg",
       height = 7,
       width = 7,
       dpi = 300)
dev.off()


#SPAD経時プロット####
load("input/LN")
load("C:/Users/miyaj/OneDrive - Tokyo University of Agriculture and Technology/ドキュメント/R/2022_PRPrediction/03results/PredictedResults_SPAD_FieldGC")

#KTHPだけでいいかな
#BILもわんちゃん
pr=results
pr=pr %>% filter(LineName %in% c("Koshihikari","Takanari","HP-a","HP-b"))

#グラフの色
col.kos="#9E4B35" #コシヒカリの色 赤　J08-50V
col.tak="#26809E" #タカナリの色 青　J72-40T
col.hpa="#3E9E4B" #HP-aの色 緑　J46-60T
col.hpb="#512E9E" #HP-bの色 紫　J89-40T

colors=c(col.kos, col.tak, col.hpa, col.hpb)

#出穂日
head=pr%>%
  dplyr::group_by(LineName) %>%
  dplyr::summarise(hd=mean(Head.D))

#プロットのテンプレート[p_tpl]
Xrange=as.POSIXct(c("2022-07-01 JST", "2022-09-30 JST"))
Xlab="Date"
Yrange=c(-3, 60)
Yscale=seq(0, 60, 20)
Ylab="SPAD"

p_tpl=ggplot()+
  scale_x_datetime(expand=c(0.05, 0.05), breaks=seq(as.POSIXct("2022-07-01 00:00:00 JST"), 
                                                    as.POSIXct("2022-09-30 00:00:00 JST"), 
                                                    "1 month"), limits=Xrange, 
                   labels=date_format(format="%b/1", tz="Asia/Tokyo"))+ #x軸の範囲と目盛りの数
  coord_cartesian(expand=c(0, 0), ylim=Yrange)+
  ggtitle("zoom")+
  theme(aspect.ratio=0.8, 
        axis.line=element_line(linetype="blank", size=0.5), 
        axis.ticks=element_line(colour="black", size=0.5), 
        panel.border=element_rect(fill=NA, size=1),
        panel.background=element_rect(fill="transparent", color=NA),
        panel.grid.major=element_line(linetype="blank"), 
        panel.grid.minor=element_line(linetype="blank"), 
        plot.title=element_text(colour="#5D311D", family="Helvetica", hjust=0.5, vjust=1, face="bold", size=20),
        axis.title=element_text(colour="black", family="Helvetica", face="bold",size=14), 
        axis.text=element_text(colour="black", family="Helvetica", size=12), 
        plot.background=element_rect(fill="transparent", color=NA),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"),  #上、右、下、左
        legend.title=element_text(colour="#5D311D", family="Helvetica", size=8),
        legend.text=element_text(colour="#5D311D", family="Helvetica", size=8),
        legend.key=element_rect(fill=NA),
        legend.background=element_rect(fill=NA),
        legend.key.width=unit(0.8, "line"),
        legend.direction="vertical",
        legend.spacing.y = unit(0.5, 'mm'),
        legend.key.size = unit(3, "mm"),
        legend.position = "",
        strip.placement="", 
        strip.background=element_blank(),
        strip.text=element_text(colour="#5D311D", face="bold", family="Helvetica", size=16, 
                                hjust=0.5, vjust=0.5, margin=margin(0.1, 0.1, 0.1, 0.1)), #margin(top, right, bottom, left)
        panel.spacing=unit(0.5, "lines"))


#PredictionPlot
#pdf("02output/01_FuJo_my.pdf", height=7, width=10)
for (i in 21) {
  pr_ <- bind_cols(pr[c(1:25)],pr[PRE[i]])
  colnames(pr_)[colnames(pr_)==PRE[i]] <- "Model"
  sum <- bind_cols(aggregate(data=pr_,SPAD~date+LineName,FUN = mean),
                   aggregate(data=pr_,SPAD~date+LineName,FUN = sd),
                   aggregate(data=pr_,Model~date+LineName,FUN = mean),
                   aggregate(data=pr_,Model~date+LineName,FUN = sd))
  colnames(sum) <- c("date","LineName","Mean","date2","LineName2","SD","date3","LineName3","prem","date4","LineName4","preul")
  sum$year <- format(sum$date,format = "%Y")
  pr_$LineName <- factor(pr_$LineName,levels = LN_KTHP)
  sum$LineName <- factor(sum$LineName,levels = LN_KTHP)
  head$LineName <- factor(head$LineName,levels = LN_KTHP)
  
  p=p_tpl+
    geom_vline(data=head, aes(xintercept=hd, color=LineName), linetype="dashed", size=0.3)+ #出穂日
    geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
    geom_line(data=sum, aes(x=date, y=Mean), color="black", alpha=0.8, size=0.3)+
    geom_point(data=pr_, aes(x=date, y=SPAD), color="gray40", size=0.5)+
    geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
    geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
    geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
    scale_color_manual(values=colors)+
    scale_fill_manual(values=colors)+
    theme(axis.text.x=element_text(hjust=0.5))+
    labs(x=Xlab, y=Ylab, title=PRE[i])+
    facet_wrap(. ~ LineName, ncol =2,scales = "free")
  print(p)
}

graph2ppt(p,file="output/SPAD_KTHP_lineplot",
          height=7,
          width=10)

ggsave(filename = "output/01_SPAD_KTHP_pre.jpeg",
       height = 7,
       width = 10,
       dpi = 300)
dev.off()

p=p_tpl+
  geom_vline(data=head, aes(xintercept=hd, color=LineName), linetype="dashed", size=0.3)+ #出穂日
  geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
  geom_line(data=sum, aes(x=date, y=Mean), color="gray10", alpha=0.8, size=0.8)+
  geom_point(data=pr_, aes(x=date, y=SPAD), color="gray30", size=0.8)+
  # geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
  # geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
  # geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
  scale_color_manual(values=colors)+
  scale_fill_manual(values=colors)+
  theme(axis.text.x=element_text(hjust=0.5))+
  labs(x=Xlab, y=Ylab, title=PRE[i])+
  facet_wrap(. ~ LineName, ncol =2,scales = "free")
print(p)
graph2ppt(p,file="output/SPAD_KTHP_lineplot2",
          height=7,
          width=10)




# #MeasurePlot
# for (i in 21) {
#   pr_ <- bind_cols(pr[c(1:25)],pr[PRE[i]])
#   colnames(pr_)[colnames(pr_)==PRE[i]] <- "Model"
#   sum <- bind_cols(aggregate(data=pr_,PhotoRate~date+LineName,FUN = mean),
#                    aggregate(data=pr_,PhotoRate~date+LineName,FUN = sd),
#                    aggregate(data=pr_,Model~date+LineName,FUN = mean),
#                    aggregate(data=pr_,Model~date+LineName,FUN = sd))
#   colnames(sum) <- c("date","LineName","Mean","date2","LineName2","SD","date3","LineName3","prem","date4","LineName4","preul")
#   sum$year <- format(sum$date,format = "%Y")
#   pr_$LineName <- factor(pr_$LineName,levels = LN_KTHP)
#   sum$LineName <- factor(sum$LineName,levels = LN_KTHP)
#   head$LineName <- factor(head$LineName,levels = LN_KTHP)
#   
#   p=p_tpl+
#     geom_vline(data=head, aes(xintercept=hd, color=LineName), linetype="dashed", size=0.3)+ #出穂日
#     geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
#     geom_line(data=sum, aes(x=date, y=Mean), color="black", alpha=0.8, size=0.3)+
#     geom_point(data=pr_, aes(x=date, y=PhotoRate), color="black", size=0.7,alpha=0.9,shape=16)+
#     # geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
#     # geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
#     # geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
#     scale_color_manual(values=colors)+
#     scale_fill_manual(values=colors)+
#     theme(axis.text.x=element_text(hjust=0.5))+
#     labs(x=Xlab, y=Ylab, title=PRE[i])+
#     facet_wrap(. ~ LineName, ncol =4,scales = "free")
#   print(p)
# }
ggsave(filename = "output/01_SPAD_KTHP_meas.jpeg",
       height = 7,
       width = 10,
       dpi = 300)
dev.off()

#BILやるかあ
pr=results
pr=pr[grep("BIL",pr$LineName),]

#グラフの色
# col.kos="#ff4b00" #コシヒカリの色 赤　J08-50V
# col.tak="#005aff" #タカナリの色 青　J72-40T
# col.hpa="#03af7a" #HP-aの色 緑　J46-60T
# col.hpb="#960099" #HP-bの色 紫　J89-40T

# colors=c(col.kos, col.tak, col.hpa, col.hpb)

#出穂日
head=pr%>%
  dplyr::group_by(LineName) %>%
  dplyr::summarise(hd=mean(Head.D))

#プロットのテンプレート[p_tpl]
Xrange=as.POSIXct(c("2022-07-01 JST", "2022-09-30 JST"))
Xlab="Date"
Yrange=c(-3, 60)
Yscale=seq(0, 60, 20)
Ylab="SPAD"
p_tpl=ggplot()+
  scale_x_datetime(expand=c(0.05, 0.05), breaks=seq(as.POSIXct("2022-07-01 00:00:00 JST"), 
                                                    as.POSIXct("2022-09-30 00:00:00 JST"), 
                                                    "1 month"), limits=Xrange, 
                   labels=date_format(format="%b/1", tz="Asia/Tokyo"))+ #x軸の範囲と目盛りの数
  coord_cartesian(expand=c(0, 0), ylim=Yrange)+
  ggtitle("zoom")+
  theme(aspect.ratio=0.8, 
        axis.line=element_line(linetype="blank", size=0.5), 
        axis.ticks=element_line(colour="black", size=0.5), 
        panel.border=element_rect(fill=NA, size=1),
        panel.background=element_rect(fill="transparent", color=NA),
        panel.grid.major=element_line(linetype="blank"), 
        panel.grid.minor=element_line(linetype="blank"), 
        plot.title=element_text(colour="#5D311D", family="Helvetica", hjust=0.5, vjust=1, face="bold", size=20),
        axis.title=element_text(colour="black", family="Helvetica", face="bold",size=14), 
        axis.text=element_text(colour="black", family="Helvetica", size=12), 
        plot.background=element_rect(fill="transparent", color=NA),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"),  #上、右、下、左
        legend.title=element_text(colour="#5D311D", family="Helvetica", size=8),
        legend.text=element_text(colour="#5D311D", family="Helvetica", size=8),
        legend.key=element_rect(fill=NA),
        legend.background=element_rect(fill=NA),
        legend.key.width=unit(0.8, "line"),
        legend.direction="vertical",
        legend.spacing.y = unit(0.5, 'mm'),
        legend.key.size = unit(3, "mm"),
        legend.position = "",
        strip.placement="", 
        strip.background=element_blank(),
        strip.text=element_text(colour="#5D311D", face="bold", family="Helvetica", size=16, 
                                hjust=0.5, vjust=0.5, margin=margin(0.1, 0.1, 0.1, 0.1)), #margin(top, right, bottom, left)
        panel.spacing=unit(0.5, "lines"))


#PredictionPlot
# #pdf("02output/01_FuJo_my.pdf", height=7, width=10)
# for (i in 21) {
#   pr_ <- bind_cols(pr[c(1:25)],pr[PRE[i]])
#   colnames(pr_)[colnames(pr_)==PRE[i]] <- "Model"
#   sum <- bind_cols(aggregate(data=pr_,SPAD~date+LineName,FUN = mean),
#                    aggregate(data=pr_,SPAD~date+LineName,FUN = sd),
#                    aggregate(data=pr_,Model~date+LineName,FUN = mean),
#                    aggregate(data=pr_,Model~date+LineName,FUN = sd))
#   colnames(sum) <- c("date","LineName","Mean","date2","LineName2","SD","date3","LineName3","prem","date4","LineName4","preul")
#   sum$year <- format(sum$date,format = "%Y")
#   # pr_$LineName <- factor(pr_$LineName,levels = LN_KTHP)
#   # sum$LineName <- factor(sum$LineName,levels = LN_KTHP)
#   # head$LineName <- factor(head$LineName,levels = LN_KTHP)
#   
#   p=p_tpl+
#     geom_vline(data=head, aes(xintercept=hd, color=LineName), linetype="dashed", size=0.3)+ #出穂日
#     geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
#     geom_line(data=sum, aes(x=date, y=Mean), color="black", alpha=0.8, size=0.3)+
#     geom_point(data=pr_, aes(x=date, y=SPAD), color="black", size=0.7,alpha=0.5,shape=16)+
#     geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
#     geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
#     geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
#     # scale_color_manual(values=colors)+
#     # scale_fill_manual(values=colors)+
#     scale_colour_brewer(palette="Paired")+
#     scale_fill_brewer(palette="Paired")+
#     theme(axis.text.x=element_text(hjust=0.5))+
#     labs(x=Xlab, y=Ylab, title=PRE[i])+
#     facet_wrap(. ~ LineName, ncol = 2)
#   print(p)
# }
# ggsave(filename = "output/03_PR_BIL_pre.jpeg",
#        height = 7,
#        width = 7,
#        dpi = 300)
# dev.off()

colors = rep("#9E4B35",length(unique(pr$LineName)))
for (i in 21) {
  pr_ <- bind_cols(pr[c(1:25)],pr[PRE[i]])
  colnames(pr_)[colnames(pr_)==PRE[i]] <- "Model"
  sum <- bind_cols(aggregate(data=pr_,SPAD~date+LineName,FUN = mean),
                   aggregate(data=pr_,SPAD~date+LineName,FUN = sd),
                   aggregate(data=pr_,Model~date+LineName,FUN = mean),
                   aggregate(data=pr_,Model~date+LineName,FUN = sd))
  colnames(sum) <- c("date","LineName","Mean","date2","LineName2","SD","date3","LineName3","prem","date4","LineName4","preul")
  sum$year <- format(sum$date,format = "%Y")
  # pr_$LineName <- factor(pr_$LineName,levels = LN_KTHP)
  # sum$LineName <- factor(sum$LineName,levels = LN_KTHP)
  # head$LineName <- factor(head$LineName,levels = LN_KTHP)
  
  p=p_tpl+
    geom_vline(data=head, aes(xintercept=hd, color=LineName), linetype="dashed", size=0.3)+ #出穂日
    geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
    geom_line(data=sum, aes(x=date, y=Mean), color="black", alpha=0.8, size=0.3)+
    geom_point(data=pr_, aes(x=date, y=SPAD), color="gray40", size=0.5)+
    geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
    geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
    geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
    scale_color_manual(values=colors)+
    scale_fill_manual(values=colors)+
    theme(axis.text.x=element_text(hjust=0.5))+
    labs(x=Xlab, y=Ylab, title=PRE[i])+
    facet_wrap(. ~ LineName, ncol = 2)
  print(p)
}
graph2ppt(p,file="output/SPAD_BIL_lineplot",
          height=7,
          width=10)
ggsave(filename = "output/03_SPAD_BIL_pre.jpeg",
       height = 7,
       width = 7,
       dpi = 300)
dev.off()

#MeasurePlot
for (i in 21) {
  pr_ <- bind_cols(pr[c(1:25)],pr[PRE[i]])
  colnames(pr_)[colnames(pr_)==PRE[i]] <- "Model"
  sum <- bind_cols(aggregate(data=pr_,SPAD~date+LineName,FUN = mean),
                   aggregate(data=pr_,SPAD~date+LineName,FUN = sd),
                   aggregate(data=pr_,Model~date+LineName,FUN = mean),
                   aggregate(data=pr_,Model~date+LineName,FUN = sd))
  colnames(sum) <- c("date","LineName","Mean","date2","LineName2","SD","date3","LineName3","prem","date4","LineName4","preul")
  sum$year <- format(sum$date,format = "%Y")
  # pr_$LineName <- factor(pr_$LineName,levels = LN_KTHP)
  # sum$LineName <- factor(sum$LineName,levels = LN_KTHP)
  # head$LineName <- factor(head$LineName,levels = LN_KTHP)
  
  p=p_tpl+
    geom_vline(data=head, aes(xintercept=hd),color="#9E4B35", linetype="dashed", size=0.3)+ #出穂日
    geom_ribbon(data=sum, aes(x=date, ymax=Mean+SD, ymin=Mean-SD), fill="gray80")+
    geom_line(data=sum, aes(x=date, y=Mean), color="gray10", alpha=0.8, size=0.8)+
    geom_point(data=pr_, aes(x=date, y=SPAD), color="gray30", size=0.8)+
    # geom_ribbon(data=sum, aes(x=date, ymax=prem+preul, ymin=prem-preul, fill=LineName), alpha=0.4)+
    # geom_line(data=sum, aes(x=date, y=prem, color=LineName), alpha=0.8, size=0.6)+
    # geom_point(data=pr_, aes(x=date, y=Model, color=LineName), size=0.9)+
    # scale_color_manual(values=colors)+
    # scale_fill_manual(values=colors)+
    scale_colour_brewer(palette="Paired")+
    scale_fill_brewer(palette="Paired")+
    theme(axis.text.x=element_text(hjust=0.5))+
    labs(x=Xlab, y=Ylab, title=PRE[i])+
    facet_wrap(. ~ LineName, ncol = 2)
  print(p)
}
graph2ppt(p,file="output/SPAD_BIL_lineplot2",
          height=7,
          width=10)
# ggsave(filename = "output/02_PR_BIL_meas.jpeg",
#        height = 7,
#        width = 7,
#        dpi = 300)
dev.off()





rm(list=ls(envir=globalenv()), envir=globalenv())
gc()
gc()

#光合成相関プロット####

library(tidyverse)
library(ggthemes)
library(ggpubr)
library(scales)
library(export)

load("input/LN")
load("C:/Users/miyaj/OneDrive - Tokyo University of Agriculture and Technology/ドキュメント/R/2022_PRPrediction/01results/result.22")

#KTHP
pr=results
pr=pr %>% filter(LineName %in% c("Koshihikari","Takanari","HP-a","HP-b"))
pr$LineName=factor(pr$LineName,levels = c("Koshihikari","Takanari","HP-a","HP-b"))

#グラフの色
col.kos="#9E4B35" #コシヒカリの色 赤　J08-50V
col.tak="#26809E" #タカナリの色 青　J72-40T
col.hpa="#3E9E4B" #HP-aの色 緑　J46-60T
col.hpb="#512E9E" #HP-bの色 紫　J89-40T
colors=c(col.kos, col.tak, col.hpa, col.hpb)

Xlab=bquote("Measured "~italic(A)~" (µmol" ~CO[2]~ m^-2~s^-1*")")
Ylab=bquote("Predicted "~italic(A)~" (µmol" ~CO[2]~ m^-2~s^-1*")")
Xrange=seq(0, 60, 20)
Yrange=seq(0, 100000, 20)

p_tpl <- ggplot()+
  theme(aspect.ratio=1, 
        axis.line=element_line(linetype="blank", size=0.5), 
        axis.ticks=element_line(colour="black", size=0.5), 
        panel.border=element_rect(fill=NA, size=1),
        panel.background=element_rect(fill="transparent", color=NA),
        panel.grid.major=element_line(linetype="blank"), 
        panel.grid.minor=element_line(linetype="blank"), 
        plot.title=element_text(colour="#5D311D", family="Helvetica", hjust=0.5, vjust=1, face="bold", size=20),
        axis.title=element_text(colour="black", family="Helvetica", face="bold",size=8), 
        axis.text=element_text(colour="black", family="Helvetica", size=8), 
        legend.position = "",
        plot.background=element_rect(fill="transparent", color=NA),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"),  #上、右、下、左
        strip.placement="", 
        strip.background=element_blank(),
        strip.text=element_text(colour="#5D311D", face="bold", family="Helvetica", size=16, 
                                hjust=0.5, vjust=0.5, margin=margin(0.1, 0.1, 0.1, 0.1)), #margin(top, right, bottom, left)
        panel.spacing=unit(0.5, "lines"))


for (i in 135:135) {
  tmp <- bind_cols(pr[1:25],pr[colnames(pr)==PRE[i]])
  colnames(tmp)[colnames(tmp)==PRE[i]] <- "Model"
  tmp[!tmp$Model==0,]

  p <- p_tpl+
    geom_abline(slope = 1,intercept = 0,color = "gray20")+
    geom_point(data = tmp,
               mapping = aes(x = PhotoRate,y = Model,group = LineName,color = LineName),
               stat = "identity",
               size = 1,
               alpha = 0.8,
               shape = 16)+
    geom_smooth(data = tmp,
                mapping = aes(x = PhotoRate,y = Model,group = LineName,color = LineName),
                method = "lm",
                formula = y~x,
                stat = "smooth",
                se = FALSE,
                size = 1.2,
                alpha = 0.6)+
    scale_color_manual(values = colors)+
    scale_x_continuous(breaks = Xrange,limits = c(0,50))+
    scale_y_continuous(breaks = Yrange,limits = c(0,50))+
    labs(x = Xlab,y = Ylab,title = PRE[i])+
    facet_wrap(~LineName,ncol = 2,scales = "free")
  print(p)
}
graph2ppt(p,file="output/PR_KTHP_corplot",
          height=4,width=4)

ggsave(filename = "output/01_PRplot_KTHP.jpeg",
       height = 2.8,
       width = 2.8,
       dpi = 500)

dev.off()


#BIL
pr=results
pr=pr[grep("BIL",pr$LineName),]

Xlab=bquote("Measured "~italic(A)~" (µmol" ~CO[2]~ m^-2~s^-1*")")
Ylab=bquote("Predicted "~italic(A)~" (µmol" ~CO[2]~ m^-2~s^-1*")")
Xrange=seq(0, 60, 20)
Yrange=seq(0, 100000, 20)

p_tpl <- ggplot()+
  theme(aspect.ratio=1, 
        axis.line=element_line(linetype="blank", size=0.5), 
        axis.ticks=element_line(colour="black", size=0.5), 
        panel.border=element_rect(fill=NA, size=1),
        panel.background=element_rect(fill="transparent", color=NA),
        panel.grid.major=element_line(linetype="blank"), 
        panel.grid.minor=element_line(linetype="blank"), 
        plot.title=element_text(colour="#5D311D", family="Helvetica", hjust=0.5, vjust=1, face="bold", size=20),
        axis.title=element_text(colour="black", family="Helvetica", face="bold",size=8), 
        axis.text=element_text(colour="black", family="Helvetica", size=8), 
        legend.position = "",
        plot.background=element_rect(fill="transparent", color=NA),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"),  #上、右、下、左
        strip.placement="", 
        strip.background=element_blank(),
        strip.text=element_text(colour="#5D311D", face="bold", family="Helvetica", size=16, 
                                hjust=0.5, vjust=0.5, margin=margin(0.1, 0.1, 0.1, 0.1)), #margin(top, right, bottom, left)
        panel.spacing=unit(0.5, "lines"))


colors = rep("#9E4B35",length(unique(pr$LineName)))
for (i in 135:135) {
  tmp <- bind_cols(pr[1:25],pr[colnames(pr)==PRE[i]])
  colnames(tmp)[colnames(tmp)==PRE[i]] <- "Model"
  tmp[!tmp$Model==0,]
  
  p <- p_tpl+
    geom_abline(slope = 1,intercept = 0,color = "gray20")+
    geom_point(data = tmp,
               mapping = aes(x = PhotoRate,y = Model,group = LineName,color = LineName),
               stat = "identity",
               size = 1,
               alpha = 0.8,
               shape = 16)+
    geom_smooth(data = tmp,
                mapping = aes(x = PhotoRate,y = Model,group = LineName,color = LineName),
                method = "lm",
                formula = y~x,
                stat = "smooth",
                se = FALSE,
                size = 1.2,
                alpha = 0.6)+
    scale_color_manual(values = colors)+
    scale_x_continuous(breaks = Xrange,limits = c(0,50))+
    scale_y_continuous(breaks = Yrange,limits = c(0,50))+
    labs(x = Xlab,y = Ylab,title = PRE[i])+
    facet_wrap(~LineName,ncol = 2)
  print(p)
}
graph2ppt(p,file="output/PR_BIL_corplot",
          height=7,width=7)

ggsave(filename = "output/01_PRplot_BIL.jpeg",
       height = 2.8,
       width = 2.8,
       dpi = 500)
dev.off()

#SPAD相関プロット####
load("input/LN")
load("C:/Users/miyaj/OneDrive - Tokyo University of Agriculture and Technology/ドキュメント/R/2022_PRPrediction/03results/PredictedResults_SPAD_FieldGC")

pr=results
pr=pr %>% filter(LineName %in% c("Koshihikari","Takanari","HP-a","HP-b"))
pr$LineName=factor(pr$LineName,levels = c("Koshihikari","Takanari","HP-a","HP-b"))

#グラフの色
col.kos="#9E4B35" #コシヒカリの色 赤　J08-50V
col.tak="#26809E" #タカナリの色 青　J72-40T
col.hpa="#3E9E4B" #HP-aの色 緑　J46-60T
col.hpb="#512E9E" #HP-bの色 紫　J89-40T
colors=c(col.kos, col.tak, col.hpa, col.hpb)

Xlab="Measured SPAD"
Ylab="Predicted SPAD"
Xrange=seq(0, 60, 20)
Yrange=seq(0, 60, 20)

p_tpl <- ggplot()+
  theme(aspect.ratio=1, 
        axis.line=element_line(linetype="blank", size=0.5), 
        axis.ticks=element_line(colour="black", size=0.5), 
        panel.border=element_rect(fill=NA, size=1),
        panel.background=element_rect(fill="transparent", color=NA),
        panel.grid.major=element_line(linetype="blank"), 
        panel.grid.minor=element_line(linetype="blank"), 
        plot.title=element_text(colour="#5D311D", family="Helvetica", hjust=0.5, vjust=1, face="bold", size=20),
        axis.title=element_text(colour="black", family="Helvetica", face="bold",size=8), 
        axis.text=element_text(colour="black", family="Helvetica", size=8), 
        legend.position = "",
        plot.background=element_rect(fill="transparent", color=NA),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"),  #上、右、下、左
        strip.placement="", 
        strip.background=element_blank(),
        strip.text=element_text(colour="#5D311D", face="bold", family="Helvetica", size=16, 
                                hjust=0.5, vjust=0.5, margin=margin(0.1, 0.1, 0.1, 0.1)), #margin(top, right, bottom, left)
        panel.spacing=unit(0.5, "lines"))


for (i in 21) {
  tmp <- bind_cols(pr[1:25],pr[colnames(pr)==PRE[i]])
  colnames(tmp)[colnames(tmp)==PRE[i]] <- "Model"
  tmp[!tmp$Model==0,]
  
  p <- p_tpl+
    geom_abline(slope = 1,intercept = 0,color = "gray20")+
    geom_point(data = tmp,
               mapping = aes(x = SPAD,y = Model,group = LineName,color = LineName),
               stat = "identity",
               size = 1,
               alpha = 0.8,
               shape = 16)+
    geom_smooth(data = tmp,
                mapping = aes(x = SPAD,y = Model,group = LineName,color = LineName),
                method = "lm",
                formula = y~x,
                stat = "smooth",
                se = FALSE,
                size = 1.2,
                alpha = 0.6)+
    scale_color_manual(values = colors)+
    scale_x_continuous(breaks = Xrange,limits = c(0,60))+
    scale_y_continuous(breaks = Yrange,limits = c(0,60))+
    labs(x = Xlab,y = Ylab,title = PRE[i])+
    facet_wrap(~LineName,ncol = 2,scales = "free")
  print(p)
}
graph2ppt(p,file="output/SPAD_KTHP_corplot",
          height=4,width=4)

ggsave(filename = "output/01_SPADplot_KTHP.jpeg",
       height = 2.8,
       width = 2.8,
       dpi = 500)
dev.off()


#BIL
pr=results
pr=pr[grep("BIL",pr$LineName),]

Xlab="Measured SPAD"
Ylab="Predicted SPAD"
Xrange=seq(0, 60, 20)
Yrange=seq(0, 60, 20)

p_tpl <- ggplot()+
  theme(aspect.ratio=1, 
        axis.line=element_line(linetype="blank", size=0.5), 
        axis.ticks=element_line(colour="black", size=0.5), 
        panel.border=element_rect(fill=NA, size=1),
        panel.background=element_rect(fill="transparent", color=NA),
        panel.grid.major=element_line(linetype="blank"), 
        panel.grid.minor=element_line(linetype="blank"), 
        plot.title=element_text(colour="#5D311D", family="Helvetica", hjust=0.5, vjust=1, face="bold", size=20),
        axis.title=element_text(colour="black", family="Helvetica", face="bold",size=8), 
        axis.text=element_text(colour="black", family="Helvetica", size=8), 
        legend.position = "",
        plot.background=element_rect(fill="transparent", color=NA),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"),  #上、右、下、左
        strip.placement="", 
        strip.background=element_blank(),
        strip.text=element_text(colour="#5D311D", face="bold", family="Helvetica", size=16, 
                                hjust=0.5, vjust=0.5, margin=margin(0.1, 0.1, 0.1, 0.1)), #margin(top, right, bottom, left)
        panel.spacing=unit(0.5, "lines"))


colors = rep("#9E4B35",length(unique(pr$LineName)))
for (i in 21) {
  tmp <- bind_cols(pr[1:25],pr[colnames(pr)==PRE[i]])
  colnames(tmp)[colnames(tmp)==PRE[i]] <- "Model"
  tmp[!tmp$Model==0,]
  
  p <- p_tpl+
    geom_abline(slope = 1,intercept = 0,color = "gray20")+
    geom_point(data = tmp,
               mapping = aes(x = SPAD,y = Model,group = LineName,color = LineName),
               stat = "identity",
               size = 1,
               alpha = 0.8,
               shape = 16)+
    geom_smooth(data = tmp,
                mapping = aes(x = SPAD,y = Model,group = LineName,color = LineName),
                method = "lm",
                formula = y~x,
                stat = "smooth",
                se = FALSE,
                size = 1.2,
                alpha = 0.6)+
    scale_color_manual(values = colors)+
    scale_x_continuous(breaks = Xrange,limits = c(0,60))+
    scale_y_continuous(breaks = Yrange,limits = c(0,60))+
    labs(x = Xlab,y = Ylab,title = PRE[i])+
    facet_wrap(~LineName,ncol = 2)
  print(p)
}
graph2ppt(p,file="output/SPAD_BIL_corplot",
          height=7,width=7)

ggsave(filename = "output/03_SPADplot_BIL.jpeg",
       height = 2.8,
       width = 2.8,
       dpi = 500)
dev.off()


#草丈相関プロット####
#間違えて消しちゃった。。。
load("input/LN")
load("C:/Users/miyaj/OneDrive - Tokyo University of Agriculture and Technology/ドキュメント/R/2022_PRPrediction/01results/result.22")

#KTHP
pr=results
pr=pr %>% filter(LineName %in% c("Koshihikari","Takanari","HP-a","HP-b"))
pr$LineName=factor(pr$LineName,levels = c("Koshihikari","Takanari","HP-a","HP-b"))

#グラフの色
col.kos="#9E4B35" #コシヒカリの色 赤　J08-50V
col.tak="#26809E" #タカナリの色 青　J72-40T
col.hpa="#3E9E4B" #HP-aの色 緑　J46-60T
col.hpb="#512E9E" #HP-bの色 紫　J89-40T
colors=c(col.kos, col.tak, col.hpa, col.hpb)

Xlab=bquote("Measured "~italic(A)~" (µmol" ~CO[2]~ m^-2~s^-1*")")
Ylab=bquote("Predicted "~italic(A)~" (µmol" ~CO[2]~ m^-2~s^-1*")")
Xrange=seq(0, 60, 20)
Yrange=seq(0, 100000, 20)

p_tpl <- ggplot()+
  theme(aspect.ratio=1, 
        axis.line=element_line(linetype="blank", size=0.5), 
        axis.ticks=element_line(colour="black", size=0.5), 
        panel.border=element_rect(fill=NA, size=1),
        panel.background=element_rect(fill="transparent", color=NA),
        panel.grid.major=element_line(linetype="blank"), 
        panel.grid.minor=element_line(linetype="blank"), 
        plot.title=element_text(colour="#5D311D", family="Helvetica", hjust=0.5, vjust=1, face="bold", size=20),
        axis.title=element_text(colour="black", family="Helvetica", face="bold",size=8), 
        axis.text=element_text(colour="black", family="Helvetica", size=8), 
        legend.position = "",
        plot.background=element_rect(fill="transparent", color=NA),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"),  #上、右、下、左
        strip.placement="", 
        strip.background=element_blank(),
        strip.text=element_text(colour="#5D311D", face="bold", family="Helvetica", size=16, 
                                hjust=0.5, vjust=0.5, margin=margin(0.1, 0.1, 0.1, 0.1)), #margin(top, right, bottom, left)
        panel.spacing=unit(0.5, "lines"))


for (i in 135:135) {
  tmp <- bind_cols(pr[1:25],pr[colnames(pr)==PRE[i]])
  colnames(tmp)[colnames(tmp)==PRE[i]] <- "Model"
  tmp[!tmp$Model==0,]
  
  p <- p_tpl+
    geom_abline(slope = 1,intercept = 0,color = "gray20")+
    geom_point(data = tmp,
               mapping = aes(x = PhotoRate,y = Model,group = LineName,color = LineName),
               stat = "identity",
               size = 1,
               alpha = 0.8,
               shape = 16)+
    geom_smooth(data = tmp,
                mapping = aes(x = PhotoRate,y = Model,group = LineName,color = LineName),
                method = "lm",
                formula = y~x,
                stat = "smooth",
                se = FALSE,
                size = 1.2,
                alpha = 0.6)+
    scale_color_manual(values = colors)+
    scale_x_continuous(breaks = Xrange,limits = c(0,50))+
    scale_y_continuous(breaks = Yrange,limits = c(0,50))+
    labs(x = Xlab,y = Ylab,title = PRE[i])+
    facet_wrap(~LineName,ncol = 2,scales = "free")
  print(p)
}
graph2ppt(p,file="output/PR_KTHP_corplot",
          height=4,width=4)

ggsave(filename = "output/01_PRplot_KTHP.jpeg",
       height = 2.8,
       width = 2.8,
       dpi = 500)

dev.off()


#BIL
pr=results
pr=pr[grep("BIL",pr$LineName),]

Xlab=bquote("Measured "~italic(A)~" (µmol" ~CO[2]~ m^-2~s^-1*")")
Ylab=bquote("Predicted "~italic(A)~" (µmol" ~CO[2]~ m^-2~s^-1*")")
Xrange=seq(0, 60, 20)
Yrange=seq(0, 100000, 20)

p_tpl <- ggplot()+
  theme(aspect.ratio=1, 
        axis.line=element_line(linetype="blank", size=0.5), 
        axis.ticks=element_line(colour="black", size=0.5), 
        panel.border=element_rect(fill=NA, size=1),
        panel.background=element_rect(fill="transparent", color=NA),
        panel.grid.major=element_line(linetype="blank"), 
        panel.grid.minor=element_line(linetype="blank"), 
        plot.title=element_text(colour="#5D311D", family="Helvetica", hjust=0.5, vjust=1, face="bold", size=20),
        axis.title=element_text(colour="black", family="Helvetica", face="bold",size=8), 
        axis.text=element_text(colour="black", family="Helvetica", size=8), 
        legend.position = "",
        plot.background=element_rect(fill="transparent", color=NA),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5), "cm"),  #上、右、下、左
        strip.placement="", 
        strip.background=element_blank(),
        strip.text=element_text(colour="#5D311D", face="bold", family="Helvetica", size=16, 
                                hjust=0.5, vjust=0.5, margin=margin(0.1, 0.1, 0.1, 0.1)), #margin(top, right, bottom, left)
        panel.spacing=unit(0.5, "lines"))


colors = rep("#9E4B35",length(unique(pr$LineName)))
for (i in 135:135) {
  tmp <- bind_cols(pr[1:25],pr[colnames(pr)==PRE[i]])
  colnames(tmp)[colnames(tmp)==PRE[i]] <- "Model"
  tmp[!tmp$Model==0,]
  
  p <- p_tpl+
    geom_abline(slope = 1,intercept = 0,color = "gray20")+
    geom_point(data = tmp,
               mapping = aes(x = PhotoRate,y = Model,group = LineName,color = LineName),
               stat = "identity",
               size = 1,
               alpha = 0.8,
               shape = 16)+
    geom_smooth(data = tmp,
                mapping = aes(x = PhotoRate,y = Model,group = LineName,color = LineName),
                method = "lm",
                formula = y~x,
                stat = "smooth",
                se = FALSE,
                size = 1.2,
                alpha = 0.6)+
    scale_color_manual(values = colors)+
    scale_x_continuous(breaks = Xrange,limits = c(0,50))+
    scale_y_continuous(breaks = Yrange,limits = c(0,50))+
    labs(x = Xlab,y = Ylab,title = PRE[i])+
    facet_wrap(~LineName,ncol = 2)
  print(p)
}
graph2ppt(p,file="output/PR_BIL_corplot",
          height=7,width=7)

ggsave(filename = "output/01_PRplot_BIL.jpeg",
       height = 2.8,
       width = 2.8,
       dpi = 500)
dev.off()



#草丈全系統推移plot####
load("input/LN")
data <- read.xlsx("C:/Users/miyaj/OneDrive - Tokyo University of Agriculture and Technology/ドキュメント/宮下大輝/_東京農工大学/_作物学研究室/_実験/2022_府中本町圃場/実測データ/草丈/2022_草丈測定.xlsx",sheet="入力")
data$Date <- as.Date(data$Date,origin = "1899-12-30")          
data <- data[data$Transplant == "After",]

colors = c("#9E4B35","#26809E")

p=ggplot()+
  geom_line(data = data[grep("SL12|BIL12",data$LineName),]
            ,mapping = aes(x = Date,y = Height,group = LineName)
            ,stat = "summary",alpha = 0.15,color = "#9E4B35",fun="mean",size=0.5)+
  geom_line(data = data[-grep("SL12|BIL12|Koshi",data$LineName),]
            ,mapping = aes(x = Date,y = Height,group = LineName)
            ,stat = "summary",alpha = 0.15,color = "#26809E",fun="mean",size=0.5)+
  geom_line(data = data[grep("Koshi|Taka",data$LineName),]
            ,mapping = aes(x = Date,y = Height,color = Background)
            ,fun="mean"
            ,stat = "summary"
            ,size = 1.4)+
  theme(legend.position = "none")+
  scale_color_manual(values = colors)+
  ggtitle("Plant Height")+
  xlab("Date")+
  ylab("Height (cm)")+
  scale_y_continuous(breaks = seq(0,160,40),limits = c(0,NA))+
  scale_x_date(date_breaks = "1 month",date_labels = "%m/%d")+
  theme_base()
print(p)

graph2ppt(p,file="output/Height_lineplot",
          height=6,
          width=9)

ggsave(filename = "PlantHeight_KoshiTaka.jpeg",
       width = 8,
       height = 6,
       dpi = 500,
       limitsize = FALSE)
dev.off()

#光合成全系統推移plot####
data <- read.xlsx("C:/Users/miyaj/OneDrive - Tokyo University of Agriculture and Technology/ドキュメント/宮下大輝/_東京農工大学/_作物学研究室/_実験/2022_府中本町圃場/分析データ/光合成/2022_PhotoRate.xlsx")
data$Date <- as.Date(data$Date,origin = "1899-12-30")
data <- mutate(data,Background = if_else(grepl("Koshi|SL12|BIL12",data$LineName),"Koshi","Taka"))

p2=ggplot()+
  geom_line(data = data[grep("SL12|BIL12",data$LineName),]
            ,mapping = aes(x = Date,y = PhotoRate,group = LineName)
            ,stat = "summary",fun="mean",alpha = 0.2,color = "#9E4B35")+
  geom_line(data = data[-grep("SL12|BIL12|Koshi",data$LineName),]
            ,mapping = aes(x = Date,y = PhotoRate,group = LineName)
            ,stat = "summary",fun="mean",alpha = 0.2,color = "#26809E")+
  geom_line(data = data[grep("Koshi|Taka",data$LineName),]
            ,mapping = aes(x = Date,y = PhotoRate,color = Background)
            ,stat = "summary",fun="mean"
            ,size = 1)+
  theme(legend.position = "none")+
  scale_color_manual(values = colors)+
  ggtitle("PhotoRate")+
  xlab("Date")+
  ylab("PhotoRate")+
  scale_y_continuous(breaks = seq(0,160,20),limits = c(0,NA))+
  scale_x_date(date_breaks = "1 month",date_labels = "%m/%d")+
  theme_base()
print(p2)

graph2ppt(p2,file="output/PR_lineplot",
          height=6,
          width=9)

ggsave(filename = "2022KT_PhotoRate.jpeg",
       width = 7,
       height = 5,
       dpi = 500,
       limitsize = FALSE)
dev.off()


#SPAD全系統推移plot####
load("data/All_PhotoRate")
PR=PR[PR$year==2022,]
data=PR
data[grep("Koshi|Taka",data$LineName),"Background"]=data[grep("Koshi|Taka",data$LineName),"LineName"]

colors = c("#9E4B35","#26809E")
p3=ggplot()+
  geom_line(data = data[grep("SL12|BIL12",data$LineName),]
            ,mapping = aes(x = date,y = SPAD,group = LineName)
            ,stat = "summary",fun="mean",alpha = 0.15,color = "#9E4B35")+
  geom_line(data = data[-grep("SL12|BIL12|Koshi",data$LineName),]
            ,mapping = aes(x = date,y = SPAD,group = LineName)
            ,stat = "summary",fun="mean",alpha = 0.15,color = "#26809E")+
  geom_line(data = data[grep("Koshi|Taka",data$LineName),]
            ,mapping = aes(x = date,y = SPAD,color = Background)
            ,stat = "summary",fun="mean"
            ,size = 1)+
  theme(legend.position = "none")+
  scale_color_manual(values = colors)+
  ggtitle("SPAD")+
  xlab("Date")+
  ylab("SPAD")+
  scale_y_continuous(breaks = seq(0,160,20),limits = c(0,NA))+
  scale_x_date(date_breaks = "1 month",date_labels = "%m/%d")+
  theme_base()
print(p3)

graph2ppt(p3,file="output/SPAD_lineplot",
          height=6,
          width=9)

ggsave(filename = "2022KT_SPAD.jpeg",
       width = 8,
       height = 6,
       dpi = 500,
       limitsize = FALSE)
dev.off()


#Dry Weight全系統推移plot####
load("data/2022_Alldata")
data=DW
#data[grep("Koshi|Taka",data$LineName),"Background"]=data[grep("Koshi|Taka",data$LineName),"LineName"]
data[data$LineName=="Koshi","LineName"]="Koshihikari"
data[data$LineName=="Taka","Takanari"]="Takanari"
data$Date=as.Date(data$Date,origin = "1899-12-30")

colors = c("#9E4B35","#26809E")
p3=ggplot()+theme_base()+
  geom_line(data = data[grep("SL12|BIL12",data$LineName),]
            ,mapping = aes(x = Date,y = Weight,group = LineName)
            ,stat = "summary",fun="mean",alpha = 0.2,color = "#9E4B35")+
  geom_line(data = data[-grep("SL12|BIL12|Koshi",data$LineName),]
            ,mapping = aes(x = Date,y = Weight,group = LineName)
            ,stat = "summary",fun="mean",alpha = 0.2,color = "#26809E")+
  geom_line(data = data[grep("Koshi|Taka",data$LineName),]
            ,mapping = aes(x = Date,y = Weight,color = Background)
            ,stat = "summary",fun="mean"
            ,size = 1)+
  theme(legend.position = "none")+
  scale_color_manual(values = colors)+
  ggtitle("Dry Weight")+
  xlab("Date")+
  ylab("Dry Weight (g)")+
  #scale_y_continuous(breaks = seq(0,160,20),limits = c(0,NA))+
  scale_x_date(date_breaks = "1 month",date_labels = "%m/%d")
print(p3)

graph2ppt(p3,file="output/DW_lineplot",
          height=6,
          width=9)

ggsave(filename = "DW_lineplot.jpeg",
       width = 8,
       height = 6,
       dpi = 500,
       limitsize = FALSE)
dev.off()


#山口さんplot
library(openxlsx)

date=paste("2022/",c("06/15","06/22","06/29","07/06","07/17","07/22","07/29"),sep = "")

d=data.frame()
for (i in 2:8) {
  d=rbind(d,cbind(read.xlsx(xlsxFile = "C:/Users/miyaj/OneDrive - Tokyo University of Agriculture and Technology/ドキュメント/宮下大輝/_東京農工大学/_作物学研究室/_実験/2022_府中本町圃場/実測データ/LAI2200/LAI-2200_Summary.xlsx",sheet = i),Date=date[i-1]))
}
#成形
d$Date=as.Date(d$Date,origin="1899-12-30")
d=d %>% 
  select(Date,LineName=Genotype,Rep,LAI,SEL,ACF,MTA,SEM,DIFN) %>% 
  filter(!LineName == "紫")
d[d$LineName=="Koshi","LineName"]="Koshihikari"
d[d$LineName=="Taka","LineName"]="Takanari"
d[grep("S12",d$LineName),"LineName"]=paste("SL12",substr(d[grep("S12",d$LineName),"LineName"],4,5),sep = "")
d[grep("S13",d$LineName),"LineName"]=paste("SL13",substr(d[grep("S13",d$LineName),"LineName"],4,5),sep = "")
d[grep("B",d$LineName),"LineName"]=paste("BIL",substr(d[grep("B",d$LineName),"LineName"],2,5),sep = "")

LAI=d
#save(LAI,file = "data/2022_LAI2200")
#write.csv(LAI,file = "C:/Users/miyaj/OneDrive - Tokyo University of Agriculture and Technology/ドキュメント/宮下大輝/_東京農工大学/_作物学研究室/_実験/2022_府中本町圃場/実測データ/LAI2200/LAI-2200_Summary.csv",row.names=FALSE)

#プロット
col=colnames(LAI)[4:9]
colors = c("#9E4B35","#26809E")
caption = c("葉面積指数の平均値(mm-2)","LAIの標準誤差","Apparent クランピングファクター","葉角度（°）の平均値（葉が全て水平ならば，0°，葉が全て垂直ならば90°）","MTAの標準誤差","林冠の開空割合")
for (i in 1:length(col)) {
  p4=ggplot()+theme_base()+
    geom_line(data = LAI[grep("SL12|BIL12",LAI$LineName),]
              ,mapping = aes_string(x = "Date",y = col[i],group = "LineName")
              ,stat = "summary",fun="mean",alpha = 0.2,color = "#9E4B35")+
    geom_line(data = LAI[-grep("SL12|BIL12|Koshi",LAI$LineName),]
              ,mapping = aes_string(x = "Date",y = col[i],group = "LineName")
              ,stat = "summary",fun="mean",alpha = 0.2,color = "#26809E")+
    geom_line(data = LAI[grep("Koshi|Taka",LAI$LineName),]
              ,mapping = aes_string(x = "Date",y = col[i],color = "LineName")
              ,stat = "summary",fun="mean"
              ,size = 1)+
    #theme(legend.position = "none")+
    scale_color_manual(values = colors)+
    ggtitle(col[i])+
    xlab("Date")+
    ylab(col[i])+
    labs(caption = caption[i])
    #scale_y_continuous(breaks = seq(0,160,20),limits = c(0,NA))+
    scale_x_date(date_breaks = "2 week",date_labels = "%b/%d")
  print(p4)
  ggsave(filename = paste("output/LAI2200_",col[i],"_lineplot.jpeg",sep = ""),
         width = 8,
         height = 6,
         dpi = 300)
}
dev.off()




#データ数確認
rm(list=ls(envir=globalenv()), envir=globalenv())
gc()
gc()
load("data/2022_Alldata")

n1=Height[Height$Transplant=="After","Height"]
n1=na.omit(n1)
length(n1)

n2=na.omit(PR$ID)
length(n2)

load("data/All_PhotoRate")
PR=PR[PR$year==2022,]
n3=na.omit(PR$SPAD)
length(n3)


#光合成系統間差ー回帰直線の傾きー####
rm(list=ls(envir=globalenv()), envir=globalenv())
gc()
gc()

load("input/LN")
load("data/ALl_PhotoRate")
PR=PR[PR$year==2022,]
PR <- PR[PR$regular == "T",]
PR <- PR[-24]
PR$Field <- as.character(PR$Field)
PR <- na.omit(PR)
#PR=PR[-2216,]

LN=unique(PR$LineName)
heading <- aggregate(data = PR,Head.D~LineName+Field,FUN = mean)

pdf(file = "output/2022PhotoRate_lineplot.pdf", height=7, width=10)
for (j in 1:length(LN)) {
  p=ggplot()+theme_base()+
    geom_point(data = PR[PR$date > min(heading[heading$LineName==LN[j],"Head.D"])&PR$LineName==LN[j],],
               mapping = aes(x = date,y = PhotoRate),
               stat = "identity",
               size = 0.8)+
    geom_line(data = PR[PR$date > min(heading[heading$LineName==LN[j],"Head.D"])&PR$LineName==LN[j],],
              mapping = aes(x = date,y = PhotoRate),
              stat = "summary",
              fun = mean)+
    geom_smooth(data = PR[PR$date > min(heading[heading$LineName==LN[j],"Head.D"])&PR$LineName==LN[j],],
                mapping = aes(x = date,y = PhotoRate),
                stat = "smooth",
                method = "lm",
                formula = "y ~ x",
                se = FALSE,
                size = 1.5)+
    labs(title = LN[j])+
    scale_y_continuous(breaks = seq(0,60,20),limits = c(0,62))
  print(p)
}
dev.off()

#回帰直線の傾きを求めるー光合成ー
coe=data.frame()
LineName=data.frame()
for (j in 1:length(LN)) {
  for (i in unique(PR[PR$LineName==LN[j],"Field"])) {
    coe=rbind(coe,coefficients(lm(PhotoRate~date,PR[PR$date > heading[heading$LineName==LN[j]&heading$Field==i,"Head.D"]&PR$LineName==LN[j],]))[2])
    LineName=rbind(LineName,LN[j])
  }
}
coe=cbind(LineName,coe)
colnames(coe)=c("LineName","coe")

#棒グラフ
LN_SL12=LN_CSSL[1:41]
LN_SL13=LN_CSSL[42:80]
coe$LineName=factor(coe$LineName,levels = c(LN_KTHP,LN_BIL,LN_CSSL))
# colors = c(rep("#63564E",length(LN_KTHP)),
#            rep("#B0998B",length(LN_BIL)),
#            rep("#B37856",length(LN_SL12)),
#            rep("#7F563D",length(LN_SL13)))
colors = c(rep("#594D46",length(LN_KTHP)),
           rep("#8C796D",length(LN_BIL)-1),
           rep("#CC8962",length(LN_SL12)),
           rep("#9F6B4D",length(LN_SL13)))

p=ggplot()+theme_base()+
  geom_bar(data=coe,mapping = aes(x = LineName,y = coe,fill = LineName),stat = "identity",width = 0.7)+
  theme(axis.text.x = element_text(size=10,angle = 90,hjust = 1,vjust = 0.5),
        legend.position = "none")+
  scale_y_reverse(expand = c(0,0),limits = c(0,-0.99))+
  scale_fill_manual(values = colors)+
  ylab("Regression Coefficient")
print(p)

graph2ppt(p,file="output/PR_RegressionCoe_barplot",
          height=5,
          width=12)


#回帰直線の傾きを求めるーSPADー
load("input/LN")
load("data/ALl_PhotoRate")
PR=PR[PR$year==2022,]
PR <- PR[PR$regular == "T",]
PR <- PR[-24]
PR <- na.omit(PR)
#PR=PR[-2216,]

LN=unique(PR$LineName)
heading <- aggregate(data = PR,Head.D~LineName,FUN = mean)

pdf(file = "output/2022SPAD_lineplot.pdf", height=7, width=10)
for (j in 1:length(LN)) {
  p=ggplot()+theme_base()+
    geom_point(data = PR[PR$date > min(heading[heading$LineName==LN[j],"Head.D"])&PR$LineName==LN[j],],
               mapping = aes(x = date,y = SPAD),
               stat = "identity",
               size = 0.8)+
    geom_line(data = PR[PR$date > min(heading[heading$LineName==LN[j],"Head.D"])&PR$LineName==LN[j],],
              mapping = aes(x = date,y = SPAD),
              stat = "summary",
              fun = mean)+
    geom_smooth(data = PR[PR$date > min(heading[heading$LineName==LN[j],"Head.D"])&PR$LineName==LN[j],],
                mapping = aes(x = date,y = SPAD),
                stat = "smooth",
                method = "lm",
                formula = "y ~ x",
                se = FALSE,
                size = 1.5)+
    labs(title = LN[j])+
    scale_y_continuous(breaks = seq(0,60,20),limits = c(0,62))
  print(p)
}
dev.off()

#傾き算出はここから
coe=data.frame()
LineName=data.frame()
for (j in 1:length(LN)) {
  #for (i in unique(PR[PR$LineName==LN[j],"Field"])) {
    coe=rbind(coe,coefficients(lm(SPAD~date,PR[PR$date > heading[heading$LineName==LN[j],"Head.D"]&PR$LineName==LN[j],]))[2])
    LineName=rbind(LineName,LN[j])
#  }
}
coe=cbind(LineName,coe)
colnames(coe)=c("LineName","coe")

#棒グラフ
LN_SL12=LN_CSSL[1:41]
LN_SL13=LN_CSSL[42:80]
coe$LineName=factor(coe$LineName,levels = c(LN_KTHP,LN_BIL,LN_CSSL))
# colors = c(rep("#63564E",length(LN_KTHP)),
#            rep("#B0998B",length(LN_BIL)),
#            rep("#B37856",length(LN_SL12)),
#            rep("#7F563D",length(LN_SL13)))
colors = c(rep("#594D46",length(LN_KTHP)),
           rep("#8C796D",length(LN_BIL)-1),
           rep("#CC8962",length(LN_SL12)),
           rep("#9F6B4D",length(LN_SL13)))

p=ggplot()+theme_base()+
  geom_bar(data=coe,mapping = aes(x = LineName,y = coe,fill = LineName),stat = "identity",width = 0.7)+
  theme(axis.text.x = element_text(size=10,angle = 90,hjust = 1,vjust = 0.5),
        legend.position = "none")+
  scale_y_reverse(expand = c(0,0),limits = c(0,-0.7))+
  scale_fill_manual(values = colors)+
  ylab("Regression Coefficient")
print(p)

graph2ppt(p,file="output/SPAD_RegressionCoe_barplot",
          height=4,
          width=12)


#各系統ごとに回帰直線を引いてその傾きを見てみる####
load("data/2022_Alldata")
rm(Biomass,DW,PR,Spike,Tiller)
Height$Date=as.Date(Height$Date,origin = "1899-12-30")
Height[Height$LineName=="Koshi","LineName"]="Koshihikari"
Height[Height$LineName=="Taka","LineName"]="Takanari"

data=Height
data <- data[data$Transplant == "After",]
data <- data %>% 
  dplyr::filter(!(LineName == "BIL1222" & No == "210" & Col == "4")) %>% 
  dplyr::filter(!(LineName == "SL1202" & No == "115" & Col == "3")) %>% 
  dplyr::filter(!(LineName == "SL1209" & No == "106" & Col == "4")) %>% 
  dplyr::filter(!(LineName == "SL1224" & No == "141" & Col == "4")) %>% 
  dplyr::filter(!(LineName == "SL1224" & No == "141" & Col == "5")) %>% 
  dplyr::filter(!(LineName == "SL1320" & No == "302" & Col == "3")) %>% 
  dplyr::filter(!(LineName == "SL1327" & No == "295" & Col == "4")) %>% 
  dplyr::filter(!(LineName == "SL1335" & No == "54" & Col =="4")) %>% 
  dplyr::filter(!(LineName == "SL1335" & No == "278" & Col =="4" & Date == "2022-07-04"))

p=ggplot()+theme_base()+
  geom_point(data = data,mapping = aes(x = Date,y = Height),stat = "identity")+
  geom_line(data = data,mapping = aes(x = Date,y = Height),stat = "summary",fun = mean)+
  geom_smooth(data = data,mapping = aes(x = Date,y = Height),stat = "smooth",formula = "y ~ x",method = "lm",se = FALSE)+
  facet_wrap(~LineName)
print(p)

mean=aggregate(data = data,Height ~ LineName + Date,FUN = mean)

load("input/LN")

#各測定日の草丈平均の最大値からその日の日付を抽出し、その日以前のデータを抜き出ししている
#data[data$Date<mean[mean$Height==max(mean[mean$LineName==LN[i],"Height"]),"Date"],]
data2=data.frame()
for (i in 1:length(LN)) {
  data2=rbind(data2,data[data$Date<=mean[mean$Height==max(mean[mean$LineName==LN[i],"Height"]),"Date"]&data$LineName==LN[i],])
}

p2=ggplot()+theme_base()+
  geom_point(data = data2,mapping = aes(x = Date,y = Height),stat = "identity")+
  geom_line(data = data2,mapping = aes(x = Date,y = Height),stat = "summary",fun = "mean")+
  geom_smooth(data = data2,mapping = aes(x = Date,y = Height),stat = "smooth",formula = "y ~ x",method = "lm",se = FALSE)+
  facet_wrap(~LineName)
print(p2)


coe=data.frame()
LineName=data.frame()
LN_=unique(data2$LineName)
for (i in 1:length(LN_)) {
  coe=rbind(coe,coefficients(lm(Height~Date,data2[data2$Date<=mean[mean$Height==max(mean[mean$LineName==LN_[i],"Height"]),"Date"]&data2$LineName==LN_[i],]))[2])
  LineName=rbind(LineName,LN_[i])
}
coe=cbind(LineName,coe)
colnames(coe)=c("LineName","coe")
coe$LineName=factor(coe$LineName,levels = c(LN_KTHP,LN_BIL,LN_CSSL))
coe=arrange(coe,LineName)



LN_SL12=LN_CSSL[1:41]
LN_SL13=LN_CSSL[42:80]

color=c(rep("black",4),rep("#784F46",9),rep("#CC543A",length(LN_SL12)),rep("#D58C7B",length(LN_SL13)))

colors = c(rep("#594D46",length(LN_KTHP)),
           rep("#8C796D",length(LN_BIL)-1),
           rep("#CC8962",length(LN_SL12)),
           rep("#9F6B4D",length(LN_SL13)))

# p=ggplot()+theme_base()+
#   geom_bar(data = coe,mapping = aes(x = reorder(LineName,coe),y = coe,fill = LineName),stat = "identity")+
#   theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5,size = 7),
#         legend.position = "none")+
#   scale_fill_manual(values = color)+
#   scale_y_continuous(breaks = seq(0,2,0.5),limits = c(0,2))
# print(p)
# ggsave(filename = "output/2022_Heightcoe_barplot.jpeg",
#        height = 5,
#        width = 10,
#        dpi = 300)
# dev.off()


p=ggplot()+theme_base()+
  geom_bar(data = coe,mapping = aes(x = LineName,y = coe,fill = LineName),stat = "identity",width = 0.7)+
  theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5,size = 10),
        legend.position = "none")+
  scale_fill_manual(values = colors)+
  scale_y_continuous(breaks = seq(0,2,1),limits = c(0,2))+
  ylab("Regression Coefficient")
print(p)
graph2ppt(p,file="output/Height_RegressionCoe_barplot",
          height=4,
          width=12)

# ggsave(filename = "output/2022_Heightcoe_barplot2.jpeg",
#        height = 5,
#        width = 10,
#        dpi = 300)
dev.off()


#積算してみたい####
rm(list=ls(envir=globalenv()), envir=globalenv())
gc()
gc()

load("input/LN")
load("data/ALl_PhotoRate")
PR=PR[PR$year==2022,]
PR <- PR[PR$regular == "T",]
PR <- PR[-24]
PR$Field <- as.character(PR$Field)
PR <- na.omit(PR)

LN=unique(PR$LineName)
heading <- aggregate(data = PR,Head.D~LineName,FUN = mean)


#棒グラフー出穂期バイオマスー
rm(list=ls(envir=globalenv()), envir=globalenv())
gc()
gc()

library(tidyverse)
library(ggthemes)
library(ggpubr)
library(scales)
library(openxlsx)
library(export)

load("data/2022_Alldata")
load("input/LN")

#棒グラフ
data = Biomass
data[data$LineName=="Koshi","LineName"]="Koshihikari"
data[data$LineName=="Taka","LineName"]="Takanari"

LN_SL12=LN_CSSL[1:41]
LN_SL13=LN_CSSL[42:80]
data$LineName=factor(data$LineName,levels = c(LN_KTHP,LN_BIL,LN_CSSL))

colors = c(rep("#594D46",length(LN_KTHP)),
           rep("#8C796D",length(LN_BIL)-1),
           rep("#CC8962",length(LN_SL12)),
           rep("#9F6B4D",length(LN_SL13)))

limits=c(0,180)
breaks=seq(0,150,50)
ylab="Biomass of Heading phase (g)"

p=ggplot()+theme_base()+
  geom_bar(data=data,mapping = aes(x = LineName,y = Weight/8,fill = LineName),stat = "identity",width = 0.7)+
  theme(axis.text.x = element_text(size=10,angle = 90,hjust = 1,vjust = 0.5),
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0),limits = limits,breaks = breaks)+
  scale_fill_manual(values = colors)+
  ylab(ylab)
print(p)
graph2ppt(p,file="output/BiomassHeading_barplot",
          height=4,
          width=12)
ggsave(filename = "output/BiomassHeading_barplot.jpeg",
       height=5,
       width=12,
       dpi = 300)
dev.off()

#分げつ
#棒グラフ
data = Tiller
data[data$LineName=="Koshi","LineName"]="Koshihikari"
data[data$LineName=="Taka","LineName"]="Takanari"

data_sum=data %>% group_by(LineName) %>% summarise(mean=mean(Tiller),sd=sd(Tiller))

LN_SL12=LN_CSSL[1:41]
LN_SL13=LN_CSSL[42:80]
data$LineName=factor(data$LineName,levels = c(LN_KTHP,LN_BIL,LN_CSSL))

colors = c(rep("#594D46",length(LN_KTHP)),
           rep("#8C796D",length(LN_BIL)-1),
           rep("#CC8962",length(LN_SL12)),
           rep("#9F6B4D",length(LN_SL13)))

limits=c(0,24)
breaks=seq(0,150,5)
ylab="Tiller"

p=ggplot()+theme_base()+
  geom_bar(data=data,mapping = aes(x = LineName,y = Tiller,fill = LineName),stat = "summary",fun="mean",width = 0.7)+
  geom_errorbar(data = data_sum,mapping = aes(x = LineName,ymax=mean+sd,ymin=mean-sd),stat = "identity",color="gray45",width=0.5)+
  theme(axis.text.x = element_text(size=10,angle = 90,hjust = 1,vjust = 0.5),
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0),limits = limits,breaks = breaks)+
  scale_fill_manual(values = colors)+
  ylab(ylab)
print(p)
graph2ppt(p,file="output/Tiller_barplot",
          height=4,
          width=12)
ggsave(filename = "output/Tiller_barplot.jpeg",
       height=5,
       width=12,
       dpi = 300)
dev.off()

#穂数
#棒グラフ
data = Spike
data$Spike_=ifelse(is.na(data$LateSpile),data$Spike,data$Spike-data$LateSpile)
data[data$LineName=="Koshi","LineName"]="Koshihikari"
data[data$LineName=="Taka","LineName"]="Takanari"

data_sum=data %>% group_by(LineName) %>% summarise(mean=mean(Spike_),sd=sd(Spike_))

LN_SL12=LN_CSSL[1:41]
LN_SL13=LN_CSSL[42:80]
data$LineName=factor(data$LineName,levels = c(LN_KTHP,LN_BIL,LN_CSSL))

colors = c(rep("#594D46",length(LN_KTHP)),
           rep("#8C796D",length(LN_BIL)-1),
           rep("#CC8962",length(LN_SL12)),
           rep("#9F6B4D",length(LN_SL13)))

limits=c(0,24)
breaks=seq(0,150,5)
ylab="Spike"

p=ggplot()+theme_base()+
  geom_bar(data=data,mapping = aes(x = LineName,y = Spike_,fill = LineName),stat = "summary",fun="mean",width = 0.7)+
  geom_errorbar(data = data_sum,mapping = aes(x = LineName,ymax=mean+sd,ymin=mean-sd),stat = "identity",color="gray45",width=0.5)+
  theme(axis.text.x = element_text(size=10,angle = 90,hjust = 1,vjust = 0.5),
        legend.position = "none")+
  scale_y_continuous(expand = c(0,0),limits = limits,breaks = breaks)+
  scale_fill_manual(values = colors)+
  ylab(ylab)
print(p)
graph2ppt(p,file="output/Spike_barplot",
          height=4,
          width=12)
ggsave(filename = "output/Spike_barplot.jpeg",
       height=5,
       width=12,
       dpi = 300)
dev.off()


#LASSOの結果どのくらいの遺伝子が選ばれたんだろうね
#Height
#Km1モデル（1000repなのでそれぞれ見てみる）
load("C:/Users/miyaj/OneDrive - Tokyo University of Agriculture and Technology/ドキュメント/R/2022_HeightPrediction/01results/models_K")
v=vector()
for (i in 1:1000) {
 v=c(v,nrow(N0G[[1]][[i]])-1) 
}


min(v)
max(v)
mean(v)
median(v)
hist(v)

#Km1モデル 100*3000（1000repなのでそれぞれ見てみる）
load("C:/Users/miyaj/OneDrive - Tokyo University of Agriculture and Technology/ドキュメント/R/2022_HeightPrediction/03results/models_K")
v2=vector()
for (i in 1:1000) {
  v2=c(v2,nrow(N0G[[1]][[i]])-1) 
}

min(v2)
max(v2)
mean(v2)
median(v2)
hist(v2)


#PhotoRate
#Mm5
load("C:/Users/miyaj/OneDrive - Tokyo University of Agriculture and Technology/ドキュメント/R/2022_PRPrediction/01results/models_M")
v3=vector()
for (i in 1:1000) {
  v3=c(v3,nrow(N0G[[5]][[i]])-1) 
}

min(v3)
max(v3)
mean(v3)
median(v3)
hist(v3)


#PhotoRate
#Mm5
load("C:/Users/miyaj/OneDrive - Tokyo University of Agriculture and Technology/ドキュメント/R/2022_PRPrediction/03results/models_K")
v4=vector()
for (i in 1:1000) {
  v4=c(v4,nrow(N0G[[1]][[i]])-1) 
}

min(v4)
max(v4)
mean(v4)
median(v4)
hist(v4)
