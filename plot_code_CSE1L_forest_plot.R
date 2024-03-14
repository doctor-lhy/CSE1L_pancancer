
library(tidyverse)
load('plot_data_CSE1L_forestplot.RData')
library(forestplot)

png("forestplot.png", width = 8, height = 20,res = 300,units = 'in')
forestplot(labeltext=tabletext,
           mean=c(NA,log2(as.numeric(table_dat_all$HR))),
           lower=c(NA,log2(as.numeric(table_dat_all$lower.95CI))), 
           upper=c(NA,log2(as.numeric(table_dat_all$upper.95CI))),
           graph.pos=7,
           graphwidth = unit(.25,"npc"),
           fn.ci_norm="fpDrawDiamondCI",
           col=fpColors(box='#616161FF', lines='#757575FF' , zero = "black"),
           boxsize=0.4,
           lwd.ci=1,
           ci.vertices.height = 0.1,ci.vertices=F,
           zero=0,
           lwd.zero=2,
           xticks = c(-1,0,1,2,3,4,5),
           lwd.xaxis=2,
           xlab=expression("log"[2]~"HR"),
           hrzl_lines=list("1" = gpar(lwd=2, col="black"),#黑实线
                           "2" = gpar(lwd=1, col="grey50", lty=2),#灰色虚线
                           "7" = gpar(lwd=1, col="grey50", lty=2),
                           "12" = gpar(lwd=1, col="grey50", lty=2),
                           "17" = gpar(lwd=1, col="grey50", lty=2),
                           "22" = gpar(lwd=1, col="grey50", lty=2),
                           "27" = gpar(lwd=1, col="grey50", lty=2),
                           "32" = gpar(lwd=1, col="grey50", lty=2),
                           "37" = gpar(lwd=1, col="grey50", lty=2),
                           "42" = gpar(lwd=1, col="grey50", lty=2),
                           "47" = gpar(lwd=1, col="grey50", lty=2),
                           "52" = gpar(lwd=1, col="grey50", lty=2),
                           "57" = gpar(lwd=1, col="grey50", lty=2),
                           "62" = gpar(lwd=1, col="grey50", lty=2),
                           "67" = gpar(lwd=2, col="black")),
           txt_gp=fpTxtGp(label=gpar(cex=1),
                          ticks=gpar(cex=1),
                          xlab=gpar(cex=1),
                          title=gpar(cex=1)),
           lineheight = unit(.75,"cm"),
           colgap = unit(0.3,"cm"),
           mar=unit(rep(1.5, times = 4), "cm"),
           new_page = F
)
invisible(dev.off())