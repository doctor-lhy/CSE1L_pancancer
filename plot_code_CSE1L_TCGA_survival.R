
library(tidyverse)
gene_used <- 'CSE1L'
plot_data <- readRDS(str_glue('plot_data_{gene_used}_tcga_sur_all.rds'))
png(str_glue('{gene_used}_tcga_sur_all.png'),res = 300,height = 5,width = 7,units = 'in')
ggplot(data = plot_data)+
  geom_point(aes(y = hr,x = -log10(pvalue),fill = group),shape = 21,size = 2)+
  geom_text_repel(aes(x = -log10(pvalue),y = hr,label = tumor),size = 2)+
  geom_hline(yintercept = 1,linetype = 'dashed',show.legend = T)+
  geom_vline(xintercept = -log10(0.05),linetype = 'dashed',show.legend = T)+
  theme_bw()+
  theme(axis.title.y.right = element_blank(),  #
        axis.ticks.y = element_blank(),      #
        axis.text.y = element_text(margin = margin(r = 0),colour = 'black'), #
        strip.text.y.left = element_text(angle = 0),
        axis.text.x = element_text(colour = 'black',angle = 90,hjust = 1, vjust = 0.5),
        panel.spacing = unit(0, "mm"),
        strip.background = element_blank(),
        panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"),
        panel.grid=element_blank(),
        strip.text.x = element_text(size = 8,color = 'black')
  )+
  scale_fill_manual(name = '', values = c('#A50026FF','#F67E4BFF','#6EA6CDFF','#364B9AFF'))+
  labs(y = 'HR',x = bquote(~-Log[10]~'Pvalue'))
dev.off()
saveRDS(plot_data,file = str_glue('plot_data_{gene_used}_tcga_sur_all.rds'))
