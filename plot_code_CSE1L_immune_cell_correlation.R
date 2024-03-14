############---cibersort
library(tidyverse)
gene_used <- 'CSE1L'
plot_dat <- readRDS('plot_data_{gene_used}_ciber_cor.rds')

png(str_glue('{gene_used}_ciber_cor.png'),height = 8,width = 10,res = 300,units = 'in')
ggplot()+
  geom_tile(data = plot_dat,aes(y = tumor,x = celltype,fill = rho),color = 'white',size = 1)+
  geom_text(data = plot_dat,aes(y = tumor,x = celltype,label = pvalue_label))+
  scale_fill_gradientn(name = "* P<0.05 \n Spearman",colors = cols)+
  theme(
    panel.border = element_rect(fill=NA,color="black", size=0.5, linetype="solid"),
    panel.background = element_blank(),
    legend.text = element_text(size=8),
    legend.title = element_text(size = 8),
    axis.text.x = element_text(color = 'black',angle = 45,vjust = 1,hjust = 1,size = 10),
    axis.text.y = element_text(color = 'black',size = 10),
    legend.key = element_rect(fill = "white"),
    plot.title = element_text(hjust = 0.5)
  )+
  labs(x = '',y = '')
dev.off()
saveRDS(plot_dat,file = str_glue('plot_data_{gene_used}_ciber_cor.rds'))
