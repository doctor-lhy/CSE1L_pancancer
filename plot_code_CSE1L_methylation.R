library(tidyverse)

gene_used <- 'CSE1L'
png(str_glue('{gene_used}_methylation.png'),res = 300,height = 2.3,width = 10,units = 'in')
ggplot(data = dat,aes(x = sample_group,y = methy_median))+
  geom_bar(stat = 'identity',aes(fill = sample_group),#position=position_dodge(width = 0.1),
           color = 'white',width = 0.8)+
  geom_text(aes(label = label,y = 0.29,x = 1.5))+
  facet_grid(~Cancer.type)+
  labs(x = '',y = 'methylation')+
  scale_fill_manual(name = '',values = c('#2C7AA1FF','#FFA400FF'))+
  # scale_fill_brewer(palette = 'Set2')+
  theme_classic()+
  theme(#axis.text.x = element_text(angle = 45,color = 'black',hjust=1),
    axis.text.y = element_text(color = 'black',size = 8),
    panel.spacing = unit(0, "mm"),
    strip.background = element_blank(),
    axis.text.x = element_text(color = 'black',size = 8),
    # axis.text.y = element_text(),
    strip.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid")
  )+
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust =1))+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
dev.off()
  
saveRDS(dat,file = str_glue('plot_data_{gene_used}_methylation.rds'))
