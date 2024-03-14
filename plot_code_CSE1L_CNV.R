library(tidyverse)
gene_used <- 'CSE1L'

##############-----cnv
dat_cnv <- readRDS(str_glue('plot_data_{gene_used}_CNV_pancancer.rds'))
dat_cnv <- dat %>% filter(variable == 'cnv')
dat_cnv$Group <- ifelse(dat_cnv$p_value<0.05,'Significant','Not significant')
library(ggpubr)
dat_cnv$Group <- factor(dat_cnv$Group %>% as.character(),levels = c('Significant','Not significant'))
png(str_glue('{gene_used}_CNV_pancancer.png'),res = 300,height = 3,width = 10,units = 'in')
ggbarplot(dat_cnv, x = "tumor", y = "pearson_r",
           fill = 'Group',   color = 'Group',                             
           sort.val = "desc",                        
           add = "segments",                             
           xlab="",
           add.params = list(color = "lightgray", size = 2),
           rotate = F
           # group = "Group"
           
)+
  labs(y = 'Pearson')+
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))+
  scale_fill_nejm()+
  scale_color_nejm()
dev.off()
saveRDS(dat_cnv,file = str_glue('plot_data_{gene_used}_CNV_pancancer.rds'))
