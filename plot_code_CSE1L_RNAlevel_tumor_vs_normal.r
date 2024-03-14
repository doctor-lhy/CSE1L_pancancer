library(tidyverse)
gene_used <- 'CSE1L'

#########-------------------RNA-seq:tumor vs normal
exp_used <- readRDS(str_glue('01plot_data_pancancer_{gene_used}_exp.rds'))
png(str_glue('pancancer_{gene_used}_exp.png'),width = 10,height = 8,units = 'in',res =300)
p1 <- ggplot(data = exp_used,aes(x = group,y = exp,color = group))+
  geom_boxplot(notch = T,width = 0.6,outlier.size = 0)+
  facet_wrap(~tumor,scales = 'free',nrow = 4)+theme_classic()+
  theme(panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"),
        panel.background = element_blank(),strip.background = element_rect(color = 'white'),
        legend.text = element_text(size=8),
        legend.title = element_blank(),axis.text.x = element_text(color = 'black'),
        axis.text.y = element_text(color = 'black'))+
  labs(x= '',y=str_glue('{gene_used} Expression\n(TPM)'))+ scale_y_continuous(expand = expansion(mult = c(0, 0.15)))+
  ggpubr::stat_compare_means(method = 'wilcox.test',comparisons = list(c('Normal','Tumor')),label = 'p.signif')
print(p1)
dev.off()


###########_----TISCH：

library(tidyverse)
dat <- read_csv('TISCH_CSE1L_heatmap.csv')
dat %>% head


dat <- split(dat,dat$`Sales per employee (y)`)
keep_dat <- map(dat,function(x){
  x <- max(x$`Sales per employee (value)`,na.rm = T)
  x <- x>0
  return(x)
}) %>% unlist

dat <- dat[keep_dat]
dat <- map(dat,function(x){
  x$scale_tpm <- scale(x$`Sales per employee (value)`)
  return(x)
}) %>% do.call(rbind,.)
library(viridis)

dat$Category<- factor(as.character(dat$Category),
                      levels = c("Malignant cells", "Immune cells","Stromal cells", "Others" 
                      ))

png(str_glue('TISH_{gene_used}_pancancer.png'),res = 300,height = 8,width = 5,units = 'in')
ggplot()+
  geom_tile(data = dat,aes(y = dat$`Sales per employee (y)`,
                           x = dat$Category,fill = scale_tpm),color = 'white',size = 1)+
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
  labs(x = '',y = '')+
  scale_fill_viridis_c(name = 'Scaled TPM')
dev.off()


saveRDS(dat,file = 'plot_data_TISH_CSE1L_pancancer.rds')