#######################-----

library(RColorBrewer)
gene_used <- 'CSE1L'
cols <- colorRampPalette(brewer.pal(10, "RdBu"))(50) %>% rev()
r$pvalue_sig <- ifelse(r$pvalue<0.05,'*','')
r <- readRDS('plot_data_{gene_used}_immune_check_cor.RData')
png(str_glue('{gene_used}_immune_check_cor.png'),res = 300,height = 10,width = 4,units = 'in')
ggplot()+
  geom_tile(data = r,aes(x = gene,y = tumor,fill = rho),color = 'white',size = 1)+
  geom_text(data = r,aes(x = gene,y = tumor,label = pvalue_sig))+
  scale_fill_distiller(name = "Spearman's correlation\n coefficient",
                       palette = "RdBu",limits = c(-1,1)*max(abs(r$rho)))+
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


save(r,cols,file = str_glue('plot_data_{gene_used}_immune_check_cor.RData'))


