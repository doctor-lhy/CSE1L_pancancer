library(tidyverse)
gene_used <- 'CSE1L'
#########-heshuair运行
# dd <- data.table::fread("/data/home/liziyue/ruan/heshuai_pancancer_harmony/00data/TCGA_sur/tcga_RSEM_gene_tpm.gz",data.table = F)
# 
# clin <- data.table::fread("/data/home/liziyue/ruan/heshuai_pancancer_harmony/00data/TCGA_sur/Survival_SupplementalTable_S1_20171025_xena_sp",data.table = F)
# 
# colnames(clin)[2:3] <- c("TCGA_id","type")
# index <- intersect(clin$sample,colnames(dd))
# clin <- clin %>% filter(sample %in% index)
# 
# colnames(dd)[1] <- "gene_id"
# dd <- dd[,c('gene_id',index)]
# 
# ### 探针转换
# gtf1 <- rtracklayer::import('/data/home/liziyue/ruan/heshuai_pancancer_harmony/00data/TCGA_sur/gencode.v23.annotation.gtf.gz')
# gtf_df <- as.data.frame(gtf1)
# 
# 
# ### 提取编码mRNA,基因注释,行列转置
# mRNA_exprSet <- gtf_df %>%
#   select(c(gene_name,gene_id)) %>%
#   inner_join(dd,by ="gene_id") %>%
#   #去除一列
#   select(-gene_id) %>%
#   #去重
#   distinct(gene_name,.keep_all = T) %>%
#   #列名变成行名
#   column_to_rownames("gene_name")
# identical(colnames(mRNA_exprSet),clin$sample)
# #
# exp <- mRNA_exprSet
# 
# #############-----group
# # #----检查分类：是否是肿瘤样本
# table(substring(mRNA_exprSet %>% colnames(),14,15))
# clin$sample_type <- substring(clin$sample,14,15)
# 
# #----只保留原发灶:注意这里只有11是normal,本来>10是normal，这里只有11;
# group_anno <- data.frame(row.names = colnames(exp),
#                          group = substring(colnames(exp),14,15),stringsAsFactors = F)
# group_anno <- group_anno %>% filter(group %in% c('01','11'))
# 
# group_anno$group <- plyr::mapvalues(group_anno$group,from = c('01','11'),
#                                     to = c('Tumor','Normal'))
# exp <- t(exp)
# exp <- exp[group_anno %>% rownames(),]
# save(exp,clin,file = 'TCGA_tpm_data_tumor_normal.RData')
load('/data/home/liziyue/ruan/win/01test_tmp/01CD45/DDX56/TCGA_tpm_data_tumor_normal.RData')

exp_used <- data.frame(row.names = names(exp[,gene_used]),
                       exp = exp[,gene_used],stringsAsFactors = F)

group_anno <- data.frame(row.names = rownames(exp),
                         group = substring(rownames(exp),14,15),stringsAsFactors = F)
group_anno <- group_anno %>% filter(group %in% c('01','11'))

group_anno$group <- plyr::mapvalues(group_anno$group,from = c('01','11'),
                                    to = c('Tumor','Normal'))

exp_used$group <- group_anno$group
clin <- clin %>% column_to_rownames(var=  'sample')

exp_used$tumor <- clin[rownames(exp_used),'type']


keep_tumor <- exp_used %>% select(tumor,group) %>% distinct() %>% group_by(tumor) %>% summarise(num = n())
keep_tumor <- keep_tumor %>% filter(num == 2) %>% pull(tumor)
exp_used <- exp_used %>% filter(tumor %in% keep_tumor)
exp_used$exp <- 2^(exp_used$exp-0.001)


saveRDS(exp_used,file = str_glue('01plot_data_pancancer_{gene_used}_exp.rds'))
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


###########_----TISCH画图：

library(tidyverse)
dat <- read_csv('/data/home/liziyue/ruan/win/CSE1L/TISCH_CSE1L_heatmap.csv')
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