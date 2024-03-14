
library(impute)
library(limma)
library(tidyverse)
rt <- read.table("cellmer_drug.txt",sep="\t",header=T,check.names=F)
rt <- as.matrix(rt)
rownames(rt) <- rt[,1]
drug <- rt[,2:ncol(rt)]
dimnames <- list(rownames(drug),colnames(drug))
data <- matrix(as.numeric(as.matrix(drug)),nrow=nrow(drug),dimnames=dimnames)

#----插补
exp <- read.table("cellmer_geneExp.txt", sep="\t", header=T, row.names = 1, check.names=F)
dim(exp)
exp[1:4, 1:4]


Gene <- gene_used<- genelist <- 'CSE1L'

exp <- exp[genelist,]

outTab <- data.frame()
drug <- data
i <- 1
# for(Gene in row.names(exp)){
  x <- as.numeric(exp[Gene,])
  for(Drug in row.names(drug)){
    y <- as.numeric(drug[Drug,])
    temp  <- try(cor.test(x,y,method="spearman",na.action = "na.rm"),silent=FALSE)
    if('try-error' %in% class(temp))            
    {
      temp <- NA                               
    }else{
      corT <- temp
      cor <- corT$estimate
      pvalue <- corT$p.value
      if(pvalue < 0.05){
        outVector <- cbind(Gene,Drug,cor,pvalue)
        outTab <- rbind(outTab,outVector)
      }
    }
i <- i+1
}
# }
plot_dat <- outTab
plot_dat[,'_log10p'] <- -log10(plot_dat$pvalue %>% as.numeric())
plot_dat$cor <- as.numeric(plot_dat$cor)
plot_dat <- plot_dat %>% distinct()
library(RColorBrewer)
library(ggpubr)  
cols <- colorRampPalette(brewer.pal(10, "RdBu"))(50) %>% rev()
plot_dat$group <- ifelse(plot_dat$cor>0,
                         'Positive correlation',
                         'Negative correlation')
plot_dat$Group <- factor(plot_dat$group %>% as.character(),levels = c('Positive correlation',
                                                                      'Negative correlation'))

setwd('E:/pan-cancer-gene/CSE1L')
library(ggpubr)
plot_dat <- read_rds('plot_dataCSE1L_yaomin.rds')
gene_used <- 'CSE1L'
plot_dat$group <- ifelse(plot_dat$cor>0,
                         'positive_cor',
                         'negtive_cor')
plot_dat$Group <- factor(plot_dat$group %>% as.character(),levels = c('正相关',
                                                                      '负相关'))

png(str_glue('{gene_used}_drugsensitivity.png'),res = 300,height = 5,width = 7,units = 'in')
ggbarplot(plot_dat,x = 'Drug',y = 'cor',fill = "Group",
          sort.val = "asc",          # Sort the value in descending order
          sort.by.groups = FALSE,
          rotate = TRUE,
          palette = 'jco',color= 'white'
        )+labs(y = 'Spearman',x = '',fill = 'group')
dev.off()

saveRDS(plot_dat,file = str_glue('plot_data{gene_used}_drugsensitivity.rds'))
