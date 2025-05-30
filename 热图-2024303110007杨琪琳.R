####酶与其他因子的相关性，什么影响酶#####
dat <- read.csv("enzyme data.csv",header=T)
head(dat)
colnames(dat)

DJK<-subset(dat,Region=="DJK")#其他方法DJK<-dat[dat$Region=="DJK",]
MES<-subset(dat,Region=="MES")

dat1<-DJK[,c(7:20)]
dat2<-MES[,c(7:20)]

#method1:heatmap上套上

library(psych)
res1<-corr.test(dat1[,1:8],dat1[,10:14],method = "spearman",adjust = "holm")
res1

res2<-corr.test(dat1[,1:8],dat2[,10:14],method = "spearman",adjust = "holm")
res2

res<-cbind(res1$r,res2$r)
res

#extract p value
pval<-cbind(res1$p,res2$p)
pval

#Sig value to symbol*
pval[pval>=0 & pval < 0.001] <- "***"
pval[pval>=0.001 & pval < 0.01] <- "**"
pval[pval>=0.01 & pval < 0.05] <- "*"
pval[pval>=0.05 & pval <= 1] <- ""
pval

library(ComplexHeatmap)
library(circlize)# color package
col_rnorm = colorRamp2(c(-1, 0, 1), c("skyblue","white", "red"))

Heatmap(res,
        col=col_rnorm, 
        cluster_rows = FALSE, cluster_columns = FALSE,
        heatmap_legend_param = list(title = "Spearman corr"),
        column_split = rep(c("DJK","MES"),each=5), #3
        #column_title_gp = gpar(col = c("#1874CD", "#28B489", "#8B7E66")), #"#1874CD", "#28B489", "#C9AA78"
        #column_names_gp = gpar(col = c("#1874CD", "#28B489", "#8B7E66")), #fontsize = c(10, 14, 8) "#1874CD", "#009ACD", "#8B7E66"
        #row_split = rep(c("hi","Invetebrates","Protists","Fungi"),4),
        cell_fun = function(j, i, x, y, width, height, fill) { #Heatmap function present sig by labeling p value as a 'label',not for num matrix
          grid.text(pval[i, j], x, y, vjust = 0.7,
                    gp = gpar(col="black"))#fontsize = 13, put in gpar
        }
)
