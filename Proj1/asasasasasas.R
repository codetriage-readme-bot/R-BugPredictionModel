# Selecting the Project
setwd("C:/Users/VeNuSs/Documents/CS 6890/lucene/lucene")

# Reading files from project
sv = read.csv("single-version-ck-oo.csv",sep=";",header = TRUE,row.names=1)
sv_xb=sv[,1:18]
cm = read.csv("change-metrics.csv",sep=";",header = TRUE,row.names=1)
cm_xb=cm[,1:16]
bm = read.csv("bug-metrics.csv",sep=";",header = TRUE,row.names=1)
bm_xb=bm[,1:6]

# Finding correlation using spearman method
cor_sv=cor(sv_xb,method="spearman")
cor_cm=cor(cm_xb,method="spearman")
cor_bm=cor(bm_xb,method="spearman")

#Sorting the bugs column, selecting the top five variables and creating a dataframe, finding correlation of newly created dataframe 
svb=sort(cor_sv[,18],decreasing=TRUE)[2:6]
dfsv_names = names(svb)
dfsv = sv_xb[,dfsv_names]
cor_lucenesv= cor(dfsv,method="spearman")
View(cor_lucenesv)

cmb=sort(cor_cm[,16],decreasing=TRUE)[2:6]
dfcm_names=names(cmb)
dfcm= cm_xb[,dfcm_names]
cor_lucenecm= cor(dfcm,method="spearman")
View(cor_lucenecm)

bmb=sort(cor_bm[,6],decreasing=TRUE)[2:3]
dfbm_names=names(bmb)
dfbm= bm_xb[,dfbm_names]
cor_lucenebm= cor(dfbm,method="spearman")
View(cor_lucenebm)