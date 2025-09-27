# this script is  > co -occurrence and correlation of ct findings.
#--------------------------------------------------------------

library(ggplot2)
library(reshape2)
library(dplyr)

ct_names <- c("Nodule_CT","GGO_CT","Consolidation_CT","Halo_CT","Pleuraleffusion_CT","Cavity_CT")
df_3[ct_names] <- lapply(df_3[ct_names], function(x) ifelse(x==0,1,0))
co_occurance <- t(as.matrix(df_3[ct_names])) %*%  as.matrix(df_3[ct_names])
co_long <- melt(co_occurance)
colnames(co_long) <- c("CT1","CT2","Count")
library(ggplot2)
ggplot(co_long, aes(x = CT1, y= CT2, fill= Count)) +
    geom_tile(color= "white") +
    geom_text(aes(label = Count)) +
    scale_fill_gradientn(colors = c("white", "orange", "red")) +
    theme_minimal() +
    labs(title = "CT findings " , fill = "n") +
    theme(axis.text.x = element_text(angle= 45, hjust=1))

#the main heatmap
library(reshape2)
library(ggplot2)

mat <- co_occurance
mat[upper.tri(mat, diag = TRUE)] <- NA 

df <- melt(mat, na.rm = TRUE)
colnames(df) <- c("Var1", "Var2", "value")


ggplot(df, aes(x = Var2, y = Var1, fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 0)), size = 5) +
    scale_fill_gradientn(colors = c("white", "orange", "red")) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        panel.grid = element_blank()
    ) +
    labs(x = "", y = "", fill = "Co-occurrence")
#cor
df_3[ct_names] <- lapply(df_3[ct_names], function(x) ifelse(x==0,1,0))
ct_matrix <- as.matrix(df_3[ct_names]) 
cor_mat<- cor(ct_matrix,method = "pearson")

