###PCA for categorical variables
install.packages("FactoMineR")
install.packages("vcd")
install.packages("factoextra")

library(FactoMineR)
library(vcd)
library(factoextra)

#factorial analysis of mixed data (FAMD)
pca_famd <- FAMD(pca_data, ncp=2, graph = TRUE)
pca_famd

#how colours correspond to cos squared and analysis contribution values
fviz_famd_ind(pca_famd, col.ind =  "cos2",
              gradient.cols = c("blue", "orange", "red"),
              repel = TRUE)
fviz_contrib(pca_famd, "var", axes=1)

pca_famd$ind$coord