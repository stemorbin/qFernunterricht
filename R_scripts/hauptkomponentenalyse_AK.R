setwd("/home/cygnid/OneDrive/PHLU/BS/MA/V2/stat/")
source("data_manip.R")

#-------------------------- LIBRARIES LADEN ---------------------------------------------------------------
library(GGally)
library(psych)
library(FactoMineR)
library(factoextra)

#-------------------------- DATENMANIPULATION  ------------------------------------------------------------

pupData_items <- pupData_items_long_rc %>% select(rater, item, value) %>% pivot_wider(names_from = item, values_from = value)
teaData_items <- teaData_items_long_rc %>% select(rater, item, value) %>% pivot_wider(names_from = item, values_from = value)

#-------------------------- KOMPONENTEN ---------------------------------------------------------------------
factAnaly <- function (fData, fGroup) {
  cor.mat <- cor_mat(fData %>% select(starts_with(fGroup)), method = "kendall")
  cor.pmat <-cor.mat %>% cor_get_pval()
  
  cat("\n")
  print(cor.pmat)

  cPlot <- ggcorr(fData %>% select(starts_with(fGroup)),
                  method = c("complete", "kendall"),
                  nbreaks = 9,
                  high = mycols[5],
                  label = TRUE,
                  label_size = 4,
                  label_round = 1)
  
  ggsave("cor_vak.png", width = 200, height = 200, units = "mm", plot = cPlot)
  
  cat("\n")
  print(solve(data.matrix(cor.mat)[,-1]))

  cat("\n")
  print(KMO(fData %>% select(starts_with(fGroup))))

  res.pca <- PCA(fData %>% select(starts_with(fGroup)),
                 scale = TRUE,
                 graph = FALSE)

  cat("\n Eigenwerte\n")
  print(get_eigenvalue(res.pca))

  sPlot <- fviz_eig(res.pca, choice = "eigenvalue",
                    addlabels = TRUE,
                    barfill = mycols[5],
                    barcolor = mycols[5],
                    xlab = "Komponente",
                    ylab = "Eigenwert",
                    ylim = c(0, 3),
                    main = "")
  
  ggsave("scree_vak.png", width = 200, height = 200, units = "mm", plot = sPlot)

  ePlot <- fviz_pca_var(res.pca,
                        col.var = "cos2",
                        gradient.cols = c("#00AFBB", "#808080", mycols[5]),
                        repel = TRUE,
                        title = "",
                        ylab = paste("Komponente 2 (", round(get_eigenvalue(res.pca)[2,2],1), "%)", sep = ""),
                        xlab = paste("Komponente 1 (", round(get_eigenvalue(res.pca)[1,2],1), "%)", sep = "")
  )

  ggsave("vfm_vak.png", width = 200, height = 200, units = "mm", plot = ePlot)
  
  print(factanal(fData %>% select(starts_with(fGroup)),
                 factor=2,
                 rotation="varimax",
                 scores="regression"))

  return(list(cPlot, sPlot, ePlot))
}

#-------------------------- KOMPONENTENANALYSE -----------------------------------------------------------------
factAnaly(pupData_items, "VAK")
factAnaly(teaData_items, "LVAK")

AK_fac1 <- pupData_items %>% select(VAK03,VAK05,VAK06,VAK07) + 2
AK_fac1 <- AK_fac1 %>% rowwise() %>% mutate(m = mean(c(VAK03,VAK07,VAK05,VAK06)))
AK_fac1 <- AK_fac1 - 2

AK_fac2 <- pupData_items %>% select(VAK01,VAK02,VAK04) + 2
AK_fac2 <- AK_fac2 %>% rowwise() %>% mutate(m = mean(c(VAK01,VAK02,VAK04)))
AK_fac2 <- AK_fac2 - 2

summary(AK_fac1$m)
summary(AK_fac2$m)

plot(density(AK_fac1$m))
plot(density(AK_fac2$m))

wilcox.test(AK_fac1$m,AK_fac2$m)