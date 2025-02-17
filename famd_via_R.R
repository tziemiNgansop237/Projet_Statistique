# Charger les bibliothèques nécessaires
if (!require(FactoMineR)) install.packages("FactoMineR")
if (!require(factoextra)) install.packages("factoextra")
if (!require(arrow)) install.packages("arrow")  # Pour lire les fichiers Parquet
if (!require(dplyr)) install.packages("dplyr")  # Pour la manipulation de données

library(FactoMineR)
library(factoextra)
library(arrow)
library(dplyr)
library(tidyverse)
# Importer les datasets
data_num <- read_parquet("C:/Users/damso/Documents/Projet_Statistique/data_num.parquet")
data_cat <- read_parquet("C:/Users/damso/Documents/Projet_Statistique/category_data.parquet")

# Vérifier les structures des datasets
skim(data_num)  # Aperçu des données numériques
skim(data_cat)  # Aperçu des données catégoriques

# Convertir les colonnes de `data_cat` en facteur

data_cat <- lapply(data_cat, as.factor)

data_cat<- as.data.frame(data_cat)

data_num<- as.data.frame(data_num)

write_parquet(data_cat,"C:/Users/damso/Documents/Projet_Statistique/categoryConverted_data.parquet")

# Fusionner les deux bases sur `unique_learner_id`
data_merged <- bind_cols(data_num, data_cat)
  
skim(data_merged)  
# Supprimer la colonne `unique_learner_id`
data_merged <- data_merged %>% select(-unique_learner_id)


write_parquet(data_merged,"C:/Users/damso/Documents/Projet_Statistique/dataReductable.parquet")


# Vérifier les dimensions de la base fusionnée
cat("Dimensions de la base fusionnée :", dim(data_merged), "\n")


famd0 <- FAMD(as.data.frame(data_merged), ncp = 30, graph = FALSE)

famd1 <- FAMD(as.data.frame(data_merged), ncp = 15, graph = FALSE)

famd2 <- FAMD(as.data.frame(data_merged), ncp = 10, graph = FALSE)

famd3<-  FAMD(as.data.frame(data_merged), ncp = 5, graph = FALSE)

famd_final <- FAMD(as.data.frame(data_merged), ncp = 2, graph = FALSE)






# Résumé des résultats finaux
result0 <- famd0
res0 <-summary(result0)
result_df0 <- as.data.frame(result0$ind$coord) # Récupérer les coordonnées des individus
result_var_df0 <- as.data.frame(result0$var$coord) # Récupérer les coordonnées des individus
result_df_variance0 <- as.data.frame(result0$eig) # Récupérer les variances expliquées


# Résumé des résultats finaux
result1 <- famd1
res1 <-summary(result1)
result_df1 <- as.data.frame(result1$ind$coord) # Récupérer les coordonnées des individus
result_var_df1 <- as.data.frame(result1$var$coord) # Récupérer les coordonnées des individus
result_df_variance1 <- as.data.frame(result1$eig) # Récupérer les variances expliquées


# Résumé des résultats finaux
result2 <- famd2
res2 <-summary(result2)
result_df2 <- as.data.frame(result2$ind$coord) # Récupérer les coordonnées des individus
result_var_df2 <- as.data.frame(result2$var$coord) # Récupérer les coordonnées des individus
result_df_variance2 <- as.data.frame(result2$eig) # Récupérer les variances expliquées


# Résumé des résultats finaux
result3 <- famd3
res3 <-summary(result3)
result_df3 <- as.data.frame(result3$ind$coord) # Récupérer les coordonnées des individus
result_var_df3 <- as.data.frame(result3$var$coord) # Récupérer les coordonnées des individus
result_df_variance3 <- as.data.frame(result3$eig) # Récupérer les variances expliquées


# Résumé des résultats finaux
result <- famd_final
res <-summary(result)
result_df <- as.data.frame(result$ind$coord) # Récupérer les coordonnées des individus
result_var_df <- as.data.frame(result$var$coord) # Récupérer les coordonnées des individus
result_df_variance <- as.data.frame(result$eig) # Récupérer les variances expliquées

print(famd1$eig)

print(famd2$eig)

print(famd3$eig)


print(famd_final$eig)


# Tracer le scree plot (éboullis des valeurs propres)
fviz_screeplot(famd0, addlabels = TRUE, ylim = c(0, 50))

# Tracer la variance expliquée cumulée
plot(cumsum(eig_values[,2]), type = "b", pch = 19, col = "blue",
     xlab = "Nombre de dimensions", ylab = "Variance expliquée cumulée (%)",
     main = "Variance expliquée cumulée par les composantes")
abline(h = 80, col = "red", lty = 2)  # Ligne de référence à 80%




# Définir le chemin d'exportation
export_path <- "C:/Users/damso/Documents/Projet_Statistique"

# Vérifier si le dossier existe, sinon le créer
if (!dir.exists(export_path)) {
  dir.create(export_path, recursive = TRUE)
}

# Créer une liste pour stocker les résultats des différentes FAMD
famd_list <- list(
  famd0= famd0,
  famd1 = famd1,
  famd2 = famd2,
  famd3 = famd3,
  famd_final = famd_final
)

# Initialiser des listes pour stocker les résultats
result_total <- list()
res_total <- list()
coord_ind_total <- list()
coord_var_total <- list()
variance_total <- list()

# Boucle pour extraire, stocker et exporter les résultats
for (name in names(famd_list)) {
  famd <- famd_list[[name]]
  
  result_total[[name]] <- famd
  res_total[[name]] <- summary(famd)
  coord_ind_total[[name]] <- as.data.frame(famd$ind$coord)  # Coordonnées des individus
  coord_var_total[[name]] <- as.data.frame(famd$var$coord)  # Coordonnées des variables
  variance_total[[name]] <- as.data.frame(famd$eig)         # Variance expliquée
  
  # Exporter les fichiers CSV avec les noms appropriés
  write.csv(coord_ind_total[[name]], file = file.path(export_path, paste0(name, "_coord_ind.csv")), row.names = TRUE)
  write.csv(coord_var_total[[name]], file = file.path(export_path, paste0(name, "_coord_var.csv")), row.names = TRUE)
  write.csv(variance_total[[name]], file = file.path(export_path, paste0(name, "_variance.csv")), row.names = TRUE)
}

# Vérification des fichiers exportés
list.files(export_path)
