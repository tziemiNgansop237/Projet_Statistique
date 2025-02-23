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
library(skimr)

# Importer les datasets



data_num <- read_parquet("C:/Users/damso/Documents/data/data_num.parquet")
data_cat <- read_parquet("C:/Users/damso/Documents/data/category_data.parquet")


# Convertir les colonnes de `data_cat` en facteur

data_cat <- lapply(data_cat, as.factor)

data_cat<- as.data.frame(data_cat)

data_num<- as.data.frame(data_num)


# Fusionner les deux bases sur `unique_learner_id`
data_merged <- bind_cols(data_num, data_cat)

# Supprimer la colonne `unique_learner_id`
data_merged <- data_merged %>% select(-unique_learner_id)

# Vérifier les dimensions de la base fusionnée
cat("Dimensions de la base fusionnée :", dim(data_merged), "\n")



# Compter les variables catégorielles et numériques
num_vars <- sum(sapply(data_merged, is.numeric))
cat_vars <- sum(sapply(data_merged, function(x) is.factor(x) | is.character(x)))

# Afficher les résultats
cat("Nombre de variables numériques :", num_vars, "\n")
cat("Nombre de variables catégorielles :", cat_vars, "\n")


famd_final <- FAMD(as.data.frame(data_merged), ncp = 20, graph = FALSE)




# Résumé des résultats finaux
result <- famd_final
res <-summary(result)
result_df <- as.data.frame(result$ind$coord) # Récupérer les coordonnées des individus
result_var_df <- as.data.frame(result$var$coord) # Récupérer les coordonnées des individus
result_df_variance <- as.data.frame(result$eig) # Récupérer les variances expliquées




# Tracer le scree plot (éboullis des valeurs propres)
fviz_screeplot(famd_final, addlabels = TRUE, ylim = c(0, 50))

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

#exportation des dataframe
# Exporter les résultats en CSV
write.csv(result_df, "C:/Users/damso/Documents/data/result_ind_coord.csv", row.names = TRUE)
write.csv(result_var_df, "C:/Users/damso/Documents/data/result_var_coord.csv", row.names = TRUE)
write.csv(result_df_variance, "C:/Users/damso/Documents/data/result_variance.csv", row.names = TRUE)




# Vérification des fichiers exportés
list.files(export_path)
