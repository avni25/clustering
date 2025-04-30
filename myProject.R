# install.packages("readxl")
# install.packages(c("tidyverse", "cluster", "factoextra"))
# Load necessary libraries
# Load necessary libraries
library(readxl)
library(dplyr)
library(factoextra)
library(cluster)
library(ggplot2)
library(fpc)
library(dbscan)
library(NbClust)
library(clValid)

FUNclusters <- c("kmeans", "pam", "clara", "fanny","hclust", "agnes", "diana")
hc_methods  <- c("average", "ward.D", "ward.D2", "complete", "single", "centroid", "median")
hc_metrics  <- c("manhattan", "maximum", "canberra", "binary", "minkowski")
# hc_clusts   <- c("ward.D", "ward.D2", "single", "complete","average","mcquitty","median" , "centroid")

# Step 1: Load the data
my_data <- read_excel("ramp_db.xlsx", sheet = "Sheet7")


# -----------------------Prepare Data------------------------------------------------------

# Define column names (including city_name)
num_cols <- c("city_code","country_id","city_id", "flight_amount", "cat_A", "cat_B", "cat_C", "cat_D", "cat_E", "cat_F",
              "ron", "avg_ground_time", as.character(1:24))

# Subset only those columns
my_data <- my_data[, num_cols]

# Remove rows with missing values
my_data <- na.omit(my_data)

# Save city names for labeling
city_names <- my_data$city_code

# numeric_data <- my_data %>% select(-c("country_id", "city_id")) # Exclude country_id and city_id

# Extract only numeric columns for clustering
numeric_data <- my_data %>% select(-city_code)

# Remove columns with zero variance
numeric_data <- numeric_data[, apply(numeric_data, 2, var) != 0]

# Scale the data
data_scaled <- scale(numeric_data)

# Set row names to city names
rownames(data_scaled) <- city_names
# ------------------------------------------------------------------------------------------


# -----------------------Determine optimal number of clusters-------------------------------
# Determine optimal number of clusters with (Elbow method)
set.seed(123)
fviz_nbclust(data_scaled, kmeans, method = "wss") + labs(title = "Elbow Method for Determining Optimal Clusters")

# Determine optimal number of clusters with (silhouette method)
fviz_nbclust(data_scaled, kmeans, method="silhouette") + theme_classic()
# ------------------------------------------------------------------------------


# ------------ Choosing the Best Clustering Algorithms -----------------------
clmethods  <- c("hierarchical","kmeans","pam","clara")
intern     <- clValid(data_scaled, nClust=2:6, clMethods=clmethods, validation="internal")
print(summary(intern))
# ------------------------------------------------------------------------------

# ------------ KMEANS ANALYSIS -----------------------

set.seed(123)
kmeans_max_sil_val <- 0
temp_metric <- ""
cluster_num <- 2
for(cluster_amount in 2:10){
  for (i in 1:length(hc_metrics)) {
    kmeans_ev     <- eclust(data_scaled, FUNcluster="kmeans", hc_metric=hc_metrics[i], k=cluster_amount, nstart=25,graph=FALSE) 
    sil_val <- kmeans_ev$silinfo$avg.width
    if(sil_val > kmeans_max_sil_val){
      kmeans_max_sil_val <- sil_val
      temp_metric <- hc_metrics[i]
      cluster_num <- cluster_amount
      cat("kmeans_max_sil_val: ", kmeans_max_sil_val, " hc_metric= ", hc_metrics[i], " cluster_num = ", cluster_amount, "\n")
    }
  }
}

kmeans_result <- kmeans(data_scaled, centers = cluster_num, nstart = 25)
kmeans_ev     <- eclust(data_scaled, FUNcluster="kmeans", hc_metric=temp_metric, k=cluster_num, nstart=25,graph=FALSE) 
print(kmeans_result$cluster)
cat("kmeans_result sil value: ",kmeans_ev$silinfo$avg.width)
fviz_silhouette(kmeans_ev, palette="jco", ggtheme=theme_classic())


dd <- cbind(my_data, cluster = kmeans_result$cluster) # add cluster column to elements
print(dd)

# Visualize clusters with city names with kmeans
fviz_cluster(kmeans_result, data = data_scaled,
             geom = c("point", "text"),
             repel = FALSE,
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "K-Means Clustering of Cities")
# ------------------------Evaluate Results---------------------------------------------

# silhouette coefficient: shows how element suitable for the cluster it is assigned[-1, 1]
max_sil_value <- 0
for(cluster_amount in 2:10){
  for (i in 1:length(FUNclusters)) {
    for(j in 1:length(hc_metrics)){
      kmeans_result <- eclust(data_scaled, FUNcluster=FUNclusters[i], hc_metric=hc_metrics[j], k=cluster_amount, nstart=25,graph=FALSE)
      fviz_silhouette(kmeans_result, palette="jco", ggtheme=theme_classic())
      sil_val <- kmeans_result$silinfo$avg.width
      if (sil_val > max_sil_value) {
        max_sil_value <- sil_val
        print(paste("max_sil_val: ", max_sil_value ," FUNCluster =", FUNclusters[i], " hc_metrics =", hc_metrics[i], "cluster_amount: ", cluster_amount))
      } 
    }
  }  
}
cat("max sil val: ", max_sil_value)
kmeans_result <- eclust(data_scaled, FUNcluster="pam", hc_metric="maximum", k=2, nstart=25,graph=FALSE)
fviz_silhouette(kmeans_result, palette="jco", ggtheme=theme_classic())
print(kmeans_result$silinfo)
cat("kmeans sil value: ",kmeans_result$silinfo$avg.width) # silhouette value

# Visualize clusters with city names with kmeans
fviz_cluster(kmeans_result, data = data_scaled,
             geom = c("point", "text"),
             repel = FALSE,
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "K-Means Clustering of Cities")

# --------------------------CLARA----------------------------------------------------
clara_metrics <- c("euclidean", "manhattan")
clara_max_sil_val <- 0
for(cluster_amount in 2:10){
  for(i in 1:length(clara_metrics)){
    clara_res <- clara(data_scaled, k=cluster_amount, metric = clara_metrics[i], samples = 50, pamLike = TRUE)
    sil_val = clara_res$silinfo$avg.width
    if(sil_val > clara_max_sil_val){
      clara_max_sil_val <- sil_val
      cat("clara_max_sil_val: ", sil_val, " k= ", cluster_amount," metric = ", clara_metrics[i] ,"\n")
    }
  }
}

clara_res <- clara(data_scaled, k=2, metric = "manhattan", samples = 50, pamLike = TRUE)
print(clara_res)
fviz_silhouette(clara_res, palette="jco", ggtheme=theme_classic()) # visualize silhouette values
print(clara_res$silinfo) # Evaluate Results
cat("clara sil value: ",clara_res$silinfo$avg.width) # silhouette value

# Visualize clusters with city names with CLARA
fviz_cluster(clara_res, data = data_scaled,
             geom = c("point", "text"),
             repel = FALSE,
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "CLARA Clustering of Cities")

# Visualize CLARA clusters
fviz_cluster(clara_res,
             # palette=c("#00AFBB","#FC4E07"),#color palette
             ellipse.type="t",# Concentrationellipse
             geom="point",pointsize=1,
             ggtheme=theme_classic())

# --------------------------- DBSCAN ---------------------------------------------

set.seed(123)

dbscan::kNNdistplot(data_scaled, k=3) 
abline(h=7, lty = 2) # determining the optimal eps value
db<-fpc::dbscan(data_scaled,eps=7,MinPts=3)

# Visualize DBSCAN clusters
fviz_cluster(db,data=data_scaled,stand=FALSE,
             ellipse=FALSE,show.clust.cent=FALSE,
             geom="point",palette="jco",ggtheme=theme_classic())

# --------------------------- AGGLOMERATIVE HIERARCHICAL CLUSTERING ---------------------------------------------

dist_methods <- c( "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")

cor_coph <- 0
dist_method <- ""
hc_method <- ""
final_hc_res <- NULL  

for(i in 1:length(dist_methods)){
  for(j in 1:length(hc_methods)){
    res_dist  <- dist(data_scaled,  method=dist_methods[i])
    hc_result <- hclust(d=res_dist, method=hc_methods[j]) # hc_methods
    # Visualize dendrograms
    res_coph <- cophenetic(hc_result)
    # The closer the value of the correlation coefficient is to 1, the more accurately the clustering solution reflects your data
    temp_cor_coph <- cor(as.vector(res_dist), as.vector(res_coph))
    if(!is.na(temp_cor_coph) && temp_cor_coph > cor_coph){
      cor_coph    <- temp_cor_coph
      dist_method <- dist_methods[i]
      hc_method   <- hc_methods[j]
      final_hc_res <- hc_result
      cat("cor_coph: ", cor_coph, " dist_method= ",dist_method," hc_method= ", hc_method,"\n")
    }
  }
}



# Cutin 4  groups and color by groups
if (!is.null(final_hc_res)){
  fviz_dend(final_hc_res,
            k=4,     #Cutin four groups
            cex=0.5, #labelsize
            k_colors=c("#2E9FDF","#00AFBB","#E7B800","#FC4E07"),
            color_labels_by_k=TRUE,  # colorlabelsbygroups
            rect=TRUE               #Add rectangle around groups
  )
}else {
  cat("No valid clustering result found.\n")
}



res_dist  <- dist(data_scaled,  method="ward.D2")
hc_result <- hclust(d=res_dist, method="average") 
res_coph  <- cophenetic(hc_result)
cor_coph  <- cor(res_dist, res_coph)

fviz_dend(hc_result,
          k=4,     #Cutin four groups
          cex=0.5, #labelsize
          k_colors=c("#2E9FDF","#00AFBB","#E7B800","#FC4E07"),
          color_labels_by_k=TRUE,  # colorlabelsbygroups
          rect=TRUE               #Add rectangle around groups
)

# --------------------------- PAM ---------------------------------------------

pam_metrics <- c("euclidean", "manhattan")

fviz_nbclust(data_scaled,pam,method="silhouette") + theme_classic()

pam_res <- pam(data_scaled, 2, metric="manhattan")
cat("pam_sil_value: ",pam_res$silinfo$avg.width)

fviz_cluster(pam_res,
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # Concentration ellipse
             repel = FALSE, # Avoid label overplotting (slow)
             ggtheme = theme_classic()
)
















