################################################################################
#                        Rental Dataset: PCA, FA & Clustering                   #
################################################################################
# Author: Moka Kaleji                                                             
# Affiliation: Statistics for high-dimensional dataset, University of Bologna             
#                                                                                 
# Description:
#   This script conducts a comprehensive exploratory analysis on the rental dataset,
#   using Principal Component Analysis (PCA) for dimensionality reduction, Factor
#   Analysis (FA) for latent structure, and Clustering Analysis (CA) for segmenting
#   observations. Outputs include diagnostic plots, cluster assignments, and
#   a summary table exported to Word.
################################################################################

########################## 1. Load Required Libraries ###########################
library(factoextra)  # Clustering visualization and diagnostics
library(cluster)     # Hierarchical clustering and silhouette analysis
library(ggplot2)     # General-purpose plotting
library(NbClust)     # Determine optimal number of clusters
library(dplyr)       # Data manipulation
library(psych)       # Factor Analysis functions
library(flextable)   # Create Word tables
library(officer)     # Export Word documents
################################################################################

########################## 2. Data Preparation #################################
# Load dataset; expects a header row with variable names.
data <- read.table("rental.txt", header = TRUE)

# Exclude non-numeric identifier (e.g., city name) for multivariate analysis
data_numeric <- data[, -1]

# Standardize variables to zero mean, unit variance
data_scaled <- scale(data_numeric)
################################################################################

########################## 3. Principal Component Analysis ######################
# Compute PCA on standardized data, extract loadings and scores
pca_model <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

# Retain first three principal components for downstream clustering
pca_data <- as.data.frame(pca_model$x[, 1:3])

# Optional: Visualize scree plot to inspect explained variance
fviz_screeplot(pca_model, addlabels = TRUE, ylim = c(0, 50)) +
  ggtitle("Scree Plot: Variance Explained by Principal Components")
################################################################################

#################### 4. Optimal Cluster Selection #############################
# Elbow method: within-cluster sum of squares
fviz_nbclust(pca_data, kmeans, method = "wss") +
  ggtitle("Elbow Method: Optimal Number of Clusters")

# Silhouette method: average silhouette width
fviz_nbclust(pca_data, kmeans, method = "silhouette") +
  ggtitle("Silhouette Analysis: Optimal Number of Clusters")

# NbClust: majority rule across multiple indices (optional)
# nb <- NbClust(pca_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
# fviz_nbclust(nb) + ggtitle("NbClust Results for Optimal Clusters")
################################################################################

########################## 5. K-Means Clustering ################################
set.seed(123)  # For reproducibility
num_clusters <- 3  # Choose based on diagnostic results
kmeans_model <- kmeans(pca_data, centers = num_clusters, nstart = 25)

# Attach cluster labels to original dataset
data$Cluster <- factor(kmeans_model$cluster)

# Visualize clusters in PCA space
fviz_cluster(kmeans_model, data = pca_data, geom = "point", ellipse.type = "convex") +
  ggtitle("K-Means Clusters Projected into PCA Space")
################################################################################

########################## 6. Cluster Summary Table ############################
# Compute mean values of key variables by cluster
data_summary <- data %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Population    = round(mean(pop,    na.rm = TRUE), 2),
    Avg_Rented_Housing = round(mean(rnthsg, na.rm = TRUE), 2),
    Avg_Total_Housing  = round(mean(tothsg, na.rm = TRUE), 2),
    Avg_Rent           = round(mean(rent,   na.rm = TRUE), 2),
    Avg_Income         = round(mean(avginc, na.rm = TRUE), 2)
  )

# Create Word-friendly flextable
ft_clusters <- flextable(data_summary) %>%
  set_caption("Table 1: Cluster Descriptive Statistics") %>%
  autofit()

# Export summary table to a Word document
doc <- read_docx() %>%
  body_add_flextable(ft_clusters)

print(doc, target = "Cluster_Descriptions.docx")
################################################################################

########################## 7. Factor Analysis (FA) ################################
# Perform exploratory factor analysis; choose number of factors by parallel analysis
fa_parallel <- fa.parallel(data_scaled, fa = "fa", n.iter = 100)

# Extract factor loadings and scores using Maximum Likelihood
num_factors <- fa_parallel$nfa  # Suggested by parallel analysis
fa_model <- fa(data_scaled, nfactors = num_factors, rotate = "varimax", scores = "regression")

# Attach FA scores to data frame
fa_scores <- as.data.frame(fa_model$scores)
fa_scores$Cluster <- data$Cluster

# Visualize cluster assignments in the space of the first two FA dimensions
ggplot(fa_scores, aes(x = ML1, y = ML2, color = Cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  ggtitle("Cluster Membership in Factor Analysis Space")
################################################################################

#################### 8. Clustering Quality Validation ##########################
# Compute silhouette widths to assess cluster separation
sil <- silhouette(kmeans_model$cluster, dist(pca_data))

# Plot silhouette scores
fviz_silhouette(sil) +
  ggtitle("Silhouette Plot: Cluster Cohesion and Separation")
################################################################################
