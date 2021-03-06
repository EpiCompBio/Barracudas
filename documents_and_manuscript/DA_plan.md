## Cluster analysis of multi-morbidity in UK Biobank participants

## Data Analysis Plan

### Introduction
  
  This data analysis plan describes the aims, methodology and outcomes that will be used for the cluster analysis of multi-morbidity in UK Biobank participants. These analyses will be carried out by the Barracudas (MSc Health Data Analytics and Machine Learning students).
  
 ![Barracuda](Barracuda.jpg)
 
### Aims
  
  The overall objective of this study is to identify subsets of individuals suffering from multi-morbidity who share common environmental and/or biological pathways. To do this we are going to analyse UK Biobank participants.
  * We will identify sub-groups of individuals suffering from multi-morbidity
  * We will compare cluster analysis methods
  * We will attempt to identify possible shared and distinct underlying physio-pathological processes between clusters

### Dataset
  
  UK Biobank data
  
  UK UK Biobank is a prospective study investigating the contributions of genetic predisposition, and lifestyle and environmental exposures to the development of disease in 500 000 people aged 40-69 in the UK.

  The dataset contains numerical, binary and categorical variables.

  Using the six disease variables present in the dataset, there are 61 000 UK Biobank participants with multimorbidity.
  
  Excluding hypertension, there are roughly 20 000 participants.

### Exploratory Data Analysis
  
  * Dataset overview-table one (Josh)
    * Numbers of diseases across important epidemiological factors e.g. age, sex, smoking, alcohol
  
  * Plot distributions of diseases across important epidemiological factors
  
### Outcomes
  
  * Clusters of Biobank participants with multi-morbidity (across different methods)
  * Data similarity measures/distance metrics for clusters
  
## Clustering
  
### Algorithms
  
  1. K Means (Everyone)
  2. Partioning around Medoids (Joel)
  3. DBSCAN (Joel)
  4. Gaussian Mixture Models (Abi)
  5. DIANA and hierarchical clustering with various linkage methods (Josh)
  
### Pre-Processing
  
  Clustering methods require specific inputs to compute clusters
  Some only work with continuous features (kmeans) while others require a similarity/distance matrix.
  Almost none of the algorithms directly work with the initial mixed data
  
  We have two approaches to our mixed data set, other than working straight on it
  
  1. Make meaningful continuous features out of our dataset through adapted dimensionality reduction
  2. Make meaningful similarity matrices out of the mixed data
  
#### Approach 1
  
  Explore dimensionality reduction techniques for mixed datasets
  
  * FAMD => Mixed PCA with FactoMineR package
  
#### Approach 2
  
  * Use approach one then some distance matrix for continuous features
  * Use **Gower distance** & **random forests** on the whole dataset to compute similarities from the mixed data
  
#### Pipelines 
  
  * Kmeans (BASELINE = BAD)
  
  * FAMD 
    * Kmeans (Looks promising, retrieves real clusters on example data well)
    * PAM 
    * DBSCAN
    
  * RF
    * DBSCAN
    * PAM
 
  * Gower distance matrix 
    * PAM
    * DBSCAN
    * GMM
  
  * Kamila R-package (looks very promising)
  
  * Options to explore if time
  
    *  Auto-encoder (one hot encode everything and throw a neural network at it)
        * Kmeans
        * Some distance calculation => DBSCAN
        * Some distance calculation => PAM 
    * FAMD 
      * Some distance calculation) => PAM
      * Some distance calculation) => DBSCAN
        
## Cluster Evaluation and Interpretation
  
### Cluster Evaluation
  
  * Silhouette coefficient-how well defined are the clusters for each model
  * Calinski-Harabasz criterion
  * Interpretability of the clusters (this will be the big thing)
  
  NOTE : Mathematical criteria all favour certain cluster shapes and thus need to be used carefully. They can be of good use to choose the number of clusters
  for the proper clustering method for example.
  
### Cluster Interpretation
  
  * Interpretation of components and clusters from FAMD
  * Dimensionality reduction and interpretation of coefficients
  * Univariate tests for all variables?
  * Interpretation plots?

