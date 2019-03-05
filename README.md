# Barracudas : Translational Datas Science project

## Methods for dimensionality reduction : 

### Principal Components Analysis (Not useful here since we have mixed data, but it is for the principle)

Dimensionality reduction through Principal Components Analysis 

### Factor Analysis of Mixed Data : 

Dimensionality reduction of mixed data through FAMD

http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/115-famd-factor-analysis-of-mixed-data-in-r-essentials/

http://www.numdam.org/article/RSA_2004__52_4_93_0.pdf


### Auto-encoders (THIS IS OUT)

Dimensionality reduction through Neural Networks :

https://blog.keras.io/building-autoencoders-in-keras.html 

This doesn't appear to work very well, since even for model data the clusters aren't correctly retrieved with stuff like kmeans. 


## Methods for clustering : 


### Kmeans

### Partitioning around medoids

https://en.wikipedia.org/wiki/K-medoids

### Spectral Clustering 

Changing the space so it becomes easier to makes clusters with stuff like kmeans

http://www.di.fc.ul.pt/~jpn/r/spectralclustering/spectralclustering.html#using-r-kernlab-package

### DBSCAN

Density based clustering

https://www.aaai.org/Papers/KDD/1996/KDD96-037.pdf

### Affinity Propagation 

http://science.sciencemag.org/content/315/5814/972

### Gaussian Mixture Models 

Model based clustering 

https://www.coursera.org/learn/ml-clustering-and-retrieval/home/week/4

### Unsupervised Random Forests 

Compute a proximity matrix through random forests and then use a standard algorithm such as PAM

# Methods to analyse clustering results  : 

### Silhouette : 

Mean proximity to points in cluster vs closest point of other cluster

https://en.wikipedia.org/wiki/Silhouette_(clustering)

### Other clustering criteria : 

https://cran.r-project.org/web/packages/clusterCrit/vignettes/clusterCrit.pdf
