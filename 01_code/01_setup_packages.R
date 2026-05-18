# Instalación de la librería Pacman
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# Agregamos la librerias necesarias
library(pacman)
p_load(rio,tidyverse,sf,tidymodels,gt,gtsummary,osmdata,leaflet,spatialsample, 
       stringr,stringi,dplyr,caret,glmnet,rpart,e1071,pROC,patchwork,PRROC,
       ranger,ROSE,here,kableExtra,ggplot2,tidyr,knitr,webshot2)
