#!/bin/bash
sudo apt-get install libcurl4-openssl-dev libxml2-dev

sudo apt-get -y install r-base r-base-dev
mkdir ~/R ~/R/library
echo 'R_LIBS_USER="~/R/library"' >  $HOME/.Renviron 

sudo cat<<EOF | Rscript -
install.packages(c("RJSONIO","igraph","httr","Unicode","RColorBrewer"),repos="http://cran.rstudio.com/")
biocLite("RCytoscape")
source("http://bioconductor.org/biocLite.R")
EOF
