## AOZORAtools
AOZORA download and analysis tools

## Install
conda create -n AOZORAtools -c conda-forge R r-devtools r-data.table r-reticulate r-r.utils r-mlr r-randomforest python=3.8 unidic-lite mecab-python3 
conda activate AOZORAtools
pip install asari
R
devtools::install_github('bigbrolv/AOZORAtools')


## Use
library(devtools)
library(data.table)

library(R.utils)
library(mlr)
library(randomForest)
library(reticulate)
library(AOZORAtools)
