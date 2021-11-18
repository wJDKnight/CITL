# CITL (Causal inference with time-lagged information)
CITL addresses the limitations of the existing methods for inferring time-lagged causal relationships among genes on single-cell RNA sequencing scRNA-seq data. It adopts the changing information of genes estimated by “RNA velocity” for causal inference algorithm. Based on the Time-lagged assumption, CITL can infer time-lagged relationships which are validated by published literature. More information and demonstration is in out article (Wei et al., 2021).     
Here is the R implementation of CITL, including how to prapare data and how to adopt CITL on the data. Before using CITL, annotations of unspliced/spliced reads could be obtained using [velocyto CLI](http://velocyto.org/velocyto.py/tutorial/cli.html) or [kallisto](https://linnarssonlab.org/loompy/kallisto/index.html) first.   

The R package of CITL is at https://github.com/wJDKnight/CITL-Rpackage/ 
# Requirements
  R packages: bnlearn; snow  

# Basic usage
    cd analysis
    Rscript CITL.R Spliced_path changing_path gene_name n_cluster
## Input Parameters  
Input data: current expression matrix (Spliced.csv); changing expression matrix (delta_s.csv); genes' name list (gene_names.csv).
    
    Spliced_path: path to Spliced.csv
    changing_path: path to delta_s.csv
    gene_name: path to gene_names.csv
    n_cluster: the number of computational threads

## Output
A "from-to" table recording the time-lagged causal gene pairs and their cur_cur/cur_cha value (results.csv)  

# Tutorials

## [Preparing data](https://github.com/wJDKnight/CITL/blob/main/tutorial/prepare_data.ipynb) 
This tutorial includes the preparation of two data sets referred in out article is showed. It is about how to exact data from `.loom` file, the parameters out article adopted, and what the shape of requied data is.   
## [Custom CITL](https://wjdknight.github.io/CITL/tutorial/customCITL.html)   
This tutorial describes the procedures of CITL and lists important parameters, which are the same as those in the article. 

# Reference
Wei et al.(2021), Inferring time-lagged causality using the derivative of single-cell expression
