# CITL
Causal inference with time-lagged information

The RNA velocity should be pre-calculated (http://velocyto.org/).
  
Dependency：
  R package: bnlearn; snow
  
Usage:

    Rscript CITL.R Spliced_path changing_path gene_name n_cluster


Prepared data:

  current expression matrix (Spliced.csv); changing expression matrix (delta_s.csv); gene name list (gene_names.csv).
  
Dependency：

  R package bnlearn; snow
  

Input Parameters:
  
    Spliced_path: path to Spliced.csv
    changing_path: path to delta_s.csv
    gene_name: path to gene_names.csv
    n_cluster: the number of computational threads

    
Output:

  a "from-to" table recording the time-lagged causal gene pairs and their cur_cur/cur_cha value (results.csv)
