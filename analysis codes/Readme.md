prepared data
  current expression matrix (Spliced.csv); changing expression matrix (delta_s.csv); gene name list (gene_names.csv).
  
Dependency：
  R package bnlearn; snow
  

Input Parameters:
  
    Spliced_path: path to Spliced.csv
    changing_path: path to delta_s.csv
    gene_name: path to gene_names.csv
    n_cluster: the number of computational threads
    
Output:
  results.csv a "from-to" table recording time-lagged causal pairs
    
    
