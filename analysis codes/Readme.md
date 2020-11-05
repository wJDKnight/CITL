Input: 
  current expression matrix Spliced.csv; changing expression matrix delta_s.csv; gene name list gene_names.csv; cluster labels (optional)
  
Dependency：
  R package bnlearn; snow
  
Output:
  results.csv a "from-to" table
  
  Parameters:
  
    n_cluster: the number of computational threads
    
    maxsx: the max number of variables in CI test
    
    
