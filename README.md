# CITL
causal inference with time-lagged information

The RNA velocity should be pre-calculated (http://velocyto.org/).


Input of CITL: 
  current expression matrix (spliced.csv); changing expression matrix (delta_s.csv); gene name list (gene_names.csv)
Dependency：
  R package bnlearn; snow
Output:
  a "from-to" table recording the time-lagged causal gene pairs and their cur_cur/cur_cha value (results.csv)
