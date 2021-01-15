# CITL
Causal inference with time-lagged information

The RNA velocity should be pre-calculated (http://velocyto.org/).
  
Dependency：
  R package: bnlearn; snow
  
Usage:

  Rscript CITL.R Spliced_path changing_path gene n_cluster

Output:
  a "from-to" table recording the time-lagged causal gene pairs and their cur_cur/cur_cha value (results.csv)
