<style> 
td {
    white-space:nowrap
}
table {
    overflow: scrollX;
}
</style>

## Data Description

This directory contains the code that preprocess the shotgun proteomics data from Maurice as well as the preprocessed and cleaned data.

SEC fractions were analyzed using a shotgun proteomics method. Very briefly, samples were first digested using trypsin, peptides were extracted and resuspended and injected in to a LCMS. The LCMS data was processed using the commercial software [Byonic](https://www.proteinmetrics.com/products/byonic/). The `data-raw` directory from the top-level folder has the data exported from the Byonic. The protein list generated by Byonic was put through a filter to come up with a final list of proteins. For a given protein, if the protein identification from any of the 8 fractions passed the filter, was kept in the final list. The parameters of the filter are below.

| parameter            	| cutoff (>=) 	|
|----------------------	|-------------	|
| # of unique peptides 	| 2           	|
| # of spectra         	| 0           	|
| \|Log Prob\|          | 4.6         	|
| Best \|Log Prob\|     | 6.9         	|
| Best score           	| 400         	|
| Coverage %           	| 0           	|
| # AA's in protein    	| 0           	|

Some proteins were also manually excluded because they seem to be contaminant. Those are: K1C14, K2C6B, K2C1, K1C10, K2C5, K1C9, K22E, K2C1B, TRY1, TRY3, TRY6, SHRM3.

The protein list is defined now and the next thing is to quantify them. We use the XIC (e**X**tracted **I**on **C**urrent) given by the Byonic software. Each spectrum has a XIC value. We decide which spectrum to report as a representative spectrum for quantification based on two criteria: 

1. It is observed in most samples 
2. It has the highest response (higest value).

The XIC table generated are then stored in the `data.rda` file. To get the access to the data, use the command below in R.

```r
xic = readRDS("data/xic.rds")
```

The `xic.rds` contains a single `data.frame` object.

```r
head(xic)
```

|uniprot_id |short_name |long_name                  |species                         | OX|GN | PE|  n|peptide_label |sequence        | charge|Mod_AAs |ms_alias |        F0|        F1|         F2|         F3|         F4|         F5|         F6|         F7| sample_coverage|
|:----------|:----------|:--------------------------|:-------------------------------|--:|:--|--:|--:|:-------------|:---------------|------:|:-------|:--------|---------:|---------:|----------:|----------:|----------:|----------:|----------:|----------:|---------------:|
|P02768     |ALBU       |HUMAN Serum albumin        |Homo sapiens GN=ALB PE=1 SV=2   | NA|   | NA| NA|s287-298      |YICENQDSISSK    |      2|NA      |NA       | 165850000|  12032000|    7742100| 4.9756e+07| 3.0851e+07| 2.1342e+07| 9.0556e+09| 6967600000|               8|
|P02647     |APOA1      |HUMAN Apolipoprotein A-I   |Homo sapiens GN=APOA1 PE=1 SV=1 | NA|   | NA| NA|s165-173      |LSPLGEEMR       |      2|NA      |NA       |  79569000|  73246000| 1307700000| 3.3460e+10| 7.1809e+10| 1.4564e+11| 5.8230e+10| 1890100000|               8|
|P04114     |APOB       |HUMAN Apolipoprotein B-100 |Homo sapiens GN=APOB PE=1 SV=2  | NA|   | NA| NA|s642-654      |SVSLPSLDPASAK   |      2|NA      |NA       |        NA| 351550000|  375310000| 6.3224e+07| 3.9130e+07| 1.9093e+07| 7.8281e+06|    1312400|               7|
|P02649     |APOE       |HUMAN Apolipoprotein E     |Homo sapiens GN=APOE PE=1 SV=1  | NA|   | NA| NA|s210-224      |AATVGSLAGQPLQER |      2|NA      |NA       |        NA|  29692000|  149100000| 4.2077e+08| 3.6029e+08| 7.7250e+07| 1.2141e+07|         NA|               6|
|P08519     |APOA       |HUMAN Apolipoprotein(a)    |Homo sapiens GN=LPA PE=1 SV=1   | NA|   | NA| NA|s-----        |GTYSTTVTGR      |      2|NA      |NA       |        NA|        NA|   25347000|         NA|         NA| 3.9624e+06|         NA|         NA|               2|
|P05090     |APOD       |HUMAN Apolipoprotein D     |Homo sapiens GN=APOD PE=1 SV=1  | NA|   | NA| NA|s177-187      |MTVTDQVNCPK     |      2|NA      |NA       |   1309500|  10630000|   59035000| 1.5255e+09| 3.7214e+09| 2.0569e+09| 2.5349e+08|    9259400|               8|

The first 13 columns are variables to each protein and the last ones are XIC for each fraction of each protein.