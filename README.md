# degu-pop-crash
Code and data for a population demographics paper on a degu population from Central Chile. 
We aimed to investigate 1) the demography and population dynamics of a degu population using a dataset from 2009 to 2020, 2) what environmental factors affected population parameters and 3) the factors that potentially caused or contributed to the local extinction of this population in 2020. Although the IUCN red list status classifies degus as  ‘Least Concern’ (Roach 2016), it remains unclear why some populations can survive through adverse events while others experience local extinction. Examining what environmental factors affected the population dynamics of this population helps to identify which populations are most vulnerable to forecasted climate change and aids in developing strategies to mitigate future local extinctions from happening.
 
Title: Extremely low primary production after a decade-long drought contributed to the local extinction of a group-living rodent
Authors: 
1Annemarie van der Marel, 2Madan K Oli, 3Azad Hossain, 4Praveena Krishnan, 4,5Tim Wilson, 6Loreto A. Correa, 1Luis A Ebensperger, 3Loren D Hayes
Affiliations:
1Departamento de Ecología, Facultad de Ciencias Biológicas, Pontificia Universidad Católica de Chile, Santiago, Chile
2Department of Wildlife Ecology and Conservation, University of Florida, Gainesville, FL 32611, USA
3Department of Biology, Geology, and Environmental Sciences, University of Tennessee at Chattanooga, Chattanooga, TN 37403, USA	
4NOAA Air Resources Laboratory, Atmospheric Turbulence and Diffusion Division, Oak Ridge, TN 37830, USA
5Oak Ridge Associated Universities, Oak Ridge, TN 37830, USA 
6Escuela de Medicina Veterinaria, Facultad de Medicina y Ciencias de la Salud, Universidad Mayor, Camino La Pirámide 5750, Huechuraba, Santiago, Chile

## Code
The code used is available in the main branch. First, we used the 'summarize_capture_data30daywindow.Rdata' to select the trapping windows to include in our mark-recapture modeling framework. Then, we used the 'environmental covariates_v11.Rdata' to select the environmental covariates, check for multicollinearity, and create figures. We then used the 'covariates model selection.R' script to write down all the potential models for model selection that was then run in Rmark. Finally, we used the 'results and plots degu pop dynamics_11032025.Rdata' to look at the model results and visualize the results for the models without environmental covariates, models with environmental covariates and year, and models with environmental covariates excluding year. 

## Data
Data folder contains the raw data.

## Results
Results folder contains the generated data from the raw data.

## Results_models with/without covariates
These folders contain the CSV files with the full model results. 

## R session info 
For the code run on everything except the Rmark package. 
> sessionInfo()
R version 4.3.2 (2023-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 11 x64 (build 22631)

Matrix products: default


locale:
[1] LC_COLLATE=English_Canada.utf8 
[2] LC_CTYPE=English_Canada.utf8   
[3] LC_MONETARY=English_Canada.utf8
[4] LC_NUMERIC=C                   
[5] LC_TIME=English_Canada.utf8    

time zone: America/Santiago
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets 
[6] methods   base     

other attached packages:
 [1] gghighlight_0.4.1 ggpubr_0.6.0      ggrepel_0.9.5    
 [4] paletteer_1.6.0   lubridate_1.9.3   forcats_1.0.0    
 [7] stringr_1.5.1     dplyr_1.1.4       purrr_1.0.2      
[10] readr_2.1.4       tidyr_1.3.0       tibble_3.2.1     
[13] ggplot2_3.5.1     tidyverse_2.0.0  

loaded via a namespace (and not attached):
 [1] gtable_0.3.5       xfun_0.41         
 [3] rstatix_0.7.2      tzdb_0.4.0        
 [5] vctrs_0.6.5        tools_4.3.2       
 [7] generics_0.1.3     parallel_4.3.2    
 [9] fansi_1.0.6        pkgconfig_2.0.3   
[11] lifecycle_1.0.4    compiler_4.3.2    
[13] farver_2.1.2       textshaping_0.3.7 
[15] munsell_0.5.1      carData_3.0-5     
[17] htmltools_0.5.7    yaml_2.3.8        
[19] pillar_1.9.0       car_3.1-2         
[21] crayon_1.5.2       abind_1.4-5       
[23] tidyselect_1.2.0   digest_0.6.33     
[25] stringi_1.8.3      rematch2_2.1.2    
[27] labeling_0.4.3     cowplot_1.1.3.9000
[29] fastmap_1.1.1      grid_4.3.2        
[31] colorspace_2.1-0   cli_3.6.2         
[33] magrittr_2.0.3     utf8_1.2.4        
[35] broom_1.0.5        withr_3.0.1       
[37] scales_1.3.0       backports_1.4.1   
[39] bit64_4.0.5        timechange_0.2.0  
[41] rmarkdown_2.25     bit_4.0.5         
[43] gridExtra_2.3      ggsignif_0.6.4    
[45] ragg_1.2.7         hms_1.1.3         
[47] evaluate_0.23      knitr_1.45        
[49] rlang_1.1.2        Rcpp_1.0.12       
[51] glue_1.6.2         rstudioapi_0.15.0 
[53] vroom_1.6.5        R6_2.5.1          
[55] systemfonts_1.0.5 

## License 
Creative Commons Attribution 4.0	CC-BY-4.0
