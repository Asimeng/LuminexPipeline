# R LuminexPipeline package
<p>
This is the R package repository for the LuminexPipeline utility—a utility for processing and analysing multiplexed (Luminex) ELISA assay data in a robust and reproducible manner. The functionalities of the pipeline include importing raw Luminex files (.txt files), data cleaning and tidying, recording metadata (e.g., values beyond detectable limits), imputation of missing values, and standardising analyte names with an analyte reference list to ensure consistency. The Luminex Pipeline performs these tasks while recording all the processing and analytical steps for record-keeping and reproducibility purposes. 
A junk of the functions in this package were written by Ncité Lima DaCamara as part of her PhD dissertation. My MSc dissertation contributed to this utility with additional functions especially for statistical computations, unit testing, documentation and compiling all the code into an R package for easy code distribution and installation. 

This utility can be installed by either of the following two methods.  

First option: In R using the `devtools::install_github()` function. The argument to this function is the GitHub package repository, “Asimeng/LuminexPipeline”.  <br>

`devtools::install_github(“Asimeng/LuminexPipeline”)`  <br>

NB: This method requires the user to have the devtools R package installed. 

The second option: Via the Linux command line using the already-built source file (compressed source code). This file is available for download at https://github.com/Asimeng/LuminexPipeline/blob/main/inst/package_source_code/LuminexPipeline.tar.gz. Once downloaded, package installation can be initiated by executing the following command on the command line.  <br>

`R CMD INSTALL LuminexPipeline.tar.gz` <br>

Note: This code snippet assumes that the downloaded binary package file is in the current working directory. One will need to provide the full path to the file if working from a different directory.
An alternative installation method in the R environment using the downloaded binary file could also be done using the code below:  <br>

`install.packages(“path_to_source_file”,repos=NULL,type=“source”)`  <br>

We have intentions to submit this to the comprehensive R archive network (CRAN)
</p>
