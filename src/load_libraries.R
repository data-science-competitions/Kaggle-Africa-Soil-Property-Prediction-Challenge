################################################################################
#                                Load Libraries                                #
################################################################################
# 1. Add any project specific configuration here.
Sys.setlocale("LC_TIME", "English") # uses english opertaing system naming convention

# 2. Install missing packages for the project and load the required CRAN packages.
packages.loader <- function(packages.list){
        suppressPackageStartupMessages(
                for (p in packages.list){
                        if(!require(p, character.only=TRUE)){
                                install.packages(p,dep=TRUE) # install form CRAN
                                require(p, character.only=TRUE)
                        } # end if require
                } # end for packages list
        ) # end suppressPackageStartupMessages
} # end functions packages.loader
packages.list = c("GA","memoise",                                               # Genetic algorithms
                  "e1071",                                                      # SVM
                  "caret",                                                      # Classification and regression tools
                  "hashmap",                                                    # Map keys to values
                  "snow","doSNOW","doRNG","doParallel","foreach",               # Parallel computing
                  "wavelets","wavethresh",                                      # Discrete Wavelet Transforms (DWT) tools
                  "rpivotTable",                                                # Report tools
                  "manipulate")                                                 # Plotting Tools
packages.loader(packages.list)


#############
# Constants #
#############
GLOBALS = list(FEATURE_SELECTION_FOLDER='data',
               FEATURE_SELECTION_VIA_GA='features selected by GA')




# Optional:
#install.packages("tidyverse")
#library(tidyverse)
#tidyverse_update()

# 3. Clean up
rm(packages.list, packages.loader)