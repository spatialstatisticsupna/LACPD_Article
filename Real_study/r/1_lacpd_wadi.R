###############################################################################
# R CODE: Locally adaptive change point detection
###############################################################################
# Moradi, M., Montesino-SanMartin, M., Ugarte, M.D., Militino, A.F.
# Public University of Navarre
# License: Availability of material under 
# [CC-BY-SA](https://creativecommons.org/licenses/by-sa/2.0/).

###############################################################################
# PACKAGE
###############################################################################

# install
# library(devtools)
# install.packages()

# load
library(Scp)

###############################################################################
# DATA PATHS
###############################################################################

# inputs
root.git <- "https://raw.githubusercontent.com/mmontesinosanmartin/changepoint_article"
root.dir <- "master/Real_study/data"
files <- c("field1_7.RData",
           "field2_21.RData",
           "field3_29.RData")
n.files <- length(files)

# outputs
out.dir <- "./"

###############################################################################
# ANALYSIS
###############################################################################

# Paramters
# windows
this.k <- "02:10"
# iterations
this.m <- 100
# adjusting method
this.adj <- "BY"

# Run: for each file
for(i in 1:n.files){
  
  # load the dataset
  f.i <- file.path(root.git, root.dir, files[i])
  load(url(f.i))

  # result
  resl <- list()
  
  # for each pixel
  for (j in 1:nrow(this.data)) {
    
    # the time-series for this pixel
    x <- this.data[j,]
    
    # print message (every 10% completed)
    p <- round(j/nrow(this.data)*100)
    if (p %% 10 == 0 & p > 0) print(paste0(p, "%"))
    
    # run the analysis
    resl[[j]] <- Re_mk(x = x,
                       k = eval(parse(text=this.k)),
                       m = this.m,
                       adjust = TRUE,
                       method = this.adj,
                       history = TRUE)
  }
  
  # save output
  suffix   <- paste(gsub(":","",this.k), this.m, this.adj, sep = "_")
  out.file <- paste0(gsub(".RData","",basename(f.i)),"_",suffix, ".RData")
  out.path <- file.path(out.dir,out.file)
  save(sample.roi, # study region
       sample.val, # NDVI RasterBrick
       this.data,  # NDVI data matrix
       resl,       # All results
       file = out.path)
}
