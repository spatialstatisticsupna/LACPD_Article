Naming convention: 
field + N + _ + Cp + .RData
N:  field number as in the manuscript - 1,2, and 3
Cp: index of the change point (1=1986, 2=1987, etc.)

Files contain:
sample.roi: an sf object with the border of the study area
sample.val: a RasterBrick of NDVI observations with 34 layers (one image per year)
this.data:  the RasterBrick turned into a matrix