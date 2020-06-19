Naming convention: 
field + N + _ + Cp + k + _ + m.RData
N:  field number as in the manuscript - 1,2, and 3
Cp: index of the change point (1=1986, 2=1987, etc.)
k: adaptive window configuration (0210 means k=2:10) in the LACPD procedure.
m: number of subsamplings (100 means m=100) in the LACPD procedure.

Files contain:
sample.roi: an sf object with the border of the study area
sample.val: a RasterBrick of NDVI observations with 34 layers (one image per year)
resl: the results from the LACPD procedure with Mann-Kendal, k, and m over sample.val.
this.data:  the RasterBrick turned into a matrix