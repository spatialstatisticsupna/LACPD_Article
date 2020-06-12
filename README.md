# Supplementary material: A locally trend-based change-point detection technique


This repository provides the data-sets and R codes used in the analysis: 
_"A locally trend-based change-point detection technique"_
(Moradi et al., 2020).

## Table of contents

 - [Package installation](#Package-installation)
 - [Simulations](#Simulations)
 - [Real case-study](#Real-case-study)
 - [References](#References)

# Package installation


# Simulations 


# Real case-study

## Dataset

The real case study aims to detect the transformation of bare land into cultivated areas in the Wadi As-Sirham valley, Saudi Arabia (Figure 1). The detection is based on a time-series of yearly NDVI scenes obtained from Landsat 4-8 Collection 1 Level 2 surface reflectance imagery and applying maximum value compositing technique on a yearly basis. 

<p align="center">
  <img align="center" width="400" src="./Real_study/graphs/wadi_as_sirham.png" alt="Itoiz reservoir">
  <p align="center"> Figure 1: Wadi As-Sirham valley in Saudi Arabia. The graph shows the maximum value composite (MVC) of the region in 2019 from Landsat 7-8 scenes. The fields used in the analysis are framed within red rectangles. The location of the region  </p>
</p>

The data is available
[here](https://github.com/mmontesinosanmartin/changepoint_article/tree/master/Real_study/data)
The folder has the following files:

 - `field1_7.RData`: the dataset for the agricultural beginning it activity in 1992.
 - `field2_21.RData`: the dataset for the agricultural beginning it activity in 2006.
 - `field3_29.RData`: the dataset for the agricultural beginning it activity in 2014.

Each `.RData` contains:

 - `sample.roi`: a `sf` delimiting the boundaries of the study area for each field.
 - `sample.val`: a `RasterBrick` of the 34-year time series of the NDVI.
 - `this.data`: `sample.val` turned into a `matrix`.

## R Code

The analysis and results of the real case study can be reproduced using the
`R` code avialable 
[here](https://github.com/mmontesinosanmartin/changepoint_article/tree/master/Real_study/r).
The folder contains:

  - `1_lacpd_wadi.R`: applies the LACPD procedure to the time-series of NDVI images.
  - `2_graphs_wadi.R`: collects, organizes, and represents the results of the analysis.


# References