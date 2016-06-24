# Shiny Application for CTD data Validation
This shiny application is designed for validating oceanographic data collected from a CTD sensor array. The data from each sensor is modeled using a variation-weighted smoothing function. Observations that are more the 2 standard deviations are flagged for removal. Multiple tab views provides an overview, and details for each sensor with interactive graphs. This allows for altering what points are included/excluded by the model. Data is then downloadable as a zip file containing the data in 1-meter depth bins, excluded data values, and data used for binning. 
  
Shiny Modules are used to dynamically create tabs for each sensor. The oce package is used to parse the data file, seabird format in included examples. See the oce package details for other sensors that are supported. 

This is an early release of a preliminary tool intended to replace an outdated software package that is no longer supported. There are still refinement issues to work out. This packaged is shared here to highlight some of the work being done in R by staff with the City of San Diego's Ocean Monitoring Program.

View a copy of the poster we created for UseR!2016 titled "Making Shiny Seaworthy: A weighted smoothing model for validating oceanographic data at sea."

![UseR!2016 Poster](https://github.com/kbyron/Shiny_CTD_Validation/blob/master/MakingShinySeaworthy.pdf)
