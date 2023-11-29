##Geographically Weighted Regression=name
##GWmodel=group
##Layer=vector
##Dependent_Variable=Field Layer

##QgsProcessingParameterField|explana_variables|Expanatory Variables|None|Layer|-1|True|False
##bandwidth_kernel=selection "gaussian";"exponential";"bisquare";"tricube";"boxcar" "gaussian"
##bandwidth_approach=selection "CV";"AIC";"AICc";"BIC" "CV"
##bandwidth_adaptive=boolean FALSE
##Regression.points=optional vector

##gwr_kernel=selection gaussian;exponential;bisquare;tricube;boxcar gaussian
##gwr_adaptive=boolean FALSE
##QgsProcessingParameterNumber|p|Power|0|2|False|1
##longlat=boolean FALSE
##F123Test=boolean FALSE

##Output=output vector
##folder_path=output folder
# Libraries
library(sp)
library(GWmodel)

shorten_names <- function(names) {
  name_dict <- list()
  new_names <- character(length(names))
  
  for (i in seq_along(names)) {
    name <- names[i]
    short_name <- substr(name, 1, 10)
    
    if (!is.null(name_dict[[short_name]])) {
      count <- name_dict[[short_name]] + 1
    } else {
      count <- 1
    }
    
    if (count > 1) {
      short_name <- substr(short_name, 1, 8)
      short_name <- paste0(short_name, "_", count - 1)
    }
    
    name_dict[[short_name]] <- count
    new_names[i] <- short_name
  }
  
  dupes <- duplicated(new_names)
  if (any(dupes)) {
    for (i in which(dupes)) {
      base_name <- substr(new_names[i], 1, 8)
      count <- 2
      new_name <- paste0(base_name, "_", count)
      
      while (new_name %in% new_names) {
        count <- count + 1
        new_name <- paste0(base_name, "_", count)
      }
      
      new_names[i] <- new_name
    }
  }
  
  return(new_names)
}
explana_variables = shorten_names(explana_variables)

# create formula
formula_str = paste0(Dependent_Variable, " ~ ")
for (x in 1:length(explana_variables)) {
    
  if (x != length(explana_variables)) {
    formula_str = paste0(formula_str, explana_variables[x], " + ")
  } else {
    formula_str = paste0(formula_str, explana_variables[x])
  }
  
}

formula = formula(formula_str)

data = na.omit(Layer)

spatial_polygons = as(data, "Spatial")
approach_list = c("CV", "AIC", "AICc", "BIC")
kernel_list = c("gaussian", "exponential", "bisquare", "tricube", "boxcar")


# calculate bandwidth
bw = bw.gwr(formula, spatial_polygons, approach = approach_list[bandwidth_approach + 1], kernel = kernel_list[bandwidth_kernel + 1], adaptive = bandwidth_adaptive)

# fitting model 
gwr.fit <- gwr.basic(formula, data = spatial_polygons,
  bw = bw,  kernel = kernel_list[gwr_kernel + 1], adaptive = gwr_adaptive, p = p, longlat = longlat, F123.test = F123Test)

# Output files to folder 
# spatialpolygon dataframe save to shapefile
dir.create(paste0(folder_path, "/GWR_result"))
folder_path = paste0(folder_path, "/GWR_result/GWR_result")

gwr.write(gwr.fit, folder_path)

Output=st_as_sf(gwr.fit$SDF)
print(gwr.fit)
# cat result
