##Geographical Temporal Weighted Regression=name
##GWmodel=group
##Layer=vector
##Dependent_Variable=Field Layer
##QgsProcessingParameterField|explana_variables|Expanatory Variables|None|Layer|-1|True|False
##Regression.points=optional vector
##QgsProcessingParameterField|reg.tv|Regression Location Timetag|None|Regression.points|-1|False|True
##QgsProcessingParameterField|obs.tv|Observation Timetag|None|Layer|-1
##bandwidth_kernel=selection gaussian;exponential;bisquare;tricube;boxcar gaussian
##bandwidth_adaptive=boolean FALSE
##bandwidth_approach=selection CV;AIC;AICc;BIC CV
##gtwr_kernel=selection gaussian;exponential;bisquare;tricube;boxcar gaussian
##gtwr_adaptive=boolean FALSE
##QgsProcessingParameterNumber|p|Power|0|2|False|1
##longlat=boolean FALSE
##QgsProcessingParameterNumber|theta|Theta|1|0|False|0|1
##QgsProcessingParameterNumber|lamda|Lamda|1|0.05|False|0|1
##t.units=String auto
##QgsProcessingParameterNumber|ksi|Ksi|1|0|False|0|3.14
#QgsProcessingParameterMatrix|st.dMat|st_Mat 
##QgsProcessingParameterNumber|unit_interval|Group Interval (unit)|0|None|True|0
##unit_type=selection Default;Year;Month;Day Default
##QgsProcessingParameterString|date_format|Time variable's format (eg. 2002-08-16 => yyyy-mm-dd or 2002-08 => yyyy-mm)|None|False|True
##Output=output vector
##folder_path=output folder
# libraries
library(GWmodel)
library(sp)
library(ggplot2)
library(sf)
library(RColorBrewer)
library(dplyr)

datetype_check = function(date) {
  if (grepl("yyyy", date) & grepl("mm", date) & grepl("dd", date)) {
    return("day")
  } else if (grepl("yyyy", date) & grepl("mm", date)) {
    return("month")
  } else if (grepl("yyyy", date)){
    return("year")
  } else {
    return("default")
  }
}

tran_date_format = function(date_format, type) {
  if (type == "day") {
    format = gsub("yyyy", "%Y", date_format)
    format = gsub("mm", "%m", format)
    format = gsub("dd", "%d", format)
    return(format)
  } else if (type == "month") {
    format = gsub("yyyy", "%Y", date_format)
    format = gsub("mm", "%m", format)
    format = paste0(format, "-%d")
    return(format)
  } else if (type == "year") {
    format = gsub("yyyy", "%Y", date_format)
    format = paste0(format, "%m-%d")
    return(format)
  }
}

transDate = function(date, type) {
  if (type == "day") {
    return(date)
  } else if (type == "month") {
    return(paste0(date, "-01"))
  } else if (type == "year") {
    return(paste0(date, "-01-01"))
  }
}

# 日期修正 
# if the time variable is date
if (date_format != "") {
  date_format_type = datetype_check(date_format)

  date_format = tran_date_format(date_format, date_format_type)
}

# rename selection
# define shorten_names function
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

create_timegroup = function(data, date_variable_name = "date", date_format = "%Y-%m-%d", interval_units = 30, dft = date_format_type, unit_type = unit_type, trans_format = date_format) {
  
  data[[date_variable_name]] = as.Date(transDate(as.character(data[[date_variable_name]]), dft), format = trans_format)
  # data[[date_variable_name]] = as.Date(data[[date_variable_name]], format = date_format)
  
  sorted_data <- data[order(data[[date_variable_name]]), ]
  
  date = sorted_data[[date_variable_name]]

  if (unit_type == "year") {
    group = cut(date, breaks = seq(min(date), max(date) + interval_units * 366, by = paste0(interval_units, " years")))
    names = paste0(interval_units, "YearsGroup")
    sorted_data[[names]] = group
    return(sorted_data) 
  } else if (unit_type == "Month") {
    group = cut(date, breaks = seq(min(date), max(date) + interval_units * 31, by = paste0(interval_units, " months")))
    names = paste0(interval_units, "MonthsGroup")
    sorted_data[[names]] = group
    return(sorted_data) 
  } else if (unit_type == "Day") {
    group = cut(date, breaks = seq(min(date), max(date) + interval_units, by = paste0(interval_units, " days")))
    names = paste0(interval_units, "DaysGroup")
    sorted_data[[names]] = group
    return(sorted_data) 
  }
  
}

group_sequence <- function(seq, n) {

  breaks <- seq(min(seq) - 0.1, max(seq) + n, by=n)

  group <- cut(seq, breaks, labels=FALSE, right=TRUE)
  first_in_group <- breaks[group]
  return(ceiling(first_in_group))
}


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
unit_type_list = c("Default", "Year", "Month", "Day")
unit_type_selection = unit_type_list[unit_type + 1]

if (!is.null(unit_interval) && unit_interval != 0 && unit_type_selection != "Default") {

  data = create_timegroup(data, obs.tv, date_format = date_format, interval_units = unit_interval, unit_type = unit_type_selection)
  names = paste0(unit_interval, unit_type_selection, "sGroup")
  time.tag = as.Date(data[[names]], format = "%Y-%m-%d")
  t.units = "auto"
} else if (!is.null(unit_interval) && unit_interval != 0 && unit_type_selection == "Default"){
  data = data[order(data[[obs.tv]]), ]
  time.tag = group_sequence(data[[obs.tv]], unit_interval)
} else {
  time.tag = data[[obs.tv]]
}




for (x in 1:length(explana_variables)) {
  data[[explana_variables[x]]] = as.numeric(data[[explana_variables[x]]])
}
spatial_polygons = as(data, "Spatial")
approach_list = c("CV", "AIC", "AICc", "BIC")
kernel_list = c("gaussian", "exponential", "bisquare", "tricube", "boxcar")

# Calculate  bandwidth
bw = bw.gtwr(formula, spatial_polygons, approach = approach_list[bandwidth_approach + 1], kernel = kernel_list[bandwidth_kernel + 1],
  obs.tv = time.tag, adaptive = bandwidth_adaptive)



# Fit model
gtwr.fit  = gtwr(formula, spatial_polygons, st.bw = bw,  #regression.points = Regression.points, reg.tv = Regression.points[[reg.tv]],
  obs.tv = time.tag, kernel  = kernel_list[gtwr_kernel + 1], adaptive = gtwr_adaptive,
  p = p, theta  = theta, longlat = longlat, lamda = lamda, t.units = t.units, ksi = ksi)


# Output files to folder 
# spatialpolygon dataframe save to shapefile
dir.create(paste0(folder_path, "/GTWR_result"))
folder_path = paste0(folder_path, "/GTWR_result")
output_text = capture.output(gtwr.fit)
writeLines(output_text, paste0(folder_path, "/GTWR_result.txt"))
# writeOGR(gtwr.fit$SDF, dsn = folder_path, layer = "SDF_result", driver = "ESRI Shapefile", check_exists = TRUE, overwrite_layer = TRUE)
st_write(st_as_sf(gtwr.fit$SDF), paste0(folder_path, "/SDF_result.shp"))
# gwr.write(gtwr.fit, folder_path)


Output=st_as_sf(gtwr.fit$SDF)

n = nrow(gtwr.fit$SDF@data)
t_values = qt(0.95, n - 1)

for (i in 1:length(explana_variables)) {
  var = explana_variables[i]
  
  gtwr.fit$SDF@data[paste0(var, "_Rate_pv")] = ifelse(abs(gtwr.fit$SDF@data[paste0(var, "_TV")]) > t_values, "Significant", "Nonsignificant")
}

df = st_as_sf(gtwr.fit$SDF)

if (grepl("Undefined Cartesian SRS with unknown unit", crs(df))) {
  st_crs(df) <- ""
}


RdYlBu_palette <- colorRampPalette(brewer.pal(100, "RdYlBu"))


for (i in 1:length(explana_variables)) {
  var = explana_variables[i]

  fill_var = var
  alpha_var = paste0(var, "_Rate_pv") 

  scale_alpha = if(length(unique(df[[alpha_var]])) == 2) {
    scale_alpha_manual(values = c(0, 1),
      guide = guide_legend(override.aes = list(fill = c("white", "black"))))
  } else if (unique(df[[alpha_var]]) == "Significant") {
    scale_alpha_manual(values = c(1),
      guide = guide_legend(override.aes = list(fill = c("black"))))
  } else {
    scale_alpha_manual(values = c(0),
      guide = guide_legend(override.aes = list(fill = c("white"))))
  }
  plot = if(class(spatial_polygons) == "SpatialPointsDataFrame") {
    ggplot(df) +
      geom_sf(aes_string(colour = fill_var, alpha = alpha_var))
  } else {
    ggplot(df) +
      geom_sf(aes_string(fill = fill_var, alpha = alpha_var))
  }
  plot = plot +
    facet_wrap(~ time_stamp) +
    scale_colour_gradientn(colors = RdYlBu_palette(n), na.value = "white") +
    scale_fill_gradientn(colors = RdYlBu_palette(n), na.value = "white") +
    scale_alpha +
    theme_minimal()

  file_name = paste0(folder_path, "/", var, "_Plot.jpeg")
  ggsave(file_name, plot, width = 9.32, height = 6.56, dpi = 300, units = "in")
  

}

print(gtwr.fit)