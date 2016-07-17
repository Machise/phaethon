library(stringr)

# Counts
n_inv  = 500
n_park = 5 

# Inverter Labels
invs = paste0("inv_", str_pad(as.character(1:n_inv), 3, pad="0") )

# Park Labels
park_size = c(150 , 150 , 100 , 50 , 50)
parks = character()
for(i in seq_along(park_size)){
  label = paste0("park_", as.character(i))
  parks = c(parks, rep(label, park_size[i]))
}

parks = model.matrix(~ 0 + parks)


# Inverter Type
## micro, string, central
## 0 = central
## 1 = micro
## 2 = string
type_size =
  c(125, 25, 0,
    75 , 50, 25,
    0  , 75, 25,
    40 ,  0, 10,
    0  , 30, 20)

inv_type = character()
for(i in seq_along(type_size)){
  label = paste0(as.character(i %% 3))
  inv_type = c(inv_type, rep(label, type_size[i]))
}

inv_type = model.matrix(~ 0 + inv_type)




# System Attributes
# - Inverter ID
# - inv_001
# - Inverter Type
  # - central(0) 
  # - microinverter(1)
  # - string(2)
# - Date of Installation
  # - Age in Days
# - Inverter Park
  # - park_001
# 
# Failure Modes
# - Infant Mortality
# - fail_X, fail_Y
# - Old Age
# - fail_Z
# 
# System Conditions
# - Inverter output
# - Detrended, made stationary
# - Daily count of error codes
# - Three Error Codes
# 
# Park Conditions
# - Temperature
# - Humidity
# 
# Maintenance History
# - Repair
# - Replacement

