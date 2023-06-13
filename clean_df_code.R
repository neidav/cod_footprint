# Load Libraries
library(raster)
library(rgeos)
library(rgdal)
library(stringr)
library(dplyr)

# Micah's functions 
source("/Users/neidavillanueva/Desktop/cod_footprint/micahs_functions.R")

# Specify the file paths
shp_path <- "/Users/neidavillanueva/Desktop/cod_footprint"  # Project shape file location
cod_poly <- readOGR(shp_path, "cod_poly_shapefile")  # Shape file we want to search (output of kml converter)
cod_poly <- spTransform(cod_poly, masp)  # Project to Mass state plane coordinates (meters)
colnames(cod_poly@data) <- c("poly_number",'Description','int_number')
cod_poly_data <- data.frame(cod_poly@data$Description, cod_poly@data$int_number)
colnames(cod_poly_data) <- c('Description','interview')

# ----------CONSTANTS-----------
# make list of all values we are trying to pull from description (notes) column
years <- c( "2004", "2010", "2013")
months <- c("jan","january", "feb", "february", "mar", "march","april","may", "apr", "june", "jun",
            "july", "jul", "aug", "august", "september", "sept","sep", "oct", "october",
            "nov", "november", "dec", "december")
abundance <- c("high", "medium","med","mid","low", "bycatch")
gf_cod <- c("groundfish", "ground fish", "ground", "cod", "avoid", "avoidance")
tod <- c("day", "night", "both")
term_list <- list(years, months, abundance, gf_cod, tod)

# make empty df to make function read into
# this function makes an data frame based on the description part of the 
# cod_poly_data frame
make_clean_df<- function(df_outline){
  final_df <- data.frame(matrix(ncol=1, nrow = length(df_outline)))
  for (terms in term_list) {
    # make df by extracting info from description and rename column with our value and give unique ids
    temp_df <- str_extract_all(df_outline, paste(terms, collapse = '|')) %>%
      sapply(., paste, collapse = ", ")
    final_df <- cbind(final_df, temp_df)
  }
  return(final_df)
}

# use function and make the value the name of the original 
# data frame row column that contains notes 
final_df <- make_clean_df(cod_poly_data$Description)

# clean df made above (renaming the columns)
final_df_2 <- final_df%>%
  data.frame()%>%
  select(-c(1)) %>%
  rename("year"=temp_df,
         "month"=temp_df.1,
         "abundance"=temp_df.2,
         "gf_cod"=temp_df.3,
         "tod"=temp_df.4)%>%
  cbind(.,cod_poly_data)
#------Actually Cleaning up the Data frame!!------------------------
# this code uses regular expression to rewrite abbreviated terms as the whole name 
# and will also get rid of repeated terms 
# if another variable is added create a new function for it, copying the same format

# creating a list of the abbreviated months that will be changed
months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

# Function to replace abbreviated months with full forms
replace_months <- function(string) {
  
  for (month in months) {
    # assigning full month value/name based on the lower month being searched 
    full_month <- tolower(month.name[match(month, tolower(month.abb))])
    # replace the abbreviated month with the full month and make
    # (regex knows the location of where to replace this)
    string <- gsub(paste0("\\b", month, "\\b"), full_month, string, ignore.case = TRUE)
  }
  # this does the same for a variation of September spelled 'sept' 
  string <- gsub("\\bsept\\b", "september", string, ignore.case = TRUE)
  # makes it lower case and returns it
  string <- unique(strsplit(string, ", ")[[1]])
  return(tolower(paste(string, collapse = ", ")))

}

# Function to remove repeated phrases and rewrite variations
# these all essentially do the same as months, just with whatever term we input
cleanup_gf_cod <- function(string) {
  string <- gsub("\\b(ground fish)+\\b", "groundfish", string, ignore.case = TRUE)
  string <- gsub("\\b(cod)+\\b", "cod", string, ignore.case = TRUE)
  # this is where it gets rid of the repeats
  string <- unique(strsplit(string, ", ")[[1]])
  return(paste(string, collapse = ", "))
}

# Function to remove repeated phrases and rewrite variations 
cleanup_year <- function(string) {
  years <- unique(unlist(strsplit(string, ", ")))
  return(paste(years, collapse = ", "))
}

# Function to remove repeated phrases and rewrite variations 
cleaned_abundance <- function(string) {
  string <- gsub("\\bmed\\b|\\bmid\\b", "medium", string, ignore.case = TRUE)
  string <- gsub("\\bhigh\\b|\\bhgh\\b|\\bhi\\b", "high", string, ignore.case = TRUE)
  string <- gsub("\\blow\\b|\\blo\\b", "low", string, ignore.case = TRUE)
  string <- unique(strsplit(string, ", ")[[1]])
  return(paste(string, collapse = ", "))
}

cleaned_tod <- function(string){
    string <- gsub("\\bday\\b", "day", string, ignore.case = TRUE)
    string <- gsub("\\bnight\\b", "night", string, ignore.case = TRUE)
    string <- unique(strsplit(string, ", ")[[1]])
  return(paste(string, collapse = ", "))
  
}

# Apply the function to each row of the data frame (these are the cols we renamed above) 
# this is the main thing that WOULD be changed if needed

final_df_2$new_months <- apply(final_df_2, 1, function(row) replace_months(row["month"]))
final_df_2$cleaned_gf_cod <- apply(final_df_2, 1, function(row) cleanup_gf_cod(row["gf_cod"]))
final_df_2$cleaned_year <- apply(final_df_2, 1, function(row) cleanup_year(row["year"]))
final_df_2$cleaned_abundance <- apply(final_df_2, 1, function(row) cleaned_abundance(row["abundance"]))
final_df_2$cleaned_tod <- apply(final_df_2, 1, function(row) cleaned_tod(row["tod"]))


#----------------creating the data analysis DF-----------------------------

value_function <- function(df_row_search, search_item, NewName){
  col_id <- c()
  for (condition in df_row_search){
    if ((grepl(search_item,tolower(condition))) == TRUE){
      col_id <-append(col_id,1)
    }
    else{
      col_id <-append(col_id,0)
    }
  }
  col_df <- data.frame(col_id)%>%
    rename(!!sym(NewName) := col_id)
  return(col_df)
}
# ------------This is where we make the new data frame!--------------
# make a list of the conditions you want searched (these will be column names in
# the new data frame)
years <- c( "2004", "2010", "2013")
months <- c("january","february", "march","april", "may","june",
            "july", "august", "september", "october","november","december")
abundance <- c("high", "medium","low", "bycatch")
gf_cod <- c("groundfish", "cod", "avoid")
tod <- c("day", "night")

# this will go row by row of whatever column you want and will see if the term 
# exist within the search column, if it does it will assign 1 for that position 
# otherwise it will assign 0.
make_df_function<- function(search_terms,col){
  df_lst <- list()
  for (item in search_terms){
    gen_info <- value_function(col,item,item)
    df_lst <- append(df_lst,gen_info)
  }
  return (df_lst)
}

# using the function to create lists of 0s and 1s for each term
# if another component where to be added, it would need to be added here as well
years_lst <- make_df_function(years,final_df_2$cleaned_year)
months_lst <- make_df_function(months,final_df_2$new_months)
abundance_lst <- make_df_function(abundance,final_df_2$cleaned_abundance)
fish_lst <- make_df_function(gf_cod,final_df_2$cleaned_gf_cod)
tod_lst <- make_df_function(tod,final_df_2$cleaned_tod)

# takes the lists created using the make_df_function and makes them into a df
# if another component where to be added, it would need to be added here as well
final_csv <- bind_cols(years_lst,months_lst,abundance_lst,fish_lst,tod_lst)

# Wendi Code; combined the df we just made with the interview number and then something else
final_csv_w_int_num <- cbind(final_csv,cod_poly_data)%>%
  select(c("interview",  "2004"  ,            "2010"    ,          "2013" ,             "january" ,         
          "february"       ,   "march"     ,        "april"       , "may",     "june"     ,        
          "july"            ,  "august"     ,       "september"    ,     "october"   ,       
           "november"        ,  "december"   ,       "high"         ,     "medium"    ,       
          "low"               ,"bycatch"      ,     "groundfish"     ,   "cod"         ,     
           "avoid"            , "day"          ,     "night"    ))%>%
  mutate(cod_poly_id_number= 0:(-1*(1:nrow(.))))