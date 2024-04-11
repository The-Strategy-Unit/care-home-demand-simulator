### source file for population projection shiny app
## adding in ASCFR-SALT weekly cost file too (currently using the 2022-23 return)


# LOAD packages -----
library(tidyr)
library(readxl)
library(openxlsx)
library(janitor)
library(shiny)
library(shinythemes)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)

##  --------
# to be developed - inputs from app

yr_input <- 2024
yr_end <- yr_input + 9

# LOAD data ----

#### ASFCR-SALT costings --------

cost_file_loc <- "https://files.digital.nhs.uk/BF/EA1758/ASCFR%20and%20SALT%20Data%20Tables%202022-23%20v2.1.xlsx"

## extract column headers then relevant columns
# range = "A6:E6"
cost_geog_cnames <- openxlsx::read.xlsx(cost_file_loc, sheet = "T54", startRow = 6, cols = 1:5) |> 
  clean_names() |> 
  names()

# range = "A8:E169"
cost_geog <- openxlsx::read.xlsx(cost_file_loc, sheet = "T54", startRow = 8, 
                                 cols = 1:5, colNames = FALSE) |> 
  set_names(cost_geog_cnames) |> 
  select(!la_code) |> 
  filter(str_detect(region_code, "^E"))

# range = "P7:T169"
cost_data_raw <- openxlsx::read.xlsx(cost_file_loc, sheet = "T54", startRow = 7, 
                                     cols = 16:20 ) |> 
  clean_names() |> 
  select(!learning_disability_support) |> 
  mutate(na_flag = case_when(
    if_any(c(1:4), ~.x == "[x]") ~ TRUE,
    TRUE ~ FALSE),
    across(where(is.character), ~ na_if(., "[x]")),
    across(where(is.character), ~ as.numeric(.))) |> 
  mutate(ave_unit_cost_22_23 = rowMeans(pick(where(is.numeric)), na.rm = TRUE))

# combine
cost_data <- cost_geog |> 
  bind_cols(cost_data_raw) 

cost_data_region <- cost_data |> 
  filter(is.na(la_name)) |> 
  select(c(region_code, region_name, ave_unit_cost_region = ave_unit_cost_22_23))

cost_data <- cost_data |> 
  left_join(cost_data_region) |> 
  mutate(ave_unit_cost_22_23 = case_when(is.na(ave_unit_cost_22_23) ~ ave_unit_cost_region,
                                   TRUE ~ ave_unit_cost_22_23),
         geography_code = case_when(geography_code == " " ~ region_code,
                                    TRUE ~ geography_code),
         la_name = case_when(is.na(la_name) ~ region_name,
                             TRUE ~ la_name)) |> 
  rename(area_name = la_name)|> 
  select(-(c(region_code:region_name, physical_support:mental_health_support, ave_unit_cost_region))) 

unit_cost_demo <- cost_data |> 
  filter(area_name == "England") |> 
  pull(ave_unit_cost_22_23) |> 
  first() |> 
  round(2)

# cost_missing <- cost_data |> 
#   filter(na_flag == "TRUE") |> 
#   distinct(area_name)

### pop projections ----
# pop proj - or pull all years then filter at shiny?
query <- (paste0("https://www.nomisweb.co.uk/api/v01/dataset/NM_2006_1.data.csv?geography=1853882369...1853882372,1853882374...1853882379,2092957699,1816133633...1816133783,1820327937...1820328253,1937768449...1937768456,2013265921...2013265929&projected_year=",
             yr_input, "...", yr_end, "&gender=0&c_age=209&measures=20100"))


# CREATE objects used in output ----
pop_data <- read.csv(query) |> 
  clean_names() |> 
  dplyr::select(area_name = geography_name, geography_code, 
                geography_type, projected_year, pop65plus = obs_value)

codes <- pop_data  |>  
  mutate(code_start = substr(geography_code, 1, 3))   |>  
  arrange(code_start) |> 
  mutate(geog = case_when(code_start %in% c("E06", "E07", "E08", "E09") ~ "Local Authority",
                          code_start == "E10" ~ "County",
                          code_start == "E11" ~ "Metropolitan County",
                          code_start == "E12" ~ "Region",
                          code_start == "E47" ~ "Combined Authorities",
                          code_start == "E92" ~ "England",
                          TRUE ~ geography_type))  |>  
  distinct(code_start, geog) |> 
  filter(!(code_start %in% c("E11", "E13")))


geog_level <- codes |> 
  distinct(geog) |> 
  filter(geog != "Combined Authorities") |> 
  add_row(geog ="demo")

stella_time <- tibble(year = 2024:2033,
                      TIME = seq(1, 109, by = 12))

pop_data_temp <- pop_data |> 
  mutate(code_start = substr(geography_code, 1, 3),
         # different names used for same area code
         area_name = case_match(area_name, "East" ~ "East of England", .default = area_name)) |> 
  left_join(codes) |> 
  # dplyr::select(area_name, geog, projected_year, pop65plus) |> # 4960 rows
  # distinct(area_name, geog, projected_year, .keep_all = TRUE) |> # 3710 rows
  arrange(area_name) |> 
  left_join(stella_time,join_by(projected_year == year))

pop_cost_data_temp <- cost_data |> 
  left_join(pop_data_temp) |> 
  dplyr::select(area_name, geog, projected_year, pop65plus, ave_unit_cost_22_23) |> # 4960 rows
  distinct(area_name, geog, projected_year, .keep_all = TRUE) |> # 3710 rows
  arrange(area_name) |> 
  left_join(stella_time,join_by(projected_year == year))

pop_cost_demo <- pop_cost_data_temp |> 
  drop_na() |> 
  distinct(projected_year, TIME) |> 
  mutate(area_name = "demo",
         geog = "demo",
         ave_unit_cost_22_23 = unit_cost_demo,
         pop65plus = c(22370,
                       22635,
                       22899,
                       23184,
                       23496,
                       23840,
                       24202,
                       24586,
                       24964,
                       25350)
         )

pop_cost_data <- bind_rows(pop_cost_data_temp, pop_cost_demo)

stella_time <- tibble(year = 2024:2033,
                      TIME = seq(1, 109, by = 12))


xaxis_text <- tibble(time = 1:120,
                     yy_mm = seq(ymd("2024-01-01"), length = 120, by = "1 month")) 

# clean environment
# rm(list = setdiff(ls(), c("geog_level", "pop_cost_data", "xaxis_text")))

#### end of main script ----





