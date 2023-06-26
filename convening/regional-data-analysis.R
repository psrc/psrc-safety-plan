library(tidyverse)
library(tidycensus)

data_years <- seq(2010, 2021, by=1)

metros <- c("Portland", "Bay Area", "San Diego", "Denver", "Atlanta","Washington DC", "Boston", "Miami" ,"Phoenix", "Austin", "Dallas")

# MPO Cenus Data ----------------------------------------------------------------
mpo <- read_csv("data/regional-councils-counties.csv") %>% 
  mutate(COUNTY_FIPS=str_pad(COUNTY_FIPS, width=3, side=c("left"), pad="0")) %>%
  mutate(STATE_FIPS=str_pad(STATE_FIPS, width=2, side=c("left"), pad="0")) %>%
  mutate(GEOID = paste0(STATE_FIPS,COUNTY_FIPS))

states <- mpo %>% select(STATE_FIPS) %>% distinct() %>% pull()
counties <- mpo %>% select(GEOID) %>% distinct() %>% pull()

# Download Data and Process -----------------------------------------------
processed <- NULL
for (yrs in data_years) {
  
  print(str_glue("Downloading MPO Census Data for {yrs}"))
  mpo_county_data <- NULL
  for (st in states) {
    
    c <- mpo %>% filter(STATE_FIPS %in% st) %>% select(COUNTY_FIPS) %>% pull()
    
    pop <- get_acs(geography = "county", state=st, county=c, variables = c("B03002_001","B03002_003"), year = yrs, survey = "acs5") %>% select(-moe)
    ms <- get_acs(geography = "county", state=st, county=c, variables = c("B08006_001","B08006_003"), year = yrs, survey = "acs5") %>% select(-moe)
    d <- bind_rows(pop,ms)
    
    ifelse(is.null(mpo_county_data), mpo_county_data <- d, mpo_county_data <- bind_rows(mpo_county_data,d))
    
    rm(d, c, pop, ms)
  } # end of Census Data Loop
  
  print(str_glue("Cleaning up MPO Census Data for {yrs}"))
  mpo_county_data <- mpo_county_data %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    mutate(People_of_Color=(B03002_001-B03002_003), NonSOV_Commute_Trips=(B08006_001-B08006_003)) %>%
    rename(Population=B03002_001, Non_Hispanic_White=B03002_003, Commute_Trips=B08006_001, SOV_Commute_Trips=B08006_003) %>%
    select(GEOID, Population, People_of_Color, Non_Hispanic_White, Commute_Trips, NonSOV_Commute_Trips, SOV_Commute_Trips)
  
  mpo_county_data <- left_join(mpo, mpo_county_data, by="GEOID")  
  
  print(str_glue("Processing MPO FARS Data for {yrs}"))
  fars_data <- NULL
  for (y in seq(yrs-4, yrs, by=1)) {
    
    # Open Current Years FARS Accident Data
    all_files <- as.character(unzip(paste0("X:/DSA/shiny-uploads/data/FARS",y,"NationalCSV.zip"), list = TRUE)$Name)
    
    c <- read_csv(unz(paste0("X:/DSA/shiny-uploads/data/FARS",y,"NationalCSV.zip"), all_files[1])) %>%
      mutate(COUNTY_FIPS=str_pad(COUNTY, width=3, side=c("left"), pad="0")) %>%
      mutate(STATE_FIPS=str_pad(STATE, width=2, side=c("left"), pad="0")) %>%
      mutate(GEOID = paste0(STATE_FIPS,COUNTY_FIPS)) %>%
      filter(GEOID %in% counties) %>%
      select(GEOID, FATALS) %>%
      mutate(FATAL_COLLISIONS = 1) %>%
      group_by(GEOID) %>%
      summarise(Fatalities=sum(FATALS), Fatal_Collisions =sum(FATAL_COLLISIONS)) %>%
      as_tibble
    
    ifelse(is.null(fars_data), fars_data <- c, fars_data <- bind_rows(fars_data,c))
    
    rm(c)
    
  }
  
  print(str_glue("Combining MPO Census Data and FARS data for {yrs}"))
  fars_data <- fars_data %>%
    group_by(GEOID) %>%
    summarise(Fatalities_5yr=sum(Fatalities), Fatal_Collisions_5yr =sum(Fatal_Collisions)) %>%
    as_tibble
  
  mpo_county_data <- left_join(mpo_county_data, fars_data, by="GEOID") 
  
  mpo_data <- mpo_county_data %>%
    select("MPO_AREA", "MPO_FIPS", "MPO_NAME", "Population", "Fatalities_5yr", "Fatal_Collisions_5yr") %>%
    group_by(MPO_AREA, MPO_FIPS, MPO_NAME) %>%
    summarise(population=sum(Population), injuries=sum(Fatalities_5yr), collisions=sum(Fatal_Collisions_5yr)) %>%
    mutate(injury_rate = ((injuries/5)/population)*100000) %>%
    mutate(collision_rate = ((collisions/5)/population)*100000) %>%
    mutate(year = as.character(yrs))
  
  ifelse(is.null(processed), processed <- mpo_data, processed <- bind_rows(processed, mpo_data))
  rm(mpo_data, mpo_county_data, fars_data)
  
}

print("Output final FARS data")
write_csv(processed, "data/mpo_data.csv")
