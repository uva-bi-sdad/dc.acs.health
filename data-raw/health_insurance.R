# pull NCR ACS health insurance data
# correct health district geoids and add hlth_ins_pct var to VA data

library(DBI)
library(dplyr)
library(tidycensus)
library(tidyverse)

census_api_key(Sys.getenv("CENSUS_API_KEY"))


# VA data -----------------------------------------------------

con <- get_db_conn()
va_df <- DBI::dbReadTable(con, c("dc_health_behavior_diet",
                                 "va_hdcttr_acs5_2015_2019_no_health_insurance_19_to_64"))
hd <- DBI::dbReadTable(con, c("dc_geographies", "va_hd_vdh_2021_health_district_geo_names"))

# match with new health district geoids

new_va_df <- merge(va_df, hd[ , c(1,3)], by = "region_name", all.x = TRUE)
new_va_df$geoid <- ifelse(is.na(new_va_df$geoid.y), new_va_df$geoid.x, new_va_df$geoid.y)

no_hlth_ins_df <- new_va_df %>%
  select(geoid, region_type, region_name, year, measure, value, measure_type)

# add health insurance pct as a measure

hlth_ins_df <- no_hlth_ins_df
hlth_ins_df$measure <- "hlth_ins_pct"
hlth_ins_df$value = 100 - hlth_ins_df$value

df <- rbind(no_hlth_ins_df, hlth_ins_df)

dc_dbWriteTable(con, "dc_health_behavior_diet",
                "va_hdcttr_acs5_2015_2019_no_health_insurance_19_to_64", df)
DBI::dbDisconnect(con)

rm(con,df,hd,hlth_ins_df,new_va_df,no_hlth_ins_df,va_df)


# NCR data pull -----------------------------------------

states <- c("VA", "MD", "DC")
geographies <- c("county", "tract", "block group")
years <- c(2015:2019)  # no block group data -- table B27010 has block gp data

ins <- NULL
ins_wide <- NULL

for(state in states)
{
  for(year in years)
  {
    for(geography in geographies)
    {
      # pull health insurance vars
      vars = c("B18135_013", "C27001I_007")

      ins <- get_acs(geography = geography, variables = vars, state = state, year = year,
                     geometry = FALSE, survey = "acs5", cache_table = TRUE, output = "wide") %>%
        transmute(
          geoid = GEOID,
          region_type = as.character(geography),
          region_name = NAME,
          year = year,
          no_hlth_ins_pct = round((C27001I_007E/B18135_013E)*100, 2),
          hlth_ins_pct = 100 - no_hlth_ins_pct
        ) %>%
        arrange(geoid)

      ins_wide <- rbind(ins_wide, ins)
    }
  }
}

# Format change to long and add measure_type column

ins_long <- gather(ins_wide, measure, value, no_hlth_ins_pct:hlth_ins_pct)
ins_long$measure_type <- "percent"


# NCR ------------------------------------------

ncr_counties <- c("^24021|^24031|^24033|^24017|^11001|^51107|^51059|^51153|^51013|^51510|^51683|^51600|^51610|^51685")

# filter to NCR

ncr_ins <- ins_long %>% dplyr::filter(str_detect(geoid, ncr_counties))

con <- get_db_conn()
dc_dbWriteTable(con, "dc_health_behavior_diet",
                "ncr_cttrbg_acs5_2015_2019_no_health_insurance_19_to_64", ins_long)
DBI::dbDisconnect(con)

