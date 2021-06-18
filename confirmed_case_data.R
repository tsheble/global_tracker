# import
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(rnaturalearth)) install.packages("rnaturalearth", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(rnaturalearth)) install.packages("rnaturalearth", repos = "http://cran.us.r-project.org")
if(!require(rnaturalearthdata)) install.packages("rnaturalearthdata", repos = "http://cran.us.r-project.org")
if(!require(tigris)) install.packages("tigris", repos = "http://cran.us.r-project.org")



# raw data  global cases (cumulative)

cases_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
global_cases_raw <- read_csv(cases_url) %>% as.data.frame()


format_rawdata <- function(df) {
  names(df)[1:2] = c("Province", "Country")
  df$Country <- df$Country %>% str_remove_all("[:punct:]") %>% 
    str_remove_all("[:space:]") %>% str_replace_all("KoreaSouth", "SouthKorea") 
  
  dates <- names(global_cases_raw)[which(names(global_cases_raw)=="1/22/20"):ncol(global_cases_raw)] %>% as.Date(format = c('%m/%d/%y'))
  
  df <- df %>%
    select(-c(1,3,4)) %>%
    group_by(Country) %>%
    summarize_each(funs(sum)) %>%
    data.frame()
  rownames(df) = df$Country
  df <- df[,-1] %>% t()
  df <- data.frame(dates, df) %>% rename(Date = dates)
  rownames(df) <- 1:nrow(df)
  return(df)
}

# iso data
iso_url <- 
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv" 
iso_raw <- read_csv("UID_ISO_FIPS_LookUp_Table.csv") %>% as.data.frame()

format_isodata <- function(df) {
  iso <- df %>% filter(is.na(Province_State))
  
  iso$Country_Region <- iso$Country_Region %>% str_remove_all("[:punct:]") %>% str_remove_all("[:space:]")
  return(iso)
}
iso_data <- format_isodata(iso_raw)

# format cumulative case data
global_cases_totals <- format_rawdata(global_cases_raw)
global_cases_totals

# function to return any negative case calculations as zero 
no_neg <- function(x) {
  ifelse(x < 0, 0, x)
}

# function to calculate daily new cases
get_newcases <- function(df) {
  
  # function to calculate new_cases
  new_cases <- function(x) {
    new <- no_neg(x - lag(x))
  }
  
  # apply to data frame
  global_cases_new <- df %>% mutate_at(vars(-Date), (funs(new_cases(.))))
  
  # return data frame
  return(global_cases_new)
}

# function to calculate weekly change in cases
get_weekcases <- function(df) {
  
  # function to calculate week change in cases
  
  week_cases <- function(x) {
    week <- no_neg(x -lag(x, 7))
  }
  
  # apply to data frame
  global_cases_week <- df %>% mutate_at(vars(-Date), (funs(week_cases(.))))
  
  # return data frame
  return(global_cases_week)
}

# calculate daily cases
global_daily_cases <- get_newcases(global_cases_totals)


# calculate weekly cases
global_week_cases <- get_weekcases(global_cases_totals)


# format key-value total_cases
total_cases <- global_cases_totals %>% gather(key = 'Country', value = 'total_cases', c(-Date))

# format key-value daily_cases
daily_cases <- global_daily_cases %>% gather(key = 'Country', value = 'daily_cases', c(-Date))

# format key-value week_cases
weekly_cases <- global_week_cases %>% gather(key = 'Country', value = 'weekly_cases', c(-Date))

# function to calculate population statistics

population_data <- function(total, daily, weekly, iso) {
  cases <- total %>% right_join(daily) %>% 
    right_join(weekly) 
  cases <- cases %>% full_join(iso, by = c("Country" = "Country_Region"))
  pop_stat <- cases %>%
    mutate(total_pop = total_cases/Population, 
           daily_pop = daily_cases/Population,
           weekly_pop = weekly_cases/Population)
  return(pop_stat)
}

cases <- total_cases %>% full_join(daily_cases) %>%
  right_join(weekly_cases)

# calculate population statistics
case_data <- population_data(total_cases, daily_cases, weekly_cases, iso_data)

map_url <- 'https://tile.jawg.io/jawg-terrain/{z}/{x}/{y}.png?access-token=62vQTvERwUD1LC3zLk9IPmZfgayoFturP8JMkT3ID9CCEcxJkeOAeCSdZ8uGUfPu'

spatial_data <- rnaturalearth::ne_countries(scale = 50)
spatial_data %>% as.data.frame() %>% head
map_data <- tigris::geo_join(spatial_data, case_data, 'iso_a3', 'iso3')

# range for legend
total_cases_range <- transmute(case_data %>% filter(iso3 != 'GRL'), total_case_range=total_cases/Population) %>% unique()
total_cases_range %>% quantile(na.rm = TRUE)
daily_cases_range <- transmute(case_data, daily_case_range=daily_cases/Population) %>% unique()
daily_cases_range %>% quantile(na.rm = TRUE)
weekly_cases_range <- transmute(case_data, week_cases_range=weekly_cases/Population) %>% unique()
weekly_cases_range %>% quantile(na.rm = TRUE)

# map colors
colors<-pals::linearlhot(500)[100:420]


get_countries <- function(df) {
  top_cases <- case_data %>% filter(Date == today()-1) %>% select(Combined_Key, total_cases) %>% arrange(desc(total_cases))
  top_population <- case_data %>% filter(Date == today()-1) %>% select(Combined_Key, Population) %>% arrange(desc(Population))
  country_selection <- top_cases$Combined_Key[1:25] %>% union(top_population$Combined_Key[1:25])
  df <- case_data %>% filter(Date == today()-1) %>% filter(Combined_Key %in% country_selection)
  return(df)
}

top_countries <- get_countries(case_data) %>% 
  arrange(desc(Population)) %>% 
  select(Country = Combined_Key)

df <- case_data %>% filter(Combined_Key %in% top_countries$Combined_Key)


