library(tidyverse)
library(here)
library(countrycode)

cpi_trend_link <- "https://images.transparencycdn.org/images/CPI2023_Global_Results__Trends.xlsx"
download.file(cpi_trend_link, here("data-raw", "cpi_trend.xlsx"))

cpi_trend <- readxl::read_excel(here("data-raw", "cpi_trend.xlsx"),
                                sheet = "CPI Timeseries 2012 - 2023",
                                skip = 3) %>% 
    select(matches("ISO3|CPI|Standard")) %>% 
    pivot_longer(cols = -ISO3,
                 names_to = c("var", "year"),
                 names_pattern = "(.*) (\\d{4})$") %>% 
    mutate(country = countrycode(ISO3, "iso3c", "country.name",
                                 custom_match = c("KSV" = "Kosovo")), 
           year = as.numeric(year),
           var = if_else(str_detect(var, "CPI"), "cpi", "se")) %>% 
    pivot_wider(names_from = "var",
                values_from = "value") %>% 
    select(country, year, cpi, se) %>% 
    filter(!is.na(cpi))

cpi_links <- c(
    "https://images.transparencycdn.org/images/CPI-2011-new_200601_104308.csv",
    "https://images.transparencycdn.org/images/CPI-2010-new_200601_105629.csv",
    "https://images.transparencycdn.org/images/CPI-2009-new_200601_120052.csv",
    "https://images.transparencycdn.org/images/CPI-Archive-2008-2.csv",
    "https://images.transparencycdn.org/images/CPI-2007-new_200602_092501.csv",
    "https://images.transparencycdn.org/images/CPI-2006-new_200602_095933.csv",
    "https://images.transparencycdn.org/images/CPI-2005_200602_104136.csv",
    "https://images.transparencycdn.org/images/CPI-2004_200602_110140.csv",
    "https://images.transparencycdn.org/images/CPI-2003_200602_113929.csv",
    "https://images.transparencycdn.org/images/CPI-2002_200602_115328.csv",
    "https://images.transparencycdn.org/images/CPI-2001_200603_082938.csv",
    "https://images.transparencycdn.org/images/CPI-2000_200603_083012.csv",
    "https://images.transparencycdn.org/images/CPI-1999_200603_083052.csv",
    "https://images.transparencycdn.org/images/CPI-1998_200603_083119.csv",
    "https://images.transparencycdn.org/images/CPI-Archive-1997.csv",
    "https://images.transparencycdn.org/images/CPI-Archive-1996.csv",
    "https://images.transparencycdn.org/images/CPI-Archive-1995.csv")

cpi_raw <- map(cpi_links, \(link) {
    read_csv(link,
             col_types = "ccccdc") %>%
        mutate(year = as.numeric(str_extract(link, "\\d{4}"))) %>%
        select(country, year, everything()) }) %>% 
    set_names(2011:1995)

cpi_older <- map(cpi_links, \(link) {
    read_csv(link, 
             col_types = "ccccdc") %>% 
        {`if`(!"interval" %in% names(.) & "range" %in% names(.), 
              mutate(., interval = range),
              `if`(!"interval" %in% names(.),
                   mutate(., interval = NA_character_),
                   .))} %>%
        mutate(interval = if_else(str_detect(interval, "-"),
                                  as.numeric(str_extract(interval, "\\d+.\\d$")) - as.numeric(str_extract(interval, "^\\d+.\\d")), 
                                  as.numeric(interval))) %>% 
        transmute(country = countrycode(iso, "iso3c", "country.name",
                                        custom_match = c("KSV" = "Kosovo")),
                  year = as.numeric(str_extract(link, "\\d{4}")),
                  cpi = as.numeric(str_replace(score, ",",".")) * 10,
                  se = if_else(year < 2002, 
                               interval*5/qnorm(.975),
                               interval*5/qnorm(.95)))
}) %>% 
    list_rbind() %>% 
    group_by(country) %>% 
    mutate(se = ifelse(is.na(se), max(se, na.rm = TRUE), se)) %>% 
    ungroup()

cpi <- bind_rows(cpi_trend, cpi_older) %>% 
    arrange(country, year) %>% 
    rename(cpi_se = se)

write_csv(cpi, "cpi.csv")
