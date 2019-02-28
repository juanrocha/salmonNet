## Data extraction from ComTrade
## J181017


# load libraries
library(tidyverse)
library(RSelenium)
library(httr)
library(rvest)

# Initialize Chrome and go to WoS from SU website, so it's authenticated already.

## Initialize remote driver
d <- rsDriver() # should open a chrome
remDr <- d[["client"]]
remDr$navigate("https://sub.su.se/databaser-a-o/")
Sys.sleep(3)

## search
search_field <- remDr$findElement(
    using = 'css', value = '.ng-valid')
search_field$clearElement()
search_field$sendKeysToElement(list("UN comtrade", key = "enter"))

search_field <- remDr$findElement(
    using = 'css', value = '.ng-binding'
)
search_field$clickElement()

## Authentication
me <- remDr$findElement("id", "username")
# me$clearElement()
me$sendKeysToActiveElement(list("juro7132"))
pswd <- remDr$findElement("xpath", '//*[(@id = "password")]')
# pswd$clearElement()
pswd$sendKeysToElement(list(keyring::key_get("sub", "juro7132"), key = "enter"))
Sys.sleep(30)

## navigate to download form:
button_get <- remDr$findElement('css', '.btn-large')
button_get$clickElement()

dont_show <- remDr$findElement('css','#dismiss')
dont_show$clickElement()
button_close <- remDr$findElement('css', '.btn-default')
button_close$clickElement()


# remDr$refresh()
## select monthly data:
month <- remDr$findElement('css', '#freq-m')
month$clickElement()

# Select periods
choices <- remDr$findElements('css', '.select2-search-choice-close')
choices[[1]]$clickElement() # Period
Sys.sleep(3)
choices[[1]]$sendKeysToActiveElement(list('all', key = "enter"))


choices[[4]]$clickElement() # Tradeflows
Sys.sleep(3)
choices[[4]]$sendKeysToActiveElement(list(key = "down_arrow", key = "enter"))


choices[[5]]$clickElement() # Commodities
Sys.sleep(3)
choices[[5]]$sendKeysToActiveElement(
    list("030212", key = "enter",
    "salmon", key = "enter",
    "030214",key = "enter",
    "030219", key = "enter",
    "030311",key = "enter",
    "030312", key = "enter",
    "030313",key = "enter",
    "030319",key = "enter",
    "030322",key = "enter",
    "030329" ,key = "enter",
    "030441" ,key = "enter",
    "030452" ,key = "enter",
    "030481", key = "enter",
    "030541",key = "enter",
    "160411",key = "enter"))

download_button <- remDr$findElement('css','#download-csv-top')
Sys.sleep(3)
download_button$clickElement()
Sys.sleep(30)


### This first file should give me the complete list of Reporters that I can later use to loop over.
########### Check file and get country options:
dat <- read_csv("~/Downloads/comtrade_countries.csv")

countries <- dat %>% pull(Reporter) %>% unique()

## Here I change the country names that produce errors
countries[1] <- "USA"
countries[6] <- "EU-28"
countries[60] <- "Czechia"


## move up again so you can click
body <- remDr$findElement('css', 'body')
body$sendKeysToElement(list(key = "home"))

# Set partners to all of them
choices[[3]]$clickElement() # Partners
Sys.sleep(3)
choices[[3]]$sendKeysToActiveElement(list("all", key = "enter"))

download_file <- function(country){
    ## move up again so you can click
    body <- remDr$findElement('css', 'body')
    body$sendKeysToElement(list(key = "home"))
    Sys.sleep(2)

    choices <- remDr$findElements('css', '.select2-search-choice-close')
    choices[[2]]$clickElement() # Reporters
    # choices[[2]]$clearElement()
    Sys.sleep(3)
    choices[[2]]$sendKeysToActiveElement(list(country, key = "enter"))

    download_button <- remDr$findElement('css','#download-csv-top')
    Sys.sleep(3)
    download_button$clickElement()
    Sys.sleep(25)

    return(country)
}

### Here it comes the loop
### Not working with purrr workflow
# results <- list()
# results <- map(.x = countries, safely(
#     .f = ~ download_file()
# ))

## this works individually, assuming the countries matches
# download_file(countries[2])
results <- list()
for (i in 60:length(countries)){ #seq_along(countries) when working
    results[[i]] <- download_file(countries[i])
}


### Rename files

# setwd("~/Downloads")
setwd("~/Documents/Projects/DATA/Salmon_trade")
files <- list.files()
info <- file.info(files)
info <- as_tibble(info)
info %>% mutate(name = files) %>% select(name, mtime) %>% arrange(desc(mtime))

prom_files <- files[str_which(files, "comtrade")]

for (i in seq_along(prom_files)){
    data <- read_csv(file = prom_files[i])
    file_name <- data$Reporter %>% unique() %>% str_replace_all(" ", "_") %>% str_replace_all("\\.|\\,", "")
    file.rename(prom_files[i], paste0(file_name, ".csv"))
}


setwd("~/Downloads")
data <- read_csv("comtrade_countries.csv")
data$Reporter %>% unique()


### Which ones are missing?
missing <- countries[!countries %in% (files %>% str_remove_all(".csv") %>% str_replace_all("_", " "))]
# not missing but just different spelling
missing <- missing[-c(1,2,4,7,9,10)]

### J181020: There are 125 files, one per country, on the data folder. There are 140 countries on the initial comtrade_countries file.

results <- list()
for (i in seq_along(missing)){ #seq_along(countries) when working
    results[[i]] <- download_file(missing[i])
}

## Last ones were downloaded manually due to weird duplicated names that throughs errors on the download step.
