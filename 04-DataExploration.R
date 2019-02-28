## Data exploration
## J181023

## follows the ideas of 02_exporation.R but updates to newer updated dataset.

library(tidyverse)
library(lubridate)


dat <- read_csv(
    "~/Documents/Projects/Salmon_Jessica/data/clean_salmon_data_181022.csv")

dat <- dat %>%
    ungroup() %>%
    mutate(Period = parse_date_time(Period, orders = "Ym"))

dat$Period %>% unique() %>% length() # 120 time points!
dat$link %>% unique() %>% length() # 4483 links

## The aggregation per commodity is something that might change in the future. So that step of the data cleaning will be the first of the analysis instead. In doing so, the differences in netweight reported and US value are then neglected.

dat <- dat %>% # for now all commodities are used.
    # filter(!Commodity_Code %in% c(<Insert here the codes to delete>)) %>%
    select(-starts_with("diff"), -starts_with("Commodity")) %>%
    group_by(link, Period) %>%
    mutate(
        total_netweight_tons = sum(max_netweight)/1000,
        total_value_us = sum(max_value_us)
    ) %>%
    select(-starts_with("max")) %>% unique() %>% ungroup()

dat$Year %>% range() # 2008 - 2018

## Visualize adjacency matrix over time
# ggplot(data = dat,
#     aes(exporter, importer)
# ) + geom_raster(aes(fill = total_netweight_tons)) + facet_wrap(~Period) +
#     theme_light(base_size = 8) +
#     theme(axis.text.x =element_blank(),
#         axis.text.y = element_blank())

dat %>%
    group_by(Period) %>%
    summarize(obs = n()) %>% #pull(obs) %>% range()
    arrange(Period) %>%
    print(n=120)
## From this talbe one can see that there is only one datapoint per month before 2010, and after december 2017 the data seems incomplete. There is around 1400 transactions per month, but in 2018 they drop. Perhaps the 2018 is not yet fully digitalized. So from now on I will only use data from 2010-17

dat <- dat %>%
    filter(Year > 2009 & Year < 2018)

dat$Period %>% unique() %>% length() # 96 time points left

## Delete Bunkers, that's not a country. For more info: http://www.fao.org/faostat/en/#data/EM/metadata
dat <- filter(dat, importer_name != "Bunkers")

## For visualization purposes, I also want to have the standard ISO codes per country
####J181024: Fix names later. It's been a headache and it seems I will have to do it manually later or find the correct file with non-conflicting codes / names.
# countries_fao <- read_csv(
#     "~/Documents/Projects/DATA/FAO_Capture_2017.1.1/CL_FI_COUNTRY_GROUPS.csv")
#
# ## correct the naming of columns, theny are off by one, and drop irrelevant columns
# countries_fao <- countries_fao %>%
#     select(
#         fao_code = UN_CODE,
#         un_code = ISO2_CODE,
#         iso2_code = ISO3_CODE,
#         iso3_code = Name_en,
#         name_en = Name_fr,
#         continent = EcoClass_Group,
#         region = FIGIS_ID
#     )
#
# ### Missing exporters
# miss_exp <- (dat$exporter %>% unique() %>% as.numeric() %>% sort())[!
# (dat$exporter %>% unique() %>% as.numeric() %>% sort()) %in%
# (countries_fao$fao_code %>% as.numeric() %>% sort())]
#
# ### Missing importers
# miss_imp <- (dat$importer %>% unique() %>% as.numeric() %>% sort())[!
# (dat$importer %>% unique() %>% as.numeric() %>% sort()) %in%
# (countries_fao$fao_code %>% as.numeric() %>% sort())]
#
# dat %>% filter(exporter %in% miss_exp) %>% select(exporter_name, exporter) %>% unique()
# dat %>% filter(importer %in% miss_imp) %>% select(importer_name) %>% unique()
#
# ### It seems to be a +1 problem, a country was added on the new classification that move some of the codes 1 number up.
# ## J181024: Not really. I will need to modify manually the miscoded countries. However, the problem is when replicating with other commodities, there is no guarantee that the miscoded countries would be the same.
# probs <- countries$Partner_Code[!countries$Partner_Code %in% countries_fao$fao_code]
#
# dat %>% filter(exporter %in% probs) %>% select(exporter_name, exporter) %>% unique()
# dat %>% filter(importer %in% probs) %>% select(importer_name, importer) %>% unique()
#
#
# for (i in rev(seq_along(probs))){
#     if (probs[i] %in% unique(dat$exporter)) {
#         pais <- dat %>%
#             filter(exporter == probs[i]) %>%
#             select(exporter, exporter_name) %>%
#             unique() %>% pull(exporter_name)
#         corr_code <- countries_fao %>%
#             filter(name_en == name_en[str_detect(name_en, pais)]) %>%
#             pull(fao_code)
#         dat$exporter[dat$exporter == probs[i] ] <- corr_code
#     }
# }
#
#
# dat %>% filter(exporter == probs[1]) %>% select(exporter, exporter_name) %>% unique()
# countries_fao %>% filter(name_en == "Norway")
#
#
#
# dat$exporter[dat$exporter == "251"] <- "250" # France
# dat$exporter[dat$exporter == miss_exp[2]] <- "380" # Italy
# dat$exporter[dat$exporter == miss_exp[3]] <- "578" # Norway
# dat$exporter[dat$exporter == miss_exp[4]] <- "356" # India
# dat$exporter[dat$exporter == miss_exp[5]] <- "756" # Switzerland
# dat$exporter[dat$exporter == miss_exp[6]] <- "840" # USA
#
# dat$importer[dat$importer == miss_imp[1]] <- "250" # France
# dat$importer[dat$importer == miss_imp[2]] <- "380" # Italy
# dat$importer[dat$importer == miss_imp[3]] <- "578" # Norway
# dat$importer[dat$importer == miss_imp[4]] <- "356" # India
# dat$importer[dat$importer == miss_imp[5]] <- "756" # India
# dat$importer[dat$importer == miss_imp[7]] <- "840" # USA
#
#
# ## Now running again the lines for missing countries should be an empty vector.
# ## If so, we are ready to merge and rescue the country codes.
#
# dat %>%
#     left_join(countries_fao %>%
#         select(fao_code, iso3_code)
#         , by = c("exporter" = "fao_code")) %>%
#     rename(exporter_iso = iso3_code) %>% select(exporter_iso, exporter_name) %>%
#     filter(is.na(exporter_iso)) %>% unique()
#
#
#
#     left_join(countries_fao %>%
#         select(fao_code, iso3_code)
#         , by = c("importer" = "fao_code"))  %>%
#     rename(importer_iso = iso3_code)

glimpse(dat)


### Analysis on USA's ego network
usa <- dat %>%
    filter(importer_name == "United States of America")

usa %>%
#    filter(exporter_name == "Chile") %>%
    ggplot(aes(Period, total_netweight_tons)) +
    geom_line(aes(color = link), show.legend = FALSE) +
    facet_wrap(.~exporter_name) +
    theme_light(base_size = 6)

## There is too little observations for some links, filter and drop:
usa <- usa %>%
    group_by(link) %>%
    mutate(n_obs = n()) %>%
    filter(n_obs > 60)

## Normalizing but skiping the log transformation here.
usa <- usa %>%
    group_by(link) %>%
    #mutate(log_weight = log1p(total_netweight_tons)) %>%
    mutate(
        norm_weight = (total_netweight_tons - mean(total_netweight_tons, na.rm = TRUE)) / sd(total_netweight_tons, na.rm = TRUE))

## Visualizations of the USA ego network time series.
# ggplot(usa, aes(Period, link)) +
#     geom_raster(aes(fill = norm_weight))
#
# ggplot(usa, aes(Period, norm_weight)) +
#     geom_line(aes(color = link), show.legend = FALSE) +
#     geom_vline(xintercept = parse_date_time(c("2016-02", "2016-05"), orders = "Ym"), color = 'red', alpha = 0.5) +
#     facet_wrap(.~exporter_name) +
#     theme_minimal(base_size = 8)


### Prepare dataset for ccm
usa %>% ungroup() %>% #skimr::skim()
    select(Period, link, norm_weight) %>%
    group_by(Period, link) %>%
    spread(link, norm_weight)

##### J181025: Another problem -- why are there more obs per link that time points? And why doesn't it show up on the plots? Maybe there is still many commodities here that have not been deleted?

usa %>%
    group_by(link) %>%
    tally() %>% arrange(desc(n))
