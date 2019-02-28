## Data cleaning with tidyverse. I've tried with spark but encounter some problems.

library(tidyverse)

setwd("~/Documents/Projects/DATA/Salmon_trade")

files <- list.files()

data_list <- map(files, read_csv)

### correct names:
df_names <- colnames(data_list[[1]])
new_names <- df_names %>%
    str_remove_all("[:punct:]") %>%
    str_remove_all("\\$") %>%
    str_replace_all( " ", "_")

data_list <- map(data_list,
    .f = function(df){
    colnames(df) <- new_names
    return(df)
})

data_list %>% map(function(x) class(x$Netweight_kg))

data_list <- map(data_list, function(x) {
    mutate(x, Netweight_kg = as.numeric(Netweight_kg))
})

### Now it's a data_frame
data_list <- data_list %>% bind_rows()

### Delete entries from undesired places or aggregations:
reporters <- data_list %>% pull(Reporter) %>% unique() %>% sort()
partners <- data_list%>% pull(Partner) %>% unique() %>% sort()

reporters[!reporters %in% partners]
partners[!partners %in% reporters]

## Problematic reporters or partners
probs <- c(
    str_subset(partners, " nes"),
    "World", "Special Categories", "Neutral Zone", "Free Zones", "Ic")

## This approach was following Jessica's logic but I'm not sure it works properly, at least information about link directionality is lost, which is important for the network approach.
# imports <- data_list %>%
#     filter(Reporter != "EU-27") %>%
#     filter(!Partner %in% probs) %>%
#     filter(!is.na(Commodity_Code)) %>%
#     filter(Trade_Flow == "Imports") %>%
#     select(Year, Period, Period_Desc, Reporter, Partner, Reporter_Code, Partner_Code, Commodity_Code, Netweight_kg, Trade_Value_US) %>%
#     group_by(Year, Period,  Reporter, Partner, Reporter_Code, Partner_Code) %>%
#     summarize(
#         Netweight = sum(Netweight_kg, na.rm = TRUE),
#         Trade_Value = sum(Trade_Value_US, na.rm = TRUE)
#     ) %>%
#     rename(import_weight = Netweight, import_value = Trade_Value)
#
# exports <- data_list %>%
#     filter(Reporter != "EU-27") %>%
#     filter(!Partner %in% probs) %>%
#     filter(!is.na(Commodity_Code)) %>%
#     filter(Trade_Flow == "Exports") %>%
#     select(Year, Period, Period_Desc, Reporter, Partner, Reporter_Code, Partner_Code, Commodity_Code, Netweight_kg, Trade_Value_US) %>%
#     group_by(Year, Period, Reporter, Partner, Reporter_Code, Partner_Code) %>%
#     summarize(
#         Netweight = sum(Netweight_kg, na.rm = TRUE),
#         Trade_Value = sum(Trade_Value_US, na.rm = TRUE)
#     ) %>%
#     rename(export_weight = Netweight, export_value = Trade_Value)

## This is wrong! Notice that the parallel structure only works at link level, so one has to code the link in such a way that Reporter->Parner Exports is equivalent to Reporter->Partner Imports grouped by time. A pmax on the whole column loses the information of who repors the flow, who is reported and who is partner (the direction of the link change if imports or exports.)
# dat <- full_join(imports, exports) %>%
#     mutate(
#         Netweight_kg = pmax(import_weight, export_weight, na.rm = TRUE),
#         Trade_Value_US = pmax(import_value, export_value, na.rm = TRUE)
#     )

dat <- data_list %>%
    select(-Classification, -Aggregate_Level, -Is_Leaf_Code, -Reporter_ISO, -Partner_ISO, -starts_with("2nd"), -starts_with("Customs"), -starts_with("Mode"), -starts_with("Qty"), -starts_with("Alt"), -CIF_Trade_Value_US, -FOB_Trade_Value_US, -Flag, -Gross_weight_kg) %>%
    filter(Reporter != "EU-27") %>%
    filter(!Partner %in% probs) %>%
    filter(!is.na(Commodity_Code)) %>%
    filter(Trade_Flow != "Re-exports" ) %>%
    filter(Trade_Flow != "Re-imports") %>%
    filter(!is.na(Trade_Value_US)) %>%
    filter(!is.na(Netweight_kg)) %>%
    filter(!is.na(Period_Desc))

#### Tests: This all should produce empty data_frames
# dat %>% filter(Reporter == "EU-27")
# dat %>% filter(Partner %in% probs)
# dat %>% filter(is.na(Commodity_Code))
# dat %>% filter(Trade_Flow == "Re-exports" | Trade_Flow == "Re-imports")
# dat %>% filter(is.na(Trade_Value_US) | is.na(Netweight_kg))
# dat %>% filter(is.na(Period_Desc))

## From here on, the flow always goes from left to fight, so from_to
dat <- dat %>% mutate(
    link = ifelse(
        Trade_Flow == "Exports",
        paste(Reporter_Code, Partner_Code, sep = "_"),
        paste(Partner_Code, Reporter_Code, sep = "_")
    )
)

## If the above worked correctly and the data is consisten, the expression below should be true: only 2 entries per pair of countries per commodity per period:
dat %>% group_by(link, Period, Commodity_Code) %>%
    tally() %>% # arrange(desc(n))
    pull(n) %>% table() # all(. == 2) # FALSE

## The table shows that only for 67k entries actually both countries reported (potential duplicates). For other 196k records, only one country reported. So I will split the data according to how many records we have.

dat <- dat %>%
    group_by(link, Period, Commodity_Code) %>%
    mutate(n_obs = n())

dat_1 <- dat %>%
    filter(n_obs == 1) %>%
    mutate(max_netweight = Netweight_kg,
        diff_netweight = 0,
        max_value_us = Trade_Value_US,
        diff_value_us = 0
    )

dat_2 <- dat %>%
    filter(n_obs == 2) %>%
    mutate(
        max_netweight = max(Netweight_kg), #there is no NAs
        diff_netweight = abs(diff(Netweight_kg, lag = 1)),
        max_value_us = max(Trade_Value_US),
        diff_value_us = abs(diff(Trade_Value_US))
    )

## How bad is it?
# dat_2 %>%
#     ggplot(aes(diff_value_us, diff_netweight)) +
#     geom_point(alpha = 0.2) +
#     theme_light()

### I will ignore any differences in price and select the max value when two different values are reported.
dat_2 <- dat_2 %>%
    select(-Trade_Flow, -Trade_Flow_Code, -Netweight_kg, -Trade_Value_US, -Reporter, -Reporter_Code, -Partner, -Partner_Code) %>%
    unique()

## To get unique values without duplicates, I need to drop all info about partners and reporters, but the directionality of the flow is preserved on the link variable. Do the same with dat_1 before joining

dat_1 <- dat_1 %>%
    select(-Trade_Flow, -Trade_Flow_Code, -Netweight_kg, -Trade_Value_US, -Reporter, -Reporter_Code, -Partner, -Partner_Code) %>%
    unique()

dat <- bind_rows(dat_1, dat_2) # 264377 clean observations with no duplicates.

### If necessary in the future, I can always recover the list of countries and codes to merge with the data. The codes are the same, there is 140 Reporter countries but 239 Partner countries, since the list is inclusive in Parnters, I only need their codes.

countries <- data_list %>%
    select(Partner, Partner_Code) %>%
    unique() %>%
    mutate(Partner_Code = as.character(Partner_Code))

dat <- dat %>%
    separate(
        col = link,
        into = c("exporter", "importer"),
        sep = "_",
        remove = FALSE) %>%
    left_join(.,countries, by = c("exporter" = "Partner_Code")) %>%
    rename(exporter_name = Partner) %>%
    left_join(.,countries, by = c("importer" = "Partner_Code")) %>%
    rename(importer_name = Partner)


write_csv(dat, path = "~/Documents/Projects/Salmon_Jessica/data/clean_salmon_data_181022.csv")

## clean up a bit:
rm(dat_1, dat_2, exports, imports, files, g, new_names, partners, probs, reporters, x)
