## Cleaning data from ComTrade

library(sparklyr)
library(tidyverse)

# spark_install(version = "2.1.0")
setwd("~/Documents/Projects/DATA/Salmon_trade")

sc <- spark_connect(master = 'local')
spark_salmon <- spark_read_csv(
    sc, name = "salmon",
    path = "~/Documents/Projects/DATA/Salmon_trade/*.csv" )

spark_salmon %>%
    tally()

reporters <- spark_salmon %>% pull(Reporter) %>% unique() %>% sort()
partners <- spark_salmon %>% pull(Partner) %>% unique() %>% sort()

reporters[!reporters %in% partners]
partners[!partners %in% reporters]

## Problematic reporters or partners
probs <- c(
    str_subset(partners, " nes"),
    "World", "Special Categories", "Neutral Zone", "Free Zones", "Ic")

## Aggregate for all commodities:
glimpse(spark_salmon)

spark_salmon %>% pull(Commodity_Code) %>% unique()

# spark_salmon %>%
#     filter(!is.na(Commodity_Code)) %>%
#     select(Year, Period, Period_Desc_, Trade_Flow, Trade_Flow_Code,  Reporter, Partner, Reporter_Code, Partner_Code, Commodity_Code, Netweight_kg, Trade_Value_US) %>%
#     group_by(Year, Period, Preiod_Desc_, Trade_Flow, Trade_Flow_Code,  Reporter, Partner, Reporter_Code, Partner_Code) %>%
#     summarize(
#         Netweight = sum(Netweight_kg, na.rm = TRUE),
#         Trade_Value = sum(Trade_Value_US, na.rm = TRUE)
#     )


#########
export <- spark_salmon %>% #tally()
    filter(Reporter != "EU-27") %>%
    filter(!Partner %in% probs) %>%
    filter(!is.na(Commodity_Code))
    select(2,3,4,7:14,
        Commodity_Code, Commodity, Netweight_kg, Trade_Value_US) %>%
    filter(Netweight_kg > 0) %>%
    filter(Trade_Flow == "Exports") %>%
    rename(export_weight = Netweight_kg,
        export_value = Trade_Value_US)

import <- spark_salmon %>% #tally()
    filter(Reporter != "EU-27") %>%
    filter(!Partner %in% probs) %>%
    filter(!is.na(Commodity_Code))
    select(2,3,4,7:14,
        Commodity_Code, Commodity, Netweight_kg, Trade_Value_US) %>%
    filter(Netweight_kg > 0) %>%
    filter(Trade_Flow == "Imports") %>%
    rename(import_weight = Netweight_kg,
        import_value = Trade_Value_US)

clean_data <- full_join(export, import)

clean_data <- compute(clean_data)

spark_write_csv(clean_data, path = "cleaned_salmon_data.csv")

spark_disconnect(sc)
