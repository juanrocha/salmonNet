## Data exploration
## J181023

## follows the ideas of 02_exporation.R but updates to newer updated dataset.

library(tidyverse)
library(lubridate)
library(rEDM)


dat <- read_csv(
    "~/Documents/Projects/Salmon_Jessica/data/clean_salmon_data_181022.csv")

dat <- dat %>%
    ungroup() %>%
    mutate(Period = parse_date_time(Period, orders = "Ym"))

dat$Period %>% unique() %>% length() # 120 time points!
dat$link %>% unique() %>% length() # 4483 links

## Delete Bunkers, that's not a country. For more info: http://www.fao.org/faostat/en/#data/EM/metadata
dat <- filter(dat, importer_name != "Bunkers")
dat <- filter(dat, importer_name != "Ic")
dat <- filter(dat, exporter_name != "Ic")

## The aggregation per commodity is something that might change in the future. So that step of the data cleaning will be the first of the analysis instead. In doing so, the differences in netweight reported and US value are then neglected.

dat <- dat %>% # for now all commodities are used.
    filter(!Commodity_Code %in% c('030219','160411','030329', '030452')) %>%
    select(-starts_with("diff"), -starts_with("Commodity")) %>%
    group_by(link, Period) %>%
    select(-n_obs) %>%
    mutate(
        total_netweight_tons = sum(max_netweight)/1000,
        total_value_us = sum(max_value_us)
    ) %>%
    select(-starts_with("max")) %>% unique() %>% ungroup()


## J181106: Test - if the command above keep unique values of links and there is max 120 time stamps, why are there more than 120 obs per link?
dat %>%
    group_by(link) %>%
    tally() %>% arrange(desc(n)) %>% print(n = 20)

## Found the problem. The problem is Iceland which is duplicated. The code 352 is the same, but country name appears as "Iceland", or "Ic". Delete all Ic entries. Now deleted, the test is past. No link has more than 120obs.
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

df_node_attr <- dat %>%
    group_by(exporter) %>%
    summarize(median_exported_tons = median(total_netweight_tons, na.rm = TRUE)) %>%
    rename(country = exporter) %>%
    full_join(
        (dat %>%
            group_by(importer) %>%
            summarize(median_imported_tons = median(total_netweight_tons, na.rm = TRUE)) %>%
            rename(country = importer))
    )

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
    mutate(log_weight = log1p(total_netweight_tons)) %>%
    mutate(
        norm_weight = (total_netweight_tons - mean(total_netweight_tons, na.rm = TRUE)) / sd(total_netweight_tons, na.rm = TRUE),
        norm_log_weight = (log_weight - mean(log_weight, na.rm = TRUE)) / sd(log_weight, na.rm = TRUE)
    )

## Visualizations of the USA ego network time series.
# ggplot(usa, aes(Period, link)) +
#     geom_raster(aes(fill = norm_weight))
#
g_usa <- ggplot(usa, aes(Period, total_netweight_tons)) + #norm_weight, norm_log_weight, 
    geom_line(aes(color = link), show.legend = FALSE) +
    geom_vline(xintercept = parse_date_time(c("2016-02", "2016-05"), orders = "Ym"), color = 'red', alpha = 0.5) +
    facet_wrap(.~exporter_name) + 
    labs(title = "US salmon imports", caption = "UN Comtrade Data [monthly records]") +
    theme_minimal(base_size = 7) 

ggsave(g_usa, filename = "USA_salmon.png", device = "png", width = 5, height = 5, units = "in", dpi = 600)


### Prepare dataset for ccm
df_usa <- usa %>% ungroup() %>% #skimr::skim()
    select(Period, link, norm_weight) %>%
    group_by(Period, link) %>%
    spread(link, norm_weight) %>%
    as.data.frame()

# check embedding dimension # E = 1
simplex_out <- simplex(as.data.frame(df_usa)[c(1,3)])
plot(simplex_out$E, simplex_out$rho, type = "l", xlab = "Embedding Dimension (E)", ylab = "Forecast Skill (rho)")

## check for forecasting horizon
simplex_output <- simplex(as.data.frame(df_usa)[c(1,3)], E = 4, tp = 1:12)
plot(simplex_output$tp, simplex_output$rho, type = "l", xlab = "Time to Prediction (tp)", ylab = "Forecast Skill (rho)")

## Identify non-linearity # Theta = 1
smap_output <- s_map(as.data.frame(df_usa)[c(1,3)], E = 4)
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")

## Convergent cross mapping:
chl_xmap_nor <- ccm(as.data.frame(df_usa), E=1, lib_column = "152_842", target_column="579_842", lib_sizes = seq(3,61, by =2), random_libs=FALSE)

nor_xmap_chl <- ccm(as.data.frame(df_usa), E=1, target_column = "152_842", lib_column="579_842", lib_sizes = seq(3,61, by =2), random_libs=FALSE)


chl_xmap_nor_means <- data.frame(ccm_means(chl_xmap_nor), sd.rho = with(chl_xmap_nor, tapply(rho, lib_size, sd)))

nor_xmap_chl_means <- data.frame(ccm_means(nor_xmap_chl), sd.rho = with(nor_xmap_chl, tapply(rho, lib_size, sd)))

### Plots
df3 <- rbind(chl_xmap_nor_means, nor_xmap_chl_means) %>%
  unite(xmap, lib_column, target_column, sep = " <- ")

df3 <- df3 %>% 
    mutate(`Cross-mapping` = ifelse(xmap == "152_842 <- 579_842", "CHL_USA <- NOR_USA", "NOR_USA <- CHL_USA"))

g_cmm <- ggplot(df3, aes(lib_size, rho, group = `Cross-mapping`)) +
  geom_ribbon(aes(ymin = rho - sd.rho, ymax = rho + sd.rho, fill = `Cross-mapping`), alpha = 0.3) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  geom_line(aes(color = `Cross-mapping`)) +
  labs(x = "Library size", y = "Forecasting skill") + # expression(rho)
  labs(title = "Convergent cross-mapping for Chile and Norway", 
       caption = "Data Comtrade UN [monthly records] from 2010-2018") +
  theme_light(base_size=10) + theme(legend.position = "bottom")

ggsave(g_cmm, filename = "CHL_USA_NOR_salmon.png", device = "png", width = 5, height = 5, units = "in", dpi = 600)


# J181106: The results with new dataset does not reproduce the previous results with Netherlands but works with Norway :(

##########################################
## Explore the embedding dimension for all time series of the ego-network on USA
out <- list()
for (i in 2:dim(df_usa)[2]){
    out[[i-1]] <- df_usa[c(1,i)] %>%
      simplex()
}

bestE <- out %>% map_dbl(~ .$E[which.max(.$rho)])

## Prediction decay
prediction_decay <- list()
for (i in 2:dim(df_usa)[2]){
    prediction_decay[[i-1]] <- df_usa[c(1,i)] %>%
      simplex(., E = bestE[i-1], tp = 1:10)
}

## Nonlinearity test
nonlinearity <- list()
for (i in 2:dim(df_usa)[2]){
    nonlinearity[[i-1]] <- df_usa[c(1,i)] %>%
      s_map(., E = bestE[i-1])
}

## Plots
source('~/Dropbox/Code/multiplot.R')
g <- out %>% map( .,
    function(x) {
        ggplot(data = x, aes(E,rho)) +
        geom_line() +
        theme_light(base_size = 7)
        })
p_decay <- prediction_decay %>% map( .,
    function(x) {
        ggplot(data = x, aes(tp, rho)) +
        geom_line() +
        theme_light(base_size=7)
        })

non_linear <- nonlinearity %>% map(.,
    function(x){
        ggplot(data = x, aes(theta,rho)) +
        geom_line() +
        theme_light(base_size = 7)
        })
layout <- matrix(1:20, ncol = 5, nrow = 4, byrow = T)

multiplot(plotlist = g, layout = layout)

multiplot(plotlist = p_decay, layout = layout)

multiplot(plotlist = non_linear, layout = layout)


###########

### Cross map matrix: following Hao Ye tutorial
ncol <- dim(df_usa)[2]-1
M_rho <- array(NA,dim=c(ncol,ncol),
  dimnames=list(colnames(df_usa[2:dim(df_usa)[2]]),colnames(df_usa[2:dim(df_usa)[2]])))

for (i in 1:ncol){
    for (j in 1:ncol){
        if (i!=j){
            out_temp <- ccm(df_usa,E=1,lib_column=1+i,target_column=1+j,
                            lib_sizes = dim(df_usa)[1],replace=FALSE, silent = TRUE)
            M_rho[i,j] <- out_temp$rho
            }
        }
    }
dfM_rho <- as_tibble(M_rho)
dfM_rho$i <- colnames(dfM_rho)
dfM_rho <- dfM_rho %>%
  select(21, 1:20) %>%
  gather(key = "j", value = "rho", 2:21)

ggplot(dfM_rho, aes(i,j, fill = rho)) +
  geom_raster() +
  scale_fill_gradient2(
      low = scales::muted("red"), mid = "white",
      high = scales::muted("blue"), midpoint = 0, space = "Lab",
      na.value = "grey50", guide = "colourbar") + # name = expression(rho)
  xlab("") + ylab("") + theme_light(base_size=6) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ))


############################################
## Correlation matrix: from tutorial code
M_corr <- array(NA,dim=c(ncol,ncol),
dimnames=list(colnames(df_usa[2:dim(df_usa)[2]]),colnames(df_usa[2:dim(df_usa)[2]])))

for (i in 1:ncol){
  for (j in 1:ncol){
      if (i!=j){
          cf_temp <- ccf(x=df_usa[,1+i], y=df_usa[,1+j], type = "correlation", lag.max = 6, plot = FALSE, na.action = na.pass)$acf
          M_corr[i,j] <- max(abs(cf_temp))
          }
      }
}

df_corr <- as_tibble(M_corr)
df_corr$i <- colnames(df_corr)
df_corr <- df_corr %>%
    select(21, 1:20) %>%
    gather(key = "j", value = "rho", 2:21)

ggplot(df_corr, aes(i,j, fill = rho)) +
    geom_raster() +
    scale_fill_gradient2(
      low = scales::muted("red"), mid = "white",
      high = scales::muted("blue"), midpoint = 0.5, space = "Lab",
      na.value = "grey50", guide = "colourbar") + # name = expression(rho)
    xlab("") + ylab("") + theme_light(base_size=6) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ))

##### Here is my attempt:

ind <- crossing(lib_column = colnames(df_usa)[-1], target_column = colnames(df_usa)[-1])
ind <- ind %>% filter(lib_column != target_column)

rho_list <- map2(
    .x = ind$lib_column, .y = ind$target_column ,
    .f = ~ ccm(block = df_usa, E = 1,
        lib_column = .x, target_column = .y,
        lib_sizes = seq(5,dim(df_usa)[1], by =5),
        replace = FALSE, silent = TRUE,
        random_libs = FALSE)
    )

## t-test for each relationship:
t_tests <- map(.x = rho_list, safely(
    .f = ~ t.test(.x$rho, alternative = "greater", mu = 0, na.action = na.exclude))
)

# p_vals <- map(t_tests, function(x) x$result$p.value)
t_tests <- transpose(t_tests)
fail <- t_tests$error %>% map_lgl(is_null) %>% unlist()
fail ## all should be true
ind <- ind %>%
    mutate(
        rho = map_dbl(.x = rho_list, .f = ~ mean(.x$rho, na.rm = TRUE)),
        rho_t = map_dbl(.x = t_tests$result, function(x) x$estimate ),
        p_value = map_dbl(.x = t_tests$result, function(x) x$p.value ),
        detection = ifelse(p_value <0.05 & rho > 0.1, TRUE, FALSE)
    )


## Any close to the matrix loop?
g1 <- ggplot(ind, aes(lib_column , target_column, fill = rho)) +
  geom_raster() +
  scale_fill_gradient2(
      low = scales::muted("red"), mid = "white",
      high = scales::muted("blue"), midpoint = 0, space = "Lab",
      na.value = "grey50", guide = "colourbar") + # name = expression(rho)
  xlab("") + ylab("") + theme_light(base_size=6) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ))

g2 <- ggplot(ind, aes(lib_column , target_column, fill = p_value)) +
    geom_raster() +
    scale_fill_gradient2(
        low = scales::muted("blue"), mid = "white",
        high = scales::muted("red"), midpoint = 0.05, space = "Lab",
        na.value = "grey50", guide = "colourbar") + # name = expression(rho)
    xlab("") + ylab("") + theme_light(base_size=6) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ))

multiplot(plotlist = list(g1,g2), layout = matrix(1:2, ncol = 2, nrow = 1))

###########################################################################
## Full analysis with all dataset
###########################################################################

### First transform the data:
## Normalizing but skiping the log transformation here. Currently I'm not using log transformed data, but I leave the column here calculated just in case. The reason not to use it is that the CCM result does not change, but the embeding and non-linearity tests do become noisy. Large fluctuations in the main actors like Chile are reduced so much in scale that they don't show up, when compared with small fluctuations in other countries.


dat <- dat %>%
    group_by(link) %>%
    mutate(obs = n()) %>%
    mutate(log_weight = log1p(total_netweight_tons)) %>%
    mutate(
        norm_weight = (total_netweight_tons -
            mean(total_netweight_tons, na.rm = TRUE)) / sd(total_netweight_tons, na.rm = TRUE),
        norm_log_weight = (log_weight -
            mean(log_weight, na.rm = TRUE)) /
            sd(log_weight, na.rm = TRUE)
    ) %>% ungroup()

df_dat <- dat %>%
    filter(obs > 75) %>%
    select(Period, link, norm_weight) %>%
    group_by(Period, link) %>%
    spread(link, norm_weight) %>%
    as.data.frame()

## embedding
emb <- list()
for (i in 2:dim(df_dat)[2]){
    emb[[i-1]] <- df_dat[c(1,i)] %>%
      simplex()
    emb[[i-1]]$link <- colnames(df_dat)[i]
}

# best embedding dimension:
bestE <- emb %>% map_dbl(~ .$E[which.max(.$rho)])
bestE_df <- data_frame(
    bestE = bestE,
    link = colnames(df_dat)[-1]
)
## Prediction decay
prediction_decay <- list()
for (i in 2:dim(df_dat)[2]){
    prediction_decay[[i-1]] <- df_dat[c(1,i)] %>%
      simplex(., E = bestE[i-1], tp = 1:10)
    prediction_decay[[i-1]]$link <- colnames(df_dat)[i]
}

prediction_decay %>% bind_rows() %>%
  ggplot(aes(tp, rho)) + geom_line(alpha = 0.2, aes(group = link))

## Nonlinearity test
nonlinearity <- list()
for (i in 2:dim(df_dat)[2]){
    nonlinearity[[i-1]] <- df_dat[c(1,i)] %>%
      s_map(., E = bestE[i-1])
    nonlinearity[[i-1]]$link <- colnames(df_dat)[i]
}

## Plots: I don't need a plot per variable anymore.
# g <- emb %>%
#   bind_rows() %>%
#     ggplot(aes(E,rho, group = link)) +
#         geom_line(alpha = 0.2) +
#         labs(x = "Embedding", y = expression(rho), title = "Embedding dimension") +
#         theme_light(base_size = 7)
#
# p_decay <- prediction_decay %>% bind_rows() %>%
#     ggplot(aes(tp, rho, group = link)) +
#         geom_line(alpha = 0.2) +
#         labs(x = "Prediction horizon", y = expression(rho), title = "Prediction decay") +
#         theme_light(base_size=7)
#
#
# non_linear <- nonlinearity %>% bind_rows() %>%
#     ggplot(aes(theta,rho, group = link)) +
#         geom_line(alpha = 0.2) +
#         labs(x = expression(theta), y = expression(rho), title = "Non-linearity") +
#         theme_light(base_size = 7)


# here just change the name of the plot list
layout <- matrix(1:3, ncol = 3, nrow = 1, byrow = T)
multiplot(plotlist = list(g, p_decay, non_linear), layout = layout)


#######

## Calculate the rho matrix
ind <- crossing(lib_column = colnames(df_dat)[-1], target_column = colnames(df_dat)[-1])
ind <- ind %>% filter(lib_column != target_column)
# to change the embedding dimension to the best embedding, I need to use pmap instead of map2.
# Then the arguments are a list of the same lenght, which means I need to left join
# the ind dataframe with the embedding object matching by lib_column!
ind <- ind %>% left_join(., bestE_df, by = c('lib_column' = 'link'))
ind <- ind %>% rename(E = bestE)


##### J181107: I ran out memory if I do all the job in one go. I will split the ind2 dataset and split the computation in two. Save the two objects and then filter only significant results before merging back again.
## J190502: Not necessary in new machine, it handles the full dataset at once, takes >6hrs though
# ind_a <- ind %>% slice(1:(n()/2))
# ind_b <- ind %>% slice((1 + n()/2):n())

#### Run it twice, one with _a and another with _b. It should take about 6hrs each.
## This step is unnecesary, one can do it with a dataframe as well. Also note that I was getting the same data frame in all iterations! The solution was on teh syntax of pmap. Originally I had .f = ~ ccm(...) but it's not necessary. I still get lost on how to use the ~ when passing functions to multiple objects.
param <- rlang::as_list(ind)

start <- Sys.time()
## Now rho is calculated with pmap: takes about 50'
rho_list <- pmap(
    param,
    ccm,
    block = df_dat,
        lib_sizes = seq(5, dim(df_dat)[1], by = 10),
        replace = FALSE, silent = TRUE,
        random_libs = FALSE
    )

end <- Sys.time() ## It talkes 6.68 hrs in new computer to complete -- full data

end - start

## Object size is 9Gb, length is ~280k dataframes!
## Here I saved the object that I can use to calculate the t-tests
# save(rho_list,
#     file = "~/Documents/Projects/Salmon_Jessica/data/190502_salmon_rho_list_full.Rdata")

# load(file = "~/Documents/Projects/Salmon_Jessica/data/181107_salmon_rho_list_partB.Rdata")

# T-tests again for significance
t_tests <- map(.x = rho_list, safely(
    .f = ~ t.test(.x$rho, alternative = "greater", mu = 0, na.action = na.exclude))
)
#
t_tests <- transpose(t_tests)
fail <- t_tests$error %>% map_lgl(is_null) %>% unlist()
sum(!fail) #no errors

pis <- t_tests$result %>% map(function(x) x$p.value < 0.05)

# save(t_tests,
#     file = "~/Documents/Projects/Salmon_Jessica/data/190502_salmon_t_tests_rho_full.Rdata")

## Since both rho_list and t_tests are saved on disk, here you can load them again:
# load("~/Documents/Projects/Salmon_Jessica/data/181107_salmon_rho_list_partA.Rdata") # long rho list with ~28k dataframes
# load("~/Documents/Projects/Salmon_Jessica/data/181201_salmon_t_tests_rho_partA.Rdata") # list with ~28k t-tests
# # load("~/Documents/Projects/Salmon_Jessica/data/salmon_short_rho.Rdata")
# 
# 
# ## Since you broke the analysis in part A and part B, now you need to join the results together in a df.
# ls()
# load("~/Documents/Projects/Salmon_Jessica/data/181201_salmon_t_tests_rho_partB.Rdata")

# format(object.size(t_tests), "Mb")
# t_tests_A <- t_tests
# # load("~/Documents/Projects/Salmon_Jessica/data/181113_salmon_t_tests_rho_partB.Rdata")
# t_tests_B <- t_tests
# 
# rm(t_tests)
# 
# t_tests <- c(transpose(t_tests_A), transpose(t_tests_B))
# t_tests <- transpose(t_tests)
# 
# identical(t_tests_A, t_tests_B)

# ind$rho <- map_dbl(t_tests$result, function(x) {ifelse(is_null(x), NA, x$estimate)})
# ind$p_value <- map_dbl(t_tests$result, function(x) {ifelse(is_null(x), NA, x$p.value)})
# # Nonlinearity
# ind$theta <- map_dbl(nonlinearity, function(x){
#     (x %>% arrange(desc(rho)) %>% pull(theta))[1]}
# )

ind <- ind %>%
  mutate(
    rho = map_dbl(.x = rho_list, .f = ~ mean(.x$rho, na.rm = TRUE)),
    rho_t = map_dbl(.x = t_tests$result, function(x) ifelse(is.null(x$estimate), NA, x$estimate)),
    p_value = map_dbl(.x = t_tests$result, function(x) ifelse(is.null(x$p.value), NA, x$p.value)),
    corr = map_dbl(.x = corr_list, function(x) max(abs(x))),
    detection = ifelse(p_value <0.05 & rho > 0.1, TRUE, FALSE),
    strong_detection = ifelse(p_value < 0.05 & rho > corr, TRUE, FALSE)
  )




## for comparison, I can also extract the mean rho value and the t-value as test
# of significance from zeroes
# ind$p_value <- t_tests$result %>% map_dbl(~.$p.value)
# ind$avg_rho <- t_tests$result %>% map_dbl(~.$estimate)

# save(ind,
#     file = "~/Documents/Projects/Salmon_Jessica/data/190502_salmonTrade_causal_links.Rdata")

load("~/Documents/Projects/Salmon_Jessica/data/190502_salmonTrade_causal_links.Rdata") #181202


## J190501: Calculate the cross-correlation matrix as well

start <- Sys.time()
corr_list <-  map2(
  .x = ind$lib_column, .y = ind$target_column ,
  .f = ~ ccf(x = df_dat[.x], y = df_dat[.y],
             type = "correlation", plot = FALSE,
             na.action = na.pass, demean = FALSE)$acf
)

end <- Sys.time() 

end-start # 3mins

correlation <- corr_list %>% 
  map(., function(x) max(abs(x))) %>%
  unlist()

## reload the ind data with all rho's calculated, merge with correlations

ind



#### To-do
# Filter the links that don't have predictive value.
# Reconstruct the network and see if there is actual A-B-C connections or if it's all random.

### CCM interpretation: ccm(dat, lib_col, target_col) means that you're trying to predict target with library. If you have predictive power (rho is > 0), then it means that the time series of library contains information about target, so the causal link flows from target to library. The opposite.

ind <- ind %>%
    separate(lib_column, into = c("A", "B"), sep = "_", remove = FALSE) %>%
    separate(target_column, into = c("C", "D"), sep = "_", remove = FALSE)

## Motifs: A->B is the time series of the trade flow from country A to country B, C->D is the time series of trade flow from country C to country D. There is few interesting motifs:
## A->B, C->D when B == D is the most important motif I'm looking for, it's when trade on country A is influenced / caused, by trade in country C; because C->D is the target column and causality flows in reverse as information.
## A->B, C->D when A == C is the case when two time series are causaly related because they share the same driver (or the same country is exporting, so governed by the dynamics of the same fish stock resources).
## A->B, C->D when A != C & B != D means there must be a higher order interaction, a correlation due to unobserved issues (e.g. climate), or problems

ind %>% filter(p_value < 0.05, A == B | C == D) ## this should not exist!!! 538 cases related to country 124

df <- ind %>% 
  mutate(id = seq(1:dim(ind)[1])) %>% ## this id allows me to retrieve objects in rho_list
  filter(detection == TRUE, strong_detection == TRUE, B == D) ## 3737 links out of 324330 analyzed!

ind %>% filter(strong_detection == TRUE, A == C) ## 9654 links out of 14092 where A==C
ind %>% filter(strong_detection == TRUE,A != C, B != D, A != D, B != C, A != B, C != D)  ## 176964 links out of 324330 analyzed.


## If D == B, then there is a causal link between A <- C, the time series of A-B has information of the time series C-D.

df ## now contains the strong detection links
# so to visualize them, use the index id
x <- 1441
rho_list[[x]] %>%
  ggplot(aes(lib_size, rho)) + 
  geom_smooth() + 
  geom_point() +
  geom_hline(aes(yintercept = 0), color ="red") +
  geom_hline(aes(yintercept = df %>% filter(id == x) %>% pull(corr)), color = "orange") +
  theme_minimal()


### load the countries data:
countries <- read_csv(
    "~/Documents/Projects/Salmon_Jessica/data/clean_salmon_countries_181116.csv")

load('~/Documents/Projects/Salmon_Jessica/world_country_coordinates.RData')

countries <- countries %>%
    mutate(
        Partner_Code = as.character(Partner_Code)
    )

df <- df %>%
    left_join(countries, by = c("A" = "Partner_Code")) %>%
    rename(country_A = Partner) %>%
    left_join(countries, by = c("C" = "Partner_Code")) %>%
    rename(country_C = Partner)

df %>%
    group_by(country_A, country_C) %>%
    summarize(
        n = n(),
        mean_rho = mean(rho, na.rm = TRUE),
        sum_rho = sum(rho, na.rm = T)
    ) %>%
    arrange(desc(n)) %>%
    print(n = 100)

df_plot <- df %>%
    group_by(country_A, country_C) %>%
    summarize(
        n = n(),
        mean_rho = mean(rho, na.rm = TRUE),
        sum_rho = sum(rho, na.rm = T),
    ) %>% ungroup() %>%
    group_by(country_A) %>%
    mutate(in_degree = n()) %>%
    ungroup() %>%
    group_by(country_C) %>%
    mutate(out_degree = n()) %>%
    ungroup()

g <- ggplot(df_plot, aes(n, mean_rho)) + geom_point()
g <- ggplot(df_plot, aes(out_degree, mean_rho)) + geom_boxplot(aes(fill = country_C), alpha = 0.5)


df_nodes <- df_plot %>%
    select(country_A, in_degree) %>%
    unique() %>%
    rename(country = country_A) %>%
    full_join(.,
        (df_plot %>%
        select(country_C, out_degree) %>%
        unique() %>%
        rename(country = country_C))
    )

df_nodes_attr <- df_node_attr %>%
    mutate(country = as.character(country)) %>%
    left_join(countries, by = c("country" = "Partner_Code")) %>%
    rename(country_name = Partner)

df_nodes_attr <- df_nodes_attr %>%
    left_join(coords, by = c("country_name" = "country")) %>%
    rename(iso_code = Importer.ISO3c)

df_nodes_attr <- df_plot %>%
        group_by(country_C) %>%
        summarize(mean_rho_mean_out = mean(mean_rho, na.rm = TRUE),
                mean_rho_var_out = var(mean_rho, na.rm = TRUE)) %>%
    full_join(df_nodes_attr, by = c("country_C" = "country_name")) %>%
    rename(country_name = country_C)

df_nodes_attr <- df_plot %>%
        group_by(country_A) %>%
        summarize(mean_rho_mean_in = mean(mean_rho, na.rm = TRUE),
                mean_rho_var_in = var(mean_rho, na.rm = TRUE)) %>%
    full_join(df_nodes_attr, by = c("country_A" = "country_name")) %>%
    rename(country_name = country_A)

df_plot <- df_plot %>%
    left_join(coords, by = c("country_A" = "country")) %>%
    rename(xend = lon, yend = lat) %>% select(-Importer.ISO3c) %>%
    left_join(coords, by = c("country_C" = "country")) %>%
    rename(xini = lon, yini = lat) %>% select(-Importer.ISO3c)

## world map as template
world <- ggplot(map_data("world"), aes(x = long, y = lat)) +
    geom_polygon(aes(group = group), color = "grey65",
               fill = "#f9f9f9", size = 0.2) +
    #coord_map(projection = "mercator" ) #
    coord_quickmap() + theme_void()

### plot the links
g <- world +
    geom_curve(data = df_plot,
        aes(x = xini, y = yini, xend = xend, yend = yend,
            color = mean_rho, alpha = mean_rho),
        arrow = arrow(length = unit(0.15, "cm")),
        curvature = -0.15, size = 0.2
    ) +
    scale_color_gradient(low = "goldenrod", high = "red") +
    guides(
        # size = guide_legend("Degree"),
        colour = guide_colorbar(bquote(bar(rho)), barwidth = 0.5),
        alpha = guide_legend(bquote(bar(rho)))
    ) + theme_void(base_size = 6)

setwd("~/Documents/Projects/salmonNets")
quartz(width = 7, height = 3, pointsize = 6)
g
quartz.save("salmon_network_strong.pdf", type = "pdf", width = 7, height = 3, pointsize = 6)

### plot the nodes
g <- g +
    geom_point(
        data = df_nodes_attr,
        aes(x = lon, y = lat, size = log1p(median_exported_tons)),
        fill = "blue", color = "blue", alpha = 0.5,
        show.legend = FALSE
    ) +
    geom_point(
        data = df_nodes_attr,
        aes(x = lon, y = lat, size = log1p(median_imported_tons)),
        fill = "red", color = "red", alpha = 0.5,
        show.legend = FALSE
    )
    # +
    # geom_text(
    #     data = df_nodes_attr,
    #     aes(x = lon, y = lat, label = iso_code),
    #     size = 2, color = "grey10",
    #     show.legend = FALSE
    # )

###
library(naniar)
g2 <- df_nodes_attr %>%
    ggplot(aes(mean_rho_mean_in, mean_rho_mean_out)) +
    geom_smooth(method = 'lm', se = TRUE) +
    geom_errorbarh(aes(
        xmin = mean_rho_mean_in - mean_rho_var_in,
        xmax = mean_rho_mean_in + mean_rho_var_in,
    ), color = "grey84", alpha = 1) +
    geom_errorbar(aes(
        ymin = mean_rho_mean_out - mean_rho_var_out,
        ymax = mean_rho_mean_out + mean_rho_var_out,
    ), color = "grey84", alpha = 0.8) +
    geom_point(
        aes(size = log1p(median_imported_tons)),
        fill = "red", color = "red", alpha = 0.3,
        show.legend = FALSE) +
    geom_point(
        aes(size = log1p(median_exported_tons)),
        fill = "blue", color = "blue", alpha = 0.3,
        show.legend = FALSE) +
    geom_text(aes(label = iso_code), size = 2) +
    labs(x = bquote(bar(rho["in"])), y = bquote(bar(rho["out"]))) +
    theme_minimal(base_size = 8)

g3 <- df_nodes_attr %>%
    left_join(df_nodes, by = c("country_name" = "country")) %>%
    # mutate(in_degree = replace_na(in_degree,0),
    #     out_degree = replace_na(out_degree,0)) %>%
    ggplot(aes(in_degree, out_degree)) +
    geom_smooth(method = 'lm', se = TRUE) +
    geom_point(
        aes(size = log1p(median_imported_tons)),
        fill = "red", color = "red", alpha = 0.3,
        show.legend = FALSE) +
    geom_point(
        aes(size = log1p(median_exported_tons)),
        fill = "blue", color = "blue", alpha = 0.3,
        show.legend = FALSE) +
    geom_text(aes(label = iso_code), size = 2) +
    labs(x = "In-degree", y = "Out-degree") +
    theme_minimal(base_size = 8)
#
# g4 <- df_nodes_attr %>%
#     left_join(df_nodes, by = c("country_name" = "country")) %>%
#     mutate(in_degree = replace_na(in_degree,0),
#         out_degree = replace_na(out_degree,0)) %>%
#     ggplot(aes(log1p(median_imported_tons), log1p(median_exported_tons))) +
#     #geom_smooth(method = 'lm', se = TRUE) +
#     geom_point(
#         aes(fill = mean_rho_mean_out),
#         alpha = 0.5,
#         show.legend = TRUE) +
#     # geom_point(
#     #     aes(size = out_degree),
#     #     color = "blue",  alpha = 0.3,
#     #     show.legend = FALSE) +
#     #geom_text(aes(label = iso_code), size = 2) +
#     scale_fill_gradient(low = "goldenrod", high = "red", na.value = "black") +
#     guides(
#         # size = guide_legend("Degree"),
#         fill = guide_colorbar(bquote(bar(rho[out])), barwidth = 0.5)
#     ) + #
#     theme_minimal(base_size = 8)

g4 <- df_nodes_attr %>%
    left_join(df_nodes, by = c("country_name" = "country")) %>%
    mutate(in_degree = replace_na(in_degree,0),
        out_degree = replace_na(out_degree,0)) %>%
    ggplot(aes(log1p(median_imported_tons), in_degree )) +
    geom_smooth(method = 'lm', se = TRUE) +
    geom_point(
        aes(color = mean_rho_mean_in),
        size = 4,
        show.legend = TRUE) +
    geom_text(aes(label = iso_code), color = "white", size = 1.5) +
    labs(x = "Log of median imported tons", y = "In-degree") +
    scale_color_gradient(low = "goldenrod", high = "red", na.value = "black") +
    guides(color = guide_colorbar(bquote(bar(rho["in"])), barwidth = 0.5)) +
    theme_minimal(base_size = 10)

g5 <- df_nodes_attr %>%
    left_join(df_nodes, by = c("country_name" = "country")) %>%
    mutate(in_degree = replace_na(in_degree,0),
        out_degree = replace_na(out_degree,0)) %>%
    ggplot(aes(log1p(median_exported_tons), out_degree )) +
    geom_smooth(method = 'lm', se = TRUE) +
    geom_point(
        aes(color = mean_rho_mean_out), size = 4,
        show.legend = TRUE) +
    geom_text(aes(label = iso_code), color = "white", size =1.5) +
    labs(x = "Log of median exported tons", y = "Out-degree") +
    scale_color_gradient(low = "goldenrod", high = "red", na.value = "black") +
    guides(color = guide_colorbar(bquote(bar(rho[out])), barwidth = 0.5)) +
    theme_minimal(base_size = 10)

quartz(width = 7, height = 7, pointsize = 9)
layout <- matrix(1:4, ncol = 2, nrow = 2, byrow = T)
multiplot(plotlist = list(g2,g3,g4,g5), layout = layout)

getwd()
quartz.save("salmon_analysis_3.pdf", type = "pdf", width = 7, height = 7, pointsize = 9 )

##### J181129: Repeat analysis with fewer commodities, there is very weird results regarding main exporters. I believe it is because I included all salmon commodities (processes, unprocessed, salmonidae), it should be only fresh, filet, smoked.

library(networkD3)

net <- simpleNetwork(
    Data = df_plot,
    Source = "country_C",
    Target = "country_A", 
    zoom = TRUE
)

# save(df_plot, net,
#     file = "~/Documents/Projects/Salmon_Jessica/data/190502_salmonTrade_network.Rdata")

# sankeyNetwork(
#     Links = df_plot,
#     Nodes = countries,
#     Source = "country_C",
#     Target = "country_A"
# )

library(network)

net <- network(
    df_plot, directed = TRUE, ignore.eval = FALSE,
    matrix.type = 'edgelist'
)

plot.network(
    net,
    vertex.col = "grey",
    label = network.vertex.names(net),
    label.cex = 1,
    vertex.border = 0,
    vertex.cex = 1,
    label.pos = 5,
    edge.col = alpha("grey", 0.25)
)


load('~/Documents/Projects/Salmon_Jessica/world_country_coordinates.RData')
coords <- as_tibble(coords)


library(circlize)
library(shape)

chordDiagram(
    df_plot, annotationTrack = c("grid"),
    annotationTrackHeight = c( 0.1),
    directional = 1, direction.type = "diffHeight+arrows",link.arr.type = "big.arrow",
    link.arr.lwd = NA)

circos.trackPlotRegion(track.index = 1,
    panel.fun = function(x, y) {
        xlim = get.cell.meta.data("xlim")
        xplot = get.cell.meta.data("xplot")
        ylim = get.cell.meta.data("ylim")
        sector.name = get.cell.meta.data("sector.index")
    #if(abs(xplot[2] - xplot[1]) < 20) {
        # circos.text(mean(xlim), mean(ylim), sector.name, facing = "bending.outside", niceFacing = TRUE, #adj = c(0.5, 0),
        # cex = 1, col = "white") #}
    # else {circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
    #     niceFacing = TRUE, adj = c(0.5, 0), cex = .8)
    # }
    } , bg.border = NA)


### What's wrong with Guinea-Bissau = 624

dat %>% filter(importer == 768) %>% #pull(link) %>% unique()
    ggplot(aes(x = Period, y = total_netweight_tons)) +
        geom_line(aes(color = as.factor(exporter)),
    show.legend = FALSE)

## Add a selection command that drops time series with only few points.
