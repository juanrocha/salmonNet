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
    filter(Commodity_Code %in% c( # excluded all frozen stuff, fishmeals, and preparations
        '030212','030213', '030214', # fresh salmon
        '030441' # filets
        )) %>%
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


dat <- dat %>%
    filter(Year > 2009 & Year < 2018)

dat$Period %>% unique() %>% length() 
glimpse(dat)


###########################################################################
## Full analysis with all dataset
###########################################################################

### First transform the data:
## Normalizing but skiping the log transformation here. Currently I'm not using log transformed data,
# but I leave the column here calculated just in case. The reason not to use it is that the CCM result 
# does not change, but the embeding and non-linearity tests do become noisy. Large fluctuations in the 
# main actors like Chile are reduced so much in scale that they don't show up, when compared with small 
# fluctuations in other countries.


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

## Nonlinearity test
nonlinearity <- list()
for (i in 2:dim(df_dat)[2]){
    nonlinearity[[i-1]] <- df_dat[c(1,i)] %>%
        s_map(., E = bestE[i-1])
    nonlinearity[[i-1]]$link <- colnames(df_dat)[i]
}

# the help for this test is a bit criptic. Check the vignettes.
# x <- test_nonlinearity(
#     ts=df_dat[,c(1,2)],  num_surr = 50, E = bestE[2-1])

nonlinear <- nonlinearity %>% map_dbl(~ .$theta[which.max(.$rho)])
df_nonlinear <- data_frame(
    best_theta = nonlinear,
    link = colnames(df_dat)[-1]
)

## Calculate the rho matrix
ind <- crossing(lib_column = colnames(df_dat)[-1], target_column = colnames(df_dat)[-1])
ind <- ind %>% filter(lib_column != target_column)
# to change the embedding dimension to the best embedding, I need to use pmap instead of map2.
# Then the arguments are a list of the same lenght, which means I need to left join
# the ind dataframe with the embedding object matching by lib_column!
ind <- ind %>% left_join(., bestE_df, by = c('lib_column' = 'link'))
ind <- ind %>% rename(E = bestE)
ind <- ind %>% left_join(., df_nonlinear, by = c('lib_column' = 'link'))
ind <- ind %>% rename(theta = best_theta)

ind

param <- rlang::as_list(ind %>% select(-theta))

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

end <- Sys.time() ## It talkes 28min

end - start

# save(rho_list, file = "~/Documents/Projects/Salmon_Jessica/data/190718_salmon_rho_list_full.Rdata")

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
#     file = "~/Documents/Projects/Salmon_Jessica/data/190718_salmon_t_tests_rho_full.Rdata")

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

## add key results to dataframe
ind <- ind %>%
    mutate(
        rho = map_dbl(.x = rho_list, .f = ~ mean(.x$rho, na.rm = TRUE)),
        rho_t = map_dbl(.x = t_tests$result, function(x) ifelse(is.null(x$estimate), NA, x$estimate)),
        p_value = map_dbl(.x = t_tests$result, function(x) ifelse(is.null(x$p.value), NA, x$p.value)),
        corr = map_dbl(.x = corr_list, function(x) max(abs(x))),
        detection = ifelse(p_value <0.05 & rho > 0.1, TRUE, FALSE),
        strong_detection = ifelse(p_value < 0.05 & rho > corr, TRUE, FALSE)
    )

######## Interpreting results
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


ind %>% filter(p_value < 0.05, A == B | C == D) ##  zero cases! great

df <- ind %>% 
    mutate(id = seq(1:dim(ind)[1])) %>% ## this id allows me to retrieve objects in rho_list
    filter(detection == TRUE,  B == D) ## 42 links out of 28,392 links analysed | 375 with weak detection... sweet!
# this is trivial case where the countries share the same exporter.
ind %>% filter(strong_detection == TRUE, A == C) ## 240 links where A==C 

## Transitivity cases: 1748 | 1383 with theta / 28392
ind %>% filter(strong_detection == TRUE, A != C, B != D, A != D, B != C, A != B, C != D, theta > 0)

### If you want visualizations, go to 04-DataExploration scrip and run lines 678 onwards.

df
print(df, n = 42)

## The important thing now is to find which nodes are reciprocal, so where external factors might be of interest to test

library(sna)

salmon_net <- network(
    x = dat %>% group_by(exporter_name, importer_name) %>% summarize(flow = sum(total_netweight_tons)),
    directed = TRUE, matrix.type = "edgelist", ignore.eval = FALSE
)

causal_net <- network(
    x = df %>% filter(theta > 0) %>% select(country_C, country_A, rho, corr, p_value),
    directed = TRUE, matrix.type = "edgelist", ignore.eval = FALSE
)


mutuality(salmon_net)
mutuality(causal_net) # 7 diads are reciprocal
grecip(causal_net)

plot(causal_net)
as.sociomatrix(causal_net, attrname = "rho") %>% heatmaply::heatmaply(Rowv = FALSE, Colv = FALSE)

## Let's test with Norway and France which is one of the reciprocal links:
# old climate data, below just upload the latest automatically!
# load("~/Documents/Projects/FindingNemo/data/NOAA_ClimateData.rda")

noaa <- read_table(
    file = "ftp://ftp.cpc.ncep.noaa.gov/wd52dg/data/indices/tele_index.nh", 
    skip = 17, progress = TRUE)
climdat <- noaa %>%
    separate(col = 5, into = c("WP", "EPNP"), sep = -6) %>%
    separate(col = 9, into = c("SCA", "TNH"), sep = -6) %>%
    separate(col = 11, into = c('POL', "PT"), sep = -6) %>%
    filter(yyyy > 2009, yyyy < 2018)

climdat <- climdat %>% map_df(function(x) as.numeric(x))


# France = 251, Norway = 579, Belgium = 56
test_dat <- data_frame(
    Period = df_dat$Period,
    FRA_BEL = df_dat$`251_56`,
    NOR_BEL = df_dat$`579_56`,
    nao = climdat$NAO,
    eao = climdat$EA,
    sca = climdat$SCA
)

test_dat <- test_dat %>%
    as.data.frame()

## embedding
emb <- list()
for (i in 2:dim(test_dat)[2]){
    emb[[i-1]] <- test_dat[c(1,i)] %>%
        simplex()
    emb[[i-1]]$link <- colnames(test_dat)[i]
}

# best embedding dimension:
bestE <- emb %>% map_dbl(~ .$E[which.max(.$rho)])
bestE_df <- data_frame(
    bestE = bestE,
    link = colnames(test_dat)[-1]
)
## Prediction decay
prediction_decay <- list()
for (i in 2:dim(test_dat)[2]){
    prediction_decay[[i-1]] <- test_dat[c(1,i)] %>%
        simplex(., E = bestE[i-1], tp = 1:10)
    prediction_decay[[i-1]]$link <- colnames(test_dat)[i]
}

## Nonlinearity test
nonlinearity <- list()
for (i in 2:dim(test_dat)[2]){
    nonlinearity[[i-1]] <- test_dat[c(1,i)] %>%
        s_map(., E = bestE[i-1])
    nonlinearity[[i-1]]$link <- colnames(test_dat)[i]
}

test_ind <- crossing(lib_column = colnames(test_dat)[-1], target_column = colnames(test_dat)[-1])
test_ind <- test_ind %>% filter(lib_column != target_column)
# to change the embedding dimension to the best embedding, I need to use pmap instead of map2.
# Then the arguments are a list of the same lenght, which means I need to left join
# the ind dataframe with the embedding object matching by lib_column!
test_ind <- test_ind %>% left_join(., bestE_df, by = c('lib_column' = 'link'))
test_ind <- test_ind %>% rename(E = bestE)
# test_ind <- test_ind %>% left_join(., df_nonlinear, by = c('lib_column' = 'link'))
# test_ind <- test_ind %>% rename(theta = best_theta)


param <- rlang::as_list(test_ind )

start <- Sys.time()
## Now rho is calculated with pmap: takes about 50'
rho_test <- pmap(
    param,
    ccm,
    block = test_dat,
    lib_sizes = seq(5, dim(test_dat)[1], by = 10),
    replace = FALSE, silent = TRUE,
    random_libs = FALSE
)

end <- Sys.time() ## It talkes 28min

end - start

start <- Sys.time()
corr_test <-  map2(
    .x = test_ind$lib_column, .y = test_ind$target_column ,
    .f = ~ ccf(x = test_dat[.x], y = test_dat[.y],
               type = "correlation", plot = FALSE,
               na.action = na.pass, demean = FALSE)$acf
)

end <- Sys.time() 

end-start # 3mins
# T-tests again for significance
t_tests2 <- purrr::map(.x = rho_test, safely(
    .f = ~ t.test(.x$rho, alternative = "greater", mu = 0, na.action = na.exclude))
)
#
t_tests2 <- transpose(t_tests2)
fail <- t_tests2$error %>% map_lgl(is_null) %>% unlist()
sum(!fail) #no errors


## add key results to dataframe
test_ind <- test_ind %>%
    mutate(
        rho = map_dbl(.x = rho_test, .f = ~ mean(.x$rho, na.rm = TRUE)),
        rho_t = map_dbl(.x = t_tests2$result, function(x) ifelse(is.null(x$estimate), NA, x$estimate)),
        p_value = map_dbl(.x = t_tests2$result, function(x) ifelse(is.null(x$p.value), NA, x$p.value)),
        corr = map_dbl(.x = corr_test, function(x) max(abs(x))),
        detection = ifelse(p_value <0.05 & rho > 0.1, TRUE, FALSE),
        strong_detection = ifelse(p_value < 0.05 & rho > corr, TRUE, FALSE)
    )


## Here I'm following again full tutorial

eao_surr <- make_surrogate_data(test_dat$eao, method = "ebisuzaki")


rho_surr_FRA <- list()
rho_surr_NOR <- list()
for (i in 1:100){
    rho_surr_FRA[[i]] <- ccm(cbind(test_dat$FRA_BEL, eao_surr[,i]), 
                             E = 4, lib_column = 1, target_column = 2,
                             lib_sizes = NROW(test_dat), replace = FALSE)
    
    rho_surr_NOR[[i]] <- ccm(cbind(test_dat$NOR_BEL, eao_surr[,i]), 
                              E = 5, lib_column = 1, target_column = 2,
                              lib_sizes = NROW(test_dat), replace = FALSE)
}
## p-value test from tutorial
(sum(test_ind %>% filter(lib_column=="FRA_BEL", target_column=="eao") %>% pull(rho) < bind_rows(rho_surr_FRA)$rho) + 1) / (length(rho_surr_FRA) + 1)

(sum(test_ind %>% filter(lib_column=="NOR_BEL", target_column=="eao") %>% pull(rho) < bind_rows(rho_surr_NOR)$rho) + 1) / (length(rho_surr_NOR) + 1)


(sum(M_rho['Thrips_imaginis','maxT_degC'] < rho_surr$maxT) + 1) / (length(rho_surr$maxT) + 1)