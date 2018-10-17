## This script uses the code from SalmonNetworks.Rmd to expand the exploration of
## salmon data to recover p-values regarding the rho coefficient in CCM.
## J180725
## Juan Rocha


## clean environment
# rm(list = ls())

## load required libraries
library(network)
library(sna)
library(tidyverse)
library(GGally)
library(rEDM)
library(gridBase)
library(grid)
library(gridExtra)


## Chile's minimal example

dat <- as_tibble(
    read.csv(
    "~/Documents/Projects/Salmon_Jessica/data/salmon_agg_clean_data.csv"))
# make the period factor so you can facet
dat <- mutate(dat,
    fac_period = factor(Period),
    log_weight = log1p(Agg.Weight))

# get rid of weird reported areas
dat <- filter(
    dat, Exporter != "Areas, nes" , Exporter != "Other Asia, nes" ,
    Exporter != "Other Europe, nes") %>%
    filter(Importer != "Areas, nes" , Importer != "Other Asia, nes" ,
          Importer != "Other Europe, nes" , Importer != "Other Africa, nes") %>%
    droplevels()

# there is a problem with missing factors
dat$Importer.ISO3c <- forcats::fct_explicit_na(dat$Importer.ISO3c, na_level = "missing")
## this is the list of extra problematic factors on importer side
dat %>% filter(Importer.ISO3c == "missing") %>% select(Importer) %>% pull() %>% droplevels() %>% levels()

dat <- filter(dat,  Importer != "Br. Virgin Isds" , Importer != "Bunkers" , Importer != "Curaçao" , Importer != "Saint-Barthélemy", Importer != "Special Categories" ) %>% droplevels()

## this first step is a waste of time, I do the same later below and accidentaly duplicate entries. For now I leave it this way because then I can explore first the link between chile and usa and then I can do it for all links incoming to usa.
df1 <- filter(dat, Exporter.ISO3c == "CHL", Importer.ISO3c == "USA") %>%
    select(Period, Exporter = Exporter.ISO3c, Importer = Importer.ISO3c,  log_weight)

time_points <- dat %>% select(Period) %>% unique() %>%
  arrange(Period) %>%
  mutate(time = row_number())

df1 <- bind_rows(df1,
  filter(dat, Importer.ISO3c == "USA")  %>%
  select(Period, Exporter = Exporter.ISO3c, Importer = Importer.ISO3c, log_weight)) %>%
  unite(link, Exporter, Importer, remove = TRUE) %>%
  left_join(time_points)

df1 <- df1 %>%
  group_by(link) %>% unique() %>%
  mutate(norm_weight = (log_weight - mean(log_weight, na.rm = TRUE)) / sd(log_weight, na.rm = TRUE),
    n_obs = n())

#### make dataframe suitable for CCM
df2 <- filter(df1, time < 30, n_obs > 25) %>%
    select(time, link, norm_weight) %>%
    spread(link, norm_weight) %>%
    as.data.frame()

## I'm skipping all plots, just focusing on extracting significant Rho
## Here is the minimal example for two links
## Convergent cross mapping:
chl_xmap_nld <- ccm(
    as.data.frame(df2), E=1, lib_column = "CHL_USA", target_column="NLD_USA",
    lib_sizes = seq(3,29, by =2), random_libs=FALSE)

nld_xmap_chl <- ccm(
    as.data.frame(df2), E=1, target_column = "CHL_USA", lib_column="NLD_USA",
    lib_sizes = seq(3,29, by =2), random_libs=FALSE)


chl_xmap_ndl_means <- data.frame(
    ccm_means(chl_xmap_nld), sd.rho = with(chl_xmap_nld, tapply(rho, lib_size, sd)))

ndl_xmap_chl_means <- data.frame(
    ccm_means(nld_xmap_chl), sd.rho = with(nld_xmap_chl, tapply(rho, lib_size, sd)))

## And here it is for the USA ego network, this is the step I want to parallelize
## plus adding a t-test to extract significance from zero:
### Cross map matrix: following Hao Ye tutorial
ncol <- dim(df2)[2]-1
M_rho <- array(NA,dim=c(ncol,ncol),
  dimnames=list(colnames(df2[2:17]),colnames(df2[2:17])))

for (i in 1:ncol){
    for (j in 1:ncol){
        if (i!=j){
            out_temp <- ccm(df2,E=1,lib_column=1+i,target_column=1+j,
                            lib_sizes = dim(df2)[1],replace=FALSE, silent = TRUE)
            M_rho[i,j] <- out_temp$rho
            }
        }
    }
dfM_rho <- as_tibble(M_rho)
dfM_rho$i <- colnames(dfM_rho)
dfM_rho <- dfM_rho %>%
  select(17, 1:16) %>%
  gather(key = "j", value = "rho", 2:17)

ggplot(dfM_rho, aes(i,j, fill = rho)) +
  geom_raster() +
  scale_fill_gradient2(
      low = scales::muted("red"), mid = "white",
      high = scales::muted("blue"), midpoint = 0, space = "Lab",
      na.value = "grey50", guide = "colourbar") + # name = expression(rho)
  xlab("") + ylab("") + theme_light(base_size=6) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ))

##### Here is my attempt:

ind <- crossing(lib_column = colnames(df2)[-1], target_column = colnames(df2)[-1])
ind <- ind %>% filter(lib_column != target_column)

rho_list <- map2(
    .x = ind$lib_column, .y = ind$target_column ,
    .f = ~ ccm(block = df2, E = 1,
        lib_column = .x, target_column = .y,
        lib_sizes = seq(3,29, by =2), replace = FALSE, silent = TRUE,
        random_libs = FALSE)
    )

## t-test for each relationship:
t_tests <- map(.x = rho_list, safely(
    .f = ~ t.test(.x$rho, alternative = "greater", mu = 0, na.action = na.exclude))
)

### J180911: Note that there is some rhos that are essentially constant, therefore the 
# t.test results in an error. For these cases, errors are reported but not p-values, and when
# I extract p-values, errors are dismissed making the vector shorter and loosing the index position
# of the significant p-values.

# p_vals <- map(t_tests, function(x) x$result$p.value)
t_tests <- transpose(t_tests)
fail <- t_tests$error %>% map_lgl(is_null) %>% unlist()


## So, one can extract only the siginificant rhos by means of
signif <-  t_tests %>%
    transpose() %>% 
    map(~ .x$result[["p.value"]] ) %>%
    map(~ .x < 0.05) 

rho_df <- rho_list[which(signif == TRUE)] %>% 
    bind_rows() %>% 



#### For the complete networks:
df4 <- dat %>%
  filter(log_weight > 1) %>%
  select(Period, Exporter = Exporter.ISO3c, Importer = Importer.ISO3c,  log_weight) %>%
  unite(link, Exporter, Importer, remove = TRUE) %>%
  left_join(time_points) %>%
  group_by(link) %>%
  mutate(norm_weight = (log_weight - mean(log_weight)) / sd(log_weight),
    n_obs = n()) %>%
  filter(n_obs > 20) # at least 2/3 of time series observed.


#### transform to data frame suitable for ccm.
df4 <- filter(df4, time < 30) %>%
    select(time, link, norm_weight) %>%
    spread(link, norm_weight) %>%
    as.data.frame()

## embedding
emb <- list()
for (i in 2:dim(df4)[2]){
    emb[[i-1]] <- df4[c(1,i)] %>%
      simplex()
    emb[[i-1]]$link <- colnames(df4)[i]
}

# best embedding dimension:
bestE <- emb %>% map_dbl(~ .$E[which.max(.$rho)])
bestE_df <- data_frame(
    bestE = bestE,
    link = colnames(df4)[-1]
)


## Calculate the rho matrix
ind <- crossing(lib_column = colnames(df4)[-1], target_column = colnames(df4)[-1])
ind <- ind %>% filter(lib_column != target_column)

# to change the embedding dimension to the best embedding, I need to use pmap instead of map2.
# Then the arguments are a list of the same lenght, which means I need to left join
# the ind dataframe with the embedding object matching by lib_column!
ind <- ind %>% left_join(., bestE_df, by = c('lib_column' = 'link'))
ind <- ind %>% rename(E = bestE)

param <- rlang::as_list(ind)

## Now rho is calculated with pmap: takes about 30'
rho_list <- pmap(
    param,
    .f = ~ ccm(block = df4,
        lib_sizes = seq(3,29, by =2), replace = FALSE, silent = TRUE,
        random_libs = FALSE)
    )

init <- 2220
end <- Sys.time()

## Object size is 9Gb, length is ~280k dataframes!
## Here I saved the object that I can use to calculate the t-tests
# save(rho_list,
#     file = "~/Documents/Projects/Salmon_Jessica/data/salmon_rho_list.Rdata")
# T-tests again for significance
# t_tests <- map(.x = rho_list, safely(
#     .f = ~ t.test(.x$rho, alternative = "greater", mu = 0, na.action = na.exclude))
# )
#
# t_tests <- transpose(t_tests)
# fail <- t_tests$error %>% map_lgl(is_null) %>% unlist()
# sum(!fail) #no errors
# save(t_tests,
#     file = "~/Documents/Projects/Salmon_Jessica/data/salmon_t_tests_rho.Rdata")

## Since both rho_list and t_tests are saved on disk, here you can load them again:
# load("~/Documents/Projects/Salmon_Jessica/data/salmon_rho_list.Rdata") # long rho list with ~28k dataframes
# load("~/Documents/Projects/Salmon_Jessica/data/salmon_t_tests_rho.Rdata") # list with ~28k t-tests
# load("~/Documents/Projects/Salmon_Jessica/data/salmon_short_rho.Rdata")



# rm(rho_list)

# this rho list extract a unique value of rho. Does not work for t-tests but just
# want to compare the value calculated from ccm originally with the t-test mean.
# rho_list_short <- pmap(
#     param,
#     .f = ~ ccm(block = df4,
#         lib_sizes = dim(df4)[1], replace = FALSE, silent = TRUE,
#         random_libs = FALSE)
#     )

# save(rho_list_short,
#     file = "~/Documents/Projects/Salmon_Jessica/data/salmon_short_rho.Rdata")

## Now I can just attach rho from short list to the index dataframe ind:
ind$rho <- rho_list_short %>% bind_rows() %>% pull(rho)

## for comparison, I can also extract the mean rho value and the t-value as test
# of significance from zeroes
ind$p_value <- t_tests$result %>% map_dbl(~.$p.value)
ind$avg_rho <- t_tests$result %>% map_dbl(~.$estimate)

### J180727: fucking pointless... the rho  and p-values are pretty much constant.
# The avg_rho from the t-statistic is different from the raw one obtained by the
# algorithm on the second run. I don't know what to do.

### J180904: I found a bug, I was using before df2 instead of df4. However the probelm persist, rho is constant, p_value is constant and I don't know why.
