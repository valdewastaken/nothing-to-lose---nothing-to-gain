####packages####

devtools::install_github("vdeminstitute/vdemdata")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(vdemdata)
library(countrycode)
library(readxl)
library(readstata13)

####V-dem data####

vdem_my <- dplyr::filter(vdemfull, vdemfull$year >= 1960)
rm(vdemfull)

vdem_short <- dplyr::select(vdem_my, country_name, year, e_autoc, e_democ, e_p_polity, e_polity2, 
                            v2regimpgroup, v2pepwrgeo, v2clgeocl, v2peapsgeo,
                            v2regsupgroupssize, v2peapspol, v2peapssoc, v2xeg_eqdr,
                            v3regsuploc, v3eltrnout, v2x_regime, v2x_regime_amb,
                            e_fh_status, e_fh_cl, e_fh_pr, e_fh_rol, e_coups, e_pt_coup,
                            v2regendtype, v2x_partipdem, v2x_partip, v2x_cspart)
rm(vdem_my)

vdem_short$c_code <- countrycode(vdem_short$country_name, "country.name", "iso3c",
                                 warn = TRUE, nomatch = NULL)
names(vdem_short)[1] <- "c_name"
saveRDS(vdem_short, "data/vdem_short.RDS")

vdem_short <- readRDS("data/vdem_short.RDS")
vdem_short <- dplyr::filter(vdem_short, c_code != "PSE")
vdem_short$c_code <- gsub("Kosovo", "RKS", vdem_short$c_code)

####wb poverty data####

p19 <- read.csv2("data/poverty_wb/1point9.csv", encoding = "UTF-8", sep = ",")
p19 <- gather(p19, year, poverty19, 5:64)
p19$year <- gsub("X", "", p19$year)
p19$poverty19 <- na_if(p19$poverty19, "")
p19$poverty19 <- as.numeric(p19$poverty)

hist(log(p19$poverty19+1))

names(p19)[1] <- "c_name"

p19 <- dplyr::filter(p19, p19$c_name != 'Central Europe and the Baltics' &
                       p19$c_name != 'Caribbean small states' & 
                       p19$c_name != 'East Asia & Pacific (excluding high income)' &
                       p19$c_name != 'Early-demographic dividend' &
                       p19$c_name != 'East Asia & Pacific' &
                       p19$c_name != 'Europe & Central Asia (excluding high income)' &
                       p19$c_name != 'Europe & Central Asia' & 
                       p19$c_name != 'European Union' & 
                       p19$c_name != 'Fragile and conflict affected situations' &
                       p19$c_name != 'High income' &
                       p19$c_name != 'Heavily indebted poor countries (HIPC)' &
                       p19$c_name != 'IBRD only' &
                       p19$c_name != 'IDA & IBRD total' &
                       p19$c_name != 'IDA total' &
                       p19$c_name != 'IDA blend' &
                       p19$c_name != 'IDA only' &
                       p19$c_name != 'Not classified' &
                       p19$c_name != 'Latin America & Caribbean (excluding high income)' &
                       p19$c_name != 'Latin America & Caribbean' &
                       p19$c_name != 'Least developed countries: UN classification' &
                       p19$c_name != 'Low income' &
                       p19$c_name != 'Lower middle income' &
                       p19$c_name != 'Low & middle income' &
                       p19$c_name != 'Late-demographic dividend' &
                       p19$c_name != 'Middle East & North Africa' &
                       p19$c_name != 'Middle East & North Africa (excluding high income)' &
                       p19$c_name != 'OECD members' &
                       p19$c_name != 'Pre-demographic dividend' &
                       p19$c_name != 'Pacific island small states' &
                       p19$c_name != 'Post-demographic dividend' &
                       p19$c_name != 'Sub-Saharan Africa (excluding high income)' &
                       p19$c_name != 'Sub-Saharan Africa' &
                       p19$c_name != 'Small states' &
                       p19$c_name != 'East Asia & Pacific (IDA & IBRD countries)' &
                       p19$c_name != 'Europe & Central Asia (IDA & IBRD countries)' &
                       p19$c_name != 'Latin America & the Caribbean (IDA & IBRD countries)' &
                       p19$c_name != 'Middle East & North Africa (IDA & IBRD countries)' &
                       p19$c_name != 'South Asia (IDA & IBRD)' &
                       p19$c_name != 'Sub-Saharan Africa (IDA & IBRD countries)' &
                       p19$c_name != 'Upper middle income' &
                       p19$c_name != 'World' &
                       p19$c_name != 'Arab World' &
                       p19$c_name != 'Euro area' &
                       p19$c_name != 'Middle income' &
                       p19$c_name != 'North America' &
                       p19$c_name != 'Other small states' &
                       p19$c_name != 'South Asia' &
                       p19$c_name != 'Channel Islands')


p19$c_code <- countrycode(p19[,1], "country.name", "iso3c", warn = TRUE, nomatch = NULL)
p19$c_code <- gsub("Kosovo", "RKS", p19$c_code)

#p19$check <- p19$c_code %in% p19$Country.Code
#unique(p19$check)

p19 <- dplyr::select(p19, c_name, year, c_code, poverty19)

###

pg19 <- read.csv2("data/poverty_wb/1pg9.csv", encoding = "UTF-8", sep = ",")
pg19 <- gather(pg19, year, poverty_gap_19, 5:64)
pg19$year <- gsub("X", "", pg19$year)
pg19$poverty19 <- na_if(pg19$poverty_gap_19, "")
pg19$poverty_gap_19 <- as.numeric(pg19$poverty_gap_19)

hist(log(pg19$poverty_gap_19+1))

names(pg19)[1] <- "c_name"

pg19 <- dplyr::filter(pg19, pg19$c_name != 'Central Europe and the Baltics' &
                        pg19$c_name != 'Caribbean small states' & 
                        pg19$c_name != 'East Asia & Pacific (excluding high income)' &
                        pg19$c_name != 'Early-demographic dividend' &
                        pg19$c_name != 'East Asia & Pacific' &
                        pg19$c_name != 'Europe & Central Asia (excluding high income)' &
                        pg19$c_name != 'Europe & Central Asia' & 
                        pg19$c_name != 'European Union' & 
                        pg19$c_name != 'Fragile and conflict affected situations' &
                        pg19$c_name != 'High income' &
                        pg19$c_name != 'Heavily indebted poor countries (HIPC)' &
                        pg19$c_name != 'IBRD only' &
                        pg19$c_name != 'IDA & IBRD total' &
                        pg19$c_name != 'IDA total' &
                        pg19$c_name != 'IDA blend' &
                        pg19$c_name != 'IDA only' &
                        pg19$c_name != 'Not classified' &
                        pg19$c_name != 'Latin America & Caribbean (excluding high income)' &
                        pg19$c_name != 'Latin America & Caribbean' &
                        pg19$c_name != 'Least developed countries: UN classification' &
                        pg19$c_name != 'Low income' &
                        pg19$c_name != 'Lower middle income' &
                        pg19$c_name != 'Low & middle income' &
                        pg19$c_name != 'Late-demographic dividend' &
                        pg19$c_name != 'Middle East & North Africa' &
                        pg19$c_name != 'Middle East & North Africa (excluding high income)' &
                        pg19$c_name != 'OECD members' &
                        pg19$c_name != 'Pre-demographic dividend' &
                        pg19$c_name != 'Pacific island small states' &
                        pg19$c_name != 'Post-demographic dividend' &
                        pg19$c_name != 'Sub-Saharan Africa (excluding high income)' &
                        pg19$c_name != 'Sub-Saharan Africa' &
                        pg19$c_name != 'Small states' &
                        pg19$c_name != 'East Asia & Pacific (IDA & IBRD countries)' &
                        pg19$c_name != 'Europe & Central Asia (IDA & IBRD countries)' &
                        pg19$c_name != 'Latin America & the Caribbean (IDA & IBRD countries)' &
                        pg19$c_name != 'Middle East & North Africa (IDA & IBRD countries)' &
                        pg19$c_name != 'South Asia (IDA & IBRD)' &
                        pg19$c_name != 'Sub-Saharan Africa (IDA & IBRD countries)' &
                        pg19$c_name != 'Upper middle income' &
                        pg19$c_name != 'World' &
                        pg19$c_name != 'Arab World' &
                        pg19$c_name != 'Euro area' &
                        pg19$c_name != 'Middle income' &
                        pg19$c_name != 'North America' &
                        pg19$c_name != 'Other small states' &
                        pg19$c_name != 'South Asia' &
                        pg19$c_name != 'Channel Islands')

pg19$c_code <- countrycode(pg19[,1], "country.name", "iso3c", warn = TRUE, nomatch = NULL)
pg19$c_code <- gsub("Kosovo", "RKS", pg19$c_code)

#pg19$check <- pg19$c_code %in% pg19$Country.Code
#unique(pg19$check)

pg19 <- dplyr::select(pg19, c_name, year, c_code, poverty_gap_19)
unique(pg19$year)


pg32 <- read.csv2("data/poverty_wb/3pg2.csv", encoding = "UTF-8", sep = ",")
pg32 <- gather(pg32, year, poverty_gap_32, 5:64)
pg32$year <- gsub("X", "", pg32$year)
pg32$poverty19 <- na_if(pg32$poverty_gap_32, "")
pg32$poverty_gap_32 <- as.numeric(pg32$poverty_gap_32)

hist(log(pg32$poverty_gap_32))

names(pg32)[1] <- "c_name"

pg32 <- dplyr::filter(pg32, pg32$c_name != 'Central Europe and the Baltics' &
                        pg32$c_name != 'Caribbean small states' & 
                        pg32$c_name != 'East Asia & Pacific (excluding high income)' &
                        pg32$c_name != 'Early-demographic dividend' &
                        pg32$c_name != 'East Asia & Pacific' &
                        pg32$c_name != 'Europe & Central Asia (excluding high income)' &
                        pg32$c_name != 'Europe & Central Asia' & 
                        pg32$c_name != 'European Union' & 
                        pg32$c_name != 'Fragile and conflict affected situations' &
                        pg32$c_name != 'High income' &
                        pg32$c_name != 'Heavily indebted poor countries (HIPC)' &
                        pg32$c_name != 'IBRD only' &
                        pg32$c_name != 'IDA & IBRD total' &
                        pg32$c_name != 'IDA total' &
                        pg32$c_name != 'IDA blend' &
                        pg32$c_name != 'IDA only' &
                        pg32$c_name != 'Not classified' &
                        pg32$c_name != 'Latin America & Caribbean (excluding high income)' &
                        pg32$c_name != 'Latin America & Caribbean' &
                        pg32$c_name != 'Least developed countries: UN classification' &
                        pg32$c_name != 'Low income' &
                        pg32$c_name != 'Lower middle income' &
                        pg32$c_name != 'Low & middle income' &
                        pg32$c_name != 'Late-demographic dividend' &
                        pg32$c_name != 'Middle East & North Africa' &
                        pg32$c_name != 'Middle East & North Africa (excluding high income)' &
                        pg32$c_name != 'OECD members' &
                        pg32$c_name != 'Pre-demographic dividend' &
                        pg32$c_name != 'Pacific island small states' &
                        pg32$c_name != 'Post-demographic dividend' &
                        pg32$c_name != 'Sub-Saharan Africa (excluding high income)' &
                        pg32$c_name != 'Sub-Saharan Africa' &
                        pg32$c_name != 'Small states' &
                        pg32$c_name != 'East Asia & Pacific (IDA & IBRD countries)' &
                        pg32$c_name != 'Europe & Central Asia (IDA & IBRD countries)' &
                        pg32$c_name != 'Latin America & the Caribbean (IDA & IBRD countries)' &
                        pg32$c_name != 'Middle East & North Africa (IDA & IBRD countries)' &
                        pg32$c_name != 'South Asia (IDA & IBRD)' &
                        pg32$c_name != 'Sub-Saharan Africa (IDA & IBRD countries)' &
                        pg32$c_name != 'Upper middle income' &
                        pg32$c_name != 'World' &
                        pg32$c_name != 'Arab World' &
                        pg32$c_name != 'Euro area' &
                        pg32$c_name != 'Middle income' &
                        pg32$c_name != 'North America' &
                        pg32$c_name != 'Other small states' &
                        pg32$c_name != 'South Asia' &
                        pg32$c_name != 'Channel Islands')

pg32$c_code <- countrycode(pg32[,1], "country.name", "iso3c", warn = TRUE, nomatch = NULL)
pg32$c_code <- gsub("Kosovo", "RKS", pg32$c_code)

#pg32$check <- pg32$c_code %in% pg32$Country.Code
#unique(pg32$check)

pg32 <- dplyr::select(pg32, c_name, year, c_code, poverty_gap_32)

p32 <- read.csv2("data/poverty_wb/3point2.csv", encoding = "UTF-8", sep = ",")
p32 <- gather(p32, year, poverty32, 5:64)
p32$year <- gsub("X", "", p32$year)
p32$poverty32 <- na_if(p32$poverty32, "")
p32$poverty32 <- as.numeric(p32$poverty)

hist(log(p32$poverty32))

names(p32)[1] <- "c_name"

p32 <- dplyr::filter(p32, p32$c_name != 'Central Europe and the Baltics' &
                       p32$c_name != 'Caribbean small states' & 
                       p32$c_name != 'East Asia & Pacific (excluding high income)' &
                       p32$c_name != 'Early-demographic dividend' &
                       p32$c_name != 'East Asia & Pacific' &
                       p32$c_name != 'Europe & Central Asia (excluding high income)' &
                       p32$c_name != 'Europe & Central Asia' & 
                       p32$c_name != 'European Union' & 
                       p32$c_name != 'Fragile and conflict affected situations' &
                       p32$c_name != 'High income' &
                       p32$c_name != 'Heavily indebted poor countries (HIPC)' &
                       p32$c_name != 'IBRD only' &
                       p32$c_name != 'IDA & IBRD total' &
                       p32$c_name != 'IDA total' &
                       p32$c_name != 'IDA blend' &
                       p32$c_name != 'IDA only' &
                       p32$c_name != 'Not classified' &
                       p32$c_name != 'Latin America & Caribbean (excluding high income)' &
                       p32$c_name != 'Latin America & Caribbean' &
                       p32$c_name != 'Least developed countries: UN classification' &
                       p32$c_name != 'Low income' &
                       p32$c_name != 'Lower middle income' &
                       p32$c_name != 'Low & middle income' &
                       p32$c_name != 'Late-demographic dividend' &
                       p32$c_name != 'Middle East & North Africa' &
                       p32$c_name != 'Middle East & North Africa (excluding high income)' &
                       p32$c_name != 'OECD members' &
                       p32$c_name != 'Pre-demographic dividend' &
                       p32$c_name != 'Pacific island small states' &
                       p32$c_name != 'Post-demographic dividend' &
                       p32$c_name != 'Sub-Saharan Africa (excluding high income)' &
                       p32$c_name != 'Sub-Saharan Africa' &
                       p32$c_name != 'Small states' &
                       p32$c_name != 'East Asia & Pacific (IDA & IBRD countries)' &
                       p32$c_name != 'Europe & Central Asia (IDA & IBRD countries)' &
                       p32$c_name != 'Latin America & the Caribbean (IDA & IBRD countries)' &
                       p32$c_name != 'Middle East & North Africa (IDA & IBRD countries)' &
                       p32$c_name != 'South Asia (IDA & IBRD)' &
                       p32$c_name != 'Sub-Saharan Africa (IDA & IBRD countries)' &
                       p32$c_name != 'Upper middle income' &
                       p32$c_name != 'World' &
                       p32$c_name != 'Arab World' &
                       p32$c_name != 'Euro area' &
                       p32$c_name != 'Middle income' &
                       p32$c_name != 'North America' &
                       p32$c_name != 'Other small states' &
                       p32$c_name != 'South Asia' &
                       p32$c_name != 'Channel Islands')


p32$c_code <- countrycode(p32[,1], "country.name", "iso3c", warn = TRUE, nomatch = NULL)
p32$c_code <- gsub("Kosovo", "RKS", p32$c_code)

#p32$check <- p32$c_code %in% p32$Country.Code
#unique(p32$check)

p32 <- dplyr::select(p32, c_name, year, c_code, poverty32)

p_nat <- read.csv2("data/poverty_wb/p_national.csv", encoding = "UTF-8", sep = ",")
p_nat <- gather(p_nat, year, poverty_nat, 5:64)
p_nat$year <- gsub("X", "", p_nat$year)
p_nat$poverty_nat <- na_if(p_nat$poverty_nat, "")
p_nat$poverty_nat <- as.numeric(p_nat$poverty)

hist(log(p_nat$poverty_nat))

names(p_nat)[1] <- "c_name"

p_nat <- dplyr::filter(p_nat, p_nat$c_name != 'Central Europe and the Baltics' &
                         p_nat$c_name != 'Caribbean small states' & 
                         p_nat$c_name != 'East Asia & Pacific (excluding high income)' &
                         p_nat$c_name != 'Early-demographic dividend' &
                         p_nat$c_name != 'East Asia & Pacific' &
                         p_nat$c_name != 'Europe & Central Asia (excluding high income)' &
                         p_nat$c_name != 'Europe & Central Asia' & 
                         p_nat$c_name != 'European Union' & 
                         p_nat$c_name != 'Fragile and conflict affected situations' &
                         p_nat$c_name != 'High income' &
                         p_nat$c_name != 'Heavily indebted poor countries (HIPC)' &
                         p_nat$c_name != 'IBRD only' &
                         p_nat$c_name != 'IDA & IBRD total' &
                         p_nat$c_name != 'IDA total' &
                         p_nat$c_name != 'IDA blend' &
                         p_nat$c_name != 'IDA only' &
                         p_nat$c_name != 'Not classified' &
                         p_nat$c_name != 'Latin America & Caribbean (excluding high income)' &
                         p_nat$c_name != 'Latin America & Caribbean' &
                         p_nat$c_name != 'Least developed countries: UN classification' &
                         p_nat$c_name != 'Low income' &
                         p_nat$c_name != 'Lower middle income' &
                         p_nat$c_name != 'Low & middle income' &
                         p_nat$c_name != 'Late-demographic dividend' &
                         p_nat$c_name != 'Middle East & North Africa' &
                         p_nat$c_name != 'Middle East & North Africa (excluding high income)' &
                         p_nat$c_name != 'OECD members' &
                         p_nat$c_name != 'Pre-demographic dividend' &
                         p_nat$c_name != 'Pacific island small states' &
                         p_nat$c_name != 'Post-demographic dividend' &
                         p_nat$c_name != 'Sub-Saharan Africa (excluding high income)' &
                         p_nat$c_name != 'Sub-Saharan Africa' &
                         p_nat$c_name != 'Small states' &
                         p_nat$c_name != 'East Asia & Pacific (IDA & IBRD countries)' &
                         p_nat$c_name != 'Europe & Central Asia (IDA & IBRD countries)' &
                         p_nat$c_name != 'Latin America & the Caribbean (IDA & IBRD countries)' &
                         p_nat$c_name != 'Middle East & North Africa (IDA & IBRD countries)' &
                         p_nat$c_name != 'South Asia (IDA & IBRD)' &
                         p_nat$c_name != 'Sub-Saharan Africa (IDA & IBRD countries)' &
                         p_nat$c_name != 'Upper middle income' &
                         p_nat$c_name != 'World' &
                         p_nat$c_name != 'Arab World' &
                         p_nat$c_name != 'Euro area' &
                         p_nat$c_name != 'Middle income' &
                         p_nat$c_name != 'North America' &
                         p_nat$c_name != 'Other small states' &
                         p_nat$c_name != 'South Asia' &
                         p_nat$c_name != 'Channel Islands')


p_nat$c_code <- countrycode(p_nat[,1], "country.name", "iso3c", warn = TRUE, nomatch = NULL)
p_nat$c_code <- gsub("Kosovo", "RKS", p_nat$c_code)

#p_nat$check <- p_nat$c_code %in% p_nat$Country.Code
#unique(p_nat$check)

p_nat <- dplyr::select(p_nat, c_name, year, c_code, poverty_nat)

p55 <- read.csv2("data/poverty_wb/5point5.csv", encoding = "UTF-8", sep = ",")
p55 <- gather(p55, year, poverty55, 5:64)
p55$year <- gsub("X", "", p55$year)
p55$poverty55 <- na_if(p55$poverty55, "")
p55$poverty55 <- as.numeric(p55$poverty)

hist(log(p55$poverty55))

names(p55)[1] <- "c_name"

p55 <- dplyr::filter(p55, p55$c_name != 'Central Europe and the Baltics' &
                       p55$c_name != 'Caribbean small states' & 
                       p55$c_name != 'East Asia & Pacific (excluding high income)' &
                       p55$c_name != 'Early-demographic dividend' &
                       p55$c_name != 'East Asia & Pacific' &
                       p55$c_name != 'Europe & Central Asia (excluding high income)' &
                       p55$c_name != 'Europe & Central Asia' & 
                       p55$c_name != 'European Union' & 
                       p55$c_name != 'Fragile and conflict affected situations' &
                       p55$c_name != 'High income' &
                       p55$c_name != 'Heavily indebted poor countries (HIPC)' &
                       p55$c_name != 'IBRD only' &
                       p55$c_name != 'IDA & IBRD total' &
                       p55$c_name != 'IDA total' &
                       p55$c_name != 'IDA blend' &
                       p55$c_name != 'IDA only' &
                       p55$c_name != 'Not classified' &
                       p55$c_name != 'Latin America & Caribbean (excluding high income)' &
                       p55$c_name != 'Latin America & Caribbean' &
                       p55$c_name != 'Least developed countries: UN classification' &
                       p55$c_name != 'Low income' &
                       p55$c_name != 'Lower middle income' &
                       p55$c_name != 'Low & middle income' &
                       p55$c_name != 'Late-demographic dividend' &
                       p55$c_name != 'Middle East & North Africa' &
                       p55$c_name != 'Middle East & North Africa (excluding high income)' &
                       p55$c_name != 'OECD members' &
                       p55$c_name != 'Pre-demographic dividend' &
                       p55$c_name != 'Pacific island small states' &
                       p55$c_name != 'Post-demographic dividend' &
                       p55$c_name != 'Sub-Saharan Africa (excluding high income)' &
                       p55$c_name != 'Sub-Saharan Africa' &
                       p55$c_name != 'Small states' &
                       p55$c_name != 'East Asia & Pacific (IDA & IBRD countries)' &
                       p55$c_name != 'Europe & Central Asia (IDA & IBRD countries)' &
                       p55$c_name != 'Latin America & the Caribbean (IDA & IBRD countries)' &
                       p55$c_name != 'Middle East & North Africa (IDA & IBRD countries)' &
                       p55$c_name != 'South Asia (IDA & IBRD)' &
                       p55$c_name != 'Sub-Saharan Africa (IDA & IBRD countries)' &
                       p55$c_name != 'Upper middle income' &
                       p55$c_name != 'World' &
                       p55$c_name != 'Arab World' &
                       p55$c_name != 'Euro area' &
                       p55$c_name != 'Middle income' &
                       p55$c_name != 'North America' &
                       p55$c_name != 'Other small states' &
                       p55$c_name != 'South Asia' &
                       p55$c_name != 'Channel Islands')


p55$c_code <- countrycode(p55[,1], "country.name", "iso3c", warn = TRUE, nomatch = NULL)
p55$c_code <- gsub("Kosovo", "RKS", p55$c_code)

#p55$check <- p55$c_code %in% p55$Country.Code
#unique(p55$check)

p55 <- dplyr::select(p55, c_name, year, c_code, poverty55)

pg55 <- read.csv2("data/poverty_wb/5pg5.csv", encoding = "UTF-8", sep = ",")
pg55 <- gather(pg55, year, poverty_gap_55, 5:64)
pg55$year <- gsub("X", "", pg55$year)
pg55$poverty19 <- na_if(pg55$poverty_gap_55, "")
pg55$poverty_gap_55 <- as.numeric(pg55$poverty_gap_55)

hist(log(pg55$poverty_gap_55))

names(pg55)[1] <- "c_name"

pg55 <- dplyr::filter(pg55, pg55$c_name != 'Central Europe and the Baltics' &
                        pg55$c_name != 'Caribbean small states' & 
                        pg55$c_name != 'East Asia & Pacific (excluding high income)' &
                        pg55$c_name != 'Early-demographic dividend' &
                        pg55$c_name != 'East Asia & Pacific' &
                        pg55$c_name != 'Europe & Central Asia (excluding high income)' &
                        pg55$c_name != 'Europe & Central Asia' & 
                        pg55$c_name != 'European Union' & 
                        pg55$c_name != 'Fragile and conflict affected situations' &
                        pg55$c_name != 'High income' &
                        pg55$c_name != 'Heavily indebted poor countries (HIPC)' &
                        pg55$c_name != 'IBRD only' &
                        pg55$c_name != 'IDA & IBRD total' &
                        pg55$c_name != 'IDA total' &
                        pg55$c_name != 'IDA blend' &
                        pg55$c_name != 'IDA only' &
                        pg55$c_name != 'Not classified' &
                        pg55$c_name != 'Latin America & Caribbean (excluding high income)' &
                        pg55$c_name != 'Latin America & Caribbean' &
                        pg55$c_name != 'Least developed countries: UN classification' &
                        pg55$c_name != 'Low income' &
                        pg55$c_name != 'Lower middle income' &
                        pg55$c_name != 'Low & middle income' &
                        pg55$c_name != 'Late-demographic dividend' &
                        pg55$c_name != 'Middle East & North Africa' &
                        pg55$c_name != 'Middle East & North Africa (excluding high income)' &
                        pg55$c_name != 'OECD members' &
                        pg55$c_name != 'Pre-demographic dividend' &
                        pg55$c_name != 'Pacific island small states' &
                        pg55$c_name != 'Post-demographic dividend' &
                        pg55$c_name != 'Sub-Saharan Africa (excluding high income)' &
                        pg55$c_name != 'Sub-Saharan Africa' &
                        pg55$c_name != 'Small states' &
                        pg55$c_name != 'East Asia & Pacific (IDA & IBRD countries)' &
                        pg55$c_name != 'Europe & Central Asia (IDA & IBRD countries)' &
                        pg55$c_name != 'Latin America & the Caribbean (IDA & IBRD countries)' &
                        pg55$c_name != 'Middle East & North Africa (IDA & IBRD countries)' &
                        pg55$c_name != 'South Asia (IDA & IBRD)' &
                        pg55$c_name != 'Sub-Saharan Africa (IDA & IBRD countries)' &
                        pg55$c_name != 'Upper middle income' &
                        pg55$c_name != 'World' &
                        pg55$c_name != 'Arab World' &
                        pg55$c_name != 'Euro area' &
                        pg55$c_name != 'Middle income' &
                        pg55$c_name != 'North America' &
                        pg55$c_name != 'Other small states' &
                        pg55$c_name != 'South Asia' &
                        pg55$c_name != 'Channel Islands')

pg55$c_code <- countrycode(pg55[,1], "country.name", "iso3c", warn = TRUE, nomatch = NULL)
pg55$c_code <- gsub("Kosovo", "RKS", pg55$c_code)

#pg55$check <- pg55$c_code %in% pg55$Country.Code
#unique(pg55$check)

pg55 <- dplyr::select(pg55, c_name, year, c_code, poverty_gap_55)

####WB GDP data####

gdp_pc <- read.csv2("data/gdp_wb/gdp_pc_ppp.csv", encoding = "UTF-8", sep = ",")
gdp_pc <- gather(gdp_pc, year, GDP_pc_ppp, 5:65)
gdp_pc$year <- gsub("X", "", gdp_pc$year)
gdp_pc$GDP_pc_ppp <- na_if(gdp_pc$GDP_pc_ppp, "")
gdp_pc$GDP_pc_ppp <- as.numeric(gdp_pc$GDP_pc_ppp)

hist(log(gdp_pc$GDP_pc_ppp))

names(gdp_pc)[1] <- "c_name"

gdp_pc <- dplyr::filter(gdp_pc, gdp_pc$c_name != 'Central Europe and the Baltics' &
                          gdp_pc$c_name != 'Caribbean small states' & 
                          gdp_pc$c_name != 'East Asia & Pacific (excluding high income)' &
                          gdp_pc$c_name != 'Early-demographic dividend' &
                          gdp_pc$c_name != 'East Asia & Pacific' &
                          gdp_pc$c_name != 'Europe & Central Asia (excluding high income)' &
                          gdp_pc$c_name != 'Europe & Central Asia' & 
                          gdp_pc$c_name != 'European Union' & 
                          gdp_pc$c_name != 'Fragile and conflict affected situations' &
                          gdp_pc$c_name != 'High income' &
                          gdp_pc$c_name != 'Heavily indebted poor countries (HIPC)' &
                          gdp_pc$c_name != 'IBRD only' &
                          gdp_pc$c_name != 'IDA & IBRD total' &
                          gdp_pc$c_name != 'IDA total' &
                          gdp_pc$c_name != 'IDA blend' &
                          gdp_pc$c_name != 'IDA only' &
                          gdp_pc$c_name != 'Not classified' &
                          gdp_pc$c_name != 'Latin America & Caribbean (excluding high income)' &
                          gdp_pc$c_name != 'Latin America & Caribbean' &
                          gdp_pc$c_name != 'Least developed countries: UN classification' &
                          gdp_pc$c_name != 'Low income' &
                          gdp_pc$c_name != 'Lower middle income' &
                          gdp_pc$c_name != 'Low & middle income' &
                          gdp_pc$c_name != 'Late-demographic dividend' &
                          gdp_pc$c_name != 'Middle East & North Africa' &
                          gdp_pc$c_name != 'Middle East & North Africa (excluding high income)' &
                          gdp_pc$c_name != 'OECD members' &
                          gdp_pc$c_name != 'Pre-demographic dividend' &
                          gdp_pc$c_name != 'Pacific island small states' &
                          gdp_pc$c_name != 'Post-demographic dividend' &
                          gdp_pc$c_name != 'Sub-Saharan Africa (excluding high income)' &
                          gdp_pc$c_name != 'Sub-Saharan Africa' &
                          gdp_pc$c_name != 'Small states' &
                          gdp_pc$c_name != 'East Asia & Pacific (IDA & IBRD countries)' &
                          gdp_pc$c_name != 'Europe & Central Asia (IDA & IBRD countries)' &
                          gdp_pc$c_name != 'Latin America & the Caribbean (IDA & IBRD countries)' &
                          gdp_pc$c_name != 'Middle East & North Africa (IDA & IBRD countries)' &
                          gdp_pc$c_name != 'South Asia (IDA & IBRD)' &
                          gdp_pc$c_name != 'Sub-Saharan Africa (IDA & IBRD countries)' &
                          gdp_pc$c_name != 'Upper middle income' &
                          gdp_pc$c_name != 'World' &
                          gdp_pc$c_name != 'Arab World' &
                          gdp_pc$c_name != 'Euro area' &
                          gdp_pc$c_name != 'Middle income' &
                          gdp_pc$c_name != 'North America' &
                          gdp_pc$c_name != 'Other small states' &
                          gdp_pc$c_name != 'South Asia' &
                          gdp_pc$c_name != 'Channel Islands')

gdp_pc$c_code <- countrycode(gdp_pc[,1], "country.name", "iso3c", warn = TRUE, nomatch = NULL)
gdp_pc$c_code <- gsub("Kosovo", "RKS", gdp_pc$c_code)

#gdp_pc$check <- gdp_pc$c_code %in% gdp_pc$Country.Code
#unique(gdp_pc$check)

gdp_pc <- dplyr::select(gdp_pc, c_name, year, c_code, GDP_pc_ppp)

gdp_growth <- read.csv2("data/gdp_wb/gdp_growth.csv", encoding = "UTF-8", sep = ",")
gdp_growth <- gather(gdp_growth, year, GDP_growth, 5:65)
gdp_growth$year <- gsub("X", "", gdp_growth$year)
gdp_growth$poverty19 <- na_if(gdp_growth$GDP_growth, "")
gdp_growth$GDP_growth <- as.numeric(gdp_growth$GDP_growth)

hist(log(gdp_growth$GDP_growth))

names(gdp_growth)[1] <- "c_name"

gdp_growth <- dplyr::filter(gdp_growth, gdp_growth$c_name != 'Central Europe and the Baltics' &
                              gdp_growth$c_name != 'Caribbean small states' & 
                              gdp_growth$c_name != 'East Asia & Pacific (excluding high income)' &
                              gdp_growth$c_name != 'Early-demographic dividend' &
                              gdp_growth$c_name != 'East Asia & Pacific' &
                              gdp_growth$c_name != 'Europe & Central Asia (excluding high income)' &
                              gdp_growth$c_name != 'Europe & Central Asia' & 
                              gdp_growth$c_name != 'European Union' & 
                              gdp_growth$c_name != 'Fragile and conflict affected situations' &
                              gdp_growth$c_name != 'High income' &
                              gdp_growth$c_name != 'Heavily indebted poor countries (HIPC)' &
                              gdp_growth$c_name != 'IBRD only' &
                              gdp_growth$c_name != 'IDA & IBRD total' &
                              gdp_growth$c_name != 'IDA total' &
                              gdp_growth$c_name != 'IDA blend' &
                              gdp_growth$c_name != 'IDA only' &
                              gdp_growth$c_name != 'Not classified' &
                              gdp_growth$c_name != 'Latin America & Caribbean (excluding high income)' &
                              gdp_growth$c_name != 'Latin America & Caribbean' &
                              gdp_growth$c_name != 'Least developed countries: UN classification' &
                              gdp_growth$c_name != 'Low income' &
                              gdp_growth$c_name != 'Lower middle income' &
                              gdp_growth$c_name != 'Low & middle income' &
                              gdp_growth$c_name != 'Late-demographic dividend' &
                              gdp_growth$c_name != 'Middle East & North Africa' &
                              gdp_growth$c_name != 'Middle East & North Africa (excluding high income)' &
                              gdp_growth$c_name != 'OECD members' &
                              gdp_growth$c_name != 'Pre-demographic dividend' &
                              gdp_growth$c_name != 'Pacific island small states' &
                              gdp_growth$c_name != 'Post-demographic dividend' &
                              gdp_growth$c_name != 'Sub-Saharan Africa (excluding high income)' &
                              gdp_growth$c_name != 'Sub-Saharan Africa' &
                              gdp_growth$c_name != 'Small states' &
                              gdp_growth$c_name != 'East Asia & Pacific (IDA & IBRD countries)' &
                              gdp_growth$c_name != 'Europe & Central Asia (IDA & IBRD countries)' &
                              gdp_growth$c_name != 'Latin America & the Caribbean (IDA & IBRD countries)' &
                              gdp_growth$c_name != 'Middle East & North Africa (IDA & IBRD countries)' &
                              gdp_growth$c_name != 'South Asia (IDA & IBRD)' &
                              gdp_growth$c_name != 'Sub-Saharan Africa (IDA & IBRD countries)' &
                              gdp_growth$c_name != 'Upper middle income' &
                              gdp_growth$c_name != 'World' &
                              gdp_growth$c_name != 'Arab World' &
                              gdp_growth$c_name != 'Euro area' &
                              gdp_growth$c_name != 'Middle income' &
                              gdp_growth$c_name != 'North America' &
                              gdp_growth$c_name != 'Other small states' &
                              gdp_growth$c_name != 'South Asia' &
                              gdp_growth$c_name != 'Channel Islands')

gdp_growth$c_code <- countrycode(gdp_growth[,1], "country.name", "iso3c", warn = TRUE, nomatch = NULL)
gdp_growth$c_code <- gsub("Kosovo", "RKS", gdp_growth$c_code)

#gdp_growth$check <- gdp_growth$c_code %in% gdp_growth$Country.Code
#unique(gdp_growth$check)

gdp_growth <- dplyr::select(gdp_growth, c_name, year, c_code, GDP_growth)

gdp_raw <- read.csv2("data/gdp_wb/gdp.csv", encoding = "UTF-8", sep = ",")
gdp_raw <- gather(gdp_raw, year, GDP, 5:65)
gdp_raw$year <- gsub("X", "", gdp_raw$year)
gdp_raw$GDP <- na_if(gdp_raw$GDP, "")
gdp_raw$GDP <- as.numeric(gdp_raw$GDP)

hist(log(gdp_raw$GDP))

names(gdp_raw)[1] <- "c_name"

gdp_raw <- dplyr::filter(gdp_raw, gdp_raw$c_name != 'Central Europe and the Baltics' &
                           gdp_raw$c_name != 'Caribbean small states' & 
                           gdp_raw$c_name != 'East Asia & Pacific (excluding high income)' &
                           gdp_raw$c_name != 'Early-demographic dividend' &
                           gdp_raw$c_name != 'East Asia & Pacific' &
                           gdp_raw$c_name != 'Europe & Central Asia (excluding high income)' &
                           gdp_raw$c_name != 'Europe & Central Asia' & 
                           gdp_raw$c_name != 'European Union' & 
                           gdp_raw$c_name != 'Fragile and conflict affected situations' &
                           gdp_raw$c_name != 'High income' &
                           gdp_raw$c_name != 'Heavily indebted poor countries (HIPC)' &
                           gdp_raw$c_name != 'IBRD only' &
                           gdp_raw$c_name != 'IDA & IBRD total' &
                           gdp_raw$c_name != 'IDA total' &
                           gdp_raw$c_name != 'IDA blend' &
                           gdp_raw$c_name != 'IDA only' &
                           gdp_raw$c_name != 'Not classified' &
                           gdp_raw$c_name != 'Latin America & Caribbean (excluding high income)' &
                           gdp_raw$c_name != 'Latin America & Caribbean' &
                           gdp_raw$c_name != 'Least developed countries: UN classification' &
                           gdp_raw$c_name != 'Low income' &
                           gdp_raw$c_name != 'Lower middle income' &
                           gdp_raw$c_name != 'Low & middle income' &
                           gdp_raw$c_name != 'Late-demographic dividend' &
                           gdp_raw$c_name != 'Middle East & North Africa' &
                           gdp_raw$c_name != 'Middle East & North Africa (excluding high income)' &
                           gdp_raw$c_name != 'OECD members' &
                           gdp_raw$c_name != 'Pre-demographic dividend' &
                           gdp_raw$c_name != 'Pacific island small states' &
                           gdp_raw$c_name != 'Post-demographic dividend' &
                           gdp_raw$c_name != 'Sub-Saharan Africa (excluding high income)' &
                           gdp_raw$c_name != 'Sub-Saharan Africa' &
                           gdp_raw$c_name != 'Small states' &
                           gdp_raw$c_name != 'East Asia & Pacific (IDA & IBRD countries)' &
                           gdp_raw$c_name != 'Europe & Central Asia (IDA & IBRD countries)' &
                           gdp_raw$c_name != 'Latin America & the Caribbean (IDA & IBRD countries)' &
                           gdp_raw$c_name != 'Middle East & North Africa (IDA & IBRD countries)' &
                           gdp_raw$c_name != 'South Asia (IDA & IBRD)' &
                           gdp_raw$c_name != 'Sub-Saharan Africa (IDA & IBRD countries)' &
                           gdp_raw$c_name != 'Upper middle income' &
                           gdp_raw$c_name != 'World' &
                           gdp_raw$c_name != 'Arab World' &
                           gdp_raw$c_name != 'Euro area' &
                           gdp_raw$c_name != 'Middle income' &
                           gdp_raw$c_name != 'North America' &
                           gdp_raw$c_name != 'Other small states' &
                           gdp_raw$c_name != 'South Asia' &
                           gdp_raw$c_name != 'Channel Islands')

gdp_raw$c_code <- countrycode(gdp_raw[,1], "country.name", "iso3c", warn = TRUE, nomatch = NULL)
gdp_raw$c_code <- gsub("Kosovo", "RKS", gdp_raw$c_code)

#gdp_raw$check <- gdp_raw$c_code %in% gdp_raw$Country.Code
#unique(gdp_raw$check)

gdp_raw <- dplyr::select(gdp_raw, c_name, year, c_code, GDP)

gdp_agriculture <- read.csv2("data/gdp_wb/agriculture_gdp.csv", encoding = "UTF-8", sep = ",")
gdp_agriculture <- gather(gdp_agriculture, year, agriculture_GDP, 5:65)
gdp_agriculture$year <- gsub("X", "", gdp_agriculture$year)
gdp_agriculture$agriculture_GDP <- na_if(gdp_agriculture$agriculture_GDP, "")
gdp_agriculture$agriculture_GDP <- as.numeric(gdp_agriculture$agriculture_GDP)

hist(log(gdp_agriculture$agriculture_GDP))

names(gdp_agriculture)[1] <- "c_name"

gdp_agriculture <- dplyr::filter(gdp_agriculture,
                                 gdp_agriculture$c_name != 'Central Europe and the Baltics' &
                                   gdp_agriculture$c_name != 'Caribbean small states' & 
                                   gdp_agriculture$c_name != 'East Asia & Pacific (excluding high income)' &
                                   gdp_agriculture$c_name != 'Early-demographic dividend' &
                                   gdp_agriculture$c_name != 'East Asia & Pacific' &
                                   gdp_agriculture$c_name != 'Europe & Central Asia (excluding high income)' &
                                   gdp_agriculture$c_name != 'Europe & Central Asia' & 
                                   gdp_agriculture$c_name != 'European Union' & 
                                   gdp_agriculture$c_name != 'Fragile and conflict affected situations' &
                                   gdp_agriculture$c_name != 'High income' &
                                   gdp_agriculture$c_name != 'Heavily indebted poor countries (HIPC)' &
                                   gdp_agriculture$c_name != 'IBRD only' &
                                   gdp_agriculture$c_name != 'IDA & IBRD total' &
                                   gdp_agriculture$c_name != 'IDA total' &
                                   gdp_agriculture$c_name != 'IDA blend' &
                                   gdp_agriculture$c_name != 'IDA only' &
                                   gdp_agriculture$c_name != 'Not classified' &
                                   gdp_agriculture$c_name != 'Latin America & Caribbean (excluding high income)' &
                                   gdp_agriculture$c_name != 'Latin America & Caribbean' &
                                   gdp_agriculture$c_name != 'Least developed countries: UN classification' &
                                   gdp_agriculture$c_name != 'Low income' &
                                   gdp_agriculture$c_name != 'Lower middle income' &
                                   gdp_agriculture$c_name != 'Low & middle income' &
                                   gdp_agriculture$c_name != 'Late-demographic dividend' &
                                   gdp_agriculture$c_name != 'Middle East & North Africa' &
                                   gdp_agriculture$c_name != 'Middle East & North Africa (excluding high income)' &
                                   gdp_agriculture$c_name != 'OECD members' &
                                   gdp_agriculture$c_name != 'Pre-demographic dividend' &
                                   gdp_agriculture$c_name != 'Pacific island small states' &
                                   gdp_agriculture$c_name != 'Post-demographic dividend' &
                                   gdp_agriculture$c_name != 'Sub-Saharan Africa (excluding high income)' &
                                   gdp_agriculture$c_name != 'Sub-Saharan Africa' &
                                   gdp_agriculture$c_name != 'Small states' &
                                   gdp_agriculture$c_name != 'East Asia & Pacific (IDA & IBRD countries)' &
                                   gdp_agriculture$c_name != 'Europe & Central Asia (IDA & IBRD countries)' &
                                   gdp_agriculture$c_name != 'Latin America & the Caribbean (IDA & IBRD countries)' &
                                   gdp_agriculture$c_name != 'Middle East & North Africa (IDA & IBRD countries)' &
                                   gdp_agriculture$c_name != 'South Asia (IDA & IBRD)' &
                                   gdp_agriculture$c_name != 'Sub-Saharan Africa (IDA & IBRD countries)' &
                                   gdp_agriculture$c_name != 'Upper middle income' &
                                   gdp_agriculture$c_name != 'World' &
                                   gdp_agriculture$c_name != 'Arab World' &
                                   gdp_agriculture$c_name != 'Euro area' &
                                   gdp_agriculture$c_name != 'Middle income' &
                                   gdp_agriculture$c_name != 'North America' &
                                   gdp_agriculture$c_name != 'Other small states' &
                                   gdp_agriculture$c_name != 'South Asia' &
                                   gdp_agriculture$c_name != 'Channel Islands')

gdp_agriculture$c_code <- countrycode(gdp_agriculture[,1], "country.name", "iso3c", warn = TRUE, nomatch = NULL)
gdp_agriculture$c_code <- gsub("Kosovo", "RKS", gdp_agriculture$c_code)

#gdp_agriculture$check <- gdp_agriculture$c_code %in% gdp_agriculture$Country.Code
#unique(gdp_agriculture$check)

gdp_agriculture <- dplyr::select(gdp_agriculture, c_name, year, c_code, agriculture_GDP)

emp_agr <- read.csv2("data/gdp_wb/agriculture_employment.csv", encoding = "UTF-8", sep = ",")
emp_agr <- gather(emp_agr, year, agriculture_employment, 5:65)
emp_agr$year <- gsub("X", "", emp_agr$year)
emp_agr$agriculture_employment <- na_if(emp_agr$agriculture_employment, "")
emp_agr$agriculture_employment <- as.numeric(emp_agr$agriculture_employment)

hist(log(emp_agr$agriculture_employment))

names(emp_agr)[1] <- "c_name"

emp_agr <- dplyr::filter(emp_agr, emp_agr$c_name != 'Central Europe and the Baltics' &
                           emp_agr$c_name != 'Caribbean small states' & 
                           emp_agr$c_name != 'East Asia & Pacific (excluding high income)' &
                           emp_agr$c_name != 'Early-demographic dividend' &
                           emp_agr$c_name != 'East Asia & Pacific' &
                           emp_agr$c_name != 'Europe & Central Asia (excluding high income)' &
                           emp_agr$c_name != 'Europe & Central Asia' & 
                           emp_agr$c_name != 'European Union' & 
                           emp_agr$c_name != 'Fragile and conflict affected situations' &
                           emp_agr$c_name != 'High income' &
                           emp_agr$c_name != 'Heavily indebted poor countries (HIPC)' &
                           emp_agr$c_name != 'IBRD only' &
                           emp_agr$c_name != 'IDA & IBRD total' &
                           emp_agr$c_name != 'IDA total' &
                           emp_agr$c_name != 'IDA blend' &
                           emp_agr$c_name != 'IDA only' &
                           emp_agr$c_name != 'Not classified' &
                           emp_agr$c_name != 'Latin America & Caribbean (excluding high income)' &
                           emp_agr$c_name != 'Latin America & Caribbean' &
                           emp_agr$c_name != 'Least developed countries: UN classification' &
                           emp_agr$c_name != 'Low income' &
                           emp_agr$c_name != 'Lower middle income' &
                           emp_agr$c_name != 'Low & middle income' &
                           emp_agr$c_name != 'Late-demographic dividend' &
                           emp_agr$c_name != 'Middle East & North Africa' &
                           emp_agr$c_name != 'Middle East & North Africa (excluding high income)' &
                           emp_agr$c_name != 'OECD members' &
                           emp_agr$c_name != 'Pre-demographic dividend' &
                           emp_agr$c_name != 'Pacific island small states' &
                           emp_agr$c_name != 'Post-demographic dividend' &
                           emp_agr$c_name != 'Sub-Saharan Africa (excluding high income)' &
                           emp_agr$c_name != 'Sub-Saharan Africa' &
                           emp_agr$c_name != 'Small states' &
                           emp_agr$c_name != 'East Asia & Pacific (IDA & IBRD countries)' &
                           emp_agr$c_name != 'Europe & Central Asia (IDA & IBRD countries)' &
                           emp_agr$c_name != 'Latin America & the Caribbean (IDA & IBRD countries)' &
                           emp_agr$c_name != 'Middle East & North Africa (IDA & IBRD countries)' &
                           emp_agr$c_name != 'South Asia (IDA & IBRD)' &
                           emp_agr$c_name != 'Sub-Saharan Africa (IDA & IBRD countries)' &
                           emp_agr$c_name != 'Upper middle income' &
                           emp_agr$c_name != 'World' &
                           emp_agr$c_name != 'Arab World' &
                           emp_agr$c_name != 'Euro area' &
                           emp_agr$c_name != 'Middle income' &
                           emp_agr$c_name != 'North America' &
                           emp_agr$c_name != 'Other small states' &
                           emp_agr$c_name != 'South Asia' &
                           emp_agr$c_name != 'Channel Islands')

emp_agr$c_code <- countrycode(emp_agr[,1], "country.name", "iso3c", warn = TRUE, nomatch = NULL)
emp_agr$c_code <- gsub("Kosovo", "RKS", emp_agr$c_code)

#emp_agr$check <- emp_agr$c_code %in% emp_agr$Country.Code
#unique(emp_agr$check)

emp_agr <- dplyr::select(emp_agr, c_name, year, c_code, agriculture_employment)

population <- read.csv2("data/gdp_wb/population.csv", encoding = "UTF-8", sep = ",")
population <- gather(population, year, population_WB, 5:65)
population$year <- gsub("X", "", population$year)
population$population_WB <- na_if(population$population_WB, "")
population$population_WB <- as.numeric(population$population_WB)

hist(log(population$population_WB))

names(population)[1] <- "c_name"

population <- dplyr::filter(population, population$c_name != 'Central Europe and the Baltics' &
                              population$c_name != 'Caribbean small states' & 
                              population$c_name != 'East Asia & Pacific (excluding high income)' &
                              population$c_name != 'Early-demographic dividend' &
                              population$c_name != 'East Asia & Pacific' &
                              population$c_name != 'Europe & Central Asia (excluding high income)' &
                              population$c_name != 'Europe & Central Asia' & 
                              population$c_name != 'European Union' & 
                              population$c_name != 'Fragile and conflict affected situations' &
                              population$c_name != 'High income' &
                              population$c_name != 'Heavily indebted poor countries (HIPC)' &
                              population$c_name != 'IBRD only' &
                              population$c_name != 'IDA & IBRD total' &
                              population$c_name != 'IDA total' &
                              population$c_name != 'IDA blend' &
                              population$c_name != 'IDA only' &
                              population$c_name != 'Not classified' &
                              population$c_name != 'Latin America & Caribbean (excluding high income)' &
                              population$c_name != 'Latin America & Caribbean' &
                              population$c_name != 'Least developed countries: UN classification' &
                              population$c_name != 'Low income' &
                              population$c_name != 'Lower middle income' &
                              population$c_name != 'Low & middle income' &
                              population$c_name != 'Late-demographic dividend' &
                              population$c_name != 'Middle East & North Africa' &
                              population$c_name != 'Middle East & North Africa (excluding high income)' &
                              population$c_name != 'OECD members' &
                              population$c_name != 'Pre-demographic dividend' &
                              population$c_name != 'Pacific island small states' &
                              population$c_name != 'Post-demographic dividend' &
                              population$c_name != 'Sub-Saharan Africa (excluding high income)' &
                              population$c_name != 'Sub-Saharan Africa' &
                              population$c_name != 'Small states' &
                              population$c_name != 'East Asia & Pacific (IDA & IBRD countries)' &
                              population$c_name != 'Europe & Central Asia (IDA & IBRD countries)' &
                              population$c_name != 'Latin America & the Caribbean (IDA & IBRD countries)' &
                              population$c_name != 'Middle East & North Africa (IDA & IBRD countries)' &
                              population$c_name != 'South Asia (IDA & IBRD)' &
                              population$c_name != 'Sub-Saharan Africa (IDA & IBRD countries)' &
                              population$c_name != 'Upper middle income' &
                              population$c_name != 'World' &
                              population$c_name != 'Arab World' &
                              population$c_name != 'Euro area' &
                              population$c_name != 'Middle income' &
                              population$c_name != 'North America' &
                              population$c_name != 'Other small states' &
                              population$c_name != 'South Asia' &
                              population$c_name != 'Channel Islands')

population$c_code <- countrycode(population[,1], "country.name", "iso3c", warn = TRUE, nomatch = NULL)
population$c_code <- gsub("Kosovo", "RKS", population$c_code)

#population$check <- population$c_code %in% population$Country.Code
#unique(population$check)

population <- dplyr::select(population, c_name, year, c_code, population_WB)

gini <- read.csv2("data/gdp_wb/gini.csv", encoding = "UTF-8", sep = ",")
gini <- gather(gini, year, gini_WB, 5:65)
gini$year <- gsub("X", "", gini$year)
gini$gini_WB <- na_if(gini$gini_WB, "")
gini$gini_WB <- as.numeric(gini$gini_WB)

hist(log(gini$gini_WB))

names(gini)[1] <- "c_name"

gini <- dplyr::filter(gini, gini$c_name != 'Central Europe and the Baltics' &
                        gini$c_name != 'Caribbean small states' & 
                        gini$c_name != 'East Asia & Pacific (excluding high income)' &
                        gini$c_name != 'Early-demographic dividend' &
                        gini$c_name != 'East Asia & Pacific' &
                        gini$c_name != 'Europe & Central Asia (excluding high income)' &
                        gini$c_name != 'Europe & Central Asia' & 
                        gini$c_name != 'European Union' & 
                        gini$c_name != 'Fragile and conflict affected situations' &
                        gini$c_name != 'High income' &
                        gini$c_name != 'Heavily indebted poor countries (HIPC)' &
                        gini$c_name != 'IBRD only' &
                        gini$c_name != 'IDA & IBRD total' &
                        gini$c_name != 'IDA total' &
                        gini$c_name != 'IDA blend' &
                        gini$c_name != 'IDA only' &
                        gini$c_name != 'Not classified' &
                        gini$c_name != 'Latin America & Caribbean (excluding high income)' &
                        gini$c_name != 'Latin America & Caribbean' &
                        gini$c_name != 'Least developed countries: UN classification' &
                        gini$c_name != 'Low income' &
                        gini$c_name != 'Lower middle income' &
                        gini$c_name != 'Low & middle income' &
                        gini$c_name != 'Late-demographic dividend' &
                        gini$c_name != 'Middle East & North Africa' &
                        gini$c_name != 'Middle East & North Africa (excluding high income)' &
                        gini$c_name != 'OECD members' &
                        gini$c_name != 'Pre-demographic dividend' &
                        gini$c_name != 'Pacific island small states' &
                        gini$c_name != 'Post-demographic dividend' &
                        gini$c_name != 'Sub-Saharan Africa (excluding high income)' &
                        gini$c_name != 'Sub-Saharan Africa' &
                        gini$c_name != 'Small states' &
                        gini$c_name != 'East Asia & Pacific (IDA & IBRD countries)' &
                        gini$c_name != 'Europe & Central Asia (IDA & IBRD countries)' &
                        gini$c_name != 'Latin America & the Caribbean (IDA & IBRD countries)' &
                        gini$c_name != 'Middle East & North Africa (IDA & IBRD countries)' &
                        gini$c_name != 'South Asia (IDA & IBRD)' &
                        gini$c_name != 'Sub-Saharan Africa (IDA & IBRD countries)' &
                        gini$c_name != 'Upper middle income' &
                        gini$c_name != 'World' &
                        gini$c_name != 'Arab World' &
                        gini$c_name != 'Euro area' &
                        gini$c_name != 'Middle income' &
                        gini$c_name != 'North America' &
                        gini$c_name != 'Other small states' &
                        gini$c_name != 'South Asia' &
                        gini$c_name != 'Channel Islands')

gini$c_code <- countrycode(gini[,1], "country.name", "iso3c", warn = TRUE, nomatch = NULL)
gini$c_code <- gsub("Kosovo", "RKS", gini$c_code)

#gini$check <- gini$c_code %in% gini$Country.Code
#unique(gini$check)

gini <- dplyr::select(gini, c_name, year, c_code, gini_WB)

HFCE <- read.csv2("data/poverty_wb/household_final_consumption_expenditure.csv",
                  encoding = "UTF-8", sep = ",")
HFCE <- gather(HFCE, year, HFCE_pc, 5:65)
HFCE$year <- gsub("X", "", HFCE$year)
HFCE$HFCE_pc <- na_if(HFCE$HFCE_pc, "")
HFCE$HFCE_pc <- as.numeric(HFCE$HFCE_pc)

hist(log(HFCE$HFCE_pc))

names(HFCE)[1] <- "c_name"

HFCE <- dplyr::filter(HFCE, HFCE$c_name != 'Central Europe and the Baltics' &
                        HFCE$c_name != 'Caribbean small states' & 
                        HFCE$c_name != 'East Asia & Pacific (excluding high income)' &
                        HFCE$c_name != 'Early-demographic dividend' &
                        HFCE$c_name != 'East Asia & Pacific' &
                        HFCE$c_name != 'Europe & Central Asia (excluding high income)' &
                        HFCE$c_name != 'Europe & Central Asia' & 
                        HFCE$c_name != 'European Union' & 
                        HFCE$c_name != 'Fragile and conflict affected situations' &
                        HFCE$c_name != 'High income' &
                        HFCE$c_name != 'Heavily indebted poor countries (HIPC)' &
                        HFCE$c_name != 'IBRD only' &
                        HFCE$c_name != 'IDA & IBRD total' &
                        HFCE$c_name != 'IDA total' &
                        HFCE$c_name != 'IDA blend' &
                        HFCE$c_name != 'IDA only' &
                        HFCE$c_name != 'Not classified' &
                        HFCE$c_name != 'Latin America & Caribbean (excluding high income)' &
                        HFCE$c_name != 'Latin America & Caribbean' &
                        HFCE$c_name != 'Least developed countries: UN classification' &
                        HFCE$c_name != 'Low income' &
                        HFCE$c_name != 'Lower middle income' &
                        HFCE$c_name != 'Low & middle income' &
                        HFCE$c_name != 'Late-demographic dividend' &
                        HFCE$c_name != 'Middle East & North Africa' &
                        HFCE$c_name != 'Middle East & North Africa (excluding high income)' &
                        HFCE$c_name != 'OECD members' &
                        HFCE$c_name != 'Pre-demographic dividend' &
                        HFCE$c_name != 'Pacific island small states' &
                        HFCE$c_name != 'Post-demographic dividend' &
                        HFCE$c_name != 'Sub-Saharan Africa (excluding high income)' &
                        HFCE$c_name != 'Sub-Saharan Africa' &
                        HFCE$c_name != 'Small states' &
                        HFCE$c_name != 'East Asia & Pacific (IDA & IBRD countries)' &
                        HFCE$c_name != 'Europe & Central Asia (IDA & IBRD countries)' &
                        HFCE$c_name != 'Latin America & the Caribbean (IDA & IBRD countries)' &
                        HFCE$c_name != 'Middle East & North Africa (IDA & IBRD countries)' &
                        HFCE$c_name != 'South Asia (IDA & IBRD)' &
                        HFCE$c_name != 'Sub-Saharan Africa (IDA & IBRD countries)' &
                        HFCE$c_name != 'Upper middle income' &
                        HFCE$c_name != 'World' &
                        HFCE$c_name != 'Arab World' &
                        HFCE$c_name != 'Euro area' &
                        HFCE$c_name != 'Middle income' &
                        HFCE$c_name != 'North America' &
                        HFCE$c_name != 'Other small states' &
                        HFCE$c_name != 'South Asia' &
                        HFCE$c_name != 'Channel Islands')

HFCE$c_code <- countrycode(HFCE[,1], "country.name", "iso3c", warn = TRUE, nomatch = NULL)
HFCE$c_code <- gsub("Kosovo", "RKS", HFCE$c_code)

HFCE <- dplyr::select(HFCE, c_name, year, c_code, HFCE_pc)

####merge wb data####

df_wb <- plyr::join_all(list(population, gdp_raw, gdp_pc, gdp_growth, gdp_agriculture, emp_agr,
                             p_nat, p19, p32, p55, pg19, pg32, pg55, gini, HFCE),
                        by = c("c_code", "year"), type = "full")
names(df_wb)

#df_wb <- plyr::join_all(list(df_wb, gini), by = c("c_code", "year"), type = "full")

#df_wb_clear <- na.omit(df_wb)

rm(emp_agr, gdp_agriculture, gdp_growth, gdp_pc, gdp_raw, gini, p_nat,
          p19, p32, p55, pg19, pg32, pg55, population, HFCE)

saveRDS(df_wb, "data/df_wb.RDS")
df_wb <- readRDS("data/df_wb.RDS")

ggpairs(df_wb[-c(1:3)])

####WID data####

all_wid <- list.files("data/wid_my",
                                pattern = ".csv$",
                                full.names = TRUE)
all_wid[235]

#2-338

interest <- c("p0p10", "p0p50", "p90p100", "p99p100")

test <- read.csv2(all_wid[2], encoding = "UTF-8", sep = ";")
test <- dplyr::filter(test, year >= 1960)
test <- dplyr::filter(test, pop == "i")
#test <- dplyr::filter(test, age == "999")
#test <- dplyr::filter(test, percentile %in% interest)
#full_wid <- test
full_wid <- spread(test, variable, value)

tic <- Sys.time()
for (i in all_wid[3:235]){
  set <- read.csv2(i, encoding = "UTF-8", sep = ";")
  set <- spread(set, variable, value)
  set <- dplyr::filter(set, year >= 1960)
  set <- dplyr::filter(set, pop == "i")
  #set <- dplyr::filter(set, age == "999")
  #set <- dplyr::filter(set, percentile != "p0p100")
  
  full_wid <- plyr::rbind.fill(full_wid, set)
}
toc <- Sys.time()
print(toc-tic)

saveRDS(full_wid, "WID_long.RDS")

#WID_my <- dplyr::filter(full_wid, year >= 1960)
full_wid$c_code <- countrycode(full_wid$country, "iso2c", "iso3c")

####oecd data####

poverty_rate <- read.csv2("data/oecd_data/poverty_rate.csv", encoding = "UTF-8", sep = ",")
poverty_rate$c_code <- countrycode(poverty_rate$X.U.FEFF.LOCATION,"iso3c", "iso3c",
                                   warn = TRUE, nomatch = NULL)

####geddes####

geddesdata <- read.csv("data/autocracies.csv")
geddesdata <- geddesdata[c('gwf_country', 'year', 'gwf_duration', 'gwf_fail')]
geddesdata$c_code <- countrycode(geddesdata$gwf_country, 'country.name', "iso3c", warn=TRUE,
                                 nomatch = NULL)
geddesdata <- dplyr::filter(geddesdata, year >= 1960)
geddesdata <- geddesdata[c('c_code', 'year', 'gwf_duration', 'gwf_fail')]
geddesdata <- geddesdata[nchar(geddesdata$c_code) < 4,]

gd_problems <- geddesdata[nchar(geddesdata$c_code) > 3,]
rm(gd_problems)

####crisis####

crisisdata <- read_xlsx("data/20160923_global_crisis_data.xlsx")
crisisdata <- dplyr::filter(crisisdata, Year >= 1960)
crisisdata <- crisisdata[c("CC3", "Country", "Year", "Banking Crisis", "Systemic Crisis")]
crisisdata$`Banking Crisis` <- na_if(crisisdata$`Banking Crisis`, 'n/a')
crisisdata$`Systemic Crisis` <- na_if(crisisdata$`Systemic Crisis`, 'n/a')

names(crisisdata)[4] <- "banking_crisis"
names(crisisdata)[5] <- "systemic_crisis"

crisisdata$c_code <- countrycode(crisisdata$CC3, "iso3c", "iso3c", warn = TRUE, 
                                 nomatch = NULL)
names(crisisdata)[3] <- "year"

crisisdata <- crisisdata[c("c_code", "year", "banking_crisis", "systemic_crisis")]

####shocks & inequality####

sjoe <- read.dta13("data/ks_data_sje.dta")
sjoe <- sjoe[,c("wb_code", "year", "nshock", "pshock", "infshock")]
names(sjoe)[1] <- "c_code"
sjoe <- dplyr::filter(sjoe, year >= 1960)

####swiid####

#swiid <- read.dta13("data/SWIID/swiid9_0/swiid9_0.dta")
swiid <- read.csv("data/SWIID/swiid9_0/swiid9_0_summary.csv")
unique(swiid$country)
swiid <- swiid[,c("country", "year", "gini_disp", "gini_mkt")]
swiid$c_code <- countrycode(swiid$country, "country.name", "iso3c")
`%notin%` <- Negate(`%in%`)
swiid <- dplyr::filter(swiid, country != "Soviet Union" | year %notin% c(1988, 1989, 1990))
swiid <- swiid[nchar(swiid$c_code) < 4,]
swiid <- swiid[,c("c_code", "year", "gini_disp", "gini_mkt")]

####urbanization data####

urbdata <- readRDS("data/data_for_rob.RDS")
urbdata <- dplyr::filter(urbdata, year >= 1960)
urbdata1 <- dplyr::select(urbdata, c_names, year, mean_year_schooling)
urbdata2 <- dplyr::select(urbdata, c_names, year, state_capacity)
urbdata3 <- dplyr::select(urbdata, c_names, year, share_urban_population_UN)

urbdata1$c_code <- countrycode(urbdata1$c_names, "country.name", "iso3c")
urbdata1$c_code <- gsub("Kosovo", "RKS", urbdata1$c_code)
urbdata1$c_code <- gsub("Vietnam", "VNM", urbdata1$c_code)
urbdata2$c_code <- countrycode(urbdata2$c_names, "country.name", "iso3c")
urbdata2$c_code <- gsub("Kosovo", "RKS", urbdata2$c_code)
urbdata2$c_code <- gsub("Vietnam", "VNM", urbdata2$c_code)
urbdata3$c_code <- countrycode(urbdata3$c_names, "country.name", "iso3c")
urbdata3$c_code <- gsub("Kosovo", "RKS", urbdata3$c_code)
urbdata3$c_code <- gsub("Vietnam", "VNM", urbdata3$c_code)

urbdata1 <- unique(urbdata1)
urbdata2 <- unique(urbdata2)
urbdata3 <- unique(urbdata3)

urbdata1 <- dplyr::select(urbdata1, c_code, year, mean_year_schooling)
urbdata2 <- dplyr::select(urbdata2, c_code, year, state_capacity)
urbdata3 <- dplyr::select(urbdata3, c_code, year, share_urban_population_UN)

urbdata1 <- na.omit(urbdata1)
urbdata2 <- na.omit(urbdata2)
urbdata3 <- na.omit(urbdata3)

urbdata <- plyr::join_all(list(urbdata1, urbdata2, urbdata3), by=c("c_code", "year"),
                          type="full")
urbdata <- unique(urbdata)

urbdata_ <- dplyr::select(urbdata, c_code, year)
data_dd <- urbdata_[duplicated(urbdata_),]

urbdata <- dplyr::filter(urbdata, c_code != "VNM" | year !="1976" | state_capacity != -1.72380170)
urbdata <- dplyr::filter(urbdata, c_code != "YEM" | year !="1990" | state_capacity != 0.31144890)

rm(urbdata1, urbdata2, urbdata3, urbdata_, data_dd)

####merge all the data####


#data_full <- plyr::join_all(list(data_full, HFCE), by = c("c_code", "year"), type = "full")
#data_clear <- plyr::join_all(list(data_clear, urbdata), by = c("c_code", "year"), type = "full")

data_full <- plyr::join_all(list(df_wb, vdem_short, geddesdata, sjoe, swiid, crisisdata, urbdata),
                            by = c("c_code", "year"), type = "full")

#13403
data_full <- plyr::join_all(list(data_full, vdem_short), by = c("c_code", "year"), type = "full")

data_full <- unique(data_full)

df_ <- dplyr::select(data_full, c_code, year)
data_dd <- df_[duplicated(df_),]
#problems <- dplyr::filter(data_full, c_code == "RUS" & year %in% c(1988, 1989, 1990))

rm(crisisdata, df_wb, geddesdata, sjoe, swiid, vdem_short, urbdata,
   df_, data_dd, problems)

####saving data####

saveRDS(df_wb, "data/df_wb.RDS")

saveRDS(data_full, "data/working_full2.RDS")
saveRDS(paneldata, "data/paneldata2.RDS")
data_full <- readRDS("data/working_full2.RDS")

df_wb <- readRDS("data/df_wb.RDS")
