####load libraries###

library(tidyr)
library(dplyr)
library(ggplot2)
library(plm)
library(stargazer)
library(lmtest)
library(sandwich)
library(zoo)
library(ggeffects)
library(margins)
library(interplot)
library(ggpubr)
library(ivreg)
library(estimatr)
library(texreg)
library(survival)
library(equatiomatic)
library(interflex)
library(mediation)

####load data####

data_full <- readRDS("working_full2.RDS")
data_full <- unique(data_full)
paneldata <- readRDS("paneldata.RDS")

####subset data####

data_full <- tidyr::drop_na(data_full, year, c_code, c_name)
data_clear <- tidyr::drop_na(data_full, poverty19)
rm(data_full)

data_cl <- dplyr::select(data_full, year, c_name)
data_dd <- data_cl[duplicated(data_cl),]
data_cl <- data_cl %>% distinct()
rm(data_cl, data_dd)

paneldata <- pdata.frame(data_full, index=c("c_name", "year"))
#paneldata <- data_clear
rm(data_clear)
paneldata <- unique(paneldata)
#paneldata <- drop_na(paneldata, poverty19, e_polity2)

####create new variables####

paneldata$regime_change <- ifelse(paneldata$e_p_polity == -88, 1, 0)
paneldata$systemic_crisis <- as.factor(paneldata$systemic_crisis)
paneldata$banking_crisis <- as.factor(paneldata$banking_crisis)
paneldata$nshock <- as.factor(paneldata$nshock)
paneldata$infshock <- as.factor(paneldata$infshock)
paneldata$pshock <- as.factor(paneldata$pshock)
paneldata$DD <- ifelse(paneldata$e_polity2 > 0 & paneldata$e_fh_status != 3, 1,0)

####creat pca regime index####

library(haven)
library(factoextra)
library(psych)

regimes <- paneldata[!is.na(paneldata$e_polity2) & !is.na(paneldata$v2x_regime) & 
                       !is.na(paneldata$e_fh_status),]

pca_res <- prcomp(regimes[c("e_polity2", "v2x_regime", "e_fh_status")], center = TRUE, scale = TRUE)
summary(pca_res)

eig.value <- get_eigenvalue(pca_res)
eig.value

pca_res$rotation[,1:2]

fviz_contrib(pca_res, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_res, choice = "var", axes = 2, top = 10)

fviz_pca_var(pca_res, col.var = "contrib", repel = TRUE)          

eig.value
fviz_eig(pca_res, addlabels = TRUE)

regimes$pca_regime <- 0.5814937*regimes$e_polity2 + 
  0.5740806*regimes$v2x_regime-0.5788398*regimes$e_fh_status

plot(pca_regime~e_polity2, data=regimes)

regimes$pca_regime_norm <- regimes$pca_regime-mean(regimes$pca_regime)
regimes$pca_regime_norm <- regimes$pca_regime_norm/sd(regimes$pca_regime_norm)

plot(pca_regime_norm~e_polity2, data=regimes)
hist(regimes$pca_regime_norm)

regimes$pca_regime_10 <- scales::rescale(regimes$pca_regime, to=c(0,1))
hist(regimes$pca_regime_10)
plot(pca_regime_10~e_polity2, data=regimes)

regimes <- regimes[c("c_name", "year", "c_code", "pca_regime", "pca_regime_10")]

paneldata <- plyr::join_all(list(data.frame(paneldata), data.frame(regimes)),
                             by = c("c_code", "year"), type="full")

rm(regimes, eig.value, pca_res)
paneldata <- pdata.frame(paneldata, index=c("c_code", "year"))
paneldata <- unique(paneldata, by=c("c_code", "year"))

####lagged variables####

paneldata$polity2_lag <- lag(paneldata$e_polity2, -1)
paneldata$polity_diff <- paneldata$polity2_lag - paneldata$e_polity2
paneldata$polity_diff_lag <- lag(paneldata$polity_diff, -1)
paneldata$polity2_lag2 <- lag(paneldata$e_polity2, -2)
paneldata$polity_diff_lag2 <- lag(paneldata$polity_diff, -2)

paneldata$pca_regime_lag <- lag(paneldata$pca_regime_10, -1)
paneldata$pca_regime_diff <- paneldata$pca_regime_lag - paneldata$pca_regime_10
paneldata$pca_regime_diff_lag <- lag(paneldata$pca_regime_diff, -1)

paneldata$poverty19_lag <- lag(paneldata$poverty19, -1)
paneldata$poverty19_diff <- paneldata$poverty19_lag - paneldata$poverty19
paneldata$poverty19_diff_lag <- lag(paneldata$poverty19_diff, -1)

paneldata$poverty32_lag <- lag(paneldata$poverty32, -1)
paneldata$poverty32_diff <- paneldata$poverty32_lag - paneldata$poverty32
paneldata$poverty32_diff_lag <- lag(paneldata$poverty32_diff, -1)

paneldata$poverty55_lag <- lag(paneldata$poverty55, -1)
paneldata$poverty55_diff <- paneldata$poverty55_lag - paneldata$poverty55
paneldata$poverty55_diff_lag <- lag(paneldata$poverty55_diff, -1)

paneldata$HFCE_pc_lag <- lag(paneldata$HFCE_pc)
paneldata$HFCE_diff <- paneldata$HFCE_pc-paneldata$HFCE_pc_lag
paneldata$HFCE_pc_diff_lag <- lag(paneldata$HFCE_diff)

paneldata$polity_ra3 <- ave(paneldata$e_polity2, paneldata$c_name,
                             FUN = function(x) rollmean(x, k=3, align="right", na.pad=T))

paneldata$polity_ra5 <- ave(paneldata$e_polity2, paneldata$c_name,
                             FUN = function(x) rollmean(x, k=5, align="right", na.pad=T))

paneldata$polity_ra3_lag <- lag(paneldata$polity_ra3, -2)

paneldata$polity_ra5_lag <- lag(paneldata$polity_ra5, -4)


paneldata$pca_ra3 <- ave(paneldata$pca_regime, paneldata$c_name,
                            FUN = function(x) rollmean(x, k=3, align="right", na.pad=T))

paneldata$pca_ra3_lag <- lag(paneldata$pca_ra3, -2)

paneldata$polity_diff_ra3 <- ave(paneldata$polity_diff, paneldata$c_name,
                            FUN = function(x) rollmean(x, k=3, align="right", na.pad=T))

paneldata$polity_diff_ra5 <- ave(paneldata$polity_diff, paneldata$c_name,
                            FUN = function(x) rollmean(x, k=5, align="right", na.pad=T))

paneldata$polity_diff_ra3_lag <- lag(paneldata$polity_diff_ra3, -2)

paneldata$polity_diff_ra5_lag <- lag(paneldata$polity_diff_ra5, -4)



paneldata$poverty19_diff_ra10 <- ave(paneldata$poverty19_diff, paneldata$c_name,
                            FUN = function(x) rollmean(x, k=10, align="right", na.pad=T))

paneldata$poverty19_diff_ra5 <- ave(paneldata$poverty19_diff, paneldata$c_name,
                            FUN = function(x) rollmean(x, k=5, align="right", na.pad=T))

paneldata$poverty32_diff_ra10 <- ave(paneldata$poverty32_diff, paneldata$c_name,
                                    FUN = function(x) rollmean(x, k=10, align="right", na.pad=T))

paneldata$poverty32_diff_ra5 <- ave(paneldata$poverty32_diff, paneldata$c_name,
                                    FUN = function(x) rollmean(x, k=5, align="right", na.pad=T))

paneldata$poverty55_diff_ra10 <- ave(paneldata$poverty55_diff, paneldata$c_name,
                                    FUN = function(x) rollmean(x, k=10, align="right", na.pad=T))

paneldata$poverty55_diff_ra5 <- ave(paneldata$poverty55_diff, paneldata$c_name,
                                    FUN = function(x) rollmean(x, k=5, align="right", na.pad=T))

paneldata$HFCE_diff_ra10 <- ave(paneldata$HFCE_diff, paneldata$c_name,
                                     FUN = function(x) rollmean(x, k=10, align="right", na.pad=T))

paneldata$HFCE_diff_ra5 <- ave(paneldata$HFCE_diff, paneldata$c_name,
                                    FUN = function(x) rollmean(x, k=5, align="right", na.pad=T))


paneldata$gwf_fail_lag <- lag(paneldata$gwf_fail, -1)
paneldata$regime_change_lag <- lag(paneldata$regime_change, -1)

paneldata <- tidyr::drop_na(paneldata, poverty19)


stargazer(dplyr::select(pdf,poverty19, poverty32, poverty55, e_polity2, pca_regime_10,
                         regime_change, gwf_fail, gwf_duration, nshock, systemic_crisis,
                         population_WB, GDP_pc_ppp, GDP_growth, mean_year_schooling,
                         gini_mkt, share_urban_population_UN, state_capacity,
                         agriculture_GDP, agriculture_employment))

GGally::ggpairs(dplyr::select(pdf,poverty19, poverty32, poverty55, e_polity2, pca_regime_10,
                              regime_change, gwf_fail, gwf_duration, nshock, systemic_crisis,
                              population_WB, GDP_pc_ppp, GDP_growth, mean_year_schooling,
                              gini_mkt, share_urban_population_UN, state_capacity,
                              agriculture_GDP, agriculture_employment))

stargazer(dplyr::select(paneldata, v2x_partip))

####visualizations####


library(rnaturalearth)
library(rnaturalearthdata)

#world <- ne_countries(scale = "medium", returnclass = "sf")
world <- map_data("world")
world$c_code <- countrycode::countrycode(world$region, "country.name", "iso3c")

data_clear <- tidyr::drop_na(data_full, year, c_code)
#rm(data_full)

paneldata1 <- pdata.frame(data_clear, index=c("c_code", "year"))
rm(data_clear)
paneldata1 <- unique(paneldata1, by=c("c_code", "year"))
paneldata1 <- dplyr::filter(paneldata1, as.numeric(as.character(year))>=1991)

pd2 <- drop_na(paneldata1, e_polity2)
df1 <- dplyr::select(pd2, c_code, year, e_polity2)
df2 <- df1 %>% 
  group_by(c_code) %>%
  summarise(firstocc = first(year), lastocc = last(year))

df1 <- data.frame(df1)

for (i in 1:nrow(df2)){
  ccode = as.character(df2$c_code[i])
  fo <- as.numeric(as.character(df2$firstocc[i]))
  lo <- as.numeric(as.character(df2$lastocc[i]))
  df2$polity_first[i] <- df1[df1$year==fo & df1$c_code==ccode,]$e_polity2 
  df2$polity_last[i] <- df1[df1$year==lo & df1$c_code==ccode,]$e_polity2
}


worldset <- inner_join(world, df2, by="c_code")

p1 <- ggplot(data = worldset, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = polity_first)) +
  scale_fill_distiller(palette ="RdBu", direction = 1) +
  xlab("")+
  ylab("")+
  labs(fill = "Polity")+
  theme_bw()

p2 <- ggplot(data = worldset, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = polity_last)) +
  scale_fill_distiller(palette ="RdBu", direction = 1) +
  xlab("")+
  ylab("")+
  labs(fill = "Polity")+
  theme_bw()



pd3 <- drop_na(paneldata1, poverty19)
df3 <- dplyr::select(pd3, c_code, year, poverty19)
df4 <- df3 %>% 
  group_by(c_code) %>%
  summarise(firstocc = first(year), lastocc = last(year))

df3 <- data.frame(df3)

for (i in 1:nrow(df4)){
  ccode = as.character(df4$c_code[i])
  fo <- as.numeric(as.character(df4$firstocc[i]))
  lo <- as.numeric(as.character(df4$lastocc[i]))
  df4$poverty_first[i] <- df3[df3$year==fo & df3$c_code==ccode,]$poverty19 
  df4$poverty_last[i] <- df3[df3$year==lo & df3$c_code==ccode,]$poverty19
}

worldset2 <- inner_join(world, df4, by="c_code")

p3 <- ggplot(data = worldset2, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = poverty_first)) +
  scale_fill_distiller(palette ="BrBG", direction = -1) +
  xlab("")+
  ylab("")+
  labs(fill = "Population in poverty (first)")+
  theme_bw()

p4 <- ggplot(data = worldset2, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = poverty_last)) +
  scale_fill_distiller(palette ="BrBG", direction = -1) +
  xlab("")+
  ylab("")+
  labs(fill = "Population in poverty (last)")+
  theme_bw()

ggpubr::ggarrange(p3, p1, p4, p2, ncol=2, nrow=2)

row1 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="First occurrence") + theme_void() 

row2 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="Last occurrence") + theme_void() 

col1 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="Population under $1.9 poverty line")+
  theme_void() 

col2 <- ggplot() + annotate(geom = 'text', x=1, y=1, label="Polity IV score") + theme_void() 

layoutplot <- "
#cccddd
aeeefff
aeeefff
bggghhh
bggghhh
"
plotlist <- list(a= row1, b = row2, c = col1, d = col2, e= p3, f=p1, g=p4, h=p2)
patchwork::wrap_plots(plotlist, guides = 'collect', design = layoutplot)

ls()
rm(df1, df2, df3, df4, p1, p2, p3, p4, paneldata1, pd2, pd3, world, worldset, worldset2, col1, col2,
   row1, row2)

dat <- as.data.frame(paneldata)
dat$year_f <- as.factor(dat$year)

p1 <- ggplot(ggpredict(lm(polity2_lag~poverty19+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt
                    +c_name+year_f,
                    data=dat),
                terms = "poverty19"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  xlab("Population under $1.9 line")+
  ylab("PolityIV")+
  ylim(c(-10,10))+
  theme_light()

p2 <- ggplot(ggpredict(lm(polity_ra3_lag~poverty19+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt
                    +c_name+year_f,
                    data=dat),
                 terms = "poverty19"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  xlab("Population under $1.9 line")+
  ylab("PolityIV 3 year rolling average")+
  ylim(c(-10,10))+
  theme_light()

ggarrange(p1, p2, ncol=2)


p1 <- ggplot(ggpredict(glm(gwf_fail~poverty_gap_19+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                     family=binomial(link = "logit"),
                    data=dat),
                 terms = "poverty_gap_19 [all]"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  xlab("Poverty gap under $1.9 line")+
  ylab("Authoritarian breakdown")+
  theme_light()

p2 <- ggplot(ggeffect(glm(gwf_fail~poverty_gap_32+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                          family=binomial(link = "logit"),
                          data=dat),
                      terms = "poverty_gap_32 [all]"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  xlab("Poverty gap under $3.2 line")+
  ylab("Authoritarian breakdown")+
  theme_light()

p3 <- ggplot(ggeffect(glm(gwf_fail~poverty_gap_55+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                          family=binomial(link = "logit"),
                          data=dat),
                      terms = "poverty_gap_55 [all]"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  xlab("Poverty gap under $5.5 line")+
  ylab("Authoritarian breakdown")+
  theme_light()

ggarrange(p1,p2,p3, ncol=3)



m1 <- glm(gwf_fail~poverty19+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
          family=binomial(link = "logit"),
          data=dat)
test <- data.frame(poverty19 = 0:100,
                   mean_year_schooling = mean(drop_na(dat,
                                                      mean_year_schooling)$mean_year_schooling),
                   GDP_pc_ppp = mean(drop_na(dat, GDP_pc_ppp)$GDP_pc_ppp),
                   gini_mkt = mean(drop_na(dat, gini_mkt)$gini_mkt))

pred <- predict(m1, test, type = "response")

plot(dat$poverty19, dat$gwf_fail, pch = 16,
     xlab = "Population under $1.9 line", ylab = "Authoritarian breakdown")

lines(test$poverty19, pred)
  
####margin effects####

interaction_plot <- function(model, effect, moderator, interaction,region="default",
                             logit=FALSE, varcov="default", minimum="min", maximum="max",
                             incr="default", num_points = 50, conf=.95, mean=FALSE, median=FALSE,
                             alph=80, title="Marginal effects plot", xlabel="Value of moderator",
                             ylabel="Estimated marginal coefficient")
{
  
  # Extract Variance Covariance matrix
  if (varcov == "default"){
    covMat = vcov(model)
    ee=covMat[effect,effect]
    ii=covMat[interaction, interaction]
    ei= covMat[effect, interaction]
  }else{
    covMat = varcov
  }
  
  if  (varcov == "psce"){
    covMat = vcovBK(model,cluster="time")
  }else{
    covMat = varcov
  }
  
  if (varcov == "robust"){
    covMat = vcovHC(model,type= "HC1")
    
  }
  
  if (region =="default") { mod_frame = model.frame(model)}
  else {mod_frame = filter(model.frame(model),ht_region==region)}
  
  
  
  # Get coefficients of variables
  if (logit== FALSE){
    beta_1 = model$coefficients[[effect]]
    beta_3 = model$coefficients[[interaction]]}else 
    { 
      beta_1 = exp(model$coefficients[[effect]])
      beta_3 = exp(model$coefficients[[interaction]]) 
    }
  
  # Set range of the moderator variable
  # Minimum
  if (minimum == "min"){
    min_val = min(mod_frame[[moderator]])
  }else{
    min_val = minimum
  }
  # Maximum
  if (maximum == "max"){
    max_val = max(mod_frame[[moderator]])
  }else{
    max_val = maximum
  }
  
  # Check if minimum smaller than maximum
  if (min_val > max_val){
    stop("Error: Minimum moderator value greater than maximum value.")
  }
  
  # Determine intervals between values of the moderator
  if (incr == "default"){
    increment = (max_val - min_val)/(num_points - 1)
  }else{
    increment = incr
  }
  
  # Create list of moderator values at which marginal effect is evaluated
  x_2 <- seq(from=min_val, to=max_val, by=increment)
  
  # Compute marginal effects
  delta_1 = beta_1 + beta_3*x_2
  
  # Compute variances
  if (varcov == "default"){
    var_1 = ee + (x_2^2)*ii + 2*x_2*ei}else{
      var_1 = covMat[effect,effect] + (x_2^2)*covMat[interaction,interaction] +
        2*x_2 * covMat[effect,interaction]
    } 
  
  # Standard errors
  se_1 = sqrt(var_1)
  
  # Upper and lower confidence bounds
  z_score = qnorm(1 - ((1 - conf)/2))
  upper_bound = delta_1 + z_score*se_1
  lower_bound = delta_1 - z_score*se_1
  
  # Determine the bounds of the graphing area
  max_y = max(upper_bound)
  min_y = min(lower_bound)
  
  # Initialize plotting window
  plot(x=c(), y=c(), ylim=c(min_y, max_y), xlim=c(min_val, max_val), xlab=xlabel,
       ylab=ylabel, main=title)
  
  # Plot estimated effects
  lines(y=delta_1, x=x_2)
  lines(y=upper_bound, x=x_2, lty=2)
  lines(y=lower_bound, x=x_2, lty=2)
  
  # Add a dashed horizontal line for zero
  abline(h=0, lty=3)
}

interaction_plot(mod19pst, varcov="robust", effect="systemic_crisis1", moderator = "poverty19",
                 interaction="poverty19:systemic_crisis1", minimum = 0, maximum = 1)

pdf <- as.data.frame(paneldata)
pdf$c_name_f <- as.factor(pdf$c_name)
pdf$year_f <- as.factor(pdf$year)

p19blm <- lm(polity_ra5_lag~poverty19+nshock+poverty19:nshock+c_name_f+year_f,
             data=pdf)
ggeffect(p19blm, terms="poverty19")
mydf <- ggeffect(p19blm, terms = "poverty19")
p1 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratic quality")+
  xlab("Proportion of population under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()

coeftest(plm(polity_diff~e_polity2+poverty19+systemic_crisis+poverty19:systemic_crisis+
               mean_year_schooling+log(GDP_pc_ppp),
             data=paneldata, model="within", effect="twoways"),
         vcov=vcovHC, type="HC1")

coeftest(plm(polity_diff_lag~e_polity2+log(poverty19+1)+nshock+log(poverty19+1):nshock,
             data=paneldata, model="within", effect="twoways"),
         vcov=vcovHC, type="HC1")

coeftest(plm(polity2_lag~poverty19+systemic_crisis+poverty19:systemic_crisis+
               mean_year_schooling+log(GDP_pc_ppp),
             data=paneldata, model="within", effect="twoways"),
         vcov=vcovHC, type="HC1")

coeftest(plm(polity_diff_lag~e_polity2+poverty19+systemic_crisis+poverty19:systemic_crisis+
               mean_year_schooling+log(GDP_pc_ppp),
             data=paneldata, model="within", effect="twoways"),
         vcov=vcovHC, type="HC1")

p19dblm <- lm(polity_diff~e_polity2+poverty19+systemic_crisis+poverty19:systemic_crisis+
                mean_year_schooling+log(GDP_pc_ppp)+state_capacity+share_urban_population_UN+
                agriculture_employment+log(population_WB)+
                c_name_f+year_f,
             data=pdf)
ggeffect(p19dblm, terms="poverty19")
mydf <- ggeffect(p19dblm, terms = "poverty19")
p2<- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratization")+
  xlab("Proportion of population under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()

ggarrange(p1, p2, ncol=2)

data_f <- dplyr::filter(pdf, as.numeric(as.character(data_full$year))>=1960)
data_f <- unique(data_f)
data_f <- tidyr::drop_na(data_f, gwf_fail, gwf_duration, regime_change)

ph1 <- ggplot(data_f, aes(regime_change))+
  geom_histogram(bins = 3)+
  xlab("Regime change")+
  ylab("Frequency")+
  theme_bw()

ph2 <- ggplot(data_f, aes(gwf_fail))+
  geom_histogram(bins = 3)+
  xlab("Authoritarian regime failure")+
  ylab("Frequency")+
  theme_bw()

ph3 <- ggplot(data_f, aes(gwf_duration))+
  geom_histogram(bins=50)+
  xlab("Authoritarian regime duration")+
  ylab("Frequency")+
  theme_bw()

ggarrange(ph1, ph2, ph3, ncol=3)

data_f <- dplyr::filter(pdf, as.numeric(as.character(data_full$year))>=1960)
data_f <- unique(data_f)
data_f <- tidyr::drop_na(data_f, poverty19)

ph1 <- ggplot(data_f, aes(poverty19))+
  geom_histogram(bins=20)+
  xlab("Proportion of population under the $1.9 poverty line")+
  ylab("Frequency")+
  theme_bw()

ph2 <- ggplot(data_f, aes(poverty32))+
  geom_histogram(bins=20)+
  xlab("Proportion of population under the $3.2 poverty line")+
  ylab("Frequency")+
  theme_bw()

ph3 <- ggplot(data_f, aes(poverty55))+
  geom_histogram(bins=20)+
  xlab("Proportion of population under the $5.5 poverty line")+
  ylab("Frequency")+
  theme_bw()

pg1 <- ggplot(data_f, aes(poverty_gap_19))+
  geom_histogram(bins=20)+
  xlab("Poverty gap under the $1.9 poverty line")+
  ylab("Frequency")+
  theme_bw()

pg2 <- ggplot(data_f, aes(poverty_gap_32))+
  geom_histogram(bins=20)+
  xlab("Poverty gap under the $3.2 poverty line")+
  ylab("Frequency")+
  theme_bw()

pg3 <- ggplot(data_f, aes(poverty_gap_55))+
  geom_histogram(bins=20)+
  xlab("Poverty gap under the $5.5 poverty line")+
  ylab("Frequency")+
  theme_bw()

ggarrange(ph1, ph2, ph3, pg1, pg2, pg3, ncol=3, nrow=2)



pdf <- as.data.frame(paneldata)
pdf$c_name_f <- as.factor(pdf$c_name)
pdf$year_f <- as.factor(pdf$year)


mod19pcadn <- plm(pca_regime_diff_lag~e_polity2+poverty_gap_19+nshock+poverty_gap_19:nshock+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                data=paneldata, model="within", effect="twoways")
coeftest(mod19pcadn, vcov=vcovHC, type="HC1")

mod32pcadn <- plm(pca_regime_diff_lag~e_polity2+poverty32+nshock+poverty32:nshock+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                  data=paneldata, model="within", effect="twoways")
coeftest(mod32pcadn, vcov=vcovHC, type="HC1")

mod55pcadn <- plm(pca_regime_diff_lag~e_polity2+poverty55+nshock+poverty55:nshock+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                  data=paneldata, model="within", effect="twoways")
coeftest(mod55pcadn, vcov=vcovHC, type="HC1")


p19blm <- lm(pca_regime_diff_lag~poverty_gap_19+nshock+poverty_gap_19:nshock+e_polity2+
               mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name_f+year_f,
             data=pdf)
ggeffect(p19blm, terms="poverty_gap_19")
mydf <- ggeffect(p19blm, terms = "poverty_gap_19")
ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratization")+
  xlab("Proportion of population under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()



p19blm <- lm(polity_diff_lag~poverty19+nshock+poverty19:nshock+e_polity2+mean_year_schooling+
               log(GDP_pc_ppp)+gini_mkt+c_name_f+year_f,
             data=pdf)
ggeffect(p19blm, terms="poverty19")
mydf <- ggeffect(p19blm, terms = "poverty19")
p1 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratization")+
  xlab("Proportion of population under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()


p32blm <- lm(polity_diff_lag~poverty32+nshock+poverty32:nshock+e_polity2+mean_year_schooling+
               log(GDP_pc_ppp)+gini_mkt+c_name_f+year_f,
             data=pdf)
ggeffect(p32blm, terms="poverty32")
mydf <- ggeffect(p32blm, terms = "poverty32")
p2 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratization")+
  xlab("Proportion of population under the $3.2 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()


p55blm <- lm(polity_diff_lag~poverty55+nshock+poverty55:nshock+e_polity2+mean_year_schooling+
               log(GDP_pc_ppp)+gini_mkt+c_name_f+year_f,
             data=pdf)
ggeffect(p55blm, terms="poverty55")
mydf <- ggeffect(p55blm, terms = "poverty55")
p3 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratization")+
  xlab("Proportion of population under the $5.5 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()

ggarrange(p1, p2, p3, ncol=3)

pp <- paneldata
pp$nnshock <- as.numeric(pp$nshock)
pp$logpov <- log(pp$poverty19+1)

interflex(estimator = "raw", Y = "polity_diff", X = "poverty19", D = "nshock",
          data=pp, na.rm=T, theme.bw=T, show.grid = F)

interflex(estimator = "raw", Y = "polity_diff_lag", X = "poverty19", D = "nshock",
          data=pp, na.rm=T, theme.bw=T, show.grid = F, ylim = c(-5,5))

interflex(estimator = "raw", Y = "polity_diff_lag", X = "logpov", D = "nshock",
          data=pp, na.rm=T, theme.bw=T, show.grid = F, ylim = c(-5,5))

interflex(estimator = "raw", Y = "polity2_lag", X = "poverty19", D = "nshock",
          data=pp, na.rm=T, theme.bw=T, show.grid = F)

interflex(estimator = "raw", Y = "polity_ra3_lag", X = "poverty19", D = "nshock",
          data=pp, na.rm=T, theme.bw=T, show.grid = F)

interflex(estimator = "raw", Y = "regime_change", X = "e_polity2", D = "poverty19",
          data=pp, na.rm=T, theme.bw=T, show.grid = F)


####regime failures####

modflpm <- plm(gwf_fail_lag~poverty19+mean_year_schooling+log(gini_mkt)+
                 share_urban_population_UN,
               data=paneldata,
               model="within", effect="individual")
summary(modflpm)
coeftest(modflpm)


modrclpm <- plm(regime_change~poverty19+mean_year_schooling+log(gini_mkt),
               data=paneldata[paneldata$e_polity2 < 6,],
               model="within", effect="individual")
summary(modrclpm)
coeftest(modrclpm)

modrclpmd <- plm(regime_change~poverty19+mean_year_schooling+log(gini_mkt),
                data=paneldata[paneldata$e_polity2 >= 6,],
                model="within", effect="individual")
summary(modrclpmd)
coeftest(modrclpmd)


coeftest(plm(gwf_fail~poverty_gap_19+
               mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
             data=paneldata, model="within", effect = "twoways"),
         vcov=vcovHC, type="HC1")

coeftest(plm(gwf_fail~poverty_gap_32+
               mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
             data=paneldata, model="within", effect = "twoways"),
         vcov=vcovHC, type="HC1")

coeftest(plm(gwf_fail~poverty_gap_55+
               mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
             data=paneldata, model="within", effect = "twoways"),
         vcov=vcovHC, type="HC1")


coeftest(plm(gwf_fail_lag~poverty_gap_19+
               mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
             data=paneldata, model="within", effect = "twoways"),
         vcov=vcovHC, type="HC1")

coeftest(plm(gwf_fail~poverty_gap_32+
               mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
             data=paneldata, model="within", effect = "twoways"),
         vcov=vcovHC, type="HC1")

coeftest(plm(gwf_fail~poverty_gap_55+
               mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
             data=paneldata, model="within", effect = "twoways"),
         vcov=vcovHC, type="HC1")

mfail1 <- plm(gwf_fail~poverty19+
                mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
              data=paneldata, model="within", effect = "twoways")
mfail2 <- plm(gwf_fail~poverty32+
                mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
              data=paneldata, model="within", effect = "twoways")
mfail3 <- plm(gwf_fail~poverty55+
                mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
              data=paneldata, model="within", effect = "twoways")
mfail4 <- plm(gwf_fail~poverty_gap_19+
                mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
              data=paneldata, model="within", effect = "twoways")
mfail5 <- plm(gwf_fail~poverty_gap_32+
                mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
              data=paneldata, model="within", effect = "twoways")
mfail6 <- plm(gwf_fail~poverty_gap_55+
                mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
              data=paneldata, model="within", effect = "twoways")
se1pl <- c(list(sqrt(diag(vcovHC(mfail1, type = "HC1"))),
                sqrt(diag(vcovHC(mfail2, type = "HC1"))),
                sqrt(diag(vcovHC(mfail3, type = "HC1"))),
                sqrt(diag(vcovHC(mfail4, type = "HC1"))),
                sqrt(diag(vcovHC(mfail5, type = "HC1"))),
                sqrt(diag(vcovHC(mfail6, type = "HC1")))))

stargazer(mfail1, mfail2, mfail3, mfail4, mfail5, mfail6, se = se1pl)

mfaill1 <- clogit(gwf_fail~poverty19+
                  mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+strata(c_name),
                data=paneldata, method="efron", robust=TRUE)
mfaill2 <- clogit(gwf_fail~poverty32+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+strata(c_name),
                  data=paneldata, method="efron", robust=TRUE)
mfaill3 <- clogit(gwf_fail~poverty55+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+strata(c_name),
                  data=paneldata, method="efron", robust=TRUE)
mfaill4 <- clogit(gwf_fail~poverty_gap_19+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+strata(c_name),
                  data=paneldata, method="efron", robust=TRUE)
mfaill5 <- clogit(gwf_fail~poverty_gap_32+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+strata(c_name),
                  data=paneldata, method="efron", robust=TRUE)
mfaill6 <- clogit(gwf_fail~poverty_gap_55+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+strata(c_name),
                  data=paneldata, method="efron", robust=TRUE)

stargazer(mfaill1, mfaill2, mfaill3)
stargazer(mfaill4, mfaill5, mfaill6)

extract_eq(glm(gwf_fail~poverty19+
                   mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                 family = binomial(link = "logit"), x = TRUE, data=paneldata),
           wrap = TRUE)

coeftest(plm(regime_change_lag~poverty_gap_19+DD+poverty_gap_19:DD+
               mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
             data=paneldata, model="within", effect = "twoways"),
         vcov=vcovHC, type="HC1")

mchange1 <- plm(regime_change_lag~poverty19+DD+poverty19:DD+
                  mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                data=paneldata, model="within", effect = "twoways")

mchange2 <- plm(regime_change_lag~poverty32+DD+poverty32:DD+
                  mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                data=paneldata, model="within", effect = "twoways")

mchange3 <- plm(regime_change_lag~poverty55+DD+poverty55:DD+
                  mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                data=paneldata, model="within", effect = "twoways")

mchange4 <- plm(regime_change_lag~poverty_gap_19+DD+poverty_gap_19:DD+
                  mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                data=paneldata, model="within", effect = "twoways")

mchange5 <- plm(regime_change_lag~poverty_gap_32+DD+poverty_gap_32:DD+
                  mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                data=paneldata, model="within", effect = "twoways")

mchange6 <- plm(regime_change_lag~poverty_gap_55+DD+poverty_gap_55:DD+
                  mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                data=paneldata, model="within", effect = "twoways")

se1pl <- c(list(sqrt(diag(vcovHC(mchange1, type = "HC1"))),
                sqrt(diag(vcovHC(mchange2, type = "HC1"))),
                sqrt(diag(vcovHC(mchange3, type = "HC1"))),
                sqrt(diag(vcovHC(mchange4, type = "HC1"))),
                sqrt(diag(vcovHC(mchange5, type = "HC1"))),
                sqrt(diag(vcovHC(mchange6, type = "HC1")))))

stargazer(mchange1, mchange2, mchange3, mchange4, mchange5, mchange6, se = se1pl)
stargazer(mchange1, mchange2, mchange3)
stargazer(mchange4, mchange5, mchange6)

pdf <- as.data.frame(paneldata)
pdf$c_name_f <- as.factor(pdf$c_name)
pdf$year_f <- as.factor(pdf$year)


p19blm <- lm(regime_change_lag~poverty19+DD+poverty19:DD+c_name_f+year_f,
             data=pdf)
ggeffect(p19blm, terms="poverty19")
mydf <- ggeffect(p19blm, terms = "poverty19")
p1 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  #ylab("Marginal effect of income shock on democratization")+
  #xlab("Proportion of population under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()


mchangel1 <- clogit(regime_change_lag~poverty19+DD+poverty19:DD+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+strata(c_name),
                  data=paneldata, method="efron", robust=TRUE)
mchangel2 <- clogit(regime_change_lag~poverty32+DD+poverty32:DD+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+strata(c_name),
                  data=paneldata, method="efron", robust=TRUE)
mchangel3 <- clogit(regime_change_lag~poverty55+DD+poverty55:DD+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+strata(c_name),
                  data=paneldata, method="efron", robust=TRUE)
mchangel4 <- clogit(regime_change_lag~poverty_gap_19+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+strata(c_name),
                  data=paneldata, method="efron", robust=TRUE)
mchangel5 <- clogit(regime_change_lag~poverty_gap_32+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+strata(c_name),
                  data=paneldata, method="efron", robust=TRUE)
mchangel6 <- clogit(regime_change_lag~poverty_gap_55+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+strata(c_name),
                  data=paneldata, method="efron", robust=TRUE)

stargazer(mfaill1, mfaill2, mfaill3)
stargazer(mfaill4, mfaill5, mfaill6)

####survival####




####iv####

coeftest(plm(poverty19~nshock+mean_year_schooling,
             data = paneldata, type="within", effect="twoways"),
         vcov=vcovHC, type="HC1")

iv_robust(polity_diff_lag2~poverty_gap_19+e_polity2+mean_year_schooling+gini_mkt+log(GDP_pc_ppp)|
            nshock+e_polity2+mean_year_schooling+gini_mkt+log(GDP_pc_ppp),
          data=paneldata, 
          fixed_effects = ~c_name+year, se_type = "HC1")


iv_robust(gwf_fail_lag~poverty19+mean_year_schooling+gini_mkt+log(GDP_pc_ppp)|
            nshock+mean_year_schooling+gini_mkt+log(GDP_pc_ppp),
          data=paneldata, 
          fixed_effects = ~c_name+year, se_type = "HC1")

texreg(iv_robust(regime_change_lag~poverty19+DD+poverty19:DD+mean_year_schooling+gini_mkt+
                   log(GDP_pc_ppp)|
            nshock+DD+nshock:DD+mean_year_schooling+gini_mkt+log(GDP_pc_ppp),
          data=paneldata, 
          fixed_effects = ~c_name+year, se_type = "HC1"), include.ci=F)


texreg(iv_robust(polity_diff_lag~poverty_gap_19+e_polity2+mean_year_schooling+gini_mkt+
                   log(GDP_pc_ppp)+share_urban_population_UN+agriculture_employment|
                   nshock+e_polity2+mean_year_schooling+gini_mkt+log(GDP_pc_ppp)+
                   share_urban_population_UN+agriculture_employment,
                 data=paneldata, 
                 fixed_effects = ~c_name+year, se_type = "HC1"), include.ci=F)


texreg(iv_robust(polity_ra3~poverty_gap_19+mean_year_schooling+gini_mkt+
                   log(GDP_pc_ppp)+share_urban_population_UN+agriculture_employment|
                   nshock+mean_year_schooling+gini_mkt+log(GDP_pc_ppp)+
                   share_urban_population_UN+agriculture_employment,
                 data=paneldata, 
                 fixed_effects = ~c_name+year, se_type = "HC1"), include.ci=F)

fitdem1 <- iv_robust(polity2_lag~poverty19+mean_year_schooling+log(GDP_pc_ppp)|
                    nshock+log(GDP_pc_ppp)+mean_year_schooling,
          data=paneldata, 
          fixed_effects = ~c_name+year, se_type = "HC1")

fitdem2 <- iv_robust(polity_ra3_lag~poverty19+mean_year_schooling+log(GDP_pc_ppp)|
            nshock+log(GDP_pc_ppp)+mean_year_schooling,
                  data=paneldata, 
                  fixed_effects = ~c_name+year, se_type = "HC1")

fitdem3 <- iv_robust(polity_ra5_lag~poverty19+mean_year_schooling+log(GDP_pc_ppp)|
            nshock+log(GDP_pc_ppp)+mean_year_schooling,
                  data=paneldata, 
                  fixed_effects = ~c_name+year, se_type = "HC1")

texreg(list(fitdem1,fitdem2, fitdem3),include.ci=F)

ffp <- iv_robust(polity2_lag2~poverty19_lag+mean_year_schooling+log(GDP_pc_ppp)+
                   gini_mkt|
                  nshock+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                data=paneldata, 
                fixed_effects = ~c_name+year, se_type = "HC1")
texreg(ffp, include.ci=F)

ff <- iv_robust(polity_diff_lag2~poverty19_lag+e_polity2+mean_year_schooling+log(GDP_pc_ppp)+
                  gini_mkt|
            nshock+e_polity2+mean_year_schooling+log(GDP_pc_ppp)+
              gini_mkt,
          data=paneldata, 
          fixed_effects = ~c_name+year, se_type = "HC1")
texreg(ff, include.ci=F)

fit_democ1 <- iv_robust(polity_diff~poverty19+e_polity2|nshock+e_polity2,
          data=paneldata, 
          fixed_effects = ~c_name+year, se_type = "HC1")

fit_democ2 <- iv_robust(polity_diff~poverty32+e_polity2|nshock+e_polity2,
                        data=paneldata, 
                        fixed_effects = ~c_name+year, se_type = "HC1")

fit_democ3 <- iv_robust(polity_diff~poverty55+e_polity2|nshock+e_polity2,
                        data=paneldata, 
                        fixed_effects = ~c_name+year, se_type = "HC1")

texreg(list(fit_democ1,fit_democ2, fit_democ3),include.ci=F)

iv_robust(polity_diff_lag~poverty19+log(gini_mkt)+mean_year_schooling+GDP_growth|nshock+
            log(gini_mkt)+mean_year_schooling+GDP_growth,
          data=paneldata, 
          fixed_effects = ~c_name+year, se_type = "HC1")

iv_robust(polity_diff_lagged~poverty19+e_polity2|infshock+e_polity2,
          data=paneldata, 
          fixed_effects = ~c_name+year, se_type = "HC1")

iv_robust(polity_diff_lagged~poverty19+e_polity2|systemic_crisis+
            poverty19+e_polity2,
          data=paneldata, 
          fixed_effects = ~c_name+year, se_type = "HC1")

####iv2####

data <- drop_na(paneldata, poverty19, nshock, mean_year_schooling, GDP_pc_ppp, gini_mkt,
                share_urban_population_UN, e_polity2)
data <- data %>% 
  dplyr::select(c_name, year, poverty19, mean_year_schooling, GDP_pc_ppp, gini_mkt, nshock,
                share_urban_population_UN, e_polity2, polity2_lag, polity_diff, polity_diff_lag,
                agriculture_employment, polity_ra3_lag, poverty32, poverty_gap_19)

modtest <- plm(poverty19~nshock+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
               model = "within", effect = "twoways", data = data)


ppoverty <- data.frame(ppoverty = modtest$model[[1]] - modtest$residuals)

model_data <- cbind(as.data.frame(as.matrix(modtest$model)),ppoverty) 
data <- cbind(data, model_data)  
data <- data[,!duplicated(colnames(data))]
data <- unique(data)

modtest <- plm(poverty32~nshock+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
               model = "within", effect = "twoways", data = data)


ppoverty <- data.frame(ppoverty32 = modtest$model[[1]] - modtest$residuals)

model_data <- cbind(as.data.frame(as.matrix(modtest$model)),ppoverty) 
data <- cbind(data, model_data)  
data <- data[,!duplicated(colnames(data))]
data <- unique(data)

dat <- as.data.frame(data)
dat$year_f <- as.factor(dat$year)
ggpredict(lm(polity2_lag~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f,
             data=dat), terms = "ppoverty")
p1 <- ggplot(ggpredict(lm(polity2_lag~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+
                            c_name+year_f,
                    data=dat), terms = "ppoverty"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  xlab("Population under $1.9 line (IV)")+
  ylab("Predicted value of PolityIV")+
  theme_light()

p2 <- ggplot(ggpredict(lm(polity_ra3_lag~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+
                      c_name+year_f,
                    data=dat), terms = "ppoverty"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  xlab("Population under $1.9 line (IV)")+
  ylab("Predicted value of PolityIV (ra 3)")+
  theme_light()

ggarrange(p1,p2,ncol=2)

mss <- plm(polity2_lag~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = data)
summary(mss) 

mss2 <- plm(polity2_lag~ppoverty19+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = data)
summary(mss2) 

mss3 <- plm(polity_ra3_lag~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = data)
summary(mss3) 

mss4 <- plm(polity2_lag~ppoverty32+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = data)
summary(mss2) 

mss5 <- plm(polity_ra3_lag~ppoverty32+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = data)
summary(mss3) 

#stargazer(mss, mss2, mss3)

ses <- c(list(sqrt(diag(vcovHC(mss2, type = "HC1"))),
              sqrt(diag(vcovHC(mss3, type = "HC1"))),
              sqrt(diag(vcovHC(mss4, type = "HC1"))),
              sqrt(diag(vcovHC(mss5, type = "HC1")))))
  
stargazer(mss2, mss3, mss4, mss5, se = ses)  


m1 <- plm(polity2_lag~poverty19+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+
            share_urban_population_UN+agriculture_employment+state_capacity,
          model = "within", effect = "twoways", data = paneldata)

m2 <- plm(polity2_lag~poverty32+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+
            share_urban_population_UN+agriculture_employment+state_capacity,
          model = "within", effect = "twoways", data = paneldata)

m3 <- plm(polity_ra3_lag~poverty19+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+
            share_urban_population_UN+agriculture_employment+state_capacity,
          model = "within", effect = "twoways", data = paneldata)

m4 <- plm(polity_ra3_lag~poverty32+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+
            share_urban_population_UN+agriculture_employment+state_capacity,
          model = "within", effect = "twoways", data = paneldata)


ses <- c(list(sqrt(diag(vcovHC(m1, type = "HC1"))),
              sqrt(diag(vcovHC(m3, type = "HC1")))))

stargazer(m1, m3, se = ses)  


m1 <- plm(pca_regime~poverty19+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+
            share_urban_population_UN+agriculture_employment+state_capacity,
          model = "within", effect = "twoways", data = paneldata)

m2 <- plm(pca_regime_lag~poverty19+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+
            share_urban_population_UN+agriculture_employment+state_capacity,
          model = "within", effect = "twoways", data = paneldata)

ses <- c(list(sqrt(diag(vcovHC(m1, type = "HC1"))),
              sqrt(diag(vcovHC(m2, type = "HC1")))))

stargazer(m1, m2, se = ses)


####shocks: poverty 19 & polity_lag####

mod19pst <- plm(polity2_lag~poverty19+systemic_crisis+poverty19:systemic_crisis,
               data=paneldata, model="within", effect="twoways")
coeftest(mod19pst, vcov=vcovHC, type="HC1")

mod19pb <- plm(polity2_lag~poverty19+banking_crisis+poverty19:banking_crisis,
               data=paneldata, model="within", effect="twoways")
coeftest(mod19pb, vcov=vcovHC, type="HC4")

mod19pn <- plm(polity2_lag~poverty19+nshock+poverty19:nshock,
               data=paneldata, model="within", effect="twoways")
coeftest(mod19pn, vcov=vcovHC, type="HC4")

coeftest(plm(polity_diff_lag~poverty19+I(poverty19^2)+nshock+poverty19:nshock+
               I(poverty19^2):nshock+e_polity2,
             data=paneldata, model="within", effect="twoways"), vcov=vcovHC, type="HC1")

stargazer(plm(polity2_lag~poverty19+I(poverty19^2)+nshock+poverty19:nshock+
                I(poverty19^2):nshock,
              data=paneldata, model="within", effect="twoways"))

mod19pi <- plm(polity2_lag~poverty19+infshock+poverty19:infshock,
               data=paneldata, model="within", effect="twoways")
coeftest(mod19pi, vcov=vcovHC, type="HC0")

se1pl <- c(list(sqrt(diag(vcovHC(mod19pst, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pb, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pn, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pi, type = "HC1")))))

stargazer(mod19pst, mod19pb, mod19pn, mod19pi, se = se1pl)

mod19pn <- plm(polity2_lagged~poverty19+nshock+poverty19:nshock+
                 mean_year_schooling+log(gini_mkt)+state_capacity+share_urban_population_UN+
                 log(population_WB)+log(GDP_pc_ppp),
               data=paneldata, model="within", effect="twoways")
coeftest(mod19pn, vcov=vcovHC, type="HC1")


mod19pi <- plm(polity2_lagged~poverty19+infshock+poverty19:infshock+
                 mean_year_schooling+log(gini_mkt)+state_capacity+share_urban_population_UN,
               data=paneldata, model="within", effect="twoways")
coeftest(mod19pi, vcov=vcovHC, type="HC1")



mod19pst <- plm(polity_ra3_lag~poverty19+systemic_crisis+poverty19:systemic_crisis,
                data=paneldata, model="within", effect="twoways")
coeftest(mod19pst, vcov=vcovHC, type="HC1")

mod19pb <- plm(polity_ra3_lag~poverty19+banking_crisis+poverty19:banking_crisis,
               data=paneldata, model="within", effect="twoways")
coeftest(mod19pb, vcov=vcovHC, type="HC1")

mod19pn <- plm(polity_ra3_lag~poverty19+nshock+poverty19:nshock,
               data=paneldata, model="within", effect="twoways")
coeftest(mod19pn, vcov=vcovHC, type="HC1")

mod19pi <- plm(polity_ra3_lag~poverty19+infshock+poverty19:infshock,
               data=paneldata, model="within", effect="twoways")
coeftest(mod19pi, vcov=vcovHC, type="HC1")

se1pl <- c(list(sqrt(diag(vcovHC(mod19pst, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pb, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pn, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pi, type = "HC1")))))

stargazer(mod19pst, mod19pb, mod19pn, mod19pi, se = se1pl, type="text")


####shocks and regime diff####

mod19pdst <- plm(polity_diff_lagged~e_polity2+poverty19+systemic_crisis+poverty19:systemic_crisis,
                data=paneldata, model="within", effect="twoways")
coeftest(mod19pdst, vcov=vcovHC, type="HC1")

modnpdst <- plm(polity_diff_lagged~e_polity2+poverty_nat+systemic_crisis+
                  poverty_nat:systemic_crisis,
                 data=paneldata, model="within", effect="twoways")
coeftest(modnpdst, vcov=vcovHC, type="HC1")

mod19pdb <- plm(polity_diff_lagged~e_polity2+poverty19+banking_crisis+poverty19:banking_crisis,
               data=paneldata, model="within", effect="twoways")
coeftest(mod19pdb, vcov=vcovHC, type="HC1")

mod19pdn <- plm(polity_diff_lagged~e_polity2+poverty19+nshock+poverty19:nshock,
               data=paneldata, model="within", effect="twoways")
coeftest(mod19pdn, vcov=vcovHC, type="HC1")

mod19pdi <- plm(polity_diff_lagged~e_polity2+poverty19+infshock+poverty19:infshock,
               data=paneldata, model="within", effect="twoways")
coeftest(mod19pdi, vcov=vcovHC, type="HC1")

se1pl <- c(list(sqrt(diag(vcovHC(mod19pdst, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pdb, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pdn, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pdi, type = "HC1")))))

stargazer(mod19pdst, mod19pdb, mod19pdn, mod19pdi, se = se1pl, type="text")

mod19pdst <- plm(polity_diff_lag~e_polity2+poverty19+systemic_crisis+
                   poverty19:systemic_crisis,
                 data=paneldata, model="within", effect="twoways")
coeftest(mod19pdst, vcov=vcovHC, type="HC1")

mod19pdb <- plm(polity_diff_lag~e_polity2+poverty19+banking_crisis+
                  poverty19:banking_crisis,
                data=paneldata, model="within", effect="twoways")
coeftest(mod19pdb, vcov=vcovHC, type="HC1")






mod19pdna <- plm(polity_diff_lag~e_polity2+poverty19+nshock+poverty19:nshock+
                  mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                data=paneldata, model="within", effect="twoways")
#coeftest(mod19pdn, vcov=vcovHC, type="HC1")

mod19pgldna <- plm(polity_diff_lag~poverty_gap_19+nshock+poverty_gap_19:nshock+e_polity2
                  +mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                  data=paneldata, model="within", effect="twoways")
#coeftest(mod19pgldn, vcov=vcovHC, type="HC1")


se1pl <- c(list(sqrt(diag(vcovHC(mod19pdna, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pgldna, type = "HC1")))))

stargazer(mod19pdna, mod19pgldna, se = se1pl)

pp <- as.data.frame(paneldata)
pp$year_f <- as.factor(pp$year)

ggplot(ggeffect(lm(polity_diff_lag~e_polity2+poverty19+nshock+poverty19:nshock+
                     mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f,
                   data=pp), terms = "poverty19 [all]"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratization")+
  xlab("Proportion of population under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()

ggplot(ggeffect(lm(polity2_lag~e_polity2+poverty19+nshock+poverty19:nshock+
                     mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f,
                   data=pp), terms = "poverty19 [all]"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratization")+
  xlab("Proportion of population under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()

ggplot(ggeffect(lm(polity_ra5_lag~e_polity2+poverty19+nshock+poverty19:nshock+
                     mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f,
                   data=pp), terms = "poverty19 [all]"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democracy")+
  xlab("Proportion of population under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()

ggplot(ggeffect(lm(polity_diff_lag~e_polity2+poverty19+systemic_crisis+poverty19:systemic_crisis+
                     mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f,
                   data=pp), terms = "poverty19 [all]"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of systemic crisis on democratization")+
  xlab("Proportion of population under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()

ggplot(ggeffect(lm(polity_diff_lag~e_polity2+poverty19+banking_crisis+poverty19:banking_crisis+
                     mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f,
                   data=pp), terms = "poverty19 [all]"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of inflation shock on democratization")+
  xlab("Proportion of population under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()

####NORM HERE####

mod19pdn <- plm(polity_diff_ra3_lag~e_polity2+poverty19+nshock+poverty19:nshock+
                  mean_year_schooling+log(GDP_pc_ppp),
                data=paneldata, model="within", effect="twoways")
coeftest(mod19pdn, vcov=vcovHC, type="HC1")

mod19pgldn <- plm(polity_diff_ra3_lag~e_polity2+poverty_gap_19+nshock+poverty_gap_19:nshock+
                    mean_year_schooling+log(GDP_pc_ppp),
                  data=paneldata, model="within", effect="twoways")
coeftest(mod19pgldn, vcov=vcovHC, type="HC1")

mod19pcadn <- plm(pca_regime_diff_lag~pca_regime+poverty19+nshock+poverty19:nshock+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+
                    share_urban_population_UN+agriculture_employment,
                data=paneldata, model="within", effect="twoways")
coeftest(mod19pcadn, vcov=vcovHC, type="HC1")

mod19pgcadn <- plm(pca_regime_diff_lag~pca_regime+poverty_gap_19+nshock+poverty_gap_19:nshock+
                     mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+
                     share_urban_population_UN+agriculture_employment,
                  data=paneldata, model="within", effect="twoways")
coeftest(mod19pgcadn, vcov=vcovHC, type="HC1")

se1pl <- c(list(sqrt(diag(vcovHC(mod19pcadn, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pgcadn, type = "HC1")))))

stargazer(mod19pcadn, mod19pgcadn, se = se1pl)
          

se1pl <- c(list(sqrt(diag(vcovHC(mod19pdn, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pgldn, type = "HC1")))))
stargazer(mod19pdn, mod19pgldn, se = se1pl)

stargazer(mod19pdn, mod19pgldn, se = se1pl),
          covariate.labels = c("Polity IV",
                              "Poverty gap under $ 1.9 poverty line",
                               "PolityIV",
                               "Proportion of population under $ 1.9 poverty line",
                               "Negative shock",
                               "Poverty gap under $1.9:Negative shock",
                #               "Population under $3.2 poverty:Negative shock",
                              "Population under $1.9 poverty:Negative shock"),
          notes.label = "Heteroskedasticity-robust standard-errors in parentheses")


mod19pcadn <- plm(pca_regime~poverty19+nshock+poverty19:nshock+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+
                    share_urban_population_UN+agriculture_employment,
                  data=paneldata, model="within", effect="twoways")
coeftest(mod19pcadn, vcov=vcovHC, type="HC1")

mod19pgcadn <- plm(pca_regime_lag~poverty19+nshock+poverty19:nshock+
                     mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+
                     share_urban_population_UN+agriculture_employment,
                   data=paneldata, model="within", effect="twoways")
coeftest(mod19pgcadn, vcov=vcovHC, type="HC1")

mod19pcadnn <- plm(pca_ra3_lag~poverty19+nshock+poverty19:nshock+
                    mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+
                    share_urban_population_UN+agriculture_employment,
                  data=paneldata, model="within", effect="twoways")

se1pl <- c(list(sqrt(diag(vcovHC(mod19pcadn, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pgcadn, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pcadnn, type = "HC1")))))

stargazer(mod19pcadn, mod19pgcadn,mod19pcadnn, se = se1pl)


pdf <- as.data.frame(paneldata)
pdf$c_name_f <- as.factor(pdf$c_name)
pdf$year_f <- as.factor(pdf$year)


p19blm <- lm(polity_diff_lag~e_polity2+poverty19+nshock+poverty19:nshock+c_name_f+year_f,
             data=pdf)
ggeffect(p19blm, terms="poverty19")
mydf <- ggeffect(p19blm, terms = "poverty19")
p1 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratization")+
  xlab("Proportion of population under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()
pg19blm <- lm(polity_diff_lag~e_polity2+poverty_gap_19+nshock+poverty_gap_19:nshock+c_name_f+year_f,
             data=pdf)
ggeffect(pg19blm, terms="poverty_gap_19")
mydf <- ggeffect(pg19blm, terms = "poverty_gap_19")
p2 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratization")+
  xlab("Poverty gap under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()

ggarrange(p1, p2, ncol=2)


mod19pdn <- plm(polity_diff_lag~e_polity2+poverty19+nshock+poverty19:nshock,
                data=paneldata, model="within", effect="twoways")
coeftest(mod19pdn, vcov=vcovHC, type="HC1")

mod32pdn <- plm(polity_diff_lag~e_polity2+poverty32+nshock+poverty32:nshock,
                data=paneldata, model="within", effect="twoways")
coeftest(mod32pdn, vcov=vcovHC, type="HC1")

mod55pdn <- plm(polity_diff_lag~e_polity2+poverty55+nshock+poverty55:nshock,
                data=paneldata, model="within", effect="twoways")
coeftest(mod55pdn, vcov=vcovHC, type="HC1")

se1pl <- c(list(sqrt(diag(vcovHC(mod19pdn, type = "HC1"))),
                sqrt(diag(vcovHC(mod32pdn, type = "HC1"))),
                sqrt(diag(vcovHC(mod55pdn, type = "HC1")))))

stargazer(mod19pdn, mod32pdn, mod55pdn, se = se1pl,
          covariate.labels = c("Polity IV",
                               "Proportion of population under $ 1.9 poverty line",
                               "Proportion of population under $ 3.2 poverty line",
                               "Proportion of population under $ 5.5 poverty line",
                               "Negative shock",
                               "Population under $1.9 poverty:Negative shock",
                               "Population under $3.2 poverty:Negative shock",
                               "Population under $5.5 poverty:Negative shock"),
          notes.label = "Heteroskedasticity-robust standard-errors in parentheses")

pdf <- as.data.frame(paneldata)
pdf$c_name_f <- as.factor(pdf$c_name)
pdf$year_f <- as.factor(pdf$year)


p19blm <- lm(polity_diff_lag~e_polity2+poverty19+nshock+poverty19:nshock+c_name_f+year_f,
             data=pdf)
ggeffect(p19blm, terms="poverty19")
mydf <- ggeffect(p19blm, terms = "poverty19")
p1 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratization")+
  xlab("Proportion of population under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()


p32blm <- lm(polity_diff_lag~e_polity2+poverty32+nshock+poverty32:nshock+c_name_f+year_f,
             data=pdf)
ggeffect(p32blm, terms="poverty32")
mydf <- ggeffect(p32blm, terms = "poverty32")
p2 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratization")+
  xlab("Proportion of population under the $3.2 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()


p55blm <- lm(polity_diff_lag~e_polity2+poverty55+nshock+poverty55:nshock+c_name_f+year_f,
             data=pdf)
ggeffect(p55blm, terms="poverty55")
mydf <- ggeffect(p55blm, terms = "poverty55")
p3 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratization")+
  xlab("Proportion of population under the $5.5 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()

ggarrange(p1, p2, p3, ncol=3)


mod19rapdn <- plm(polity_diff_ra3_lag~e_polity2+poverty19+nshock+poverty19:nshock,
                data=paneldata, model="within", effect="twoways")
coeftest(mod19rapdn, vcov=vcovHC, type="HC1")

mod32rapdn <- plm(polity_diff_ra3_lag~e_polity2+poverty32+nshock+poverty32:nshock,
                data=paneldata, model="within", effect="twoways")
coeftest(mod32rapdn, vcov=vcovHC, type="HC1")

mod55rapdn <- plm(polity_diff_ra3_lag~e_polity2+poverty55+nshock+poverty55:nshock,
                data=paneldata, model="within", effect="twoways")
coeftest(mod55rapdn, vcov=vcovHC, type="HC1")

se1pl <- c(list(sqrt(diag(vcovHC(mod19rapdn, type = "HC1"))),
                sqrt(diag(vcovHC(mod32rapdn, type = "HC1"))),
                sqrt(diag(vcovHC(mod55rapdn, type = "HC1")))))
stargazer(mod19rapdn, mod32rapdn, mod55rapdn, se = se1pl,
          covariate.labels = c("Polity IV",
                               "Proportion of population under $ 1.9 poverty line",
                               "Proportion of population under $ 3.2 poverty line",
                               "Proportion of population under $ 5.5 poverty line",
                               "Negative shock",
                               "Population under $1.9 poverty:Negative shock",
                               "Population under $3.2 poverty:Negative shock",
                               "Population under $5.5 poverty:Negative shock"),
          notes.label = "Heteroskedasticity-robust standard-errors in parentheses")

p19blm <- lm(polity_diff_ra3_lag~e_polity2+poverty19+nshock+poverty19:nshock+c_name_f+year_f,
             data=pdf)
ggeffect(p19blm, terms="poverty19")
mydf <- ggeffect(p19blm, terms = "poverty19")
p1 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratization")+
  xlab("Proportion of population under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()


p32blm <- lm(polity_diff_ra3_lag~e_polity2+poverty32+nshock+poverty32:nshock+c_name_f+year_f,
             data=pdf)
ggeffect(p32blm, terms="poverty32")
mydf <- ggeffect(p32blm, terms = "poverty32")
p2 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratization")+
  xlab("Proportion of population under the $3.2 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()


p55blm <- lm(polity_diff_ra3_lag~e_polity2+poverty55+nshock+poverty55:nshock+c_name_f+year_f,
             data=pdf)
ggeffect(p55blm, terms="poverty55")
mydf <- ggeffect(p55blm, terms = "poverty55")
p3 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democratization")+
  xlab("Proportion of population under the $5.5 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()

ggarrange(p1, p2, p3, ncol=3)


mod19pldn <- plm(polity2_lag~poverty19+nshock+poverty19:nshock,
                data=paneldata, model="within", effect="twoways")
coeftest(mod19pldn, vcov=vcovHC, type="HC1")

mod19p3dn <- plm(polity_ra3_lag~poverty19+nshock+poverty19:nshock,
                data=paneldata, model="within", effect="twoways")
coeftest(mod19p3dn, vcov=vcovHC, type="HC1")

mod19p5dn <- plm(polity_ra5_lag~poverty19+nshock+poverty19:nshock,
                data=paneldata, model="within", effect="twoways")
coeftest(mod19p5dn, vcov=vcovHC, type="HC1")

se1pl <- c(list(sqrt(diag(vcovHC(mod19pldn, type = "HC1"))),
                sqrt(diag(vcovHC(mod19p3dn, type = "HC1"))),
                sqrt(diag(vcovHC(mod19p5dn, type = "HC1")))))

stargazer(mod19pldn, mod19p3dn, mod19p5dn, se = se1pl,
          covariate.labels = c("Proportion of population under $ 1.9 poverty line",
                               "Negative shock",
                               "Population under $1.9 poverty:Negative shock"),
          notes.label = "Heteroskedasticity-robust standard-errors in parentheses")



p19blm <- lm(polity2_lag~poverty19+nshock+poverty19:nshock+c_name_f+year_f,
             data=pdf)
ggeffect(p19blm, terms="poverty19")
mydf <- ggeffect(p19blm, terms = "poverty19")
p1 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democracy")+
  xlab("Proportion of population under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()


p32blm <- lm(polity_ra3_lag~poverty19+nshock+poverty19:nshock+c_name_f+year_f,
             data=pdf)
ggeffect(p32blm, terms="poverty19")
mydf <- ggeffect(p32blm, terms = "poverty19")
p2 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democracy (ra 3)")+
  xlab("Proportion of population under the $3.2 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()


p55blm <- lm(polity_ra5_lag~poverty19+nshock+poverty19:nshock+c_name_f+year_f,
             data=pdf)
ggeffect(p55blm, terms="poverty19")
mydf <- ggeffect(p55blm, terms = "poverty19")
p3 <- ggplot(mydf, aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  ylab("Marginal effect of income shock on democracy (ra 5)")+
  xlab("Proportion of population under the $1.9 poverty line")+
  #ggtitle("Marginal effect of poverty")+
  theme_light()

ggarrange(p1, p2, p3, ncol=3)



########

mod19pcadn <- plm(pca_regime_diff~e_polity2+poverty19+nshock+poverty19:nshock,
                data=paneldata, model="within", effect="twoways")
coeftest(mod19pcadn, vcov=vcovHC, type="HC1")

mod32pcadn <- plm(pca_regime_diff~e_polity2+poverty32+nshock+poverty32:nshock,
                data=paneldata, model="within", effect="twoways")
coeftest(mod32pcadn, vcov=vcovHC, type="HC1")

mod55pcadn <- plm(pca_regime_diff~e_polity2+poverty55+nshock+poverty55:nshock,
                data=paneldata, model="within", effect="twoways")
coeftest(mod55pcadn, vcov=vcovHC, type="HC1")

se1pl <- c(list(sqrt(diag(vcovHC(mod19pcadn, type = "HC1"))),
                sqrt(diag(vcovHC(mod32pcadn, type = "HC1"))),
                sqrt(diag(vcovHC(mod55pcadn, type = "HC1")))))

stargazer(mod19pcadn, mod32pcadn, mod55pcadn, se = se1pl,
          covariate.labels = c("Polity IV",
                               "Proportion of population under $ 1.9 poverty line",
                               "Proportion of population under $ 3.2 poverty line",
                               "Proportion of population under $ 5.5 poverty line",
                               "Negative shock",
                               "Population under $1.9 poverty:Negative shock",
                               "Population under $3.2 poverty:Negative shock",
                               "Population under $5.5 poverty:Negative shock"),
          notes.label = "Heteroskedasticity-robust standard-errors in parentheses")


mod19pdi <- plm(polity_diff_lag~e_polity2+poverty19+infshock+poverty19:infshock,
                data=paneldata, model="within", effect="twoways")
coeftest(mod19pdi, vcov=vcovHC, type="HC1")

se1pl <- c(list(sqrt(diag(vcovHC(mod19pdst, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pdb, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pdn, type = "HC1"))),
                sqrt(diag(vcovHC(mod19pdi, type = "HC1")))))

stargazer(mod19pdst, mod19pdb, mod19pdn, mod19pdi, se = se1pl)



####poverty dynamics and regime: long term poverty trends####

mod19d5 <- plm(e_polity2~poverty19_diff_ra10+
                 log(population_WB)+log(GDP_pc_ppp)+log(gini_mkt),
               data=paneldata, model="within", effect="twoways")
coeftest(mod19d5, vcov=vcovHC, type="HC1")

mod19d5 <- plm(polity_diff_ra5~poverty19_diff_ra10+
                 log(population_WB)+log(GDP_pc_ppp)+log(gini_mkt),
               data=paneldata, model="within", effect="twoways")
coeftest(mod19d5, vcov=vcovHC, type="HC1")

ggplot(data=data.frame(paneldata), aes(x=poverty19_diff_ra5, y=e_polity2))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()

####mediation package####

data <- drop_na(paneldata, poverty19, nshock, mean_year_schooling, GDP_pc_ppp, gini_mkt,
                share_urban_population_UN, e_polity2, agriculture_employment, polity_ra3_lag)
data <- data %>% 
  dplyr::select(c_name, year, poverty19, mean_year_schooling, GDP_pc_ppp, gini_mkt, nshock,
                share_urban_population_UN, e_polity2, polity2_lag, polity_diff, polity_diff_lag,
                agriculture_employment, polity_ra3_lag)

modtest <- plm(poverty19~nshock+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
               model = "within", effect = "twoways", data = data)


ppoverty <- data.frame(ppoverty = modtest$model[[1]] - modtest$residuals)

model_data <- cbind(as.data.frame(as.matrix(modtest$model)),ppoverty) 
data <- cbind(data, model_data)  
data <- data[,!duplicated(colnames(data))]
data <- unique(data)
data$year_f <- as.factor(data$year)
data$loggdp <- log(data$GDP_pc_ppp)


m1 <- lm(share_urban_population_UN~ppoverty+mean_year_schooling+loggdp+
                        gini_mkt+c_name+year_f, data=data)
m2 <- lm(polity2_lag~ppoverty+share_urban_population_UN+ppoverty:share_urban_population_UN+
           mean_year_schooling+loggdp+
                        gini_mkt+c_name+year_f, data=data)

summary(plm(polity2_lag~ppoverty+mean_year_schooling+loggdp+
              gini_mkt, model = "within", effect = "twoways", data=data))
summary(plm(share_urban_population_UN~ppoverty+mean_year_schooling+loggdp+
             gini_mkt, model = "within", effect = "twoways", data=data))
summary(plm(e_polity2~ppoverty+share_urban_population_UN+ppoverty:share_urban_population_UN+
              mean_year_schooling+loggdp+
              gini_mkt, model = "within", effect = "twoways", data=data))


summary(plm(polity2_lag~ppoverty+mean_year_schooling+loggdp+
              gini_mkt, model = "within", effect = "twoways", data=data))
summary(plm(v~ppoverty+mean_year_schooling+loggdp+
              gini_mkt, model = "within", effect = "twoways", data=data))
summary(plm(polity2_lag~ppoverty+agriculture_employment+ppoverty:agriculture_employment+
              mean_year_schooling+loggdp+
              gini_mkt, model = "within", effect = "twoways", data=data))


results <- mediate(model.m = m1, model.y = m2,
                  treat='ppoverty', mediator='share_urban_population_UN')

summary(results)
plot(results)

modelsummary::modelsummary(results)


m1e <- lm(agriculture_employment~ppoverty+mean_year_schooling+loggdp+
           gini_mkt+c_name+year_f, data=data)
m2e <- lm(polity2_lag~ppoverty+agriculture_employment+ppoverty:agriculture_employment+
           mean_year_schooling+loggdp+
           gini_mkt+c_name+year_f, data=data)

resultse <- mediate(model.m = m1e, model.y = m2e,
                    treat='ppoverty', mediator='agriculture_employment')

summary(resultse)



m21 <- lm(polity2_lag~ppoverty+share_urban_population_UN+mean_year_schooling+log(GDP_pc_ppp)+
            gini_mkt+c_name+year_f, data=data)

results12 <- mediate(model.m = m1, model.y = m21,
                   treat='ppoverty', mediator='share_urban_population_UN')

summary(results12)
plot(results12)

m23 <- lm(polity_ra3_lag~ppoverty+share_urban_population_UN+mean_year_schooling+log(GDP_pc_ppp)+
            gini_mkt+c_name+year_f, data=data)

results123 <- mediate(model.m = m1, model.y = m23,
                     treat='ppoverty', mediator='share_urban_population_UN')

summary(results123)
plot(results12)

m12e <- lm(agriculture_employment~poverty19+mean_year_schooling+log(GDP_pc_ppp)+
            gini_mkt+c_name+year_f, data=data)
m22e <- lm(e_polity2~poverty19+agriculture_employment+
             mean_year_schooling+log(GDP_pc_ppp)+
            gini_mkt+c_name+year_f, data=data)

results2e <- mediate(model.m = m12e, model.y = m22e,
                    treat='poverty19', mediator='agriculture_employment')
summary(results2e)
plot(results2e)
stargazer(results2e)

m1e <- lm(share_urban_population_UN~poverty19+mean_year_schooling+log(GDP_pc_ppp)+
             gini_mkt+c_name+year_f, data=data)
m2e <- lm(e_polity2~poverty19+share_urban_population_UN+mean_year_schooling+log(GDP_pc_ppp)+
             gini_mkt+c_name+year_f, data=data)

resultse <- mediate(model.m = m1e, model.y = m2e,
                     treat='poverty19', mediator='share_urban_population_UN', robustSE = T)
summary(resultse)
plot(resultse)
stargazer(resultse)

####mediation package 2####

data <- drop_na(paneldata, poverty19, nshock, mean_year_schooling, GDP_pc_ppp, gini_mkt,
                gwf_fail)
data <- data %>% 
  dplyr::select(c_name, year, poverty19, mean_year_schooling, GDP_pc_ppp, gini_mkt, nshock,
                e_polity2, 
                v2x_partipdem, v2x_partip, v2x_cspart, gwf_fail, gwf_fail_lag)

modtest <- plm(poverty19~nshock+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
               model = "within", effect = "twoways", data = data)


ppoverty <- data.frame(ppoverty = modtest$model[[1]] - modtest$residuals)

model_data <- cbind(as.data.frame(as.matrix(modtest$model)),ppoverty) 
data <- cbind(data, model_data)  
data <- data[,!duplicated(colnames(data))]
data <- unique(data)
data$year_f <- as.factor(data$year)
data$loggdp <- log(data$GDP_pc_ppp)

data <- drop_na(data, gwf_fail_lag,v2x_partip)

m1 <- lm(v2x_partip~ppoverty+mean_year_schooling+loggdp+
           gini_mkt+c_name+year_f, data=data)
m2 <- lm(gwf_fail_lag~ppoverty+v2x_partip+
           mean_year_schooling+loggdp+
           gini_mkt+c_name+year_f, data=data)

summary(plm(gwf_fail~ppoverty+mean_year_schooling+loggdp+
              gini_mkt, model = "within", effect = "twoways", data=data))
summary(plm(v2x_partip~ppoverty+mean_year_schooling+loggdp+
              gini_mkt, model = "within", effect = "twoways", data=data))
summary(plm(gwf_fail~ppoverty+v2x_partip+
              mean_year_schooling+loggdp+
              gini_mkt, model = "within", effect = "twoways", data=data))

results <- mediate(model.m = m1, model.y = m2,
                   treat='ppoverty', mediator='v2x_partip')

summary(results)
plot(results)



##

m12 <- lm(agriculture_employment~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+
           gini_mkt+c_name+year_f, data=data)
m22 <- lm(e_polity2~ppoverty+agriculture_employment+mean_year_schooling+log(GDP_pc_ppp)+
           gini_mkt+c_name+year_f, data=data)

results2 <- mediate(model.m = m12, model.y = m22,
                   treat='ppoverty', mediator='agriculture_employment')

summary(results2)
plot(results2)

m222 <- lm(polity2_lag~ppoverty+agriculture_employment+mean_year_schooling+log(GDP_pc_ppp)+
             gini_mkt+c_name+year_f, data=data)

results22 <- mediate(model.m = m12, model.y = m222,
                    treat='ppoverty', mediator='agriculture_employment')

summary(results22)

m223 <- lm(polity_ra3_lag~ppoverty+agriculture_employment+mean_year_schooling+log(GDP_pc_ppp)+
             gini_mkt+c_name+year_f, data=data)

results223 <- mediate(model.m = m12, model.y = m223,
                     treat='ppoverty', mediator='agriculture_employment')

summary(results223)



#### mediation sorta ####

data <- drop_na(paneldata, poverty19, nshock, mean_year_schooling, GDP_pc_ppp, gini_mkt,
                agriculture_employment)
data <- data %>% 
  dplyr::select(c_name, year, poverty19, nshock, mean_year_schooling, GDP_pc_ppp, gini_mkt,
               share_urban_population_UN, e_polity2, polity2_lag, polity_diff, polity_diff_lag,
               agriculture_employment)

modtest <- plm(poverty19~nshock+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
               model = "within", effect = "twoways", data = data)


ppoverty <- data.frame(ppoverty = modtest$model[[1]] - modtest$residuals)

model_data <- cbind(as.data.frame(as.matrix(modtest$model)),ppoverty) 
data <- cbind(data, model_data)  
data <- data[,!duplicated(colnames(data))]
data <- unique(data)

modtest2 <- plm(share_urban_population_UN~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                model = "within", effect = "twoways", data = data)

pshare <- data.frame(pshare = modtest2$model[[1]] - modtest2$residuals)

model_data <- cbind(as.data.frame(as.matrix(modtest2$model)),pshare) 
data <- cbind(data, model_data)  
data <- data[,!duplicated(colnames(data))]
data <- unique(data)




modtest3 <- plm(e_polity2~pshare+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                model = "within", effect = "twoways", data = data)
summary(modtest3)  

modtest3lag <- plm(polity2_lag~pshare+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                model = "within", effect = "twoways", data = data)
summary(modtest3lag)  
  
modtest3diff <- plm(polity_diff~pshare+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                model = "within", effect = "twoways", data = data)
summary(modtest3diff) 

modtest3difflag <- plm(polity_diff_lag~pshare+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                    model = "within", effect = "twoways", data = data)
summary(modtest3difflag) 



modtest2 <- plm(agriculture_employment~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                model = "within", effect = "twoways", data = data)

pemp <- data.frame(pemp = modtest2$model[[1]] - modtest2$residuals)

model_data <- cbind(as.data.frame(as.matrix(modtest2$model)),pemp) 
data <- cbind(data, model_data)  
data <- data[,!duplicated(colnames(data))]
data <- unique(data)




modtest3 <- plm(e_polity2~pemp+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                model = "within", effect = "twoways", data = data)
summary(modtest3)  

modtest3lag <- plm(polity2_lag~pshare+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                   model = "within", effect = "twoways", data = data)
summary(modtest3lag)  

modtest3diff <- plm(polity_diff~pshare+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                    model = "within", effect = "twoways", data = data)
summary(modtest3diff) 

modtest3difflag <- plm(polity_diff_lag~pshare+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                       model = "within", effect = "twoways", data = data)
summary(modtest3difflag) 


pca_res <- prcomp(data[c("poverty19", "share_urban_population_UN", "agriculture_employment")],
                  center = TRUE, scale = TRUE)
summary(pca_res)

eig.value <- get_eigenvalue(pca_res)
eig.value

pca_res$rotation[,1:2]

fviz_contrib(pca_res, choice = "var", axes = 1, top = 10)
fviz_contrib(pca_res, choice = "var", axes = 2, top = 10)

fviz_pca_var(pca_res, col.var = "contrib", repel = TRUE)          

eig.value
fviz_eig(pca_res, addlabels = TRUE)

data$pca_poverty <- 0.5592480*data$poverty19-0.5723143*data$share_urban_population_UN+
  0.5997483*data$agriculture_employment

summary(plm(e_polity2~poverty19+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = data))

summary(plm(pca_poverty~poverty19+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = data))

summary(plm(e_polity2~pca_poverty+poverty19+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = data))

summary(plm(polity2_lag~poverty19+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = data))

summary(plm(pca_poverty~poverty19+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = data))

summary(plm(polity2_lag~pca_poverty+poverty19+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = data))


summary(plm(polity_diff~pca_poverty+nshock+pca_poverty:nshock+
              mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = data))

summary(plm(polity_diff_lag~pca_poverty+nshock+pca_poverty:nshock+
              mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = data))

dat <- data.frame(data)
dat$year_f <- as.factor(dat$year)
ggeffect(lm(polity_diff_lag~pca_poverty+nshock+pca_poverty:nshock+
              mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f, data = dat),
         terms = "pca_poverty")
ggplot(ggeffect(lm(polity_diff_lag~pca_poverty+nshock+pca_poverty:nshock+
                     mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f, data = dat),
                terms = "pca_poverty"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  theme_light()


ggplot(ggeffect(lm(polity2_lag~pca_poverty+nshock+pca_poverty:nshock+
                     mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f, data = dat),
                terms = "pca_poverty"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  theme_light()

ggplot(ggeffect(lm(polity_ra3_lag~pca_poverty+nshock+pca_poverty:nshock+
                     mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f, data = dat),
                terms = "pca_poverty"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  theme_light()


####mediation sorta *2####

data <- drop_na(paneldata, poverty19, nshock, mean_year_schooling, GDP_pc_ppp, gini_mkt)
data <- data %>% 
  dplyr::select(c_name, year, poverty19, nshock, mean_year_schooling, GDP_pc_ppp, gini_mkt,
                share_urban_population_UN, e_polity2, polity2_lag, polity_diff, polity_diff_lag)

modtest <- plm(share_urban_population_UN~nshock+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
               model = "within", effect = "twoways", data = data)
summary(modtest)

pshare <- data.frame(pshare = modtest$model[[1]] - modtest$residuals)

model_data <- cbind(as.data.frame(as.matrix(modtest$model)),pshare) 
data <- cbind(data, model_data)  
data <- data[,!duplicated(colnames(data))]
data <- unique(data)

modtest2 <- plm(poverty19~pshare+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                model = "within", effect = "twoways", data = data)
summary(modtest2)

ppoverty <- data.frame(ppoverty = modtest2$model[[1]] - modtest2$residuals)

model_data <- cbind(as.data.frame(as.matrix(modtest2$model)),ppoverty) 
data <- cbind(data, model_data)  
data <- data[,!duplicated(colnames(data))]
data <- unique(data)


modtest3 <- plm(e_polity2~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                model = "within", effect = "twoways", data = data)
summary(modtest3)  

coeftest(modtest3, vcov = vcovHC, type = "HC1")

modtest3lag <- plm(polity2_lag~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                   model = "within", effect = "twoways", data = data)
summary(modtest3lag)  

coeftest(modtest3lag, vcov = vcovHC, type = "HC1")

modtest3diff <- plm(polity_diff~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                    model = "within", effect = "twoways", data = data)
summary(modtest3diff) 

modtest3difflag <- plm(polity_diff_lag~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                       model = "within", effect = "twoways", data = data)
summary(modtest3difflag) 


####mediation sorta *3####

data <- drop_na(paneldata, poverty19, nshock, mean_year_schooling, GDP_pc_ppp, gini_mkt,
                agriculture_employment)
data <- data %>% 
  dplyr::select(c_name, year, poverty19, nshock, mean_year_schooling, GDP_pc_ppp, gini_mkt,
                share_urban_population_UN, e_polity2, polity2_lag, polity_diff, polity_diff_lag,
                agriculture_employment)

modtest <- plm(agriculture_employment~nshock+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
               model = "within", effect = "twoways", data = data)
summary(modtest)

pemp <- data.frame(pemp = modtest$model[[1]] - modtest$residuals)

model_data <- cbind(as.data.frame(as.matrix(modtest$model)),pemp) 
data <- cbind(data, model_data)  
data <- data[,!duplicated(colnames(data))]
data <- unique(data)

modtest2 <- plm(poverty19~pemp+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                model = "within", effect = "twoways", data = data)
summary(modtest2)

ppoverty <- data.frame(ppoverty = modtest2$model[[1]] - modtest2$residuals)

model_data <- cbind(as.data.frame(as.matrix(modtest2$model)),ppoverty) 
data <- cbind(data, model_data)  
data <- data[,!duplicated(colnames(data))]
data <- unique(data)


modtest3 <- plm(e_polity2~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                model = "within", effect = "twoways", data = data)
summary(modtest3)  

coeftest(modtest3, vcov = vcovHC, type = "HC1")

modtest3lag <- plm(polity2_lag~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                   model = "within", effect = "twoways", data = data)
summary(modtest3lag)  

coeftest(modtest3lag, vcov = vcovHC, type = "HC1")

modtest3diff <- plm(polity_diff~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                    model = "within", effect = "twoways", data = data)
summary(modtest3diff) 

modtest3difflag <- plm(polity_diff_lag~ppoverty+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
                       model = "within", effect = "twoways", data = data)
summary(modtest3difflag) 

####one more way####

summary(plm(e_polity2~poverty19+agriculture_employment+poverty19:agriculture_employment+
              mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data=data))

dat <- data.frame(data)
dat$year_f <- as.factor(dat$year)
  
ggeffect(lm(e_polity2~poverty19+agriculture_employment+poverty19:agriculture_employment+
              mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f, data=dat),
         terms = "poverty19")

ggplot(ggeffect(lm(e_polity2~poverty19+agriculture_employment+poverty19:agriculture_employment+
                     mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f, data=dat),
                terms = "poverty19"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  theme_light()


ggeffect(lm(polity2_lag~poverty19+agriculture_employment+poverty19:agriculture_employment+
              mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f, data=dat),
         terms = "poverty19")

ggplot(ggeffect(lm(polity2_lag~poverty19+agriculture_employment+poverty19:agriculture_employment+
                     mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f, data=dat),
                terms = "poverty19"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  theme_light()


ggeffect(lm(e_polity2~poverty19+share_urban_population_UN+poverty19:share_urban_population_UN+
              mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f, data=dat),
         terms = "poverty19")

ggplot(ggeffect(lm(e_polity2~poverty19+share_urban_population_UN+
                     poverty19:share_urban_population_UN+
                     mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f, data=dat),
                terms = "poverty19"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  theme_light()


ggeffect(lm(polity2_lag~poverty19+share_urban_population_UN+poverty19:share_urban_population_UN+
              mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f, data=dat),
         terms = "poverty19")

ggplot(ggeffect(lm(polity2_lag~poverty19+share_urban_population_UN+
                     poverty19:share_urban_population_UN+
                     mean_year_schooling+log(GDP_pc_ppp)+gini_mkt+c_name+year_f, data=dat),
                terms = "poverty19"), aes(x, predicted)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  theme_light()

####failed democratization####

paneldata$DD_lag <- lag(paneldata$DD, 1)
paneldata$DD_diff <- paneldata$DD - paneldata$DD_lag

paneldata$democ <- ifelse(paneldata$polity_diff_lag >= 6, 1, 0)

table(drop_na(paneldata, poverty19)$democ)


summary(plm(v2x_partipdem~poverty19+polity_diff_lag+poverty19:polity_diff_lag+
              mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = paneldata))

summary(plm(v2x_partip~poverty19+polity_diff_lag+poverty19:polity_diff_lag+
              mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = paneldata))

summary(plm(v2x_cspart~poverty19+polity_diff_lag+poverty19:polity_diff_lag+
              mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
            model = "within", effect = "twoways", data = paneldata))

####participation mdeiation####

data <- drop_na(paneldata, poverty19, nshock, mean_year_schooling, GDP_pc_ppp, gini_mkt)
data <- data %>% 
  dplyr::select(c_name, year, poverty19, mean_year_schooling, GDP_pc_ppp, gini_mkt, nshock,
                e_polity2,  
                v2x_partipdem, v2x_partip, v2x_cspart, share_urban_population_UN,
                agriculture_employment, state_capacity)

modtest <- plm(poverty19~nshock+mean_year_schooling+log(GDP_pc_ppp)+gini_mkt,
               model = "within", effect = "twoways", data = data)

coeftest(modtest, vcov = vcovHC, type = "HC1")

ppoverty <- data.frame(ppoverty = modtest$model[[1]] - modtest$residuals)

model_data <- cbind(as.data.frame(as.matrix(modtest$model)),ppoverty) 
data <- cbind(data, model_data)  
data <- data[,!duplicated(colnames(data))]
data <- unique(data)
data$year_f <- as.factor(data$year)
data$loggdp <- log(data$GDP_pc_ppp)

data <- drop_na(data, v2x_partip, e_polity2)

m1 <- lm(v2x_partip~ppoverty+mean_year_schooling+loggdp+
           gini_mkt+c_name+year_f, data=data)
m2 <- lm(e_polity2~ppoverty+v2x_partip+
           mean_year_schooling+loggdp+
           gini_mkt+c_name+year_f, data=data)


results <- mediation::mediate(model.m = m1, model.y = m2,
                              treat='ppoverty', mediator='v2x_partip')

summary(results)
plot(results)

modelsummary::modelsummary(results, output = "table.tex")

ms <- medsens(results, effect.type = "both")
plot(ms)
summary(ms)

GGally::ggpairs(dplyr::select(data, poverty19, e_polity2, v2x_partip, mean_year_schooling, 
                              GDP_pc_ppp, gini_mkt))


summary(plm(e_polity2~ppoverty+
              mean_year_schooling+loggdp+
              gini_mkt,
            model="within", effect = "twoways", data=data))
summary(plm(v2x_partip~ppoverty+mean_year_schooling+loggdp+
              gini_mkt,
            model="within", effect = "twoways", data=data))
summary(plm(e_polity2~ppoverty+v2x_partip+
              mean_year_schooling+loggdp+
              gini_mkt,
            model="within", effect = "twoways", data=data))

m1 <- plm(e_polity2~ppoverty+
            mean_year_schooling+loggdp+
            gini_mkt,
          model="within", effect = "twoways", data=data)
m2 <- plm(v2x_partip~ppoverty+mean_year_schooling+loggdp+
            gini_mkt,
          model="within", effect = "twoways", data=data)

m3 <- plm(e_polity2~ppoverty+v2x_partip+
            mean_year_schooling+loggdp+
            gini_mkt,
          model="within", effect = "twoways", data=data)

ses <- c(list(sqrt(diag(vcovHC(m1, type = "HC1"))),
              sqrt(diag(vcovHC(m2, type = "HC1"))),
              sqrt(diag(vcovHC(m3, type = "HC1")))))

stargazer(m1,m2,m3, se = ses)  

####cases####

pp <- dplyr::filter(paneldata, c_code == "GNB")
pp <- data.frame(pp)

pp$year <- as.numeric(as.character(pp$year))
pp <- dplyr::filter(pp, year >= 1974)

ggplot(pp[pp$year > 1985 & pp$year < 2011,], )+
  #geom_point()+
  geom_line(aes(x=year, y=e_polity2))+
  ylim(-10,10)+
  #geom_vline(xintercept=1992, col = "red")+
  geom_rect(aes(xmin=1992, xmax=1994, ymin=-Inf, ymax=Inf), fill = "red", alpha = 0.005)+
  #geom_vline(xintercept=1994, col = "red")+
  #geom_vline(xintercept=1998, col = "red")+
  geom_rect(aes(xmin=1998, xmax=2000, ymin=-Inf, ymax=Inf), fill = "red", alpha = 0.005)+
  ylab("PolityIV")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pp$poverty19
pp$v2x_partip

ggplot(pp, )+
  #geom_point()+
  geom_line(aes(x=year, y=poverty19))+
  theme_bw()

####

ppm <- dplyr::filter(paneldata, nshock == 1)
ppm$year <- as.numeric(as.character(ppm$year))


ppm <- dplyr::select(ppm, c_name, year, poverty19, e_polity2, polity_diff, polity_diff_lag)

ppmm <- dplyr::filter(paneldata, c_code == "MRT")
ppmm$year <- as.numeric(as.character(ppmm$year))

ggplot(ppmm[ppmm$year > 1998 & ppm$year < 2018,], )+
  #geom_point()+
  geom_line(aes(x=year, y=e_polity2))+
  #geom_rect(aes(xmin=1964, xmax=1966, ymin=-Inf, ymax=Inf), fill = "red", alpha = 0.005)+
  #geom_rect(aes(xmin=1974, xmax=1976, ymin=-Inf, ymax=Inf), fill = "red", alpha = 0.005)+
  geom_rect(aes(xmin=2005, xmax=2007, ymin=-Inf, ymax=Inf), fill = "red", alpha = 0.005)+
  geom_rect(aes(xmin=2009, xmax=2011, ymin=-Inf, ymax=Inf), fill = "red", alpha = 0.005)+
  geom_rect(aes(xmin=2013, xmax=2014, ymin=-Inf, ymax=Inf), fill = "red", alpha = 0.005)+
  ylab("PolityIV")+
  ylim(-10,10)+
  theme_bw()


ppmm <- dplyr::filter(paneldata, c_code == "HTI")
ppmm$year <- as.numeric(as.character(ppmm$year))

ggplot(ppmm[ppmm$year > 1985 & ppmm$year < 2018,], )+
  #geom_point()+
  geom_line(aes(x=year, y=e_polity2))+
  #geom_rect(aes(xmin=1962, xmax=1963, ymin=-Inf, ymax=Inf), fill = "red", alpha = 0.005)+
  #geom_rect(aes(xmin=1974, xmax=1976, ymin=-Inf, ymax=Inf), fill = "red", alpha = 0.005)+
  geom_rect(aes(xmin=1993, xmax=1996, ymin=-Inf, ymax=Inf), fill = "red", alpha = 0.005)+
  #geom_rect(aes(xmin=2009, xmax=2011, ymin=-Inf, ymax=Inf), fill = "red", alpha = 0.005)+
  #geom_rect(aes(xmin=2013, xmax=2014, ymin=-Inf, ymax=Inf), fill = "red", alpha = 0.005)+
  ylab("PolityIV")+
  ylim(-10,10)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ppmm$v2x_partip
ppmm$v2x_cspart
ppmm$v2x_partipdem
ppmm$poverty19

####

autocs <- dplyr::filter(paneldata, DD == 0)
autocs <- drop_na(autocs, poverty19)
autocs <- dplyr::select(autocs, c_name, year, poverty19, e_polity2)
autocs2 <- dplyr::select(paneldata, c_name, year, poverty19, e_polity2, v2x_partip)


p <- paneldata
p$year <- as.numeric(as.character(p$year))
p <- p[p$year >= 2009,]
p <- dplyr::select(p, v2x_partip, v2x_partipdem)
p <- na.omit(p)
mean(p$v2x_partip)
mean(p$v2x_partipdem)
