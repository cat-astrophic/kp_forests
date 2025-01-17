# Kyoto Protocol and forest cover

# Loading libraries

library(modelsummary)
library(countrycode)
library(stargazer)
library(sandwich)
library(leaflet)
library(ggplot2)
library(lmtest)
library(naniar)
library(lmtest)
library(dplyr)
library(AER)
library(sf)

# Project directory info

direc <- 'D:/kp_forests/'

# Data

forest <- read.csv(paste0(direc, 'data/FAOSTAT_data_en_7-26-2023.csv'))
socio <- read.csv(paste0(direc, 'data/piper.csv'))

# Creating percent difference

f.pd <- c()
f.ld <- c()

for (i in 1:nrow(forest)) {
  
  tmp <- forest %>% filter(Area == forest$Area[i])
  tmp <- tmp %>% filter(Year == forest$Year[i] - 1)
  
  if (nrow(tmp) == 1) {
    
    f.pd <- c(f.pd, 100 * (forest$Value[i] - tmp$Value[1]) / tmp$Value[1])
    f.ld <- c(f.ld, log(forest$Value[i] + 1) - log(tmp$Value[1] + 1))
    
  } else {
    
    f.pd <- c(f.pd, NA)
    f.ld <- c(f.ld, NA)
    
  }
  
}

forest$percent_difference <- f.pd
forest$log_difference <- f.ld

# Two period data

nats <- unique(forest$Area)
pre.vals <- c()
post.vals <- c()
pre.pd <- c()
post.pd <- c()
pre.ld <- c()
post.ld <- c()

for (nat in nats) {
  
  tmp <- forest %>% filter(Area == nat)
  pre <- tmp %>% filter(Year < 2005)
  post <- tmp %>% filter(Year >= 2005)
  pre.vals <- c(pre.vals, mean(pre$Value, na.rm = TRUE))
  post.vals <- c(post.vals, mean(post$Value, na.rm = TRUE))
  pre.pd <- c(pre.pd, mean(pre$percent_difference, na.rm = TRUE))
  post.pd <- c(post.pd, mean(post$percent_difference, na.rm = TRUE))
  pre.ld <- c(pre.ld, mean(pre$log_difference, na.rm = TRUE))
  post.ld <- c(post.ld, mean(post$log_difference, na.rm = TRUE))
  
  
}

df <- as.data.frame(cbind(c(pre.vals, post.vals), c(pre.pd, post.pd), c(pre.ld, post.ld), c(rep(0, length(nats)), rep(1, length(nats)))))
colnames(df) <- c('Value', 'Pct_Diff', 'Log_Diff', 'Post')

df$Log_Diff <- 100 * df$Log_Diff

# Adding country names

df <- cbind(df, c(nats, nats))
colnames(df)[ncol(df)] <- 'Country'

# Adding binded countries

binded_countries_p1 <- c('Australia', 'Austria', 'Belarus', 'Bulgaria', 'Canada', 'Croatia', 'Czechia',
                         'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary',
                         'Iceland', 'Ireland', 'Italy', 'Japan', 'Latvia', 'Liechtenstein', 'Lithuania',
                         'Luxembourg', 'Netherlands (Kingdom of the)', 'New Zealand', 'Norway',
                         'Poland', 'Portugal', 'Romania', 'Russian Federation', 'Slovakia', 'Slovenia',
                         'Spain', 'Sweden', 'Switzerland', 'Ukraine',
                         'United Kingdom of Great Britain and Northern Ireland')

df$Kyoto <- ifelse(df$Country %in% binded_countries_p1, 1, 0)

# Creating the IV

icc <- c('Afghanistan', 'Albania', 'Andorra', 'Antigua and Barbuda', 'Argentina', 'Australia', 'Austria', 'Barbados', 'Belgium', 
         'Belize', 'Benin', 'Bolivia (Plurinational State of)', 'Bosnia and Herzegovina', 'Botswana', 'Brazil', 'Bulgaria', 'Burkina Faso',
         'Cambodia', 'Canada', 'Cabo Verde', 'Central African Republic', 'Chad', 'Colombia', 'Comoros', 'Democratic Republic of the Congo', 
         'Congo', 'Costa Rica', 'Croatia', 'Cyprus', 'Czechia', 'Denmark', 'Djibouti', 'Dominica', 
         'Dominican Republic', 'Timor-Leste', 'Ecuador', 'Estonia', 'Fiji', 'Finland', 'France', 'Gabon', 'Gambia', 'Georgia', 
         'Germany', 'Ghana', 'Greece', 'Guinea', 'Guyana', 'Honduras', 'Hungary', 'Iceland', 'Ireland', 'Italy', 'Japan', 'Jordan', 
         'Kenya', 'Republic of Korea', 'Latvia', 'Lesotho', 'Liberia', 'Liechtenstein', 'Lithuania', 'Luxembourg', 'Madagascar', 'Malawi', 
         'Mali', 'Malta', 'Marshall Islands', 'Mauritius', 'Mexico', 'Mongolia', 'Montenegro', 'Namibia', 'Nauru', 'Netherlands (Kingdom of the)', 
         'New Zealand', 'Niger', 'Nigeria', 'North Macedonia', 'Norway', 'Panama', 'Paraguay', 'Peru', 'Poland', 'Portugal', 
         'Romania', 'Saint Kitts and Nevis', 'Saint Vincent and the Grenadines', 'Samoa', 'San Marino', 'Senegal', 'Serbia', 
         'Sierra Leone', 'Slovakia', 'Slovenia', 'South Africa', 'Spain', 'Sweden', 'Switzerland', 'United Republic of Tanzania',
         'Tajikistan', 'Trinidad and Tobago', 'Uganda', 'United Kingdom of Great Britain and Northern Ireland', 'Uruguay', 'Venezuela (Bolivarian Republic of)', 'Zambia', 'Zambia')

df$ICC <- ifelse(df$Country %in% icc, 1, 0)

# Update names to match socio

df[df$Country == 'Netherlands (Kingdom of the)', 'Country'] <- 'Netherlands'
df[df$Country == 'Republic of Korea', 'Country'] <- 'Korea, Rep.'
df[df$Country == 'Türkiye', 'Country'] <- 'Turkiye'
df[df$Country == 'Slovakia', 'Country'] <- 'Slovak Republic'
df[df$Country == 'United Kingdom of Great Britain and Northern Ireland', 'Country'] <- 'United Kingdom'
df[df$Country == 'United States of America', 'Country'] <- 'United States'

# Adding controls to df

nats <- unique(df$Country)

ag <- c()
fo <- c()
gdp <- c()
ex <- c()
im <- c()
pop <- c()
pol <- c()

for (nat in nats) {
  
  tmp <- socio %>% filter(Country == nat)
  tmp <- tmp %>% filter(Year < 2005)
  ag <- c(ag, mean(tmp$Agricultural.land..sq..km., na.rm = TRUE))
  fo <- c(fo, mean(tmp$Forest.rents....of.GDP., na.rm = TRUE))
  gdp <- c(gdp, mean(tmp$GDP.per.capita..constant.2015.US.., na.rm = TRUE))
  ex <- c(ex, mean(tmp$Ores.and.metals.exports....of.merchandise.exports., na.rm = TRUE))
  im <- c(im, mean(tmp$Ores.and.metals.imports....of.merchandise.imports., na.rm = TRUE))
  pop <- c(pop, mean(tmp$Population..total, na.rm = TRUE))
  pol <- c(pol, mean(tmp$Polity.Index, na.rm = TRUE))
  
}

for (nat in nats) {
  
  tmp <- socio %>% filter(Country == nat)
  tmp <- tmp %>% filter(Year >= 2005)
  ag <- c(ag, mean(tmp$Agricultural.land..sq..km., na.rm = TRUE))
  fo <- c(fo, mean(tmp$Forest.rents....of.GDP., na.rm = TRUE))
  gdp <- c(gdp, mean(tmp$GDP.per.capita..constant.2015.US.., na.rm = TRUE))
  ex <- c(ex, mean(tmp$Ores.and.metals.exports....of.merchandise.exports., na.rm = TRUE))
  im <- c(im, mean(tmp$Ores.and.metals.imports....of.merchandise.imports., na.rm = TRUE))
  pop <- c(pop, mean(tmp$Population..total, na.rm = TRUE))
  pol <- c(pol, mean(tmp$Polity.Index, na.rm = TRUE))
  
}

df <- cbind(df, ag, fo, gdp, ex, im, pop, pol)
colnames(df)[8:14] <- c('Ag_Land', 'Forest_Rents', 'GDP_pc', 'Ores_Exp', 'Ores_Imp', 'Population', 'Polity')

# Adding DiD variables

df$DID <- df$Post * df$Kyoto
df$IV <- df$Post * df$ICC

# Sample reduction based on A&F 2012

keeps <- c('India', 'Indonesia', 'Brazil', 'China', 'Turkiye', 'Chile', 'Mexico', 'Argentina', 
           'Portugal', 'Romania', 'Spain', 'Hungary', 'Switzerland', 'France', 'South Africa', 'New Zealand', 
           'Italy', 'Sweden', 'Greece', 'Slovenia', 'Norway', 'Slovak Republic', 'Austria', 'Korea, Rep.', 
           'Israel', 'Poland', 'United Kingdom', 'Japan', 'Ireland', 'Russian Federation', 'Germany', 'Netherlands', 
           'Estonia', 'Finland', 'Czechia', 'Belgium', 'Denmark', 'Canada', 'Australia', 'United States',
           'Albania', 'Cyprus', 'Singapore', 'Kazakhstan', 'Latvia', 'Uruguay', 'Lithuania', 'Costa Rica', 'Croatia', 'Panama')

dfx <- df %>% filter(Country %in% keeps)

keeps_af <- c('India', 'Indonesia', 'Brazil', 'China', 'Turkiye', 'Chile', 'Mexico', 'Argentina', 
           'Portugal', 'Romania', 'Spain', 'Hungary', 'Switzerland', 'France', 'South Africa', 'New Zealand', 
           'Italy', 'Sweden', 'Greece', 'Slovenia', 'Norway', 'Slovak Republic', 'Austria', 'Korea, Rep.', 
           'Israel', 'Poland', 'United Kingdom', 'Japan', 'Ireland', 'Russian Federation', 'Germany', 'Netherlands', 
           'Estonia', 'Finland', 'Czechia', 'Belgium', 'Denmark', 'Canada', 'Australia', 'United States')

dfaf <- df %>% filter(Country %in% keeps_af)

# Regressions - PD - expanded

pmod1 <- lm(Pct_Diff ~ DID + Post + Kyoto, data = dfx)

pmod2 <- ivreg(Pct_Diff ~ DID + Post + Kyoto | . - DID + IV, data = dfx)

pmod3 <- lm(Pct_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
            + log(Ag_Land+1), data = dfx)

pmod4 <- ivreg(Pct_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
               + log(Ag_Land+1) | . - DID + IV, data = dfx)

pmod5 <- lm(Pct_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
            + log(Ag_Land+1) + Forest_Rents, data = dfx)

pmod6 <- ivreg(Pct_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
               + log(Ag_Land+1) + Forest_Rents | . - DID + IV, data = dfx)

pmod1x <- coeftest(pmod1, vcov = vcovCL, cluster = ~Country)
pmod2x <- coeftest(pmod2, vcov = vcovCL, cluster = ~Country)
pmod3x <- coeftest(pmod3, vcov = vcovCL, cluster = ~Country)
pmod4x <- coeftest(pmod4, vcov = vcovCL, cluster = ~Country)
pmod5x <- coeftest(pmod5, vcov = vcovCL, cluster = ~Country)
pmod6x <- coeftest(pmod6, vcov = vcovCL, cluster = ~Country)

stargazer(pmod1, pmod2, pmod3, pmod4, pmod5, pmod6, type = 'text')
stargazer(pmod1x, pmod2x, pmod3x, pmod4x, pmod5x, pmod6x, type = 'text')

# Regressions - LD - expanded

lmod1 <- lm(Log_Diff ~ DID + Post + Kyoto, data = dfx)

lmod2 <- ivreg(Log_Diff ~ DID + Post + Kyoto | . - DID + IV, data = dfx)

lmod3 <- lm(Log_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
            + log(Ag_Land+1), data = dfx)

lmod4 <- ivreg(Log_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
               + log(Ag_Land+1) | . - DID + IV, data = dfx)

lmod5 <- lm(Log_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
            + log(Ag_Land+1) + Forest_Rents, data = dfx)

lmod6 <- ivreg(Log_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
               + log(Ag_Land+1) + Forest_Rents | . - DID + IV, data = dfx)

lmod1x <- coeftest(lmod1, vcov = vcovCL, cluster = ~Country)
lmod2x <- coeftest(lmod2, vcov = vcovCL, cluster = ~Country)
lmod3x <- coeftest(lmod3, vcov = vcovCL, cluster = ~Country)
lmod4x <- coeftest(lmod4, vcov = vcovCL, cluster = ~Country)
lmod5x <- coeftest(lmod5, vcov = vcovCL, cluster = ~Country)
lmod6x <- coeftest(lmod6, vcov = vcovCL, cluster = ~Country)

stargazer(lmod1, lmod2, lmod3, lmod4, lmod5, lmod6, type = 'text')
stargazer(lmod1x, lmod2x, lmod3x, lmod4x, lmod5x, lmod6x, type = 'text')

# Regressions - PD - A&F

pmod11 <- lm(Pct_Diff ~ DID + Post + Kyoto, data = dfaf)

pmod22 <- ivreg(Pct_Diff ~ DID + Post + Kyoto | . - DID + IV, data = dfaf)

pmod33 <- lm(Pct_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
             + log(Ag_Land+1), data = dfaf)

pmod44 <- ivreg(Pct_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
                + log(Ag_Land+1) | . - DID + IV, data = dfaf)

pmod55 <- lm(Pct_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
             + log(Ag_Land+1) + Forest_Rents, data = dfaf)

pmod66 <- ivreg(Pct_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
                + log(Ag_Land+1) + Forest_Rents | . - DID + IV, data = dfaf)

pmod11x <- coeftest(pmod11, vcov = vcovCL, cluster = ~Country)
pmod22x <- coeftest(pmod22, vcov = vcovCL, cluster = ~Country)
pmod33x <- coeftest(pmod33, vcov = vcovCL, cluster = ~Country)
pmod44x <- coeftest(pmod44, vcov = vcovCL, cluster = ~Country)
pmod55x <- coeftest(pmod55, vcov = vcovCL, cluster = ~Country)
pmod66x <- coeftest(pmod66, vcov = vcovCL, cluster = ~Country)

stargazer(pmod11, pmod22, pmod33, pmod44, pmod55, pmod66, type = 'text')
stargazer(pmod11x, pmod22x, pmod33x, pmod44x, pmod55x, pmod66x, type = 'text')

# Regressions - LD - A&F

lmod11 <- lm(Log_Diff ~ DID + Post + Kyoto, data = dfaf)

lmod22 <- ivreg(Log_Diff ~ DID + Post + Kyoto | . - DID + IV, data = dfaf)

lmod33 <- lm(Log_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
             + log(Ag_Land+1), data = dfaf)

lmod44 <- ivreg(Log_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
                + log(Ag_Land+1) | . - DID + IV, data = dfaf)

lmod55 <- lm(Log_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
             + log(Ag_Land+1) + Forest_Rents, data = dfaf)

lmod66 <- ivreg(Log_Diff ~ DID + Post + Kyoto + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
                + log(Ag_Land+1) + Forest_Rents | . - DID + IV, data = dfaf)

lmod11x <- coeftest(lmod11, vcov = vcovCL, cluster = ~Country)
lmod22x <- coeftest(lmod22, vcov = vcovCL, cluster = ~Country)
lmod33x <- coeftest(lmod33, vcov = vcovCL, cluster = ~Country)
lmod44x <- coeftest(lmod44, vcov = vcovCL, cluster = ~Country)
lmod55x <- coeftest(lmod55, vcov = vcovCL, cluster = ~Country)
lmod66x <- coeftest(lmod66, vcov = vcovCL, cluster = ~Country)

stargazer(lmod11, lmod22, lmod33, lmod44, lmod55, lmod66, type = 'text')
stargazer(lmod11x, lmod22x, lmod33x, lmod44x, lmod55x, lmod66x, type = 'text')

# Saving results

write.csv(stargazer(pmod22x, pmod44x, pmod66x, lmod22x, lmod44x, lomd66x), paste0(direc, 'results/main.txt'), row.names = FALSE)

# Additional analysis looking at whether the KP increased deforestation in nations with binding emissions targets, decreased deforestation elsewhere, or possibly both

keep2 <- c()

for (nat in unique(df$Country)) {
  
  tmp <- df %>% filter(Country == nat)
  
  if (sum(complete.cases(tmp)) == 2) {
    
    keep2 <- c(keep2, nat)
    
  }
  
}

df2 <- df %>% filter(Country %in% keep2)
forest0 <- df2 %>% filter(Kyoto == 0)
forest0m <- forest0 %>% filter(GDP_pc > 5000)
forest0x <- forest0 %>% filter(GDP_pc < 5000)
forest1 <- df2 %>% filter(Kyoto == 1)

het0 <- lm(Pct_Diff ~ Post + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
           + log(Ag_Land+1) + Forest_Rents, data = forest0)

hetm <- lm(Pct_Diff ~ Post + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
           + log(Ag_Land+1) + Forest_Rents, data = forest0m)

hetx <- lm(Pct_Diff ~ Post + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
           + log(Ag_Land+1) + Forest_Rents, data = forest0x)

het1 <- lm(Pct_Diff ~ Post + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
           + log(Ag_Land+1) + Forest_Rents, data = forest1)

hetall <- lm(Pct_Diff ~ Post + log(GDP_pc+1) + log(Population+1) + Polity + I(Polity^2)
             + log(Ag_Land+1) + Forest_Rents, data = df2)

het0x <- coeftest(het0, vcov = vcovCL, cluster = ~Country)
hetmx <- coeftest(hetm, vcov = vcovCL, cluster = ~Country)
hetxx <- coeftest(hetx, vcov = vcovCL, cluster = ~Country)
het1x <- coeftest(het1, vcov = vcovCL, cluster = ~Country)
hetallx <- coeftest(hetall, vcov = vcovCL, cluster = ~Country)

stargazer(hetall, hetallx, het1, het1x, het0, het0x, hetm, hetmx, hetx, hetxx, type = 'text')

write.csv(stargazer(hetallx, het1x, het0x, hetmx, hetxx), paste0(direc, 'results/heterogeniety.txt'), row.names = FALSE)

# Making summary statistics

sum.stats <- dfaf[,c(2:4, 6:7, 10, 13, 14, 8, 9)]
sum.statsx <- dfx[,c(2:4, 6:7, 10, 13, 14, 8, 9)]
sum.statsy <- df2[,c(2:4, 6:7, 10, 13, 14, 8, 9)]

sum.stats$GDP_pc <- log(sum.stats$GDP_pc+1)
sum.statsx$GDP_pc <- log(sum.statsx$GDP_pc+1)
sum.statsy$GDP_pc <- log(sum.statsy$GDP_pc+1)

sum.stats$Population <- log(sum.stats$Population+1)
sum.statsx$Population <- log(sum.statsx$Population+1)
sum.statsy$Population <- log(sum.statsy$Population+1)

sum.stats$Ag_Land <- log(sum.stats$Ag_Land+1)
sum.statsx$Ag_Land <- log(sum.statsx$Ag_Land+1)
sum.statsy$Ag_Land <- log(sum.statsy$Ag_Land+1)

nombres <- c('Percent Difference', 'Log Difference', 'Post', 'Kyoto', 'ICC', 'GDP per capita (2015 USD) (log)',
             'Population (log)', 'Polity Index', 'Agricultural Land (log sq km)', 'Forest Rents (% of GDP)')

colnames(sum.stats) <- nombres
colnames(sum.statsx) <- nombres
colnames(sum.statsy) <- nombres

datasummary_skim(sum.stats, fmt = '%.3f')
datasummary_skim(sum.statsx, fmt = '%.3f')
datasummary_skim(sum.statsy, fmt = '%.3f')

# Creating maparoos

world_sf <- read_sf(paste0(direc, '/data/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp'))

afafaf <- c(keeps_af, 'Turkey', 'Slovakia', 'Czech Republic', 'Russia', 'Korea, Republic of')

df3 <- df2 %>% filter(!Country %in% afafaf)
welp <- c(unique(df3$Country), 'Cape Verde', 'Swaziland', 'The former Yugoslav Republic of Macedonia', 'Burma')

yep <- c()

for (nat in world_sf$NAME) {
  
  if (nat %in% afafaf) {
    
    yep <- c(yep, 1)
    
  } else if (nat %in% welp) {
    
    yep <- c(yep, 2)
    
  } else {
    
    yep <- c(yep, 0)
    
  }
  
}

world_sf$AF <- yep

pal <- colorNumeric(palette = c('white', 'blue', 'red'), domain = world_sf$AF, na.color = 'transparent')

leaflet(world_sf) %>% addTiles() %>% setView(lat = 0, lng = 0, zoom = 2) %>%
  addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = 1.0, color = 'black', fillColor = ~pal(AF))

