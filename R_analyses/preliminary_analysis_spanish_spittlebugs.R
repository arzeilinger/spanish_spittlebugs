#### Analysis of Spanish spittlebug count data
#### Data from Maria Morente Diez and Alberto Fereres

rm(list = ls())
# Load libraries
my.packages <- c("tidyr", "dplyr", "data.table", "ggplot2", "openxlsx", "lubridate", "lme4")
lapply(my.packages, require, character.only = TRUE)

source("R_functions/factor2numeric.R")

#### Import data
# Adult count data files
sites <- c("Morata_de_Tajuna", "Osuna")
# Sites are different tabs in the same file for adult counts
#countsFileNames <- paste("data/sampling_raw_data_2016_", sites, ".xlsx", sep = "")
countsList <- lapply(sites, function(x) read.xlsx("data/sampling_raw_data_2016.xlsx", sheet = x, detectDates = TRUE))
# Nymph count data, only for Morata de Tajuna
nymphsData <- read.xlsx("data/sampling_raw_data_nymphs_2017_Morata_de_Tajuna.xlsx", sheet = 1, detectDates = TRUE)
# Climate data
climateFileNames <- paste("data/raw_climatic_data_2016_", sites, ".xlsx", sep = "")
climateList <- lapply(climateFileNames, function(x) read.xlsx(x, sheet = 1, detectDates = TRUE))

#### Looking at counts data
lapply(countsList, str)
lapply(countsList, summary)

# Combine data sets into a single data set
# First, get all types the same across data sets
countsList[[2]]$DATE <- ymd(countsList[[2]]$DATE)
lapply(countsList, str)

countsData <- countsList %>% rbindlist %>% as.data.frame
str(countsData)

# Rename GENUS/SPECIES column, and remove spaces between genus and species names
countsData$GENUS.SPECIES <- gsub(" ", ".", countsData$`GENUS/SPECIES`)
table(countsData$GENUS.SPECIES)
# Need to rename single Neophilaenus campestris specimen
countsData$GENUS.SPECIES[countsData$GENUS.SPECIES == "Neophilaenus.campestris"] <- "Neophilaenus.sp."

# Histogram of counts
hist(countsData[countsData$GENUS.SPECIES == "Neophilaenus.sp.",]$NUMBER.OF.INDIVIDUALS)
hist(countsData[countsData$GENUS.SPECIES == "Philaenus.spumarius",]$NUMBER.OF.INDIVIDUALS)
# Distribution of both species looks over-dispersed -- need to use negative binomial or quasi-poisson distributions
# Probably zero-inflated too

#### Summarize data: get mean for each sampling date, stratum, site, and species
countsSummary <- countsData %>% dplyr::filter(!is.na(DATE)) %>% # Remove rows that are NA for DATE
  # Filter for only Philaenus and Neophilaenus; need to keep NA's too because these provide the 0 counts (absences)
  dplyr::filter(is.na(GENUS.SPECIES) | GENUS.SPECIES == "Neophilaenus.sp." | GENUS.SPECIES == "Philaenus.spumarius") %>%
  # Calculate means and SE
  group_by(PROVINCE, DATE, STRATUM, GENUS.SPECIES) %>% summarise(meanCount = mean(NUMBER.OF.INDIVIDUALS),
                                                       nCount = sum(!is.na(NUMBER.OF.INDIVIDUALS)),
                                                       seCount = sd(NUMBER.OF.INDIVIDUALS)/sqrt(nCount))
print.data.frame(countsSummary)

#### Plot mean densities over time
dynamicsplot <- ggplot(data=countsSummary, aes(x=DATE, y=meanCount)) +
  geom_line(aes(linetype=STRATUM, colour = STRATUM), size=1.25) +
  geom_point(aes(shape=STRATUM, colour = STRATUM), size=3.5) +
  facet_wrap(~PROVINCE + GENUS.SPECIES) +
  geom_errorbar(aes(ymax=meanCount+seCount, ymin=meanCount-seCount), width=0.2) +
  scale_y_continuous(name = "Number of individuals") +
  # limits = c(0,10)) +
  # ylab("% insects on source plant") + 
  # ylim(c(0,100)) +
  xlab("Date") +
  theme_bw(base_size=18) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black"),
        panel.background = element_blank()) 
# Not a very good plot; need to work on it
dynamicsplot


#### Looking at climate data
lapply(climateList, str)

# Data sets use different column headers and have different number of columns, and different order
# Need to fix this

# Just look at Osuna because the column headers are cleaner
climateOsuna <- climateList[[2]]

# Look at intercorrelations among climate variables using a pairs() plot
pdf("output/climate_variables_intercorrelations_Osuna.pdf")
  climateOsuna %>% dplyr::select(-day) %>% pairs()
dev.off()


