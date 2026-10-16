#source(2.DataLoadScript.r)
dir.create('data',showWarnings = F)
load_all('C:/Users/cooka/Documents/git/LSTA')
library(sf)
library(stringr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

su = survey
#Question 9 Split the Data by LFAs
q9 = su[[9]]
id = grep('q9',names(cq))
v = cq[[id]]
x = re[,c(1,v)]
LFA = idLFA(x)


#Question 1 Age
q1 = su[[1]]
id = grep('^q1$',names(cq))
v = cq[[id]]
x = re[,c(1,v)]
AgeL = merge(LFA,x)
ages = tabulateAges(AgeL,groupVariable='PrimaryLFA')
#ages from fishery
saveRDS(ages,'data/ageOfRespondants.rds')
#fishery ages

# --- Load + filter data ---
b <- lobster.db('licence_ages')

ui <- AgeL$PrimaryLFA |>
  gsub("LFA ", "", x = _) |>
  as.numeric()

b <- b |>
  filter(LFA %in% ui,
         END_DATE > '2023-01-01')

# --- Compute ages ---
b$Age <- as.numeric((as.Date('2023-01-01') - as.Date(b$BIRTHDATE)) / 365)

# --- Clean age labels from AgeL ---
age_clean <- AgeL$`1. Age` |>
  str_remove(" years old") |>
  str_replace("80 \\+", "80+") |>
  str_trim()

# --- Build cuts from cleaned labels ---
cuts <- age_clean |>
  na.omit() |>
  unique()

lower <- as.numeric(str_extract(cuts, "^\\d+"))
cuts <- cuts[order(lower)]
lower <- sort(lower)

# --- Bin ages ---
b$AgeG <- cut(
  b$Age,
  breaks = c(lower, Inf),
  labels = cuts,
  right = FALSE
)

# --- Tables ---
t1 <- table(b$AgeG)
t2 <- table(age_clean)

# align levels
all_levels <- union(names(t1), names(t2))

t1 <- t1[all_levels]; t1[is.na(t1)] <- 0
t2 <- t2[all_levels]; t2[is.na(t2)] <- 0

# --- Combine + reshape ---
df_long <- rbind(Fishery = t1, Survey = t2) |>
  as.data.frame() |>
  rownames_to_column("dataset") |>
  pivot_longer(-dataset, names_to = "age", values_to = "count")

# ensure proper order
df_long$age <- factor(
  df_long$age,
  levels = c("20-29","30-39","40-49","50-59","60-69","70-79","80+")
)

df_long <- df_long |>
  group_by(dataset) |>
  mutate(prop = count / sum(count))


# --- Plot ---
ggplot(df_long, aes(x = age, y = prop, fill = dataset)) +
  geom_col(position = "dodge") +
  theme_minimal()+
  labs(x='Age Category',y='Proportion of Sample')


#Question 3 Years Captain
q3 = su[[3]]
id = grep('^q3$',names(cq))
v = cq[[id]]
x = re[,c(1,v)]
yc = merge(LFA,x)

yRC = tabulateYearsCaptain(x=yc,groupVariable='PrimaryLFA')
saveRDS(yRC,'data/yearsAsCaptainRespondants.rds')

###boat specs
q22 = su[[22]]
id = grep('q22',names(cq))
v = cq[[id]]
x = re[,c(1,v)]
boatSpecs = idBoatSpecs(x)
yc = merge(LFA,boatSpecs,by.x='Source.Name',by.y='id')

yRC = tabulateBoatSpecs(x=yc,groupVariable='PrimaryLFA', variableOfInterest = 'lengthCorrected')
saveRDS(yRC,'data/lengthofBoats.rds')

yRW = tabulateBoatSpecs(x=yc,groupVariable='PrimaryLFA', variableOfInterest = 'width')
saveRDS(yRW,'data/widthofBoats.rds')

yRF = tabulateBoatSpecs(x=yc,groupVariable='PrimaryLFA', variableOfInterest = 'fuel')
saveRDS(yRF,'data/fuelBoats.rds')

yRL = tabulateBoatSpecs(x=yc,groupVariable='PrimaryLFA', variableOfInterest = 'livewell')
saveRDS(yRL,'data/livewellBoats.rds')



#Question 8 Fishing Grounds
q8 = su[[8]]
id = grep('q8',names(cq))
v = cq[[id]]
x = re[,c(1,v)]
FishingGround = idFishingGround(x)


#Question 17 Lobster catches

q17 = su[[17]]
id = grep('q17',names(cq))
v = cq[[id]]
x = re[,c(1,v)]
LobsterCatches = idLobsterCatches(x)


#Question 23 Boat equipment
q23 = su[[23]]
id = grep('q23',names(cq))
v = cq[[id]]
x = re[,c(1,v)]
technology = idTechnology(x)


#Question 29 bait source
q29 = su[[29]]
id = grep('q29',names(cq))
v = cq[[id]]
x = re[,c(1,v)]
baitSource = idBaitSource(x)

#Question 30 bait type
q30 = su[[30]]
id = grep('q30',names(cq))
v = cq[[id]]
x = re[,c(1,v)]
baitType = idBaitType(x)

#Question 42 gear configuration
q42 = su[[44]]
id = grep('q42',names(cq))
v = cq[[id]]
x = re[,c(1,v)]
gearConf = idGearConfig(x)

#Question 45 Other Fisheries
q45 = su[[47]]
id = grep('q45',names(cq))
v = cq[[id]]
x = re[,c(1,v)]
speciesFished = idSpeciesFished(x)

spec = merge(speciesFished,LFA)
tabulateSpeciesFished(spec,groupVariable = 'PrimaryLFA')
tabulateSpeciesFished(subset(spec,PrimaryLFA=='LFA 38'),groupVariable = NA)

tabulateSpeciesFishedIncome(subset(spec,PrimaryLFA=='LFA 38'),by.lfa=F)
