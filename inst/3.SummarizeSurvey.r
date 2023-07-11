#source(2.DataLoadScript.r)
dir.create('data',showWarnings = F)
load_all('C:/Users/cooka/Documents/git/LSTA')

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
saveRDS(ages,'data/ageOfRespondants.rds')

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
