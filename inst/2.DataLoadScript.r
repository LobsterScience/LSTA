require(readxl)
require(devtools)
load_all('C:/Users/cooka/Documents/git/LSTA')

#read in responses
re = responsesReadIn(onefile=T)
su = surveyReadIn()
cq = columns_per_question() #what columns from spread sheet do we need for this question


#Question 9 Split the Data by LFAs
      q9 = su[[9]]
      id = grep('q9',names(cq))
      v = cq[[id]]
      x = re[,c(1,v)]
LFA = idLFA(x)



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


load_all('C:/Users/cooka/Documents/git/LSTA')



q7 = su[[13]]
id = grep('13',names(cq))
v = cq[[id]]
x = re[,c(1,v)]
