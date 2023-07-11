require(ggplot2)
require(bio.lobster)
require(bio.utilities)
require(devtools)

load_all('C:/Users/cooka/Documents/git/LSTA')

#fuel
l = readRDS('data/fuelBoats.rds')

l8 = l[['LFA 28']]
plotFuel(s=l8)

l8 = l[['LFA 29']]
l8 = subset(l8,fuel<1000)
plotFuel(s=l8)

l8 = l[['LFA 30']]
plotFuel(s=l8)

l8 = l[['LFA 36']]
plotFuel(s=l8)

l8 = l[['LFA 38']]
plotFuel(s=l8)




saveRDS(yRL,'data/livewellBoats.rds')

###age and loan for boat

q1 = su[[1]]
id = grep('^q1$',names(cq))
v = cq[[id]]
x = re[,c(1,v)]
i = grep('year',x$`1. Age`)
x$`1. Age`[i] = substring(x$`1. Age`[i],1,5)

q25 = su[[25]]
id = grep('^q25$',names(cq))
v = cq[[id]]
xx = re[,c(1,v)]
xx[1,2]<- NA

xc = merge(xx,x,all=T)
names(xc) = c('id','Boat_Loan','Age')
g = expand.grid(unique(xc$Age),unique(xc$Boat_Loan))
xc = merge(xc,g,by.x=c('Age','Boat_Loan'),by.y=c('Var1','Var2'),all.y=T)
xc$Number = ifelse(!is.na(xc$id),1,0.1)

xca=aggregate(Number~Boat_Loan+Age,data=xc,FUN=sum)
ggplot(xca,aes(fill=Boat_Loan, y=Number,x=Age))+geom_bar(position='dodge',stat='identity')


###age and loan for lic

q1 = su[[1]]
id = grep('^q1$',names(cq))
v = cq[[id]]
x = re[,c(1,v)]
i = grep('year',x$`1. Age`)
x$`1. Age`[i] = substring(x$`1. Age`[i],1,5)

q26 = su[[26]]
id = grep('^q26$',names(cq))
v = cq[[id]]
xx = re[,c(1,v)]
xx[1,2]<- NA

xc = merge(xx,x,all=T)
names(xc) = c('id','License_Loan','Age')
g = expand.grid(unique(xc$Age),unique(xc$License_Loan))
xc = merge(xc,g,by.x=c('Age','License_Loan'),by.y=c('Var1','Var2'),all.y=T)
xc$Number = ifelse(!is.na(xc$id),1,0.1)

xca=aggregate(Number~License_Loan+Age,data=xc,FUN=sum)
ggplot(xca,aes(fill=License_Loan, y=Number,x=Age))+geom_bar(position='dodge',stat='identity')


###age keepers per trap by LFA
        q9 = su[[9]]
        id = grep('q9',names(cq))
        v = cq[[id]]
        x = re[,c(1,v)]
        LFA = idLFA(x)
        LFA = LFA[,c(1,3)]

        q1 = su[[1]]
        id = grep('^q1$',names(cq))
        v = cq[[id]]
        x = re[,c(1,v)]
        i = grep('year',x$`1. Age`)
        x$`1. Age`[i] = substring(x$`1. Age`[i],1,5)

        q39 = su[[39]]
        id = grep('^q39$',names(cq))
        v = cq[[id]]
        xx = re[,c(1,v)]

        xc = merge(xx,x,all=T)
        xc = merge(xc,LFA,all=T)
        names(xc) = c('ID','Start','Middle','End','Age','LFA')
        xca=aggregate(Start~Age+LFA,data=xc,FUN=mean)
        xca$Period = 'Start'
        xcm=aggregate(Middle~Age+LFA,data=xc,FUN=mean)
        xcm$Period = 'Middle'

        xce=aggregate(End~Age+LFA,data=xc,FUN=mean)
        xce$Period = 'End'
        names(xca)[3]=names(xcm)[3]=names(xce)[3] = 'CatchRate'

        xm = dplyr::bind_rows(list(xca,xcm,xce))
        xm$Period = factor(xm$Period,levels=c('Start','Middle','End'))


        ggplot(xm,aes(fill=Age, y=CatchRate,x=Period))+geom_bar(position='dodge',stat='identity')

  #no lfa
    xca=aggregate(Start~Age,data=xc,FUN=mean)
    xca$Period = 'Start'
    xcm=aggregate(Middle~Age,data=xc,FUN=mean)
    xcm$Period = 'Middle'

    xce=aggregate(End~Age,data=xc,FUN=mean)
    xce$Period = 'End'
    names(xca)[2]=names(xcm)[2]=names(xce)[2] = 'CatchRate'

    xm = dplyr::bind_rows(list(xca,xcm,xce))
    xm$Period = factor(xm$Period,levels=c('Start','Middle','End'))


    ggplot(xm,aes(fill=Age, y=CatchRate,x=Period))+geom_bar(position='dodge',stat='identity')


###age catch rate changes by LFA
    q9 = su[[9]]
    id = grep('q9',names(cq))
    v = cq[[id]]
    x = re[,c(1,v)]
    LFA = idLFA(x)
    LFA = LFA[,c(1,3)]

    q1 = su[[1]]
    id = grep('^q1$',names(cq))
    v = cq[[id]]
    x = re[,c(1,v)]
    i = grep('year',x$`1. Age`)
    x$`1. Age`[i] = substring(x$`1. Age`[i],1,5)


    q40 = su[[40]]
    ida = grep('^q40a$',names(cq))
    va = cq[[ida]]
    xxa = re[,c(1,va)]

    ida = grep('^q40b$',names(cq))
    va = cq[[ida]]
    xxb = re[,c(1,va)]

    ida = grep('^q40c$',names(cq))
    va = cq[[ida]]
    xxc = re[,c(1,va)]

    b = merge(LFA,(merge(x,merge(xxa,merge(xxb,xxc,by='Source.Name')))))

    names(b) = c('ID','LFA','Age','Two_years','Five_years','Ten_years')
    b$Number = ifelse(is.na(b$Two_years),0,1)
    b$Two_years = ifelse(b$Two_years=='No','No change',b$Two_years)
    b$Five_years = ifelse(b$Five_years=='No','No change',b$Five_years)
    b$Ten_years = ifelse(b$Ten_years=='No','No change',b$Ten_years)

    xca=aggregate(Number~Age+Two_years,data=b,FUN=sum)
    gr = expand.grid(na.omit(unique(b$Age)),na.omit(unique(b$Two_years)))
    bb = merge(xca,gr,by.x=c('Age','Two_years'),by.y=c('Var1','Var2'),all.y=T)
    bb$Number = ifelse(!is.na(bb$Number),bb$Number, 0.1)
    bb$Two_years = ifelse(bb$Two_years=='No','No change',bb$Two_years)
    bb$Two_year_change = factor(bb$Two_years,levels=c('Decreased since then','No change','Increased since then',"I don't know"))

    ggplot(bb,aes(fill=Two_year_change, y=Number,x=Age))+geom_bar(position='dodge',stat='identity')

    #no age
    print(ggplot2::ggplot(as.data.frame(table(b$Two_years)),aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity')+ylab('Responses')+ xlab('Changes in Catch Rate in the Last 2 Years')+ theme(axis.text.x = element_text(angle=0),legend.position='none'))

    print(ggplot2::ggplot(as.data.frame(table(b$Five_years)),aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity')+ylab('Responses')+ xlab('Changes in Catch Rate in the Last 5 Years')+ theme(axis.text.x = element_text(angle=0),legend.position='none'))

    print(ggplot2::ggplot(as.data.frame(table(b$Ten_years)),aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity')+ylab('Responses')+ xlab('Changes in Catch Rate in the Last 10 Years')+ theme(axis.text.x = element_text(angle=0),legend.position='none'))


    print(ggplot2::ggplot(as.data.frame(table(subset(b,LFA=='LFA 38')$Two_years)),aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity')+ylab('Responses')+ xlab('Changes in Catch Rate in the Last 2 Years')+ theme(axis.text.x = element_text(angle=0),legend.position='none'))

    print(ggplot2::ggplot(as.data.frame(table(subset(b,LFA=='LFA 38')$Five_years)),aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity')+ylab('Responses')+ xlab('Changes in Catch Rate in the Last 5 Years')+ theme(axis.text.x = element_text(angle=0),legend.position='none'))

    print(ggplot2::ggplot(as.data.frame(table(subset(b,LFA=='LFA 38')$Ten_years)),aes(x=Var1,y=Freq,fill=Var1))+geom_bar(stat='identity')+ylab('Responses')+ xlab('Changes in Catch Rate in the Last 10 Years')+ theme(axis.text.x = element_text(angle=0),legend.position='none'))


    b$Number = ifelse(is.na(b$Ten_years),0,1)
    xca=aggregate(Number~Age+Ten_years,data=b,FUN=sum)
    gr = expand.grid(na.omit(unique(b$Age)),na.omit(unique(b$Ten_years)))
    bb = merge(xca,gr,by.x=c('Age','Ten_years'),by.y=c('Var1','Var2'),all.y=T)
    bb$Number = ifelse(!is.na(bb$Number),bb$Number, 0.1)
    bb$Ten_years = ifelse(bb$Ten_years=='No','No change',bb$Ten_years)
    bb$Ten_year_change = factor(bb$Ten_years,levels=c('Decreased since then','No change','Increased since then',"I don't know"))

    ggplot(bb,aes(fill=Ten_year_change, y=Number,x=Age))+geom_bar(position='dodge',stat='identity')



    b$Number = ifelse(is.na(b$Ten_years),0,1)
    xca=aggregate(Number~Age+Ten_years,data=b,FUN=sum)
    gr = expand.grid(na.omit(unique(b$Age)),na.omit(unique(b$Ten_years)))
    bb = merge(xca,gr,by.x=c('Age','Ten_years'),by.y=c('Var1','Var2'),all.y=T)
    bb$Number = ifelse(!is.na(bb$Number),bb$Number, 0.1)
    bb$Ten_years = ifelse(bb$Ten_years=='No','No change',bb$Ten_years)
    bb$Ten_year_change = factor(bb$Ten_years,levels=c('Decreased since then','No change','Increased since then',"I don't know"))

    ggplot(subset(bb,!is.na(Ten_year_change)),aes(fill=Ten_year_change, y=Number,x=Age))+geom_bar(position='dodge',stat='identity')


#q45
    q45 = su[[47]]
    id = grep('q45',names(cq))
    v = cq[[id]]
    x = re[,c(1,v)]
    LFA = idLFA(x)
    LFA = LFA[,c(1,3)]

