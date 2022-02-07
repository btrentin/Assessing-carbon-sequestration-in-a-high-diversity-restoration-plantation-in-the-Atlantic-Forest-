library(data.table) 
library(tidyverse) 
library(ggpubr) 
library(FSA) 
library(rcompanion) 
library(viridis) 
library(plotrix)

#### data ####
 
dat <- fread("./Data/data.final.txt")

#carbon equation
dat[, C.total := (1.343 + 0.088 * (dat$DBH.cm^2) + 0.005 * ((dat$DBH.cm^2) * dat$Total.height.m))]

write.table(dat, row.names = FALSE, file = "./Data/carbon.storage.txt", sep = "\t")

#add semester for each year

semester<- fread("./Data/month.semester.txt")

dat <- Reduce(function(...) merge(..., all = TRUE, by = "month"),
              list(dat, semester))
dat[,year.month := paste(dat$year, dat$semester)]

# 1 individual uses 6 square meters estimate kg.ha
dat$kg.ha <- ((dat$C.total*10000)/6)

write.table(dat, row.names = FALSE, file = "./Data/carbon.storage.species.txt", sep = "\t")

# transforming from kg/ha to t/ha
c.storage <- fread("./Data/carbon.storage.species.txt")
c.storage[,t.ha:=(c.storage$kg.ha/1000)]

# replacing NAs for zero
c.storage[is.na(c.storage)] = 0

#### comparing filling vs diversity ####
c.storage<- subset(c.storage, c.storage$year.month>="2012 2")

plot.fd<- ggplot(c.storage, aes(x=year.month, y = t.ha, fill = c.storage$species.group))+
  geom_boxplot()+
  stat_compare_means(method = "t.test", label = "p.signif")+
  scale_fill_brewer(palette="Pastel1")+
  scale_x_discrete(labels=c("2", "2.5", "3", "3.5", "4", "8.5"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  theme(legend.position = c(0.2, 0.8), legend.background = element_blank())+
  labs(y= "Total Carbon (t/ha)", x = "Age (years)", legend)+
  theme(legend.title = element_blank())

ggsave('carbon.st.filling+diversity.png', dpi = 300, units = 'cm', width = 25, height = 15)

#### comparing filling species carbon storage ####

filling<- subset(c.storage, c.storage$species.group=='filling' & c.storage$year.month>="2012 2")

plot.c.st<- ggplot(filling, aes(x=year.month, y = t.ha))+
  geom_boxplot()+
  scale_x_discrete(labels=c("2", "2.5", "3", "3.5", "4", "8.5"))+
  theme(strip.text = element_text(face = "italic"))+
  facet_wrap(~species,ncol=2)+
  labs(y= "Total Carbon (t/ha)", x = "Age (years)")

ggsave('carbon.st.filling.png', dpi = 300, units = 'cm', width = 16, height = 20)

# comparing year 2019 only

filling<- subset(c.storage, c.storage$species.group=='filling' & c.storage$year.month>="2019 2")

filling<- filling[with(filling,order(-t.ha)),]

# test for differences

kruskal.test(t.ha ~ species, data = filling)

DT = dunnTest(t.ha ~ species,
              data=filling,
              method="bonferroni") 

PT = DT$res
cldList(P.adj ~ Comparison,
        data = PT,
        threshold = 0.05)

#plot
plot.fd<- ggplot(filling, aes(x=reorder(filling$species,-filling$t.ha), y=filling$t.ha, fill = reorder(filling$species,-filling$t.ha)))+
  geom_boxplot()+
  scale_x_discrete(labels=c())+
  theme_bw()+
  labs(y= "Total Carbon (t/ha)", x = "8.5 y", legend)+
  theme(legend.title = element_blank(),legend.text = element_text(face='italic'))+
  annotate("text", x = 1:10, y = 600, label = c("a","a","ab","bc","c","c","cd","de","de","e"))

ggsave('carbon.st.filling.2019.png', dpi = 300, units = 'cm', width = 20, height = 10)

#### comparing diversity species carbon storage (top 10)####

diversity<- subset(c.storage, c.storage$species.group=='diversity')

# calculate average for each species to know which are the top 10

diversity.high<- diversity %>%   
  group_by(species, year.month, species.group) %>%   
  summarize(C.average = mean(t.ha,na.rm=TRUE),)

diversity.high<- data.table(diversity.high)

# select year 2019 to create ranking
diversity.high<- subset(diversity.high, diversity.high$year.month=='2019 2')

diversity.high<- diversity.high[with(diversity.high,order(-C.average)),]

diversity.high <- diversity.high[1:10,]

unique(diversity.high$species)

# selecting all data for the selected species

diversity.high <- filter(diversity,  species == "Albizia polycephala" | species == "Parapiptadenia rigida" | 
                           species == "Ceiba speciosa"| species == "Ficus enormis"| species == "Gymnanthes klotzschiana"| 
                           species == "Inga vera"| species == "Lafoensia pacari"| species == "Moquiniastrum polymorphum"| 
                           species == "Peltophorum dubium"| species == "Zanthoxylum rhoifolium")

diversity.high<- subset(diversity.high, diversity.high$year.month>="2012 2")

plot.d.h<- ggplot(diversity.high, aes(x=year.month, y = t.ha))+
  geom_boxplot()+
  scale_x_discrete(labels=c("2", "2.5", "3", "3.5", "4", "8.5"))+
  theme(strip.text = element_text(face = "italic"))+
  facet_wrap(~species,ncol=2)+
  labs(y= "Total Carbon (t/ha)", x = "Age (years)")

ggsave('plot.diversity.high10.png', dpi = 300, units = 'cm', width = 16, height = 20)

diversity.high<- subset(diversity.high, diversity.high$year.month=='2019 2')

# test for diferences

kruskal.test(t.ha ~ species, data = diversity.high)

DT = dunnTest(t.ha ~ species,
              data=diversity.high,
              method="bonferroni") 

PT = DT$res
cldList(P.adj ~ Comparison,
        data = PT,
        threshold = 0.05)

# plot

plot.d.h.2<- ggplot(diversity.high, 
                    aes(x=reorder(diversity.high$species,-diversity.high$t.ha), 
                        y=diversity.high$t.ha, fill = reorder(diversity.high$species,-diversity.high$t.ha)))+
  geom_boxplot()+
  scale_x_discrete(labels=c())+
  theme_bw()+
  labs(y= "Total Carbon (t/ha)", x = "8.5 y", legend=element_text(face='italic'))+
  theme(legend.title = element_blank(),legend.text = element_text(face='italic'))+
  annotate("text", x = 1:10, y = 310, label = c("a","ab","ab","ab","ab","ab","ab","ab","ab","b"))

ggsave('carbon.st.diversity.png', dpi = 300, units = 'cm', width = 20, height = 10)

#### creating table for carbon stored for each species ####
c.storage <- fread("./Data/carbon.storage.species.txt")
c.storage[,t.ha:=(c.storage$kg.ha/1000)]
c.storage[is.na(c.storage)] = 0

c.storage.sd <- 
  c.storage %>%   
  group_by(species, year.month, species.group) %>%   
  summarize(           
    C.Total.SD = sd(t.ha,na.rm=TRUE),
  )
c.storage.sd<- data.table(c.storage.sd)

c.storage.sd<- dcast(melt(as.data.table(c.storage.sd), id.vars = c("year.month", "species", "species.group")), 
                  species + species.group ~ year.month, sd.var = "C.Total.SD")

write.table(c.storage.sd, row.names = FALSE, file = "./Data/carbon.storage.table.sd.txt", sep = "\t")

c.storage <- 
  c.storage %>%   
  group_by(species, year.month, species.group) %>%   
  summarize(           
    C.Total.mean = mean(t.ha,na.rm=TRUE),
  )

c.storage<- data.table(c.storage)

c.storage<- dcast(melt(as.data.table(c.storage), id.vars = c("year.month", "species", "species.group")), 
                  species + species.group ~ year.month, t.ha.var = "C.Total.mean")

write.table(c.storage, row.names = FALSE, file = "./Data/carbon.storage.table.txt", sep = "\t")

#### stand carbon ####
c.storage <- fread("./Data/carbon.storage.species.txt")
c.storage[,t.ha:=(c.storage$kg.ha/1000)]
c.storage[is.na(c.storage)] = 0

c.storage.s <- subset(c.storage,c.storage$year.month>='2012 2')

# stand using sum by subplot 12 measurements
c.storage.av<- c.storage.s %>% 
  group_by(year.month,Plot,Subplot) %>%   
  summarize(C.total.splot = sum(C.total))

c.storage.av<- as.data.frame(c.storage.av)

c.storage.av$kg.ha<- (c.storage.av$C.total.splot * 10000/ 720)

c.storage.av$t.ha<- (c.storage.av$kg.ha /1000)

summary_stand <- c.storage.av %>% group_by(year.month) %>% summarize(m=mean(t.ha))

#plot

plot.stand<- ggplot(c.storage.av, aes(x=year.month, y = t.ha))+
  geom_jitter(shape=16, position=position_jitter(0.2), color = 'gray', size = 1)+
  scale_x_discrete(labels=c("2", "2.5", "3", "3.5", "4", "8.5"))+
  theme_bw()+
  geom_boxplot(alpha = 0.3)+
  labs(y= "Total Carbon - high diversity planting (t/ha)", x = "Age (years)")

ggsave('stand.png', dpi = 300, units = 'cm', width = 10, height = 10)

# average and error carbon stand
c.storage.2012.2 <- subset(c.storage, c.storage$year.month=='2012 2')
mean(c.storage.2012.2$t.ha)
std.error(c.storage.2012.2$t.ha)

c.storage.2013.1 <- subset(c.storage, c.storage$year.month=='2013 1')
mean(c.storage.2013.1$t.ha)
std.error(c.storage.2013.1$t.ha)

c.storage.2013.2 <- subset(c.storage, c.storage$year.month=='2013 2')
mean(c.storage.2013.2$t.ha)
std.error(c.storage.2013.2$t.ha)

c.storage.2014.1 <- subset(c.storage, c.storage$year.month=='2014 1')
mean(c.storage.2014.1$t.ha)
std.error(c.storage.2014.1$t.ha)

c.storage.2014.2 <- subset(c.storage, c.storage$year.month=='2014 2')
mean(c.storage.2014.2$t.ha)
std.error(c.storage.2014.2$t.ha)

c.storage.2019 <- subset(c.storage, c.storage$year.month=='2019 2')
mean(c.storage.2019$t.ha)
std.error(c.storage.2019$t.ha)

#### carbon regression estimates ####

c.storage <- fread("./Data/carbon.storage.species.txt")
c.storage[,t.ha:=(c.storage$kg.ha/1000)]
c.storage[is.na(c.storage)] = 0

# add plantation age columns

c.storage$plantation.age<- 
  ifelse(c.storage$year.month=='2011 1', 0.5, 
         ifelse(c.storage$year.month=='2011 2', 1,
                ifelse(c.storage$year.month=='2012 1', 1.5,
                       ifelse(c.storage$year.month=='2012 2', 2.0,
                              ifelse(c.storage$year.month=='2013 1', 2.5,
                                     ifelse(c.storage$year.month=='2013 2', 3.0,
                                            ifelse(c.storage$year.month=='2014 1', 3.5,
                                                   ifelse(c.storage$year.month=='2014 2', 4,
                                                          ifelse(c.storage$year.month=='2019 2',8.5,NA)))))))))
c.storage<-subset(c.storage, year.month>'2012 1')

# fitting linear regression species level - 7 most carbon filling and 10 most carbon diversity

#Guazuma ulmifolia

gua.ulm<- subset(c.storage, species=="Guazuma ulmifolia")

model1<- lm(gua.ulm$t.ha ~ gua.ulm$plantation.age)
summary(model1)
plot(gua.ulm$t.ha ~ gua.ulm$plantation.age)
abline(model1)

model1.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=gua.ulm)
summary(model1.1)
ggplot(gua.ulm, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method='lm', formula = y~poly(x,2))

AIC(model1,model1.1)

#Croton urucurana

cro.uru<- subset(c.storage, species=="Croton urucurana")
model2<- lm(cro.uru$t.ha ~ cro.uru$plantation.age)
summary(model2)
plot(cro.uru$t.ha ~ cro.uru$plantation.age)
abline(model2)

model2.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=cro.uru)
summary(model2.1)
ggplot(cro.uru, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method='lm', formula = y~poly(x,2))

AIC(model2,model2.1)

#Croton floribundus

cro.flo<- subset(c.storage, species=="Croton floribundus")
model3<- lm(cro.flo$t.ha ~ cro.flo$plantation.age)
summary(model3)
plot(cro.flo$t.ha ~ cro.flo$plantation.age)
abline(model3)

model3.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=cro.flo)
summary(model3.1)
ggplot(cro.flo, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))

AIC(model3,model3.1)

#Mimosa scabrella

mim.sca<- subset(c.storage, species=='Mimosa scabrella')
model4<- lm(mim.sca$t.ha ~ mim.sca$plantation.age)
summary(model4)
plot(mim.sca$t.ha ~ mim.sca$plantation.age)
abline(model4)

model4.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=mim.sca)
summary(model4.1)
ggplot(mim.sca, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))

AIC(model4,model4.1)

#Piptadenia gonoacantha

pip.gon<- subset(c.storage, species=='Piptadenia gonoacantha')
model5<- lm(pip.gon$t.ha ~ pip.gon$plantation.age)
summary(model5)
plot(pip.gon$t.ha ~ pip.gon$plantation.age)
abline(model5)

model5.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=pip.gon)
summary(model5.1)
ggplot(pip.gon, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))

AIC(model5,model5.1)

#Solanum mauritianum

sol.mau<- subset(c.storage, species=='Solanum mauritianum')
model6<- lm(sol.mau$t.ha ~ sol.mau$plantation.age)
summary(model6)
plot(sol.mau$t.ha ~ sol.mau$plantation.age)
abline(model6)

model6.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=sol.mau)
summary(model6.1)
ggplot(sol.mau, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))

AIC(model6,model6.1)

#Schinus terebinthifolia

sch.ter<- subset(c.storage, species=='Schinus terebinthifolia')
model7<- lm(sch.ter$t.ha ~ sch.ter$plantation.age)
summary(model7)
plot(sch.ter$t.ha ~ sch.ter$plantation.age)
abline(model7)

model7.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=sch.ter)
summary(model7.1)
ggplot(sch.ter, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))

AIC(model7,model7.1)

#diversity - Inga vera

ing.ver<- subset(c.storage, species=='Inga vera')
model8<- lm(ing.ver$t.ha ~ ing.ver$plantation.age)
summary(model8)
plot(ing.ver$t.ha ~ ing.ver$plantation.age)
abline(model8)

model8.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=ing.ver)
summary(model8.1)
ggplot(ing.ver, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))

AIC(model8,model8.1)

#Albizia polycephala

alb.pol<- subset(c.storage, species=='Albizia polycephala')
model9<- lm(alb.pol$t.ha ~ alb.pol$plantation.age)
summary(model9)
plot(alb.pol$t.ha ~ alb.pol$plantation.age)
abline(model9)

model9.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=alb.pol)
summary(model9.1)
ggplot(alb.pol, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))

AIC(model9,model9.1)

#Ficus enormis

fic.eno<- subset(c.storage, species=='Ficus enormis')
model10<- lm(fic.eno$t.ha ~ fic.eno$plantation.age)
summary(model10)
plot(fic.eno$t.ha ~ fic.eno$plantation.age)
abline(model10)

model10.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=fic.eno)
summary(model10.1)
ggplot(fic.eno, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))

AIC(model10,model10.1)

#Ceiba speciosa

cei.spe<- subset(c.storage, species=='Ceiba speciosa')
model11<- lm(cei.spe$t.ha ~ cei.spe$plantation.age)
summary(model11)
plot(cei.spe$t.ha ~ cei.spe$plantation.age)
abline(model11)

model11.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=cei.spe)
summary(model11.1)
ggplot(cei.spe, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))

AIC(model11,model11.1)

#Moquiniastrum polymorphum

moq.pol<- subset(c.storage, species=='Moquiniastrum polymorphum')
model12<- lm(moq.pol$t.ha ~ moq.pol$plantation.age)
summary(model12)
plot(moq.pol$t.ha ~ moq.pol$plantation.age)
abline(model12)

model12.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=moq.pol)
summary(model12.1)
ggplot(moq.pol, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))

AIC(model12,model12.1)

#Peltophorum dubium

pel.dub<- subset(c.storage, species=='Peltophorum dubium')
model13<- lm(pel.dub$t.ha ~ pel.dub$plantation.age)
summary(model13)
plot(pel.dub$t.ha ~ pel.dub$plantation.age)
abline(model13)

model13.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=pel.dub)
summary(model13.1)
ggplot(pel.dub, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))

AIC(model13,model13.1)

#Lafoensia pacari

laf.pac<- subset(c.storage, species=='Lafoensia pacari')
model14<- lm(laf.pac$t.ha ~ laf.pac$plantation.age)
summary(model14)
plot(laf.pac$t.ha ~ laf.pac$plantation.age)
abline(model14)

model14.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=laf.pac)
summary(model14.1)
ggplot(laf.pac, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))

AIC(model14,model14.1)

#Gymnanthes klotzschiana

gym.klo<- subset(c.storage, species=='Gymnanthes klotzschiana')
model15<- lm(gym.klo$t.ha ~ gym.klo$plantation.age)
summary(model15)
plot(gym.klo$t.ha ~ gym.klo$plantation.age)
abline(model15)

model15.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=gym.klo)
summary(model15.1)
ggplot(gym.klo, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))

AIC(model15,model15.1)

#Zanthoxylum rhoifolium

zan.rho<- subset(c.storage, species=='Zanthoxylum rhoifolium')
model16<- lm(zan.rho$t.ha ~ zan.rho$plantation.age)
summary(model16)
plot(zan.rho$t.ha ~ zan.rho$plantation.age)
abline(model16)

model16.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=zan.rho)
summary(model16.1)
ggplot(zan.rho, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))

AIC(model16,model16.1)

#Parapiptadenia rigida

par.rig<- subset(c.storage, species=='Parapiptadenia rigida')
model17<- lm(par.rig$t.ha ~ par.rig$plantation.age)
summary(model17)
plot(par.rig$t.ha ~ par.rig$plantation.age)
abline(model17)

model17.1<- lm(t.ha ~ plantation.age + I(plantation.age^2), data=par.rig)
summary(model17.1)
ggplot(par.rig, aes(plantation.age, t.ha) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2))

AIC(model17,model17.1)

## for most species polynomial models fitted better
# creating new data frame with estimates

x.val <- seq(from = 2, to = 10 , by =0.1) # hypothetical ages
gua.ulm <- coef(model1.1)
cro.uru <- coef(model2.1)
cro.flo <- coef(model3.1)
mim.sca <- coef(model4.1)
pip.gon <- coef(model5.1)
sol.mau <- coef(model6.1)
sch.ter <- coef(model7.1)
ing.ver <- coef(model8.1)
alb.pol <- coef(model9.1)
fic.eno <- coef(model10.1)
cei.spe <- coef(model11.1)
moq.pol <- coef(model12.1)
pel.dub <- coef(model13.1)
laf.pac <- coef(model14.1)
gym.klo <- coef(model15.1)
zan.rho <- coef(model16.1)
par.rig <- coef(model17.1)
models<- rbind(gua.ulm,cro.uru,cro.flo,mim.sca,pip.gon,sol.mau,sch.ter,ing.ver,alb.pol,fic.eno,cei.spe,moq.pol,pel.dub,laf.pac,gym.klo,zan.rho,par.rig)
models<- as.data.frame(models)
colnames(models) <- c('intercept', 'slope', 'slope2')

models$species.group <- c('filling','filling','filling','filling','filling','filling','filling','diversity','diversity','diversity','diversity','diversity','diversity','diversity','diversity','diversity','diversity')
models$species <- c('gua.ulm','cro.uru','cro.flo','mim.sca','pip.gon','sol.mau','sch.ter','ing.ver','alb.pol','fic.eno','cei.spe','moq.pol','pel.dub','laf.pac','gym.klo','zan.rho','par.rig')
models<-models[rep(seq_len(nrow(models)), each = 81), ]

models$age <- c(x.val)

summary(models)

models$C.predicted <- (models$intercept + (models$slope * models$age) + (models$slope2 * (models$age^2)))

# scenarios for modeling
## only filling species

filling<- subset(models, models$species.group=='filling')
filling$C.t.ha <- (filling$C.predicted/7)
fill <- filling %>% 
  group_by(age) %>%   
  summarize(t.ha = sum(C.t.ha))
fill.7<- as.data.frame(fill)
fill.7$spp <- "Filling (7spp)"

## only diversity species

diversity<- subset(models, models$species.group=='diversity')
diversity$C.t.ha <- (diversity$C.predicted/10)
div <- diversity %>% 
  group_by(age) %>%   
  summarize(t.ha = sum(C.t.ha))
div.10<- as.data.frame(div)
div.10$spp <- "Diversity (10spp)"

## 5 most div + 5 most fill

fill.div <- subset(models, models$species=='gua.ulm'|models$species=='cro.uru'|models$species=='cro.flo'|models$species=='mim.sca'|models$species=='pip.gon'|models$species=='ing.ver'|models$species=='alb.pol'|models$species=='fic.eno'|models$species=='cei.spe'|models$species=='moq.pol')
fill.div$C.t.ha <- (fill.div$C.predicted/10)
fill.div <- fill.div %>% 
  group_by(age) %>%   
  summarize(t.ha = sum(C.t.ha))
fill.div<- as.data.frame(fill.div)
fill.div$spp <- "Filling (5spp) + Diversity (5spp)"

## 8 most fill + 10 most div

models$C.t.ha <- (models$C.predicted/17)
all.spp <- models %>% 
  group_by(age) %>%   
  summarize(t.ha = sum(C.t.ha))
all.spp<- as.data.frame(all.spp)
all.spp$spp <- "Filling (7spp) + Diversity (10spp)"

## plot all scenarios

predictions<- rbind(fill.7,div.10,fill.div,all.spp)

ggplot(predictions, aes(x=age, y=t.ha))+
  geom_line(aes(color = spp, linetype = spp)) + 
  theme_bw()+
  labs(y= "Total Carbon (t/ha)", x = "Age (years)")+
  theme(legend.title = element_blank(),legend.position = c(0.05, 0.95),
        legend.justification = c("left", "top"),
        legend.box.just = "left",panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis(discrete=TRUE)

ggsave(filename = "./Figure/prediction.carbon.jpeg",
       width = 6.5, height = 3.5, dpi = 600 ) 

table.predictions<- subset(predictions, predictions$age=='8.5')

