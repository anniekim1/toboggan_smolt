
# This script covers the analysis of Toboggan Creek smolt data 

#Created: 7-Feb-2023
#update 9-April-2025 to incorporate strat popn estimate for 2024 smolt
# By : Kristen Peck

# Libraries

library(tidyverse)
library(lubridate)
library(readxl)
library(recapr)

options(scipen=999)

# # Load the data 
# year.summaries <- read_excel("Toboggan-YearSummaries_COPY3-Dec-2024.xlsx", sheet="TobogganFence")
# 
# cwt <- year.summaries %>% 
#   select(year=Year,CWTreleased) 

# CWT retention log ####
CWT24 <- read_excel("toboggan_smolt_dataentry_FINAL 2024.xlsx", 
                    sheet="cwt_log") %>% 
  mutate(iso.week = isoweek(tag_date),
         mort_n = ifelse(is.na(mort_n),0,mort_n),
         sacrifice_n = ifelse(is.na(sacrifice_n),0,sacrifice_n))
str(CWT24)

unique(CWT24$tag_code)

CWTretention24 <- CWT24 %>% 
  mutate(retention.rt = r_retention_n/r_sample_size) %>% 
  group_by(iso.week) %>% 
  summarize(weekly.retention = ifelse(mean(retention.rt, na.rm=T) %in% NaN, 1,
                                      mean(retention.rt, na.rm=T)))

CWTretention24bycode <- CWT24 %>% 
  mutate(retention.rt = r_retention_n/r_sample_size) %>% 
  group_by(tag_code) %>% 
  summarize(n = sum(r_sample_size, na.rm=T),n.retained = sum(r_retention_n, na.rm=T),
            weekly.retention = ifelse(mean(retention.rt, na.rm=T) %in% NaN, 1,
                                      mean(retention.rt, na.rm=T)))
CWTretention24bycode

tag.summary <- CWT24 %>% 
  left_join(CWTretention24, by="iso.week") %>%
  mutate(retained.est = (inj_n-mort_n-sacrifice_n)*weekly.retention) %>% 
  group_by(tag_code) %>% 
  summarize(injected = sum(inj_n-mort_n-sacrifice_n),est.retained = round(sum(retained.est),0)) %>%
  mutate(estd.lost = injected-est.retained)
#write_csv("CWT24.csv")
tag.summary

ave.CWTmeristics24 <- read_excel("toboggan_smolt_dataentry_FINAL 2024.xlsx", 
                                 sheet="individualfish") %>% 
  group_by(tag_code) %>% 
  summarize(n.measured = length(!is.na(fork_length_mm)), ave.FL = mean(fork_length_mm, na.rm=T),
            ave.wt = mean(weight_g, na.rm=T))
ave.CWTmeristics24



# table summary of fish and size by tag code:

size.headmold.key24 <- data.frame(head_mold_size = c(NA,unique(CWT24$head_mold_size)),
                                  category = c("little","small","big","bigger"),
                                  length_range_mm = c("<70","70-85","86-115","116-190"))

CWT.summary24 <- CWT24 %>% 
  left_join(CWTretention24, by="iso.week") %>%
  mutate(retained.est = (inj_n-mort_n-sacrifice_n)*weekly.retention) %>% 
  group_by(tag_code, head_mold_size) %>% 
  summarize(injected = sum(inj_n-mort_n-sacrifice_n),est.retained = round(sum(retained.est),0),
            perc.retention = round(est.retained/injected,2)*100) %>%
  left_join(size.headmold.key24, by = "head_mold_size") %>% 
  select(-category, -head_mold_size) 
#mutate(estd.lost = injected-est.retained)
CWT.summary24
#write_csv(CWT.summary24, "CWT.summary24.csv")



# do this again with just the raw individual fish data rather than the CWT retention log

# Indiv fish - remake into first tagging and recap in same columns with sacrifice or mort noted ####
fish24 <- read_excel("toboggan_smolt_dataentry_FINAL 2024.xlsx", 
                     sheet="individualfish") %>% 
  filter(species %in% "co-w", size_cwt %in% c(NA, "small","big","bigger","biggest")) %>% 
  mutate(fate = ifelse(!is.na(sacrifice),"sacrifice",
                       ifelse(!is.na(mort),"mort","live"))) %>% 
  mutate(tag.status = ifelse(!is.na(a_adclip) & fate %in% "live","A",
                             ifelse(!is.na(r_adclip),"R",NA))) %>% 
  mutate(k = weight_g/fork_length_mm^3 * 10^5) %>% 
  mutate(isoweek = isoweek(date), yday = yday(date),week.yday = as.numeric(paste0(isoweek,".",yday))) #this week starts on a monday, which matches our mark switch day



# CWT tag summary - individuals ####

CWTlog.indiv24 <- fish24 %>% 
  filter(!is.na(tag_code), fate %in% "live") %>% 
  group_by(isoweek,date, site, tag_code, `head mold (lbs)`) %>% 
  summarize(inj_n = sum(ifelse(!is.na(tag_code),count,0), na.rm = TRUE))   #note that in this version, sacrifices and morts are not included in injected

CWTlog.indiv24

CWTretention24raw <- read_excel("toboggan_smolt_dataentry_FINAL 2024.xlsx", 
                                sheet="cwt_log") %>% 
  select(-c(head_mold_size,inj_n, mort_n, sacrifice_n)) %>% 
  mutate(isoweek = isoweek(tag_date), retention.rt = r_retention_n/r_sample_size)

CWTretags <- CWTretention24raw %>% 
  filter(r_re_tagged_n > 0) %>% 
  select(isoweek, tag_code, r_re_tagged_n) %>% 
  group_by(tag_code) %>% 
  summarize(n_retagged = sum(r_re_tagged_n))


CWTretention24 <- CWTretention24raw %>% 
  group_by(isoweek) %>% 
  summarize(weekly.retention = ifelse(mean(retention.rt, na.rm=T) %in% NaN, 1,
                                      mean(retention.rt, na.rm=T)))

CWTlog.indiv24summary <- CWTlog.indiv24 %>% 
  left_join(CWTretention24, by="isoweek") %>%
  mutate(retained.est = inj_n*weekly.retention) %>% 
  group_by(tag_code) %>% 
  summarize(injected = sum(inj_n),est.retained = round(sum(retained.est),0)) %>%
  mutate(estd.lost = injected-est.retained) %>% 
  left_join(CWTretags) %>% 
  mutate(n_retagged=ifelse(is.na(n_retagged),0,n_retagged), 
         est.oceanreleases = est.retained+n_retagged)
#write_csv(CWTlog.indiv24summary,"CWT24logindiv.csv")

CWTlog.indiv24summary

ave.CWTmeristics24 <- fish24 %>% 
  group_by(tag_code) %>% 
  summarize(n.measured = length(!is.na(fork_length_mm)), ave.FL = round(mean(fork_length_mm, na.rm=T),1),
            sd.FL = round(sd(fork_length_mm, na.rm=T),1), ave.wt = round(mean(weight_g, na.rm=T),1),
            sd.wt = round(sd(weight_g, na.rm=T),1))
ave.CWTmeristics24

CWT.tag.summary2024 <- CWTlog.indiv24summary %>% 
  left_join(ave.CWTmeristics24, by="tag_code") %>% 
  arrange(ave.FL) %>% 
  select(-n.measured)
CWT.tag.summary2024

#write_csv(CWT.tag.summary2024,"CWT.tag.summary2024.csv")

#### sizes over the season: ####



# plot.fork.length.age <- ggplot(fish24[fish24$`GR AGE`%in% c("22","33","44")&fish24$fork_length_mm>=70,])+
#   geom_point(data=fish24[is.na(fish24$`GR AGE`)&fish24$fork_length_mm>=70,],aes(x=date, y=fork_length_mm), col="gray70")+
#   geom_point(aes(x=date, y=fork_length_mm, col=`GR AGE`))+
#   geom_smooth(aes(x=date, y=fork_length_mm, col=`GR AGE`), method = "lm")+
#   scale_y_continuous(breaks=seq(min(fish24$fork_length_mm, na.rm=T),
#                                 max(fish24$fork_length_mm, na.rm=T),25))+
#   scale_x_date(date_breaks = "1 week", date_labels = "%b-%d")+
#   theme_bw()+
#   theme(axis.text.x = element_text(angle=45, hjust = 1))+
#   labs(x="Date",y="Fork Length (mm)",col="GR Age")
# plot.fork.length.age

# # ggsave(plot=plot.fork.length.age, filename = "plot.fork.length.age.png",
# #        width=6, height=4)


## AK's changes in code above
plot.fork.length.age <- ggplot(fish24[fish24$`GR AGE` %in% c("22","33","44") & fish24$fork_length_mm >= 70, ]) +
  geom_point(
    data = fish24[is.na(fish24$`GR AGE`) & fish24$fork_length_mm >= 70, ],
    aes(x = as.Date(date), y = fork_length_mm),  # <- AK converted here
    col = "gray70"
  ) +
  geom_point(aes(x = as.Date(date), y = fork_length_mm, col = `GR AGE`)) +  # <- AK converted here
  geom_smooth(aes(x = as.Date(date), y = fork_length_mm, col = `GR AGE`), method = "lm") +  # <- AK converted here
  scale_y_continuous(breaks = seq(min(fish24$fork_length_mm, na.rm = TRUE),
                                  max(fish24$fork_length_mm, na.rm = TRUE), 25)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%b-%d") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Date", y = "Fork Length (mm)", col = "GR Age")
plot.fork.length.age
# ggsave(plot=plot.fork.length.age, filename = "plot.fork.length.age.png",
#         width=6, height=4)

summary(lm(fork_length_mm~date+`GR AGE`,fish24[fish24$`GR AGE`%in% c("22","33","44"),]))



#Fulton's condition factor

ggplot(fish24)+
  geom_point(aes(x=date, y=k, col=`GR AGE`))+
  geom_smooth(aes(x=date, y=k), method="loess")

fish24 %>% #inspecting outliers
  filter(k <.7) %>% 
  select(order, date, fork_length_mm, k)

#### Mark-recapture estimates ####
# mark summary tables ####

wkly.mark.summary <- fish24 %>% 
  mutate(first.day.of.week = format(as.Date(paste0(year,"-",isoweek,"-",1),"%Y-%W-%u"),"%b-%d")) %>% 
  group_by(isoweek,first.day.of.week) %>% 
  summarize(new.tags = sum(ifelse(tag.status %in% "A", count, 0), na.rm = TRUE), 
            recaps = sum(ifelse(tag.status %in% "R", count, 0), na.rm = TRUE),
            unmarked = sum(ifelse(is.na(tag.status), count, 0), na.rm = TRUE),
            sacrifices = sum(ifelse(fate %in% "sacrifice", count, 0), na.rm = TRUE), 
            mort = sum(ifelse(fate %in% "mort", count, 0), na.rm = TRUE) )
wkly.mark.summary

wkly.mark.summary.barn <- fish24 %>% 
  filter(site %in% "barn") %>% 
  mutate(first.day.of.week = format(as.Date(paste0(year,"-",isoweek,"-",1),"%Y-%W-%u"),"%b-%d")) %>% 
  group_by(isoweek, first.day.of.week) %>% 
  summarize(new.tags = sum(ifelse(tag.status %in% "A", count, 0), na.rm = TRUE),
            unique.mark = paste(unique(na.omit(a_caudalclip)),collapse=","),
            recaps = sum(ifelse(tag.status %in% "R", count, 0), na.rm = TRUE),
            unmarked = sum(ifelse(is.na(tag.status), count, 0), na.rm = TRUE),
            sacrifices = sum(ifelse(fate %in% "sacrifice", count, 0), na.rm = TRUE), 
            mort = sum(ifelse(fate %in% "mort", count, 0), na.rm = TRUE) )
wkly.mark.summary.barn

wkly.mark.summary.fence <- fish24 %>% 
  filter(site %in% "fence") %>% 
  mutate(first.day.of.week = format(as.Date(paste0(year,"-",isoweek,"-",1),"%Y-%W-%u"),"%b-%d")) %>% 
  group_by(isoweek,first.day.of.week) %>% 
  summarize(new.tags = sum(ifelse(tag.status %in% "A", count, 0), na.rm = TRUE), #note that these exclude sacrifices/morts
            recaps = sum(ifelse(tag.status %in% "R", count, 0), na.rm = TRUE), #note that these include sacrifices/morts
            recaps.marks = paste(unique(na.omit(r_caudalclip)),collapse=","),
            unmarked = sum(ifelse(is.na(tag.status), count, 0), na.rm = TRUE),
            sacrifices = sum(ifelse(fate %in% "sacrifice", count, 0), na.rm = TRUE), 
            mort = sum(ifelse(fate %in% "mort", count, 0), na.rm = TRUE) )
wkly.mark.summary.fence

##### simple Petersen #####

(m <- fish24 %>% 
   filter(site %in% "barn",tag.status %in% "A") %>% 
   summarize(m = sum(count, na.rm=T)))

(r <- fish24 %>% 
    filter(site %in% "fence", tag.status %in% "R") %>% 
    summarize(r = sum(count, na.rm=T)))

(c <- fish24 %>% 
    filter(site %in% "fence") %>% 
    summarize(c = sum(count, na.rm=T)))

recapr::NChapman(m,c,r)
recapr::vChapman(m,c,r)
recapr::seChapman(m,c,r)

#trap efficiency of fence RST
c/NChapman(m,c,r)*100

##### SPAS #####
#weekly stratified estimator

#visualize weekly marks

mark.recaps24barn <- fish24 %>% 
  filter(site %in% "barn", tag.status %in% "A") %>% 
  group_by(yday, week.yday, date, isoweek, site) %>% 
  summarize(top = sum(ifelse(a_caudalclip %in% "top", count, 0), na.rm = TRUE),
            bottom = sum(ifelse(a_caudalclip %in% "bottom", count, 0), na.rm = TRUE),  
            none = sum(ifelse(a_caudalclip %in% "none", count, 0), na.rm = TRUE)) %>% 
  pivot_longer(-c(yday,week.yday,date,isoweek,site))

mark.recaps24fence <- fish24 %>% 
  filter(site %in% "fence", tag.status %in% "R") %>% 
  group_by(yday, week.yday, date, isoweek, site) %>% 
  summarize(top = sum(ifelse(r_caudalclip %in% "top", count, 0), na.rm = TRUE),
            bottom = sum(ifelse(r_caudalclip %in% "bottom", count, 0), na.rm = TRUE), 
            none = sum(ifelse(r_caudalclip %in% "none", count, 0), na.rm = TRUE)) %>% 
  mutate(prop.top = ifelse(!is.na(top/(top+bottom)),top/(top+bottom),0),
         none.tops = ifelse(isoweek >19,round(none*prop.top,0),0), 
         none.bottoms = ifelse(isoweek >19,round(none*(1-prop.top),0),0),
         top = top+none.tops, bottom = bottom+none.bottoms,
         none = none-(none.tops+none.bottoms)) %>% 
  select(-c(prop.top,none.tops, none.bottoms)) %>% 
  pivot_longer(-c(yday,week.yday,date,isoweek,site))

mark.recaps24 <- mark.recaps24barn %>% 
  rbind(mark.recaps24fence)

cols_caudal = c(bottom = "gray20",top= "gray75", none= "red")

plot.mark.timing24bubble <- ggplot()+
  geom_point(data=mark.recaps24barn[mark.recaps24barn$value >0,],
             aes(x=yday, y=-10, col=name, size=value))+
  geom_bar(data=mark.recaps24fence, 
           aes(x=yday, y=value, fill=name),stat = "identity", position = "stack")+
  scale_x_continuous(breaks=seq(min(mark.recaps24$yday),max(mark.recaps24$yday),2))+
  scale_color_manual(values = cols_caudal)+
  scale_fill_manual(values = cols_caudal)+
  scale_size_continuous(range = c(.5,5),# adjust point sizes
                        breaks = seq(min(mark.recaps24$value),max(mark.recaps24$value),200)) +             
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  guides(color = "none")+
  labs(fill="Caudal \nMark",size="# Marks\nApplied", x="Julian Day", y="# Coho Recaptures")
plot.mark.timing24bubble

# ggsave(plot=plot.mark.timing24bubble, filename = "plot.mark.timing24bubble.png", width=10,
#         height=8)

plot.mark.timing24mirror <- ggplot()+
  geom_bar(data=mark.recaps24fence, 
           aes(x=yday, y=value*-1, fill=name),stat = "identity", position = "dodge")+
  geom_bar(data=mark.recaps24barn, 
           aes(x=yday, y=value, fill=name),stat = "identity", position = "dodge")+
  geom_hline(aes(yintercept =0), col="black")+
  scale_x_continuous(breaks=seq(min(mark.recaps24$yday),max(mark.recaps24$yday),2))+
  scale_color_manual(values = cols_caudal)+
  scale_fill_manual(values = cols_caudal)+
  theme_bw()+ 
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  labs(fill="Caudal \nMark", x="Julian Day", y="# Coho Marked (top) versus Recaptured (bottom)")
plot.mark.timing24mirror

# ggsave(plot=plot.mark.timing24mirror, filename = "plot.mark.timing24mirror.png", width=10,
#        height=8)



#Set up data for the SPAS analysis

mark.recaps24barn.weekly <- fish24 %>% 
  filter(site %in% "barn", tag.status %in% "A", isoweek > 18) %>% #filtering out the first week since there was no strat marking
  group_by(isoweek) %>% 
  summarize(a.top = sum(ifelse(a_caudalclip %in% "top", count, 0), na.rm = TRUE),
            a.bottom = sum(ifelse(a_caudalclip %in% "bottom", count, 0), na.rm = TRUE),
            a.1 = sum(ifelse(isoweek %in% 19, count, 0), na.rm = TRUE),
            a.2 = sum(ifelse(isoweek %in% 20, count, 0), na.rm = TRUE),
            a.3 = sum(ifelse(isoweek %in% 21, count, 0), na.rm = TRUE),
            a.4 = sum(ifelse(isoweek %in% 22, count, 0), na.rm = TRUE),
            a.5 = sum(ifelse(isoweek %in% 23, count, 0), na.rm = TRUE),
            a.6 = sum(ifelse(isoweek %in% 24, count, 0), na.rm = TRUE),
            a.7 = sum(ifelse(isoweek %in% 25, count, 0), na.rm = TRUE),
            a.8 = sum(ifelse(isoweek %in% 26, count, 0), na.rm = TRUE),
  ) 
#         none = sum(ifelse(a_caudalclip %in% "none", count, 0), na.rm = TRUE)) %>% 
#pivot_longer(-c(isoweek,site)) 
mark.recaps24barn.weekly  


mark.recaps24fence.weekly <- mark.recaps24fence %>% 
  filter(site %in% "fence", isoweek > 18) %>% 
  group_by(isoweek) %>% 
  summarize(r.top = sum(ifelse(name %in% "top", value, 0), na.rm = TRUE),
            r.bottom = sum(ifelse(name %in% "bottom", value, 0), na.rm = TRUE)) 

fish24 %>% 
  filter(site %in% "fence", isoweek > 18) %>% 
  group_by(isoweek) %>% 
  summarize(r.na= sum(ifelse(is.na(r_adclip), count, 0), na.rm=TRUE))

mark.recaps24fence.weekly # cannot quite seem to set up this matrix so did it manually below:

mark.recaps24.weekly <- mark.recaps24barn.weekly %>% 
  left_join(mark.recaps24fence.weekly)


#bah, having too much trouble getting anything except the diagonal. Doing this manually:
setupfile_TBC2024.csv <- textConnection("
  8,  2,  0,  0,  0,  0,  0,  0,  52
  0,  4,  14, 0,  0,  0,  0,  0,  28
  0,  0,  60, 47, 0,  0,  0,  0,  81
  0,  0,  0,  221,164,0,  0,  0,  618
  0,  0,  0,  0,  1047,87,0,  0,  1618
  0,  0,  0,  0,  0,  389,79, 0,  823
  0,  0,  0,  0,  0,  0,  8,  3,  174
  0,  0,  0,  0,  0,  0,  0,  6,  11
  157,82, 782,2890,3400,2252,637,167,0")
x <- as.matrix(read.csv(setupfile_TBC2024.csv, header=F))
rownames(x) <- c(mark.recaps24.weekly$isoweek,"s+1")
colnames(x) <- c(mark.recaps24.weekly$isoweek,"t+1")
x

res <- SPAS::SPAS.autopool(x)

#basic LP:
mod1 <- SPAS::SPAS.fit.model(x,
                             model.id="Logical pooling to single row (LP)",
                             row.pool.in=c(1,1,1,1,1,1,1,1), col.pool.in=1:8, 
                             row.physical.pool=FALSE)
SPAS::SPAS.print.model(mod1)


#full stratifcation:
mod2 <- SPAS::SPAS.fit.model(x,
                             model.id="Full weekly strata",
                             row.pool.in=1:8, col.pool.in=1:8)
SPAS::SPAS.print.model(mod2)



#auto (logical) pooling:
mod3 <- SPAS::SPAS.fit.model(x,
                             model.id="Auto (logical) pooling",
                             row.pool.in=c(1,1,3,4,5,6,8,8), col.pool.in=1:8, 
                             row.physical.pool=FALSE)
SPAS::SPAS.print.model(mod3)

model.list <- mget( ls()[grepl("^mod.$",ls())])
names(model.list)


report <- plyr::ldply(model.list, function(x){
  #browser()
  data.frame(#version=x$version,
    model.id         = x$model.info$model.id,
    s.a.pool         =-1+nrow(x$fit.setup$pooldata),
    t.p.pool         =-1+ncol(x$fit.setup$pooldata),
    logL.cond        = x$model.info$logL.cond,
    np               = x$model.info$np,
    AICc             = x$model.info$AICc,
    gof.chisq        = round(x$gof$chisq,1),
    gof.df           = x$gof$chisq.df,
    gof.p            = round(x$gof$chisq.p,3),
    Nhat             = round(x$est$real$N),
    Nhat.se          = round(x$se $real$N))
  
})
report

report.table <- report %>% 
  select(-c(.id,logL.cond,np)) %>% 
  mutate(deltaAICc = round(AICc-min(AICc),2)) %>% 
  select(model.id,s.a.pool,t.p.pool,AICc,deltaAICc,gof.chisq,gof.df,gof.p,Nhat,
         Nhat.se) %>% 
  arrange(deltaAICc)


# conclusion is that stratification with pooling was more parsimonious than LP model
#due to lower AIC and better GOF. Result: 35717 (+/-1317 SE)

