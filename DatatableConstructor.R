library(tidyverse)
library(readxl)
library(data.table)
library(stringr)

folder<-'C:/Users/luxma/OneDrive/Documents/AEI/Corruption Index/'
start.time<-Sys.time()

##Read files

### READ CROSSWALK FILES ###
xwalk<-read_excel(paste0(folder,'BEA_CROSSWALK.xlsx'))
xwalk2 <- read_excel(paste0(folder, '2017-Census-to-Naics-XWALK.xlsx'), 
                     sheet = '2007 - 2017 Industry Crosswalk', 
                     skip = 7)
xwalk3<-read_excel(paste0(folder,'NAICS to BEA Crosswalk.xlsx'))

#BEA Broad  
bea_broad<-unique(xwalk3$`BEA Line`)
bea_broad<-bea_broad[!grepl('NA', bea_broad)]


###Inflation- index to 2020 dollars

index<-fread(paste0(folder,'PCEPI.csv'))
index$DATE<-format(index$DATE,'%Y')
index<-index%>%filter(as.numeric(index$DATE)>2002)
index<-index[-nrow(index),]
colnames(index)<-c('year','PCEPI')
index$PCEPI<-as.numeric(index$PCEPI)
index$year<-as.numeric(index$year)
#index$year<-ifelse(index$year%%2==1,index$year+1,index$year)




###PACs

#columns that are important (for speed)
keep.col<-c(1,5,7)

#IF you put pac files in a different folder, change it here
#folder<-'pac_files folder'
pac.files<-list.files(path=folder,pattern='^pacs',full.names=TRUE)
pacs<-rbindlist(lapply(pac.files,fread,sep=',',select=keep.col))

#Format Dates
pacs$V6<-str_sub(pacs$V6,-4)

#Merge by year and industry
pacs<-data.table(pacs)
pacs$V7<-toupper(pacs$V7)
pacs<-pacs[,.(pacs.contr=sum(as.numeric(V5),na.rm=TRUE)), by = list(V1,V7)]
#Alternative to data.table but slower
#pacs<-aggregate(as.numeric(pacs$V5),by=list(pacs$V1, pacs$V7),FUN='sum',na.rm=TRUE)

#Format to remove | and rename columns
pacs[] <- lapply(pacs, gsub, pattern = '|', replacement = '', fixed = TRUE)
colnames(pacs)<-c('year','OpenSecrets.Industry','pac.contr')



###Indivs
#Only care about these columns
keep.col<-c(2,16,17,26)

#Change folder IF indiv files are in a sep. folder
#folder<- 'indivs_folder'
indiv.files<-list.files(path=folder, pattern='^indivs',full.names=TRUE)
indivs<-rbindlist(sapply(indiv.files, fread, simplify = FALSE, sep='auto', header=FALSE, select=keep.col, fill=TRUE))

#Renaming columns
colnames(indivs)<-c('year','OpenSecrets.Industry','indiv.contr','pac.double')

#Remove duplicates
indivs<-filter(indivs,!(pac.double %in% c('PB','PL','PI','PO','PU')))

#Convert to data table
indivs<-data.table(indivs)

#Convert all names to uppercase
indivs$OpenSecrets.Industry<-toupper(indivs$OpenSecrets.Industry)

#Split merged column
#This should be faster, have to test
indivs<-indivs[,c('remove','date', 'indiv.contr') :=tstrsplit(indivs$indiv.contr, ',', fixed=TRUE)]
indivs[,c('remove','date'):=NULL]
#Below also works but is slower
#indivs$indiv.contr<-lapply(indivs$indiv.contr, function(x) (strsplit(x,split=',')[[1]][3]))

#Aggregate by year and industry
indivs<-indivs[,.(indiv.contr=sum(as.numeric(indiv.contr),na.rm=TRUE)), by = list(OpenSecrets.Industry,year)]

#Combine 2020 with rest
indivs<-rbind(indivs,indivs20)
#Combine PACS and Indivs
indivs<-indivs[,year:=as.character(year)]
indivs<-indivs[,indiv.contr:=as.character(indiv.contr)]

#Older method - Works, keeps all values and is quite fast
sheet20<-merge.data.table(pacs,indivs,by=c('year', 'OpenSecrets.Industry'),all=TRUE)

#This is the data table way, supposed to be faster
#does not keep all values, but have to fix
#sheet20.1<-pacs[indivs, on = c(year='year', OpenSecrets.Industry='OpenSecrets.Industry'),all=TRUE]
#Matches unique entries from pacs&indivs



###Lobbying
keep.col<-c(5,6,8)
lobbying<-fread(paste0(folder,'lob_indus.txt'),sep='|',header=FALSE,fill=TRUE,select=keep.col)%>%
  subset(V6%in%c(2003:2020))

#Format
lobbying$V5<-as.numeric(gsub(',','',lobbying$V5))
lobbying$V8<-toupper(lobbying$V8)
colnames(lobbying)<-c('lobbying.amt','year','OpenSecrets.Industry')
lobbying$year<-as.numeric(lobbying$year)

#Aggregate Industry and year
lobbying<-lobbying[,.(lobbying.amt=sum(as.numeric(lobbying.amt),na.rm=TRUE)), by = list(year, OpenSecrets.Industry)]
lobbying<-merge.data.table(lobbying,index,by=c('year'),all.x=T)
lobbying<-lobbying%>%
  mutate(lobbying.adj=lobbying.amt/PCEPI*100)%>%
  select(year,OpenSecrets.Industry,lobbying.adj)

#Convert lobbying to a two-year period
lobbying$year<-ifelse(lobbying$year%%2==1,lobbying$year+1,lobbying$year)
lobbying<-lobbying[,.(lobbying.adj=sum(as.numeric(lobbying.adj),na.rm=TRUE)), by = list(OpenSecrets.Industry,year)]
lobbying$year<-as.character(lobbying$year)

#Adding lobbying to combined sheet
sheet20<-merge.data.table(sheet20,lobbying,by=c('year', 'OpenSecrets.Industry'),all=TRUE)

#Keep for later
union_contribution_sheet<-sheet20

#BEA Lines
BEA_temp<-data.frame(xwalk$Catcode,xwalk$`Line number (A)`,xwalk$`Line number B`, xwalk$`Line number (C)`, xwalk$`Line number D`)
colnames(BEA_temp)<- c('OpenSecrets.Industry','BEA.line.A','BEA.line.B','BEA.line.C','BEA.line.D')
BEA_temp$BEA<-NA

#Have not found a cleaner solution
for (i in 1:nrow(BEA_temp)) {
  
  if (BEA_temp$BEA.line.A[i] %in% bea_broad) {
    BEA_temp$BEA[i]<-BEA_temp$BEA.line.A[i]
  }
  if (BEA_temp$BEA.line.B[i] %in% bea_broad) {
    BEA_temp$BEA[i]<-BEA_temp$BEA.line.B[i]
  }
  if (BEA_temp$BEA.line.C[i] %in% bea_broad) {
    BEA_temp$BEA[i]<-BEA_temp$BEA.line.C[i]
  }
}
### To make it easier in the future, should make this its own xwalk

sheet20<-merge(sheet20,BEA_temp,by=c('OpenSecrets.Industry'),all.x=TRUE)

contributions_sheet<-sheet20



### READ UNION & OUTPUT DATA ###
union<-read_excel(paste0(folder,'UnionMembership.xlsx')) %>%
  subset(Year %in% seq(2004,2020,2))

### ADD 0 TO BEGINNING OF INDUSTRY CODES IN UNION SHEET SO THAT THEY MATCH CROSSWALK INDUSTRY CODES ###
### (IE IND CODE 170 ----> 0170) ###
union$Industry<-formatC(as.numeric(union$Industry), width = 4, format = 'd', flag = '0')
union$NAICS<-NA

#Need to remove this strings that show up in the beginning of column
xwalk2$`2007 NAICS Code`<-str_remove(xwalk2$`2007 NAICS Code`,'Pts. ')
xwalk2$`2007 NAICS Code`<-str_remove(xwalk2$`2007 NAICS Code`,'Part of ')

#Still have to find the data.table method for this
### FIRST ASSIGN CORRESPONDING NAICS CODES TO UNION SHEET BASED ON CENSUS INDUSTRY CODE ###
for (x in 1:nrow(union)) {
  for (y in 1:nrow(xwalk2)) {
    if (identical(substr(union$Industry[x],1,3), substr(xwalk2$`2007 Census Code`[y],1,3))) {
      if (grepl('^1', xwalk2$`2007 NAICS Code`[y])==T | grepl('^48', xwalk2$`2007 NAICS Code`[y])==T | grepl('^49', xwalk2$`2007 NAICS Code`[y])==T) {
        union$NAICS[x]<-substr(xwalk2$`2007 NAICS Code`[y], 1, 3)
      }
      else {
        union$NAICS[x]<-substr(xwalk2$`2007 NAICS Code`[y], 1, 2)
      }
    }
  }
}

#Add BEA lines to Union based on NAICS
union<-data.table(union)
setnames(xwalk3,'NAICScodefollowingfinalstruc','NAICS')

#Create file with only necessary columns
BEAtoNAICS<-xwalk3 %>% select('NAICS','BEA Line')
BEAtoNAICS<-data.table(BEAtoNAICS)
BEAtoNAICS[, names(BEAtoNAICS) := lapply(.SD, as.character)]

#Merge to include NAICS
union<-union[BEAtoNAICS, on=c('NAICS')]

#Aggregate desired years
union$Year<-as.numeric(union$Year)
colnames(union)[1]<-c('year')
vec<-union[,.(union_members_aggregate_by_BEA=sum(as.numeric(UnionMembers),na.rm=TRUE)), by = list(`BEA Line`,year)]
union$Year<-ifelse(union$Year%%2==1,union$Year+1,union$Year)
vec$Year<-ifelse(vec$Year%%2==1,vec$Year+1,vec$Year)
vec<-vec[,.(union_members_aggregate_by_BEA=mean(as.numeric(union_members_aggregate_by_BEA),na.rm=TRUE)), by = list(`BEA Line`,year)]
#Remove the row with the asterisk
#union<-union[-c(1317)] Might not be there anymore, have to check

#Here is the aggregate 
#Original code is confusing
contributions_sheet<-contributions_sheet[,`:=`(pac_contributions_aggregate_by_BEA=sum(as.numeric(pac.contr),na.rm=TRUE)), by = list(BEA,year)]
contributions_sheet<-contributions_sheet[,`:=`(indiv_contributions_aggregate_by_BEA=sum(as.numeric(indiv.contr),na.rm=TRUE)), by = list(BEA,year)]
contributions_sheet<-contributions_sheet[,`:=`(lobbying_by_BEA=sum(as.numeric(lobbying.adj),na.rm=TRUE)), by = list(BEA,year)]

#Rename and merge union & contribution data
setnames(vec,'BEA Line','BEA')
vec[, names(vec) := lapply(.SD, as.character)]
contributions_sheet[, names(contributions_sheet) := lapply(.SD, as.character)]
contributions_sheet<-contributions_sheet[vec, on=c('BEA','year')]

#Write as Excel file
#Don't want to over-write to compare to Adam's sheet
#write.csv(contributions_sheet, 'contributions_pacs_individual.csv')



###Final DF
final_df<-data.table()
final_df<-union[,.(union_members=sum(as.numeric(UnionMembers),na.rm=T)), by=list(year,`BEA Line`)]
final_df$pac_contributions_aggregate_by_BEA<-contributions_sheet[,.(pac_contributions_aggregate_by_BEA=sum(as.numeric(pac.contr),na.rm=TRUE)), by = list(year,BEA)][[3]]
final_df$indiv_contributions_aggregate_by_BEA<-contributions_sheet[,.(indiv_contributions_aggregate_by_BEA=sum(as.numeric(indiv.contr),na.rm=TRUE)), by = list(year,BEA)][[3]]
final_df$lobbying_by_BEA<-contributions_sheet[,.(lobbying_by_BEA=sum(as.numeric(lobbying.adj),na.rm=TRUE)), by = list(BEA,year)][[3]]
final_df$year<-as.numeric(final_df$year)

###Union
unionlist<-c('LB100', 'LC100', 'LC150','LE100','LE200',
             'LM100','LM150','LT000','LT100','LT300','LT400','LT500',
             'LT600','L1000','L1100','L1200','L1300','L1400','L1500','L0000','L5000','LA100',
             'LD100','LG000','LG100','LG200','LG300','LG400','LG500','LH100')

#They used a new data.frame, but it is the same as variable pacs and indivs
#The final should be the similar to sheet18
#Now to only select unionlist industries
union_contribution_sheet<-union_contribution_sheet[OpenSecrets.Industry%in% unionlist]

#Union Totals
union_contributions_total<-union_contribution_sheet[,.(pac.contr=sum(as.numeric(pac.contr),na.rm=TRUE)), by = list(year)]
union_contributions_total$indiv.contr<-union_contribution_sheet[,.(indiv.contr=sum(as.numeric(indiv.contr),na.rm=TRUE)), by = list(year)][[2]]
union_contributions_total$total.members<-union[,.(total.members=sum(as.numeric(UnionMembers),na.rm=TRUE)), by = list(year)][[2]]
union_contributions_total$union.lobbying<-union_contribution_sheet[,.(lobbying.adj=sum(as.numeric(lobbying.adj),na.rm=TRUE)), by = list(year)][[2]]

########################################
### ASSIGN UNION PROPORTIONS ##########
final_df$year<-as.character(final_df$year)
final_df<-data.table(final_df)
setkey(final_df,year)[union_contributions_total, union_share_percent:=union_members/total.members*100]

##########################################################
### CREATE COLUMN FOR UNION CONTRIBUTIONS BY PROPORTION####
options(scipen = 999)
setkey(final_df,year)[union_contributions_total, union_contributions:=indiv.contr*union_share_percent/100]
union_contributions_total$year<-as.numeric(union_contributions_total$year)
final_df$year<-as.numeric(final_df$year)
final_df<-merge(final_df,union_contributions_total,by='year',all.x=T)
final_df$union_lobbying<-final_df$union_share_percent*final_df$union.lobbying/100

############################################################
###### ASSIGN INDUSTRY OUTPUT ###############################
years<-as.character(2003:2020)
output<-read_excel(paste0(folder,'gross_output.xlsx'), skip=7) %>%
  select(ends_with(years), 'Line')
output<-data.table(output)
output<-melt(output,id.vars='Line')
colnames(output)<-c('BEA Line','year','output.amt')
output$year<-as.numeric(levels(output$year))[output$year]
output$`BEA Line`<-as.numeric(output$`BEA Line`)

#Aggregate every two years
output$year<-ifelse(output$year%%2==1,output$year+1,output$year)
output<-output[,.(output.amt=sum(as.numeric(output.amt))), by=list(year,`BEA Line`)]

#Adjust to 2020 dollars
output<-merge.data.table(output,index,by=c('year'),all.x=T)
output<-output %>%
  mutate(output.adj=as.numeric(output.amt)/PCEPI*100)

output$output.amt<-as.numeric(output$output.amt)
final_df <- rename(final_df,c('year'='year'))
final_df$`BEA Line`<-as.numeric(final_df$`BEA Line`)
final_df$year<-as.numeric(final_df$year)
final_df<-merge(final_df,output,by=c('BEA Line','year'),all.x=T)



################################################################
###### CREATE CORRUPTION RATIO ##################################
final_df$year<-ifelse(final_df$year%%2==1,final_df$year+1,final_df$year)
final_df<-final_df%>%
  group_by(year,`BEA Line`)%>%
  mutate(union_lobbying=sum(union_lobbying),
         lobbying_by_BEA=sum(lobbying_by_BEA))%>%
  select(-c('pac.contr','indiv.contr','total.members','union.lobbying','output.amt'))

final_df<-na.omit(final_df,cols=c('output.adj'))

final_df<- final_df%>%
  mutate(pac_contributions_aggregate_by_BEA = pac_contributions_aggregate_by_BEA/PCEPI*100)%>%
  mutate(indiv_contributions_aggregate_by_BEA = indiv_contributions_aggregate_by_BEA/PCEPI*100)%>%
  mutate(union_contributions = union_contributions/PCEPI*100)%>%
  mutate(spending = pac_contributions_aggregate_by_BEA + indiv_contributions_aggregate_by_BEA + union_contributions + lobbying_by_BEA + union_lobbying)%>%
  mutate(corruption_index = ((pac_contributions_aggregate_by_BEA + indiv_contributions_aggregate_by_BEA + union_contributions + lobbying_by_BEA + union_lobbying)/output.adj))

final_df$output.adj<-final_df$output.adj*10^6

#Add BEA Levels to final_df
xwalk3$`BEA Line`<-as.character(xwalk3$`BEA Line`)
xwalk4 <- read_excel(paste0(folder, 'BEA_CROSSWALK.xlsx'), 
                     sheet = 'BEA Lines and Levels')
colnames(xwalk4)<-c('BEA Line','Industry','BEA Level')
final_df<-merge(final_df,select(xwalk4,`BEA Line`,`BEA Level`),by=c('BEA Line'),all.x = T)

#
# for (i in seq(2004,2020,2)){
#   temp<-final_df[year==i]%>%
#     select(`BEA Line`,spending,output.adj,corruption_index)
#   temp<-unique(temp)
#   data.table(temp)
#   temp<-rbind(temp,t(colSums(temp)),fill=F)
#   temp$`BEA Line`[length(temp$`BEA Line`)]<-'Total'
#   temp$corruption_index[length(temp$corruption_index)]<-temp[length(temp$`BEA Line`),2]/temp[length(temp$`BEA Line`),3]
#   temp$corruption_index<-unlist(temp$corruption_index)
#   temp<-merge.data.table(temp,select(xwalk3,`BEA Line`,`BEA Name`),by=c('BEA Line'),all.x=T)
#   temp<-setorder(temp,-corruption_index)
#   temp<-temp[,c(5,1,2,3,4)]
#   temp<-unique(temp)
#   write_csv(temp, paste0('Table',i,'.csv'))
#}
####WRITE###
write.csv(final_df, 'final_data_contributions.csv')

end.time<-Sys.time()
duration<-start.time-end.time



#INCLUDING BUSINESS ASSOC.
# G<-sheet20[grep('^G1',sheet20$OpenSecrets.Industry)]%>%
#   select(c('year','OpenSecrets.Industry','pac.contr','indiv.contr'))%>%
#   group_by(year,OpenSecrets.Industry)%>%
#   summarise(pac.contr=sum(as.numeric(pac.contr),na.rm=T), indiv.contr=sum(as.numeric(indiv.contr),na.rm=T))
# 
# g.lobbying<-lobbying[grep('^G1',lobbying$OpenSecrets.Industry)]
# G<-merge.data.table(g.lobbying,G,by=c('year','OpenSecrets.Industry'),all=T)
# G$year<-as.numeric(G$year)
# G<-merge.data.table(G,index,by=c('year'))
# G<-G%>%
#   mutate(pac.contr=pac.contr/PCEPI*100)%>%
#   mutate(indiv.contr=indiv.contr/PCEPI*100)%>%
#   mutate(spending=indiv.contr+pac.contr+lobbying.adj)
# G$year<-ifelse(G$year%%2==1,G$year+1,G$year)
# G<-G%>%
#   group_by(year)%>%
#   summarise(spending=sum(spending,na.rm=T))
# G$output.adj<-NA
# total_corruption<-final_df%>%
#   group_by(year)%>%
#   summarise(spending=sum(spending,na.rm=T),output.adj=sum(output.adj,na.rm=T))
# total_corruption<-total_corruption%>%
#   mutate(corruption_index=spending/output.adj*10^6)
# total_corruption<-rbind(total_corruption,G)
# 


#BUSINESS NAMES
# keep.col<-c(2,16,17,26,40)
# indiv.files<-list.files(path=folder, pattern='^indivs',full.names=TRUE)
# indivs<-rbindlist(sapply(indiv.files, fread, simplify = FALSE, sep='auto', header=FALSE, select=keep.col, fill=TRUE))
# colnames(indivs)<-c('year','OpenSecrets.Industry','indiv.contr','pac.double','business')
# indivs<-filter(indivs,!(pac.double %in% c('PB','PL','PI','PO','PU')))
# indivs<-data.table(indivs)
# indivs$OpenSecrets.Industry<-toupper(indivs$OpenSecrets.Industry)
# b<-indivs[grep('^G1',indivs$OpenSecrets.Industry)]
# b<-unique(b$business)
# b<-toupper(b)
# b<-trimws(b)
# b<-gsub('\\.','',b)
# b<-gsub('LLC','',b)
# b<-gsub('L.L.C.','',b)
# b<-gsub('TECH','TECHNOLOGY',b)
# b<-gsub('ASSOC ','ASSOCIATED ',b)
# 
# 
# b<-unique(b)
# b<-sort(b)
# view(b)
# 
# b<-fread(paste0(folder,'business_names.csv'),select=c(1))
# colnames(b)<-c('business')
# c<-indivs[grep('^G1',indivs$OpenSecrets.Industry)]%>%
#   select(c('OpenSecrets.Industry','business'))
# b<-merge(b,c,by=c('business'),all.x=T)

### PLOTS ###

#Plot of median corruption index
d<-data.table()
d<-final_df%>%
  filter(`BEA Line`==4)%>%
  select(year,corruption_index)
colnames(d)<-c('year','ag')

ggplot(data=d)  + 
  geom_bar(aes(x=year, y=ag),stat='identity',fill='#228B22')+
  geom_vline(aes(xintercept=2008), alpha=0.9, size=2)+
  geom_vline(aes(xintercept=2014), alpha=0.9, size=2)+
  geom_vline(aes(xintercept=2018), alpha=0.9, size=2)+
  labs(title=paste0('Corruption Index: Agriculture'),x='year',y='Corruption Index')

ggplot(data=d, mapping=aes(x=d$year, y=d$diff,group=1))  + 
  geom_bar(stat='identity',fill='#9DC084')+
  labs(title=paste0('Corruption Index Difference: Agriculture-Median'),x='year',y='Corruption Index Difference')

#Corruption index distribution
corruption_index<-final_df[,corruption_index]
year<-final_df[,year]
d=data.frame(year,corruption_index)

temp2020<-temp%>%filter(year==2020)
ggplot(data=temp2020,mapping=aes(x=temp2020$output.sum.adj, y=temp2020$total.spending))+
  geom_point(size=1)+
  scale_x_log10()+
  scale_y_log10()+
  labs(title=paste0('Output vs Spending'),x='Output',y='Total Spending')


library(gganimate)
p <- ggplot(d, aes(x=corruption_index, fill=year)) +
  geom_histogram(bins=50, alpha=.8, position='identity') +
  transition_states(year)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  labs(title = 'Corruption Index: ')+
  xlab('Corruption Index [dollars/millions dollars of GDP]')

q<- ggplot(d, aes(x=corruption_index, fill=year)) +
  geom_density(alpha=0.7,position='identity')+
  transition_states(year)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  ease_aes('cubic-in')+
  labs(title = 'Corruption Index: ')
animate(p,fps=4,renderer=gifski_renderer())
anim_save('corruption_dist.gif',animation=p,fps=4,renderer=gifski_renderer())

final_df$year<-ifelse(final_df$year%%2==1,final_df$year+1,final_df$year)
final_df<-final_df%>%
  group_by(BEA,year)%>%
  summarise(pac_contributions_aggregate_by_BEA=sum(pac_contributions_aggregate_by_BEA),
            indiv_contributions_aggregate_by_BEA=sum(indiv_contributions_aggregate_by_BEA),
            lobbying_by_BEA=sum(lobbying_by_BEA))

