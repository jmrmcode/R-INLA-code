library('ggplot2')
library('scales')
library('stringr')
library('pipeR')
library('psych')

# Landsat temporal series were downloaded as .csv files that contained the 
# dates (eg., Apr 5, 1985) as rows and the reference sites (i.e., pixels) as 
# columns. Since GEE does not allow to download the completed series in one 
# go because their size, they were downloaded by year.
# The script below is aimed at removing NAs, ordering values per year and 
# site, extracting information to select references sites according to their 
# gaps, and plotting temporal series.



## To import the .csv files containing the temporal series, clean and extract information

# To get the relative paths to the csv files downloaded directly from GEE
files<-list.files(path = "D:/Cristina/Boise state University/PhD/Parcelas/Datos EVI", pattern = '.csv',full.names=T)
data2<-read.csv('D:/Cristina/Boise state University/PhD/Parcelas/ee-chart.csv')

# Loop to import and merge the .csv files
data1<-data.frame()
for(i in 1:length(files)) {
  d1<-read.csv(files[i])
  data1<-d1
  data2<-cbind(data2,data1)
}
#Changing name of first column so it is not erased
colnames(data2)[1] = "Time"
#Remove repeated columns
data2$system.time_start<-NULL
#Change the name back so it is recognized by the code
colnames(data2)[1] = "system.time_start"
# Loop to remove NA and group the data by year
n<-NULL
temporalSeries<-NULL
for(i in 2:ncol(data2)) {
  data3<-na.omit(data2[,c(1,i)])
  for(j in 1985:2017) {
    year<-str_detect(data3$system.time_start,toString(j))
    paste0('data_',j) %>>% (~data4)
    data4<-data3[year,]
    name1<-paste0(toString(j),'_',names(data3)[2])
    n <- list(j = data4)
    #n <- setNames(n$j,name1)
    temporalSeries<-do.call(c, list(temporalSeries, n))
  }
}
#temporalSeries[[65]][,,]#site x5, year 1985 (first column), second row and first column

# To average values from the same both date and site
for(i in 1:length(temporalSeries)) {
  if (nrow(temporalSeries[[i]])==0|nrow(temporalSeries[[i]])==1) {} else {
    for(j in 1:c(nrow(temporalSeries[[i]])-1)) {
      if (temporalSeries[[i]][j,1]==temporalSeries[[i]][j+1,1]) {
        temporalSeries[[i]][j,2]<-mean(c(temporalSeries[[i]][j,2],temporalSeries[[i]][j+1,2]),na.rm = T);
        temporalSeries[[i]][j+1,2]<-NA} else {}
    }
  }
}
#save(temporalSeries,file='D:/BSU_Lab/Code_Repository/temporalSeries.Rdata')
#load('D:/BSU_Lab/Code_Repository/temporalSeries.Rdata')

# To remove NA from the duplicate dates (run with the just above loop)
temporalSeries2<-lapply(temporalSeries, na.exclude) 
bbb<-temporalSeries

# To calculate number of LS images per site and year
dates_site_year<-NULL
n<-NULL
Number_dates<-NULL
m<-NULL
Year<-NULL
t<-NULL
Sites<-NULL
for (i in 1:length(temporalSeries2)) {
  number_Dates<-nrow(temporalSeries2[[i]][2])
  nam<-names(temporalSeries2[[i]][2])
  year<-sub(".*, ", "", temporalSeries2[[i]][1,1])
  if (is.na(year)) {} else {
    t<-nam
    Sites<-rbind(Sites,t)
    n<-number_Dates
    Number_dates<-rbind(Number_dates,n)
    m<-year
    Year<-rbind(Year,m)
    dates_site_year<-data.frame(Sites,Year,Number_dates)
  }
}


# To correctly name the components of the list of data frames
a<-NULL
temporalSeries3<-NULL
for(i in 1:length(temporalSeries2)) {
  names1<-names(temporalSeries2[[i]])[2]
  year<-sub(".*, ", "", temporalSeries2[[i]][1,1])
  a<-setNames(temporalSeries2[i],paste0(year,'_',names1))
  temporalSeries3<-temporalSeries3<-do.call(c, list(temporalSeries3, a))
}

#  To transform the date format downloaded from GEE in an accessible format for R
for(i in 1:length(temporalSeries3)) {
  if (nrow(temporalSeries3[[i]])==0) {} else {
    for (j in 1:length(temporalSeries3[[i]][,1])) {
      
      day<-as.numeric(substr(temporalSeries3[[i]][j,1],start=5, stop=6)[1])
      if (is.na(day)) {
        year<-as.numeric(sub(".*, ", "", temporalSeries3[[i]][j,1]))
        if (is.na(year)) {} else {
          day2<-as.numeric(substr(temporalSeries3[[i]][j,1],start=5, stop=5)[1])
          
          month1<-substr(temporalSeries3[[i]][j,1],start=1, stop=3)[1]
          m<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
          n<-which(month1==m)
          if (n < 10) {month2<-as.numeric(paste0('0',n))} else {month2<-n}
          f<-paste0(month2,'/',day2,'/',year)
          #ff<-as.Date(f,format='%m/%d/%Y')
          temporalSeries3[[i]]$ISOdate[j]<-f
          
        }  
      } else {
        
        year<-as.numeric(sub(".*, ", "", temporalSeries3[[i]][j,1]))
        if (is.na(year)) {} else {
          
          month1<-substr(temporalSeries3[[i]][j,1],start=1, stop=3)[1]
          m<-c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
          n<-which(month1==m)
          if (n < 10) {month2<-as.numeric(paste0('0',n))} else {month2<-n}
          f<-paste0(month2,'/',day,'/',year)
          #ff<-as.Date(f,format='%m/%d/%Y')
          temporalSeries3[[i]]$ISOdate[j]<-f
          
        }}
      
    }
  }}

#mean of reference sites
ref.2006<-mean(temporalSeries[][str_detect(names(temporalSeries3),"_X25090")][22][1][1]$j[,2],na.rm=T)
ref.2017<-mean(temporalSeries[][str_detect(names(temporalSeries3),"_X25090")][33][1][1]$j[,2],na.rm=T)
difRef<-ref.2017-ref.2006


#Make a table
Y<-c(2006,2017)
X25090<-c(ref.2006, ref.2017)
track<-as.data.frame(rbind(Y,X25090))
col_headings <- Y
names(track) <- col_headings
track<-track[-1,]

#create names list
site<-as.vector((colnames(data2)))
site<-site[-1]
site<-as.character(gsub("X","",site))


#mean of all sites
track1<-data.frame()
for (i in 2:298) {
  d1<-mean(temporalSeries[][str_detect(names(temporalSeries3),site[i])][22][1][1]$j[,2],na.rm=T)
  d2<-mean(temporalSeries[][str_detect(names(temporalSeries3),site[i])][33][1][1]$j[,2],na.rm=T)
  track1<-c(d1,d2)
  track<-rbind(track,track1)
}
#match each EVI with the plot number
track3<-cbind(site,track)


#x10394.2006<-mean(temporalSeries[][str_detect(names(temporalSeries3),"10394")][22][1][1]$j[,2],na.rm=T)
#x10394.2017<-mean(temporalSeries[][str_detect(names(temporalSeries3),"10394")][33][1][1]$j[,2],na.rm=T)
#x10394<-x10394.2017-x10394.2006

#Calculate differences between 2006 and 2017

change<-track3$`2017`-track3$`2006`
tra<-cbind(track3,change)