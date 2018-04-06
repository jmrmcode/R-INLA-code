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
files<-list.files(path = "G:/sagebrushRefSit", pattern = '.csv',full.names=T)
data2<-read.csv('C:/Users/juanm/Downloads/ee-chart.csv')

# Loop to import and merge the .csv files
data1<-data.frame()
data2<-data.frame()
for(i in 1:length(files)) {
  d1<-read.csv(files[i])
  data1<-d1
  data2<-rbind(data2,data1)
}
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
load('D:/BSU_Lab/Code_Repository/temporalSeries.Rdata')

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

# To calculate number of LS images per site and month
dates_site_month<-NULL
n<-NULL
Number_dates<-NULL
m<-NULL
Year<-NULL
t<-NULL
Sites<-NULL
h<-NULL
Month<-NULL
for (i in 1:length(temporalSeries2)) {
  nam<-names(temporalSeries2[[i]][2])
  year<-sub(".*, ", "", temporalSeries2[[i]][1,1])
  for(j in c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')) {
    month<-str_detect(temporalSeries2[[i]]$system.time_start,j)
    if (is.na(temporalSeries2[[i]][1,1])) {} else {
      month2<-substr(temporalSeries2[[i]][month,1],start=1, stop=3)[1]
      n<-length(temporalSeries2[[i]][month,2])
      Number_dates<-rbind(Number_dates,n)
      t<-nam
      Sites<-rbind(Sites,t)
      m<-year
      Year<-rbind(Year,m)
      h<-month2
      Month<-rbind(Month,h)
      dates_site_month<-data.frame(Sites,Year,Month,Number_dates)
    }
  }
}


#How many images are there per year in a site?
dates_site_year[which(dates_site_year$Sites=='X4'),]
boxplot(Number_dates~Year,data = dates_site_year[,21], xlab='Year', ylab='Number of images (dates)')
describeBy(dates_site_year$Number_dates, group = dates_site_year$Sites)$X4
boxplot(Number_dates~Sites)

dates#How many images are there per month and year in a site?
dates_site_month[which(dates_site_month$Sites=='X4'&dates_site_month$Year==2000),]
boxplot(Number_dates~Month,data = dates_site_month[which(dates_site_month$Sites=='X4'
                                                         &dates_site_month$Year=='2016'),], xlab='Reference sites', ylab='Number of images (dates)')


# Plot histograms
data<-dates_site_year[which(dates_site_year$Sites=='X4'|dates_site_year$Sites=='X5'|dates_site_year$Sites=='X6'),]
ggplot(data,aes(x=Number_dates))+geom_histogram()+theme_bw()+facet_grid(Sites~.)


# Plot histograms
data<-dates_site_month[which(dates_site_month$Sites=='X4'),]
ggplot(data,aes(x=Number_dates))+geom_histogram()+theme_bw()+facet_grid(Sites:Month~.)


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

# TO PLOT TEMPORAL SERIES 
#one way

EVI<-temporalSeries3[c(100:131)]#X7
EVI<-temporalSeries3[c(67:98)]#X6
EVI<-temporalSeries3[c(1:32)]#X4
EVI<-temporalSeries3[c(34:65)]#X5
EVI<-temporalSeries3[c(200:230)]#X2
EVI<-temporalSeries3[c(167:197)]#X3
EVI<-temporalSeries3[c(233:263)]#X1
EVI<-do.call(rbind,EVI)

size1<-length(EVI$system.time_start)
size2<-length(EVI$system.time_start)+1
size3<-size1+length(EVIdisturbed$system.time_start)
data<-data.frame(system.time_start = c(as.character(EVI$system.time_start),as.character(EVIdisturbed$system.time_start)),
# be careful, change the site number
EVI=c(as.numeric(EVI$X7),as.numeric(EVIdisturbed$disturbed1)),
dates=c(as.Date(EVI$ISOdate, format= '%m/%d/%Y'),as.Date(EVIdisturbed$ISOdate,format ='%m/%d/%Y') ))
data$group<-NA
data$group[c(1:size1)]<-'Reference site' ## pay attention: change the number of rows,494
data$group[c(size2:size3)]<-'Disturbed site'
data<-na.omit(data)

ggplot(data,aes(x=dates,y=EVI,shape = group,color=group)) + geom_line() + stat_smooth(method = "loess", formula = y~x,span =0.40 ,size = 2,na.rm = T) +
ggtitle("Disturbed site vs. reference n25")

#another way bu using the same data frame

# to plot only winter
ggplot(data = data[which(str_detect(data$system.time_start,c('Dec'))|
str_detect(data$system.time_start,c('Jan'))|
str_detect(data$system.time_start,c('Feb'))),],
mapping = aes(x = dates, y = EVI, group=group, colour = group)) +
  geom_point() + ggtitle("Disturbed site vs. reference n1") +
  geom_line() #+ stat_smooth(method = "loess", formula = y~x,span =0.40 ,size = 2,na.rm = T)

# to plot the complete period
ggplot(data = data,
mapping = aes(x = dates, y = EVI, group=group, colour = group)) +
geom_point() + ggtitle("Disturbed site vs. reference n1") +
geom_line() #+ stat_smooth(method = "loess", formula = y~x,span =0.40 ,size = 2,na.rm = T)


# Another way
# To select the site and year according to target ones
data<-temporalSeries3[which(names(temporalSeries3[])=='2016_X13')]


AirTempDaily <- ggplot(EVI, aes(as.Date(ISOdate,format='%m/%d/%Y'),X7)) +
  #AirTempDaily <- ggplot(data$'2016_X13', aes(as.numeric(system.time_start), X13)) +
  geom_point(na.rm=TRUE, color="black", size=2) +
  ggtitle("EVI reference site n? , 1985 - 2016") +
  xlab("Date") + ylab("EVI") + geom_line()

AirTempDaily + stat_smooth(method = "loess", formula = y ~ x, size = 2,na.rm = T)

###################################################################################
###################################################################################
###################################################################################
########################  DECOMPOSITING SERIES      ###############################
library('stats')
# reference site
mo_r <- strftime(data$dates[which(data$group=='Reference site')], "%m")
yr_r <- strftime(data$dates[which(data$group=='Reference site')], "%Y")
rf<-data.frame(mo_r,yr_r,data$EVI[which(data$group=='Reference site')])
rf.agg<-aggregate(data.EVI.which.data.group.....Reference.site... ~  yr_r + mo_r, rf, FUN = mean)
rf.agg$date <- as.POSIXct(paste(rf.agg$yr_r, rf.agg$mo_r, "01", sep = "-"))
names(rf.agg)<-c("yr_r", "mo_r","EVI", "date")

# disturbed site
mo_d <- strftime(data$dates[which(data$group=='Disturbed site')], "%m")
yr_d <- strftime(data$dates[which(data$group=='Disturbed site')], "%Y")
ds<-data.frame(mo_d,yr_d,data$EVI[which(data$group=='Disturbed site')])
ds.agg<-aggregate(data.EVI.which.data.group.....Disturbed.site... ~ mo_d + yr_d, ds, FUN = mean)
ds.agg$date <- as.POSIXct(paste(ds.agg$yr_d, ds.agg$mo_d, "01", sep = "-"))
names(ds.agg)<-c("mo_r", "yr_r","EVI", "date")

monthly<-aggregate(EVI ~ mo_r * yr_r, ds.agg, FUN = mean)
monthly$season<-NA
for (i in 1:length(monthly$EVI)) {
  if (monthly[i,1]=='01'|monthly[i,1]=='02'|monthly[i,1]=='12') {monthly[i,4]<-'winter'}
  else {
    if (monthly[i,1]=='03'|monthly[i,1]=='04'|monthly[i,1]=='05') {monthly[i,4]<-'spring'}
    else {
      if (monthly[i,1]=='06'|monthly[i,1]=='07'|monthly[i,1]=='08') {monthly[i,4]<-'summer'}
      else {
        if (monthly[i,1]=='09'|monthly[i,1]=='10'|monthly[i,1]=='11') {monthly[i,4]<-'autumn'}
        else {}
      }
    }
  }
}
monthly2<-aggregate(EVI ~ season*yr_r, monthly, FUN = mean)

rss<-ts(monthly2$EVI, frequency = 4, start = c(1986, 1), end = c(2016, 1))
bss<-ts(ds.agg$EVI, frequency = 4, start = c(1986, 1), end = c(2016, 1))

rs_dec<-stl(rss, s.window = 'per')
rs_bs<-stl(bss, s.window = 'per')

fit<- decompose(bss,type="additive")