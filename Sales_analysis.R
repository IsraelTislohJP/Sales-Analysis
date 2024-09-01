# Importing the required library

library(tidyverse)

# Reading the data into R

data<-read.csv('train.csv',sep=',')

# Checking for missing values in the data set

data.frame(missing = colSums(is.na(data)))

# Removing the rows with missing values

data<-na.omit(data)

# Extracting the Month from the order date column

dts<-data %>% separate_wider_delim(Order.Date,'/',names=c('Day','Month','Year'))
dts$Month[dts$Month == '1'] <- 'Jan'
dts$Month[dts$Month == '01'] <- 'Jan'
dts$Month[dts$Month == '2'] <- 'Feb'
dts$Month[dts$Month == '02'] <- 'Feb'
dts$Month[dts$Month == '3'] <- 'Mar'
dts$Month[dts$Month == '03'] <- 'Mar'
dts$Month[dts$Month == '4'] <- 'Apr'
dts$Month[dts$Month == '04'] <- 'Apr'
dts$Month[dts$Month == '5'] <- 'May'
dts$Month[dts$Month == '05'] <- 'May'
dts$Month[dts$Month == '6'] <- 'Jun'
dts$Month[dts$Month == '06'] <- 'Jun'
dts$Month[dts$Month == '7'] <- 'Jul'
dts$Month[dts$Month == '07'] <- 'Jul'
dts$Month[dts$Month == '8'] <- 'Aug'
dts$Month[dts$Month == '08'] <- 'Aug'
dts$Month[dts$Month == '9'] <- 'Sep'
dts$Month[dts$Month == '09'] <- 'Sep'
dts$Month[dts$Month == '10'] <- 'Oct'
dts$Month[dts$Month == '11'] <- 'Nov'
dts$Month[dts$Month == '12'] <- 'Dec'


# Analysing sales trends by Month and Year

p1<-dts %>% group_by(Month,Year) %>% summarize(ave_sales=mean(Sales),N=n()) %>%
	ggplot(aes(x=Month,y=ave_sales,show.legend=F,label=round(ave_sales,2)))+
	geom_hline(aes(yintercept=mean(dts$Sales)),color='black',linewidth=.8)+
	geom_line(linewidth=1,aes(group=1,color=Year),show.legend=F)+
	geom_point(size=2,aes(color=Year,shape=Year),show.legend=F)+
	geom_text(size=2,color='black')+
	labs(title='Monthly Sales Trend',y='Average Sales')+
	facet_wrap(~Year)+
	theme(plot.title = element_text(hjust=.5))+
	coord_flip()

print(p1)


# Analysing sales by product subcategories

p2<-dts %>% group_by(Sub.Category) %>% mutate(ave_sales = mean(Sales)) %>%
	ungroup() %>% mutate(Sub.Category=fct_reorder(Sub.Category,ave_sales)) %>%
	ggplot(aes(x=Sub.Category,y=ave_sales,color=Sub.Category,
	show.legend=F,label=round(ave_sales,2))) +
	geom_hline(aes(yintercept=mean(dts$Sales)),linewidth=.8,color='gray70')+
	geom_segment(aes(x=Sub.Category,y=mean(Sales),xend=Sub.Category,
	yend=ave_sales),linewidth=1,show.legend=F)+
	geom_point(size=6,aes(color=Sub.Category),show.legend=F)+
	coord_flip()+
	geom_text(size=2,color='black')+
	labs(title='Sales by Product Subcategory',x='',y='')+
	theme(plot.title=element_text(hjust=.5),axis.text.x=element_blank())

print(p2)




















