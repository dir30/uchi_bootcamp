#stats programming challenge
#GFD, 9/13/2022

covid_data=read.csv("owid-covid-data.csv",na.strings="",header=T)
colnames(covid_data)

new_cases=covid_data$new_cases
repro_rate=covid_data$reproduction_rate 

relevant_data_idx=which(repro_rate!="NA") #only want data without "NA"

pearson_corr=cor.test(new_cases[relevant_data_idx],repro_rate[relevant_data_idx])
#p < 2.2 x 10^-16
#This test would indicate a significant relationship between new cases of covid
#19 and the reproduction rate. This could mean that new cases decreased fertility,
#indicating an effect of covid on reproductive systems. It could indicate the 
#global pandemic turned people off to having children. It could be argued
#a non-sensical way, that people not having babies leads to more covid cases.
#Since it is a correlation test, all we can argue is that there exists
#a relationship.

#looking at each year
year_vector=substr(covid_data$date,1,4)
rows_2020=which(year_vector=="2020")
rows_2021=which(year_vector !="2021")
rows_2022=which(year_vector !="2022")

pearson_2020=cor.test(new_cases[rows_2020],repro_rate[rows_2020])
#p=.0033
pearson_2021=cor.test(new_cases[rows_2021],repro_rate[rows_2021])
#2.2 x 10^-16
pearson_2022=cor.test(new_cases[rows_2022],repro_rate[rows_2022])
#p=2.516 x 10^-13

#looking at september each year
year_day_vector=substr(covid_data$date,1,7)
rows_09_2020=which(year_day_vector=="2020-09")
rows_09_2021=which(year_day_vector !="2021-09")
rows_09_2022=which(year_day_vector !="2022-09")

pearson_09_2020=cor.test(new_cases[rows_09_2020],repro_rate[rows_09_2020])
#p=0.09933

pearson_09_2021=cor.test(new_cases[rows_09_2021],repro_rate[rows_09_2021])
#p<2.2e-16

pearson_2022=cor.test(new_cases[rows_09_2022],repro_rate[rows_09_2022])
#p<2.516 x 10^-16

#we find no significant correlation in september 2020. 
#If we are arguing that covid-19 negatively impacts reproductive systems, 
#we could argue that the strain present in september 2020 did not target
#repro systems, but strains in consecutive years did. 

#This seems unlikely, especially considering that oct 2020 does have a significant
#correlation.
rows_10_2020=which(year_day_vector=="2020-10")
pearson_10_2020=cor.test(new_cases[rows_10_2020],repro_rate[rows_10_2020])
#p=.00518

#Rather, we probably don't achieve a significant correlation in 09/2020
#(hit a p<.05) due to randomness in our sample. It's impossible to tell, because 
#this test is simply looking for correlations/relationships in the data. It does
#not give us the tools to imply causation / to know the underlying
#structure of the relationship between the two.

