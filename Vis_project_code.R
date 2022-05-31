library(tidyverse)
library(ggplot2)
library(plotly)
# Read in the main data set 
df = read.csv("pgadata_2010_2018.csv", header = T, sep=",")

# Data pre-processing for the main data set
# Changing the column names
names(df) = c("name", "rounds", "fwy", "year", "avg_distance", "gir", "avg_putts", 
              "avg_scrambling", "avg_score", "fedex_points", "wins", "top10",
              "SG_putting", "avg_SG_total", "SG_OTT", "SG_APR", "SG_ARG", "earnings")

# Replacing NA values with zeros, removing string characters and casting column datatypes
df$wins = df$wins %>% replace_na(0)
df$top10 = df$top10 %>% replace_na(0)
df$fedex_points = as.numeric(df$fedex_points)
df$fedex_points = df$fedex_points %>% replace_na(0)
df$earnings = str_replace(df$earnings, "[$]", "")
df$earnings = str_remove_all(df$earnings, ",")
df$earnings = as.numeric(df$earnings)


# Question 1 - How has the profile of PGA Tour players evolved over the the last decade? 
# Filtered Data Frame 2010-11
data10_11 = df %>% filter(year %in% c(2010, 2011))
# Filtered Data Frame 2017-18
data17_18 = df %>%
  filter(year %in% c(2017,2018))
# Filtered Data Frame 2014-15
data14_15 = df %>% filter(year %in% c(2014, 2015))

data10_11$section = "2010-11"
data14_15$section = "2014-15"
data17_18$section = "2017-18"
data1011_1415_1718 = rbind(data10_11, data14_15, data17_18)

# EVOLUTION OF DRIVING DISTANCE
# Density plot of the average driving distances (2010-11/2014-15/2017-18)
data1011_1415_1718 %>% ggplot(aes(avg_distance, fill=section))+
  geom_density(alpha=.3)+
  geom_vline(xintercept = mean(data10_11$avg_distance), col="red", size=1, linetype="dashed")+
  geom_vline(xintercept = mean(data14_15$avg_distance), col="darkgreen", size=1, linetype="dashed")+
  geom_vline(xintercept = mean(data17_18$avg_distance), col="lightblue", size=1, linetype="dashed")+
  theme_minimal()+
  labs(x="Average Driving Distance", title="2010-11 vs 2014-15 vs 2017-18 Average Driving Distance Distributions")

# Collect the mean driving distances for each year. This will be used in the next vis
driving_distance_year = df %>% group_by(year) %>% summarise(avg_distance_year=mean(avg_distance))
# Count plot of driving distance per year 
df %>%
  ggplot()+
  geom_count(aes(x=year,y=avg_distance, col=year))+
  geom_line(data=driving_distance_year, aes(x=year, y=avg_distance_year), size=2, col="red")+
  labs(x="Year",y="Driving Distance", title="Driving Distances per year")+
  theme_minimal()+
  labs(col="Year", size="Occurences")


# DRIVING ACCURACY EVOLUTION
# How Fairway Accuracy has evolved over the time period
df %>% 
  group_by(year) %>% 
  summarise(driving_accuracy=mean(fwy), driving_accuracy_median=median(fwy)) %>% 
  ggplot(aes(year, driving_accuracy))+
  geom_point(aes(colour=driving_accuracy), size=5)+
  geom_point(aes(year, driving_accuracy_median), col="red", size=3)+
  geom_smooth(se=F, col="red")+
  geom_smooth(method=lm, col="red", se=F)+
  labs(y="PGA Tour Average Driving Accuracy (%)", x = "Year", title="Evolution of Driving Accuracy (2010-18)", col="Driving Accuracy (%)")+
  theme_minimal()

# This is when I realized that our question - how has the profile of the average PGA Tour player evolved over the 
# last decade is quite limiting in terms of their performance. Because apart from Driving distance and Driving accuracy, all of the other performance 
# metrics have essentially remained constant. So, I'll now look to see how the profile of the PGA Tour has evolved over the 
# last decade 

# Year grouped DF
year_df = df %>% group_by(year)

# EARNGINS EVOLUTION 
earnings_df = data.frame(year_df %>% summarise(earnings=mean(earnings, na.rm=T), rounds=mean(rounds)))
earnings_df$earnings_per_round = earnings_df$earnings / earnings_df$rounds

# Earnings per round line plot
earnings_df %>% ggplot(aes(x = year, y=earnings_per_round))+
  geom_line(col="green", size=2)+
  geom_point(col="red", size=3)+
  geom_smooth(se=F)+
  labs(x="Year", y="Earnings Per Round ($)", title="Average PGA Tour Earnings per Round ($)")+
  theme_minimal()

# How has the distribution of high finishes evolved over the last decade
# Bar plots of top 10 distribution split by year
df %>%
  ggplot(aes(x=top10, fill=year))+
  geom_bar()+
  facet_wrap(~year)+
  theme_light()+
  labs(x="Top 10's", title="Frequency of Top 10's per season")

#############################################
# Q2

# What are the general factors that separate the different levels of performance across a season
# Let's analyse this according to the conventional statistics and according to the strokes gained statistics.
summary(df$avg_SG_total)
# Group all of the instances in the data based on performance
df[df$avg_SG_total>0.5685,"group"] = "elite"
df[df$avg_SG_total <= 0.5685 & df$avg_SG_total >= -0.2547,"group"] = "average"
df[df$avg_SG_total<.2547,"group"] = "poor"

# Produce filtered data frames for each of the performance groups 
elite_seasons = df %>% filter(avg_SG_total>0.5685)
average_seasons = df %>% filter(avg_SG_total <= 0.5685 & avg_SG_total >= -0.2547)
poor_seasons = df %>% filter(avg_SG_total < -.2547)

# A couple of density plots to confirm SG & performance relationship
df %>% 
  ggplot(aes(x=earnings, fill=group))+
  geom_density(alpha=.3)

df %>% 
  ggplot(aes(x=top10, fill=group))+
  geom_density(alpha=.3)



# AVERAGES OF THE PERFORMANCE METRICS PER GROUP
averages_per_group = rbind(elite_seasons %>% summarise(distance=mean(avg_distance), accuracy=mean(fwy), gir=mean(gir), scrambling=mean(avg_scrambling), putts=mean(avg_putts)),
                           average_seasons %>% summarise(distance=mean(avg_distance), accuracy=mean(fwy),gir=mean(gir), scrambling=mean(avg_scrambling), putts=mean(avg_putts)),
                           poor_seasons %>% summarise(distance=mean(avg_distance), accuracy=mean(fwy),gir=mean(gir), scrambling=mean(avg_scrambling), putts=mean(avg_putts)))
scaled_avg_groups = scale(averages_per_group)

averages_per_group$groups = c("elite", "average", "poor")

scaled_grouped_df = data.frame(groups = c("elite", "average", "poor"), distance = scaled_avg_groups[,1], accuracy = scaled_avg_groups[,2], gir = scaled_avg_groups[,3], scrambling = scaled_avg_groups[,4], putting = scaled_avg_groups[,5]*-1)
scaled_grouped_df
# Now make a stacked bar chart using the scaled data

stacked_bar <- plot_ly(scaled_grouped_df, x=~groups, y=~distance, type = 'bar', name="Driving Distance")
stacked_bar <- stacked_bar %>% add_trace(y=~accuracy, name="Driving Accuracy")
stacked_bar <- stacked_bar %>% add_trace(y=~gir, name="GIR")
stacked_bar <- stacked_bar %>% add_trace(y=~scrambling, name="Scrambling")
stacked_bar <- stacked_bar %>% add_trace(y=~putting, name="Putting")
stacked_bar <- stacked_bar %>% layout(yaxis = list(title="Scaled Values", tickvals=seq(-1.5, 2.5, 0.25), barmode='stack'), 
                                      title = "Relative Scores of each performance statistic by season performance")
# Output the stacked bar chart
stacked_bar
# These stacked bar charts nicely illustrate the different statistical profiles of the different performance 
# groups in the data

# Could look at this for strokes gained - rather than the conventional statistics 
SG_avg_groups = df %>% group_by(group) %>% summarise(SG_OTT=mean(SG_OTT), SG_APR=mean(SG_APR), SG_ARG=mean(SG_ARG), SG_putting=mean(SG_putting))
stacked_bar_SG <- plot_ly(SG_avg_groups, x=~group, y=~SG_OTT, type = 'bar', name="Strokes Gained of the Tee (OTT)")
stacked_bar_SG <- stacked_bar_SG %>% add_trace(y=~SG_APR, name="Stroked Gained Approach (APR)")
stacked_bar_SG <- stacked_bar_SG %>% add_trace(y=~SG_ARG, name="Stroked Gained Around the Green (ARG)")
stacked_bar_SG <- stacked_bar_SG %>% add_trace(y=~SG_putting, name="Stroked Gained Putting")
stacked_bar_SG <- stacked_bar_SG %>% layout(yaxis = list(title="Strokes Gained", tickvals=seq(-0.2, 0.4, 0.1), barmode="stack"),
                                            title = "Strokes Gained breakdown by performance group")
stacked_bar_SG

# Side Question #

# First we need to identify the exceptional seasons

# Here is a nice Vis showing the best seasons
# This nicely shows the seasons which we're going to evaluate
exceptional = df %>% filter(wins >= 1 & avg_SG_total>1.5)
ggplot(exceptional, aes(earnings, avg_SG_total, label=name))+
  geom_text(colour="blue", check_overlap = F, size=2)+
  geom_point(data=df, colour="Black")+
  geom_point(data=exceptional, colour="red")+
  labs(x="Earnings ($)", y="Average SG Total", title="Exceptional Seasons")+
  theme_classic()


# Radar chart of the profile of the elite seasons vs the average season
library(fmsb)
exceptional_stats = colMeans(exceptional[,c(-1,-19)])
average_stats = colMeans(df[,c(-1,-19)])
exceptional_vs_rest = rbind(exceptional_stats, average_stats)
exceptional_vs_rest_conventional = exceptional_vs_rest[,c(2,4,5,6,7)]
exceptional_vs_rest_SG = exceptional_vs_rest[,c(12,14,15,16)]
# Inverse the putting statistics again
exceptional_vs_rest_conventional[,4] = exceptional_vs_rest_conventional[,4]*-1

# Radar Chart axis - conventional data 
max_conventional = c(80, 315, 80, -27, 80)
min_conventional = c(35, 250, 35, -30.5, 35)
# Radar Chart axis - SG data 
max_SG = c(1,1,1,1)
min_SG = c(-.5,-.5,-.5,-.5)
exceptional_vs_rest_conventional = rbind(max_conventional, min_conventional, exceptional_vs_rest_conventional)
exceptional_vs_rest_conventional = data.frame(exceptional_vs_rest_conventional)
exceptional_vs_rest_SG = rbind(max_SG, min_SG, exceptional_vs_rest_SG)
exceptional_vs_rest_SG = data.frame(exceptional_vs_rest_SG)

# Radar Chart conventional Stats 
radar_colours = c(rgb(alpha=.5, red=.05, green=.65, blue=.65), rgb(alpha=.5, red=.3, green=.3, blue=.7))
radarchart(exceptional_vs_rest_conventional, seg=5, plty=1, cglty=6, cglcol="grey",
           pcol="darkgreen", pfcol=radar_colours, plwd=2, title="Exceptional vs Average Season (Conventional Stats)")
legend(x=1.5, y=1, legend=rownames(exceptional_vs_rest_conventional[-c(1,2),]), col = radar_colours, pch=15)
# Radar Chart SG Stats
radarchart(exceptional_vs_rest_SG, seg=5, plty=1, cglty=6, cglcol="grey",
           pcol="darkgreen", pfcol=radar_colours, plwd=2, title="Exceptional vs Average Season (SG Stats)")
legend(x=1.5, y=1, legend=rownames(exceptional_vs_rest_SG[-c(1,2),]), col = radar_colours, pch=15)

# Pie Chart - Exceptional SG
exceptional_SG = exceptional %>% summarise(OTT=mean(SG_OTT), APR=mean(SG_APR), ARG=mean(SG_ARG), Putting=mean(SG_putting))
exceptional_df = data.frame(statistic=c("SG OTT", "SG APR", "SG ARG", "SG Putting"), data=c(exceptional_SG[[1]], exceptional_SG[[2]], exceptional_SG[[3]], exceptional_SG[[4]]) )
colours <- c('rgb(128,0,100)', 'rgb(218,0,100)', 'rgb(22,10,120)', 'rgb(171,104,87)', 'rgb(114,147,203)')
exceptional_pie <- plot_ly(exceptional_df, labels = ~statistic, values = ~data, type = 'pie',
               textinfo = 'label+percent',
               #insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste(data, statistic),
               marker = list(colors = colours,
                             line = list(color = 'white', width = 4)),
               showlegend = F)
exceptional_pie <- exceptional_pie %>% layout(title = "Exceptional Seasons' Strokes Gained",
                      xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
                      yaxis = list(showgrid = F, zeroline = F, showticklabels = F))

exceptional_pie



# Line chart for showing SG evolution for each aspect for exceptional seasons
exceptional_SG = data.frame(exceptional %>% group_by(year) %>% summarise('SG OTT'=mean(SG_OTT), 'SG APR'=mean(SG_APR), 'SG ARG'=mean(SG_ARG), 'SG Putting'=mean(SG_putting)))
exceptional_lines <- plot_ly(exceptional_SG, x=~year, y=~SG.OTT, type = 'scatter', name="SG OTT", mode="lines+markers")
exceptional_lines <- exceptional_lines %>% add_trace(y=~SG.Putting, name="SG Putting")
exceptional_lines <- exceptional_lines %>% add_trace(y=~SG.APR, name="SG Approach")
exceptional_lines <- exceptional_lines %>% add_trace(y=~SG.ARG, name="SG Around the Green")
exceptional_lines <- exceptional_lines %>% layout(yaxis = list(title="Strokes Gained", barmode='stack'),
                                                  title="Exceptional Seasons Strokes Gained by Year")
exceptional_lines




# Side Q
# What are the relationships between the different areas of the game
# Positive relationship generally between OTT & Approach play
df %>% 
  ggplot(aes(SG_APR, SG_OTT))+
  geom_point(aes(col=group))+
  geom_smooth(col="yellow", size=2)+
  theme_minimal()

# Negative relationship between OTT & Putting
df %>% 
  ggplot(aes(SG_putting, SG_OTT))+
  geom_point(aes(col=group))+
  geom_smooth(col="yellow", size=2)+
  theme_minimal()

# Postive relationship between Putting and ARG
df %>% 
  ggplot(aes(SG_ARG, SG_putting))+
  geom_point(aes(col=group))+
  geom_smooth(col="yellow", size=2)+
  theme_minimal()

# Generally negative relationship between ARG and OTT
df %>% 
  ggplot(aes(SG_OTT, SG_ARG))+
  geom_point(aes(col=group))+
  geom_smooth(col="yellow", size=2)+
  theme_minimal()

# Generally positive relationship between ARG and APR 
df %>% 
  ggplot(aes(SG_APR, SG_ARG))+
  geom_point(aes(col=group))+
  geom_smooth(col="yellow", size=2)+
  theme_minimal()



## LAST Q2 Side question
#################################################################
# How does the profile of different players vary according to the
# country / geographical region which they are from
# Read in the data which contains nationalities
df1 = read.csv("pgafulldata.csv", header=T, sep=",") 

filt_df1 = df1[,c("PLAYER.NAME", "Driving.Distance", "Driving.Accuracy.Percentage", "GIR.Percentage.from.Fairway", "Scrambling", "SG..Total", "Country")]
names(filt_df1) = c("name", "driving_distance", "driving_accuracy", "GIR", "Scrambling", "avg_SG_total", "Country")
filt_df1 = filt_df1[filt_df1$Country!="",]
country_data = filt_df1 %>% group_by(Country) %>% summarise("Driving Distance"=mean(driving_distance, na.rm=T), "Driving Accuracy"=mean(driving_accuracy, na.rm=T), GIR=mean(GIR, na.rm=T), Scrambling=mean(Scrambling, na.rm=T), "SG TOTAL"=mean(avg_SG_total, na.rm=T))
country_data = drop_na(country_data)
country_data$Country
country_data[country_data$Country=="GER","Country"] = "DEU"
country_data[country_data$Country=="TPE","Country"] = "TWN"
country_data[country_data$Country=="RSA","Country"] = "ZAF"
country_data[country_data$Country=="ZIM","Country"] = "ZWE"

# Merging the Great Britian data
GBR = country_data[country_data$Country %in% c('ENG', 'NIR', 'SCO'),]
GBR 
GBR_data = GBR %>% summarise("Driving Distance"=mean(`Driving Distance`), "Driving Accuracy"=mean(`Driving Accuracy`), GIR=mean(GIR), Scrambling=mean(Scrambling), "SG TOTAL"=mean(`SG TOTAL`))
GB = rbind(GBR, cbind(data.frame(Country="GBR"), GBR_data))
country_data = rbind(country_data, GB[GB$Country=="GBR",])
# Produce the interactive choropleth 
choropleth = plot_ly(country_data, type='choropleth', locations=country_data$Country, z=country_data$`SG TOTAL`, text=country_data$Country, colorscale="PuRd")
choropleth




#####################################################################










##### Q3 ##################################################
# Rory McIlroy Radar chart 
## The first thing we want to understand is the profile of his game

# Collect Rory McIlroy's data from 2011, 2015 and 2019 and add it to the data frame
rory_2015 = data.frame(name="Rory McIlroy", rounds=34, fwy=67.69, year=2015, avg_distance=304, gir=71.03, avg_putts=NA, avg_scrambling=NA, avg_score=69.320, fedex_points=1567, wins=2, top10=7, SG_putting=-0.064, avg_SG_total=1.786, SG_OTT=1.097, SG_APR=0.525, SG_ARG=0.227, earnings=4863312, group="elite")
rory_2011 = data.frame(name="Rory McIlroy", rounds=18, fwy=60.29, year=2011, avg_distance=307.2, gir=68.3, avg_putts=NA, avg_scrambling=NA, avg_score=69.479, fedex_points=NA, wins=1, top10=4, SG_putting=-0.173, avg_SG_total=1.022, SG_OTT=1.194, SG_APR=0.409, SG_ARG=-.124, earnings=1905609, group="elite")
rory_2019 = data.frame(name="Rory McIlroy", rounds=57, fwy=61.82, year=2019, avg_distance=313.5, gir=68.55, avg_putts=28.38, avg_scrambling=63.38, avg_score=69.057, fedex_points=2315, wins=3, top10=14, SG_putting=0.425, avg_SG_total=2.551, SG_OTT=1.195, SG_APR=0.633, SG_ARG=.297, earnings=7785286, group="elite")
df = rbind(df, rory_2015, rory_2011, rory_2019)



rory_SG = df %>% filter(name=="Rory McIlroy") %>% summarise(Overall=mean(avg_SG_total),
                                                         Putting=mean(SG_putting), Driving=mean(SG_OTT), Approach=mean(SG_APR), Scrambling=mean(SG_ARG))

avg_sg = df %>% summarise(Overall=mean(avg_SG_total),
                          Putting=mean(SG_putting), Driving=mean(SG_OTT), Approach=mean(SG_APR), Scrambling=mean(SG_ARG))

rory_stats = data.frame(rory_SG)

# Radar Chart axis
max = c(2,2,2,2,2)
min = c(-2,-2,-2,-2,-2)
rory_radar = rbind(max,min,rory_stats,avg_sg)
rory_radar = cbind(rory_radar, row.names = c("max", "min", "Rory", "Tour Avg"))
radar_colours = c(rgb(alpha=.5, red=.05, green=.65, blue=.65), rgb(alpha=.5, red=.3, green=.3, blue=.7))
# Radar Chart of Rory McIlroy vs the average over the entire decade
radarchart(rory_radar, seg=5, plty=1, cglty=6, cglcol="grey",
           pcol="darkgreen", pfcol=radar_colours, plwd=2, title="Rory Mcilroy vs Average PGA Tour player (2010-19)")
legend(x=1.5, y=1, legend=rownames(rory_radar[-c(1,2),]), col = radar_colours, pch=15)


# Rory Radar chart with driving distance & accuracy split
rory1 = df %>% filter(name=="Rory McIlroy") %>% summarise("Driving Distance"=mean(avg_distance), "Driving Accuracy"=mean(fwy),
                                                          Putting=mean(SG_putting), Approach=mean(SG_APR), Scrambling=mean(SG_ARG))
avg_sg1 = df %>% summarise("Driving Distance"=mean(avg_distance), "Driving Accuracy"=mean(fwy),
                           Putting=mean(SG_putting), Approach=mean(SG_APR), Scrambling=mean(SG_ARG))

max1 = c(315, 85, 2, 2, 2)
min1 = c(240, 35, -2, -2, -2)

rory_radar1 = rbind(max1, min1, rory1,avg_sg1)
rory_radar1 = data.frame(rory_radar1)
rory_radar1 = cbind(rory_radar1, row.names = c("Max", "Min", "Rory", "Tour Avg"))
radarchart(rory_radar1, seg=5, plty=1, cglty=6, cglcol="grey",
           pcol="darkgreen", pfcol=radar_colours, plwd=2, title="Rory Mcilroy vs Average PGA Tour player (2010-19)")
legend(x=1.5, y=1, legend=rownames(rory_radar1[-c(1,2),]), col = radar_colours, pch=15)



# Rory Evolution over the last decade
# Collect Rory McIlroy's average Strokes Gained for Majors for each year 
rory_SG_majors = data.frame(year=c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019),
                            avg_SG_total=c(1.2, 2.315, 1.5325, 0.7875, 3.37, 3.09, 1.3125, 1.6675, 1.8425, 1.8175))
rory_SG_majors$name = "Rory McIlroy SG Majors"
#####

# group the dataframe by years 
avg_SG_year = data.frame(df %>% group_by(year) %>%
                           summarise(avg_SG_total=mean(avg_SG_total)))
avg_SG_year$name = "Average"
# Alter the 2019 Average SG per year - as Rory McIlroy's 2019 season is the only existing one.
# Let's keep this average as the same as 2018
avg_SG_year[10,"avg_SG_total"] = avg_SG_year[9,"avg_SG_total"]

rory_SG_year = data.frame(df %>% filter(name=="Rory McIlroy") %>% group_by(year) %>%
                            summarise(avg_SG_total=mean(avg_SG_total)))

rory_SG_year$name = "Rory McIlroy SG"

SG_total_year = rbind(avg_SG_year, rory_SG_year, rory_SG_majors)
SG_total_year

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


rory_performance <- SG_total_year %>% accumulate_by(~year)


rory_performance = rory_performance %>%
  plot_ly(
    x = ~year, 
    y = ~avg_SG_total,
    split = ~name,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines+markers', 
    line = list(simplyfy = F)
  )
rory_performance <- rory_performance %>% layout(
  xaxis = list(
    title = "Year",
    zeroline = F
  ),
  yaxis = list(
    title = "Total Strokes Gained",
    zeroline = F
  ),
  title="Rory McIlroy SG Performance 2010-19"
) 
rory_performance = rory_performance %>% animation_opts(
  frame = 1000, 
  transition = 20, 
  redraw = FALSE
)
rory_performance = rory_performance %>% animation_slider(
  hide = T
)
rory_performance = rory_performance %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom"
)

rory_performance


# Stacked bar charts - Rory McIlroy - Stokes Gained evolution 2010-19
rory_data = data.frame(df %>% filter(name=="Rory McIlroy") %>% select(year, SG_putting, SG_OTT, SG_APR, SG_ARG) %>% arrange(year))

rory_stacked_bar <- plot_ly(rory_data, x=~year, y=~SG_putting, type = 'bar', name="SG Putting")
rory_stacked_bar <- rory_stacked_bar %>% add_trace(y=~SG_OTT, name="SG OTT")
rory_stacked_bar <- rory_stacked_bar %>% add_trace(y=~SG_APR, name="SG APR")
rory_stacked_bar <- rory_stacked_bar %>% add_trace(y=~SG_ARG, name="SG ARG")
rory_stacked_bar <- rory_stacked_bar %>% layout(yaxis = list(title="SG Total", tickvals=seq(-0.5, 2.5, 0.25), barmode='stack'), 
                                                title = "Rory McIlroy SG 2010-19")

rory_stacked_bar


# what does Rory need to do to maximise his SG OTT
df%>% filter(name=="Rory McIlroy") %>% 
  ggplot(aes(fwy, avg_distance, label=SG_OTT, col=SG_OTT))+
  geom_text(size=6,check_overlap = F)+
  theme_minimal()+
  labs(col="SG OTT", y="Average Driving Distance (Yards)", x="Driving Accuracy (%)",
       title="Rory McIlroy Driving Distance vs Driving Accuracy (SG OTT)")

