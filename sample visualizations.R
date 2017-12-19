library(shiny)
library(shinydashboard)
library(RGoogleAnalytics)
library(ggplot2)
library(dplyr)
library(scales)


#####################################################
# 
# rPivotTable
#
#####################################################
# pivottable-------------------------------------------------------------------------------------------------
rpivotTable(data = total_df , rows = c("year","month"),cols="DOMAIN_",
            vals = "sessions", aggregatorName = "Sum", rendererName = "Table",
            width="200px", height="300px")


#####################################################
# 
# ggplot
#
#####################################################

# ggplot theme-------------------------------------------------------------------------------------------------
# define ggplot2 theme
theme_tm <- function(){
  theme(
    text                = element_text(family = "myfont", size = 16),
    panel.grid.major.y  = element_line(color = '#f2f2f2', size = .1),
    panel.grid.minor.y  = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    legend.title        = element_blank(),
    axis.line.x =       element_line(colour = "#cdd0d6",size=.05),
    panel.background=element_blank(),
    plot.margin=unit(c(0,0,0.5,0.7),"cm"),
    axis.text.y = element_text(size=16),
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16)
  )
}

#ggplot line
total_df%>%
  filter(year!=2014)%>%
  group_by(year,month) %>%
  summarise(pageviews = sum(pageviews),
            sessions=sum(sessions)) %>%
  ggplot(aes_string(x="month", y=input$metric,colour="year",group="year")) +
  geom_line(size=0.8)+
  theme_tm()+
  scale_color_manual(values=c("#CCCCCC", "#99CCFF", "#003366"))+
  scale_x_discrete(limits=seq(1, 12,1))+
  scale_y_continuous(labels=comma)

# bar chart
ggplot(data_question1,aes( x = salary,y=count,fill=type)) +
  geom_bar(stat="identity",position = position_dodge())+
  facet_wrap(~type)+
  theme_tm()+
  theme(axis.text.x = element_text(size=13))+
  geom_text(aes(label = count), position = position_dodge(0.9),vjust=0)



#####################################################
# 
# highcharts
#
#####################################################

# funnel-------------------------------------------------------------------------------------------------


highchart() %>%
  hc_add_series_labels_values(funnel_df$key,funnel_df$value, source_ = rownames(funnel_df),
                              type = "funnel",
                              #name = "NDGIT",
                              #size = 400,
                              dataLabels = list(enabled = TRUE),
                              showInLegend = TRUE,
                              animation= list(duration=15000))%>%
  hc_legend(layout="horizontal",
            verticalAlign = "top")%>%
  hc_exporting(enabled = TRUE)



#timeseries-------------------------------------------------------------------------------------------------
highchart() %>% 
  hc_title(text = "title") %>%
  hc_subtitle(text = "subtitle") %>%
  hc_add_series_xts(uPVs.ts, id = "gateway",name="uPVs LP",animation= list(duration=2000),color="#7cb5ec") %>% 
  hc_add_series_xts(total_leads.ts, id = "total_leads",name="Total Leads",animation= list(duration=3000),color = "#434348") %>% 
  hc_add_series_xts(red_leads.ts, id = "red_leads",name="Red Leads",animation= list(duration=4000),color = "#f15c80") %>% 
  hc_add_series_xts(green_leads.ts, id = "green_leads",name="Green Leads",animation= list(duration=5000),color="#90ed7d") %>% 
  hc_add_series_xts(orders.ts, id = "orders",name="Orders",animation= list(duration=6000),color="#8085e9") %>% 
  hc_rangeSelector(inputEnabled = TRUE) %>% 
  hc_scrollbar(enabled = FALSE) %>%
  hc_legend(enabled = TRUE)%>%
  hc_tooltip(table = TRUE)%>%
  #hc_yAxis(labels = list(format = "{value}%"))%>%
  hc_yAxis(labels=list(align="left"))%>%   
  hc_exporting(enabled = TRUE)%>%
  hc_yAxis(gridLineColor="#f2f4f7")

# normal line chart-------------------------------------------------------------------------------------------------
highchart() %>%
 
  hc_xAxis(categories = b[,1]) %>% 
  hc_add_series(data = b$total_vs_green, type = "line", name="Total Leads -> Green Leads",color="#7cb5ec")%>%
  hc_add_series(data = b$uPVs_vs_total, type = "line", name="uPVs LP -> Total Leads",color="#434348")%>%
  hc_add_series(data = b$green_vs_order, type = "line", name="Green Leads -> Orders",color="#f15c80")%>%
  hc_legend(enabled = TRUE) %>%
  #hc_yAxis(tickInterval = 1) %>%
  #hc_xAxis(tickInterval = 5) %>%
  hc_tooltip(table = TRUE)%>%
  hc_yAxis(labels = list(format = "{value}%")) %>%
  hc_xAxis(tickInterval = if(input$Range_Selector2 == "created_week2") {5} else {1}) %>%
  hc_rangeSelector(inputEnabled = TRUE)%>%
  hc_chart(zoomType = "xy") %>%
  hc_exporting(enabled = TRUE)%>%
  hc_plotOptions(series = list(marker = list(enabled = FALSE)))%>%
  hc_navigator(enabled = FALSE)












