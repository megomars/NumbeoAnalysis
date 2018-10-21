#setwd("~/Google Drive/2015Dataviz/INTERACTIVE2015/INTERACTIVES/INTERACTIVE - CostOfLiving/cost_of_living_final/data/data-analysis")

#load libraries
library(jsonlite,dplyr,ggplot2,scales,showtext)

#Custom fonts (removed from github repo)
font_add("BergenSans-Regular", "BergenSans-Regular.otf")
showtext_auto()

#Load and inspect data (removed from github repo)
df<-fromJSON("datavalid.json")
glimpse(df)

ids <- df$id
ids
names <- df$name
names
countries <- df$countries

#Capitalize first letter of categories
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
categories <- unlist(lapply(names,firstup))
categories

x <-1
y <-1

#############################################

#Loop through chart creation
for (x in 1:length(countries)){
  query<-countries[x][[1]][1:2]
  yquery <- nrow(countries[x][[1]])
  #Comment line below for testing only one country
  #for (y in 1:1){
  for (y in 1:yquery){
    cname <- countries[1][[1]][y,1]
    cond <- query %>% filter(country==cname)
    if (nrow(cond) == 0){
      next()
    }
    xval <- cond$average_price_usd
    #Countries that are prefixed with "The"
    if(cname %in% c("United Arab Emirates", "Philippines", "Palestinian Territories", "Netherlands", "United Kingdom", "United States")){
      mname <- paste("the",cname)
    }
    else{
      mname <- cname
    }
    glmedian <- median(query$average_price_usd)
    tree <- query %>% arrange(average_price_usd)
    cheapest <- tree[1,]
    expensive <-tree[nrow(query),]
    exp <- paste0(expensive$country,"\n $",format(expensive$average_price_usd,big.mark = ","))
    che <- paste0(cheapest$country,"\n $",format(cheapest$average_price_usd,big.mark = ","))
    
    ggplot(query, aes(x=average_price_usd))+
      geom_histogram(fill=rgb(0.06,0.6,0.93,0.12), col=rgb(0.7,0.74,0.77,0.12), size=.15, bins = 110)+
      geom_vline(xintercept = xval, color = "#2e9de5", linetype = "solid", size=2)+
      geom_vline(xintercept = glmedian, color = "#eeeeee", linetype = "solid", size=0.5)+
      annotate("text", label = "  Global  median", x = glmedian, y=-0.8, color = "#ffffff", size = 2, lineheight=3)+ #global average
      annotate("text", label = exp, x = expensive$average_price_usd, y=-0.8, color = "#ffffff", size = 2, lineheight=0.9)+ #max
      annotate("text", label = che, x = cheapest$average_price_usd, y=-0.8, color = "#ffffff", size = 2, lineheight=0.9)+ #min
      labs(title=categories[x], subtitle=cname,y = "Number of countries", x = "", caption="Source: Numbeo")+
      scale_x_continuous(labels = dollar)+
      scale_y_continuous(breaks= c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25))+
      theme(plot.title=element_text(size=12, 
                                         face="bold", 
                                         family="BergenSans-Regular",
                                         color="#cccccc",
                                         lineheight=1),  # title
                 plot.subtitle=element_text(size=12, 
                                            family="BergenSans-Regular",
                                            face="bold",
                                            color="#2e9de5"),  # subtitle
                 plot.caption=element_text(size=5, colour = "#757575", family="BergenSans-Regular"),  # caption
                 axis.title.x=element_text(size=6, colour = "#757575", family="BergenSans-Regular"),  # X axis title
                 axis.title.y=element_text(size=6, colour = "#757575", family="BergenSans-Regular"),  # Y axis title
                 axis.text.x=element_text(size=8, colour = "#757575", family="BergenSans-Regular"),  # X axis text
                 axis.text.y=element_text(size=8, colour = "#757575", family="BergenSans-Regular"), # Y axis text
                 plot.background = element_rect(fill = "#1c1c1c", colour = "#1c1c1c"),
                 panel.background = element_rect(fill = "#1c1c1c"),
                 panel.grid.major = element_line(colour = "#1f1f1f"),
                 panel.grid.minor = element_line(colour = "#1f1f1f"))
    
    #https://ggplot2.tidyverse.org/reference/theme.html
    #export the images
    filename<-paste0("images/",cname,"/",cname,"_q",x,".png")
    dir.create(dirname(filename), showWarnings = FALSE)
    ggsave(filename, width = 5, height = 2.8)
  }
}


#############################################
# Finding interesting facts:

# x has one of the highest prices for ----- worldwide
# x has one of the cheapest prieces for ----- worldwide

highest_statements <- c()
lowest_statements <- c()
mean_statements <- c()
median_statements <- c()
words<- c("an inexpensive restaurant meal","a mcDonalds meal", "a can of Coke","a liter of milk", "a loaf of bread", "a dozen eggs", "a kilogram of cheese", "a pack of cigarettes", "a monthly transport ticket", "a liter of petrol", "a Volkswagen Golf", "rent in the city center", "high speed internet", "a gym membership", "a movie ticket", "a pair of designer jeans", "private preschool fees", "international primary school fees", "a taxi fare", "apples", "potatoes","a cup of cappuccino", "rice","bananas","beef")
rounded <- c(2,2,2,2,2,2,2,2,2,2,0,0,0,0,2,0,0,0,2,2,2,2,2,2,2)

calcme <-function(x){
  ca <- countries[x][[1]][1:2]
  hi_st <- head(ca %>% arrange(desc(average_price_usd)), n=1)
  highest_statements <<- append(highest_statements,paste(hi_st$country,"has one of the highest prices for",words[x],"worldwide."))
  
  lo_st <- tail(ca %>% arrange(desc(average_price_usd)), n=1)
  lowest_statements <<- append(lowest_statements,paste(lo_st$country,"has one of the lowest prices for",words[x],"worldwide."))
  
  mean_st <- mean(ca$average_price_usd)
  mean_statements <<- append(mean_statements, paste0("The global average for ",words[x]," is $",format(round(mean_st,digits=rounded[x]),big.mark = ",")))
  
  median_st <- median(ca$average_price_usd)
  median_statements <<- append(median_statements, paste0("The global median for ",words[x]," is $",format(round(mean_st,digits=rounded[x]),big.mark = ",")))
}

l <-0
for (l in 1:25){
  calcme(l)
}

statement_df <- c(highest_statements,lowest_statements,mean_statements)
statement_df
statement_df %>% toJSON() %>% write_json("dyk.json")

justtest <- countries[2][[1]][1:2]
head(justtest %>% arrange(desc(average_price_usd)), n=10)
tail(justtest %>% arrange(desc(average_price_usd)), n=10)

#This is the end of the analysis
# @Haddadme
