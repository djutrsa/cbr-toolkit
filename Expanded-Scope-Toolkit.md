# CBR Expanded Scope Framework tool, built in R
#
# Author: 
# David Jutrsa
# 
# For any questions or comments, please email:  
# david.jutrsa@gmail.com
# 


# STARTING WITH A CLEAN SLATE
#
rm(list=ls())        # clears the Environment
cat("\014")          # clears the Console
options(scipen=999)  # Turns scientific notation off

# SET WORKING DIRECTORY AND LOAD THE REQUIRED PACKAGES:
#
setwd("U:/RProject") # sets the working directory for file loading and saving
library(igraph)      # loads the igraph package

# LOAD THE INDEX DATA FILE:
#
# ---------
# Note: It is crucial that the index data file is in the declared working directory
# (i.e. the directory set with the setwd() command)
# ---------
#

# To be most useful, an index file should be prepare in such a way as to have two types of data:
# - scores (for example, yearly Corruption Perception Index scores)
# - changes (for example, year-on-year changes in the scores)
# 
# Additionally, the data should be in the proper format
# (i.e. columns with scores and changes should be numbers)
#
# See example below for illustrative purposes

transparency <- as.data.frame(read.csv(file = "transparency.csv", fill = TRUE, header = TRUE, colClasses = NA))
transparency[is.na(transparency)] <- ""
transparency <- as.data.frame(transparency)

transparency$score2015 <- as.numeric(transparency$score2015)
transparency$score2016 <- as.numeric(transparency$score2016)
transparency$change2015 <- as.numeric(transparency$change2015)
transparency$change2016 <- as.numeric(transparency$change2016)

# LOAD THE SWIFT/EXPANDED SCOPE DATA FILE:
#
# ---------
# Note: It is crucial that the data file is in the declared working directory
# (i.e. the directory set with the setwd() command)
# ---------
payments <- as.data.frame(read.csv(file = "7banks.csv", header=TRUE, colClasses = NA))
payments <- payments[complete.cases(payments),]
payments1 <- payments

# Making sure all of the columns are
# in the right format (strings or numbers)
#
payments1$Initial.Ordering.BIC8 <- as.character(payments1$Initial.Ordering.BIC8)
payments1$BIC8 <- as.character(payments1$BIC8)
payments1$Initial.Ordering.Country <- as.character(payments1$Initial.Ordering.BIC8)
payments1$Net.USD.Amount <- as.numeric(payments1$Net.USD.Amount)

# Creates a variable "flow", which will be used to determine 
# whether a transaction was an inflow or outflow
#
payments1 <- within(payments1, flow <- 0)
payments1[(payments1$Initial.Ordering.BIC8 == payments1$BIC8),"flow"] <- 1
payments1[(payments1$End.Beneficiary.BIC8 == payments1$BIC8),"flow"] <- -1

df <- as.data.frame(payments1)

# -------------------------------------- #
#           SETTINGS TOGGLE MENU         #
# -------------------------------------- #

date <- Sys.Date()                       # Today's date
pathway_length_toggle <- "f"             # r for "correspondent relationships", e for "endpoint" (ordering & beneficiary) or f for "full"
system_or_individual_view <- "s"         # s for "system" or b for "bank"
country_or_institution_view <- "c"       # c for "country" or i for "institution"
color_using_index <- "yes"               # yes or no. Requires an additional file if "yes"
selected_bank <- "BRRLGTGC"              # Needs to be a BIC 8-letter code
selected_country <- "GT"                 # Needs to be an ISO 2-letter code
selected_period <- 6                     # Selects the period (1-6) for which the map will be generated
currency <- "USD"                        # Needs to be a 3-letter currency ticker. Use "All" if not specifying a single currency

# -------------------------------------- #
#
#

# FILTER CURRENCY:
#
if (currency != "All") {
  dataframe_currency <- df[which(df$Currency==currency),]
} else { 
  dataframe_currency <- df
}

# FILTER INSTITUTION
#

if (system_or_individual_view == "b") {
  dataframe_currency <- dataframe_currency[which(dataframe_currency$BIC8 == selected_bank),]
} else dataframe_currency <- dataframe_currency

# FILTER PERIOD:
#
# The data is usually in the Month-YY format
# (e.g. "Jan-12")
#
# If the data is in a different format, the below code
# should be amended accordingly 
#
year1 = "12"
year2 = "13"
year3 = "14"
year4 = "15"
year5 = "16"
year6 = "17"

# For dataframes with dates in format "Month-YY"
#
aggregate_by_year1 <- function(df, year) {
df <- df[which(df$Period == paste("Jan", "-", year, sep = "") 
             | df$Period == paste("Feb", "-", year, sep = "")               
             | df$Period == paste("Mar", "-", year, sep = "") 
             | df$Period == paste("Apr", "-", year, sep = "") 
             | df$Period == paste("May", "-", year, sep = "") 
             | df$Period == paste("Jun", "-", year, sep = "") 
             | df$Period == paste("Jul", "-", year, sep = "") 
             | df$Period == paste("Aug", "-", year, sep = "") 
             | df$Period == paste("Sep", "-", year, sep = "")
             | df$Period == paste("Oct", "-", year, sep = "") 
             | df$Period == paste("Nov", "-", year, sep = "")
             | df$Period == paste("Dec", "-", year, sep = "")),]
return(df)
}

# For dataframes with dates in format "YY-Month"
#
aggregate_by_year2 <- function(df, year) {
  df <- df[which(df$Period == paste(year, "-", "Jan", sep = "") 
               | df$Period == paste(year, "-", "Feb", sep = "")               
               | df$Period == paste(year, "-", "Mar", sep = "") 
               | df$Period == paste(year, "-", "Apr", sep = "") 
               | df$Period == paste(year, "-", "May", sep = "") 
               | df$Period == paste(year, "-", "Jun", sep = "") 
               | df$Period == paste(year, "-", "Jul", sep = "") 
               | df$Period == paste(year, "-", "Aug", sep = "") 
               | df$Period == paste(year, "-", "Sep", sep = "")
               | df$Period == paste(year, "-", "Oct", sep = "") 
               | df$Period == paste(year, "-", "Nov", sep = "")
               | df$Period == paste(year, "-", "Dec", sep = "")),]
  return(df)
}

df1 <- aggregate_by_year2(dataframe_currency,year1)
df2 <- aggregate_by_year2(dataframe_currency,year2)
df3 <- aggregate_by_year2(dataframe_currency,year3)
df4 <- aggregate_by_year2(dataframe_currency,year4)
df5 <- aggregate_by_year2(dataframe_currency,year5)
df6 <- aggregate_by_year2(dataframe_currency,year6)

# BIND GRAPHS:
# This function breaks the transaction with multiple parties into bilateral transfers
# depending on whether the transaction was an inflow or an outflow
#
# For example, a payment from Bank A going through Bank B to Bank C
# (that is A->B->C) is broken up into two flows: A->B and B->C
#
# Additionally, the way the transaction is broken up depends on the
# toggle menu "pathway_length_toggle" variable" (see above for more detail)
# 

if (pathway_length_toggle == "r") {
# CORRESPONDENT BANKS ONLY
  bind_graphs <- function(df) {
    graph1 <- df[which(df$flow == 1),]
    graph1 <- graph1[c("BIC8","BIC8.Country","Counterparty.BIC8", "Counterparty.BIC8.Country","Currency","Net.USD.Amount", "Transactions")]
    colnames(graph1) <- c("from","from.country", "to", "to.country", "currency","weight", "volume")
    
    graph2 <- df[which(df$flow == -1),]
    graph2 <- graph2[,c("Counterparty.BIC8", "Counterparty.BIC8.Country", "BIC8", "BIC8.Country","Currency","Net.USD.Amount", "Transactions")]
    colnames(graph2) <- c("from","from.country", "to", "to.country", "currency","weight", "volume")
    graph <- rbind(graph1,graph2)
    rm(graph1,graph2)
    graph <- graph[,c(1,3,6,7,5,2,4)]
    return(graph)
  }
  graphm1 <- bind_graphs(df1)
  graphm2 <- bind_graphs(df2)
  graphm3 <- bind_graphs(df3)
  graphm4 <- bind_graphs(df4)
  graphm5 <- bind_graphs(df5)
  graphm6 <- bind_graphs(df6)
}

if (pathway_length_toggle == "e") {
# ENDPOINTS (Ordering and Beneficiary) ONLY
  bind_graphs2 <- function(df) {
    graph <- df[,c("Initial.Ordering.BIC8","Initial.Ordering.Country","End.Beneficiary.BIC8", "End.Beneficiary.Country","Currency","Net.USD.Amount", "Transactions")]
    colnames(graph) <- c("from","from.country", "to", "to.country", "currency","weight", "volume")
 #   graph <- graph[,c(1,3,6,7,5,2,4)]
    return(graph)
  }
  graphm1 <- bind_graphs2(df1)
  graphm2 <- bind_graphs2(df2)
  graphm3 <- bind_graphs2(df3)
  graphm4 <- bind_graphs2(df4)
  graphm5 <- bind_graphs2(df5)
  graphm6 <- bind_graphs2(df6)
}
  
if (pathway_length_toggle == "f") {
# FULL PAYMENT CHAIN
  bind_graphs2 <- function(df) {
    graph1 <- df[,c("Initial.Ordering.BIC8","Initial.Ordering.Country","Counterparty.BIC8", "Counterparty.BIC8.Country","Currency","Net.USD.Amount", "Transactions")]
    colnames(graph1) <- c("from","from.country", "to", "to.country", "currency","weight", "volume")
    graph2 <- df[,c("Counterparty.BIC8", "Counterparty.BIC8.Country", "End.Beneficiary.BIC8", "End.Beneficiary.Country","Currency","Net.USD.Amount", "Transactions")]
    colnames(graph2) <- c("from","from.country", "to", "to.country", "currency","weight", "volume")
    graph <- rbind(graph1,graph2)
    rm(graph1,graph2)
    graph <- graph[,c(1,3,6,7,5,2,4)]
    return(graph)
  }
  graphm1 <- bind_graphs2(df1)
  graphm2 <- bind_graphs2(df2)
  graphm3 <- bind_graphs2(df3)
  graphm4 <- bind_graphs2(df4)
  graphm5 <- bind_graphs2(df5)
  graphm6 <- bind_graphs2(df6)
}

# FINALIZE GRAPH
#
finalize_graph <- function(graph) {

  graph <- graph[!is.na(graph$from),]
  graph <- graph[!is.na(graph$to),]
  graph$from <- as.character(graph$from)
  graph$to <- as.character(graph$to)
  graph$weight <- as.numeric(graph$weight)
  graph$volume <- as.numeric(graph$volume)
  # Remove circular flows (Bank X --> Bank X)
  graph <- graph[which(graph$from != graph$to),]
  return(graph)
}

graphm1 <- finalize_graph(graphm1)
graphm2 <- finalize_graph(graphm2)
graphm3 <- finalize_graph(graphm3)
graphm4 <- finalize_graph(graphm4)
graphm5 <- finalize_graph(graphm5)
graphm6 <- finalize_graph(graphm6)

# Aggregate graph to make the plotting faster:
#
graggregate <- function(graph) {
  graph <- aggregate(graph$weight~graph$from+graph$to,FUN = sum)
  colnames(graph) <- c("from","to","weight")
  graph$from <- as.character(graph$from)
  graph$to <- as.character(graph$to)
  graph$weight <- as.numeric(graph$weight)
  return(graph)
}

# Country-based aggregation of flows
#
graggregate_country <- function(graph) {
  graph <- aggregate(graph$weight~graph$from.country+graph$to.country,FUN = sum)
  colnames(graph) <- c("from","to","weight")
  graph$from <- as.character(graph$from)
  graph$to <- as.character(graph$to)
  graph$weight <- as.numeric(graph$weight)
  graph <- graph[which(graph$from != graph$to),]
  return(graph)
}

if (country_or_institution_view == "i") {
  grapha1 <- graggregate(graphm1)
  grapha2 <- graggregate(graphm2)
  grapha3 <- graggregate(graphm3)  
  grapha4 <- graggregate(graphm4)
  grapha5 <- graggregate(graphm5)
  grapha6 <- graggregate(graphm6)
} else if (country_or_institution_view == "c") {
  grapha1 <- graggregate_country(graphm1)
  grapha2 <- graggregate_country(graphm2)
  grapha3 <- graggregate_country(graphm3)  
  grapha4 <- graggregate_country(graphm4)
  grapha5 <- graggregate_country(graphm5)
  grapha6 <- graggregate_country(graphm6)
}

# Adjust edge thickness based on period-on-period change in weight (i.e. transaction amount)
#
adjust_edges <- function(old_graph, new_graph) {
  new_graph <- within(new_graph, edge_multiplier <- 1)
  for (i in 1:nrow(new_graph)) {
    from <- new_graph[i,1]
    to <- new_graph[i,2]
    old_sum <- sum(old_graph$weight[which(old_graph$from == from & old_graph$to == to)])
    new_sum <- sum(new_graph$weight[which(new_graph$from == from & new_graph$to == to)])
    if(old_sum != 0) {
      if (new_sum - old_sum > 0) {
        new_graph$edge_multiplier[i] <- 2
      } else if (new_sum - old_sum <0) {
        new_graph$edge_multiplier[i] <- 0.5
      }
    }
  }
  return(new_graph)
}

grapha1 <- adjust_edges(grapha1, grapha1)
grapha2 <- adjust_edges(grapha1, grapha2)
grapha3 <- adjust_edges(grapha2, grapha3)
grapha4 <- adjust_edges(grapha3, grapha4)
grapha5 <- adjust_edges(grapha4, grapha5)
grapha6 <- adjust_edges(grapha5, grapha6)

# CREATE NODE LIST:
#

# Institution-level
create_temp1 <- function(graph) {
  temp1 <- unique(graph$from)
  temp1 <- as.character(temp1)
  temp1 <- as.data.frame(temp1)
  colnames(temp1) <- c("temp1")
  
  temp2 <- unique(graph$to)
  temp2 <- as.character(temp2)
  temp2 <- as.data.frame(temp2)
  colnames(temp2) <-  c("temp1")
  
  temp1 <- rbind(temp1,temp2)
  temp1 <- unique(temp1)
  temp1$temp1 <- as.character(temp1$temp1)
  temp1 <- as.data.frame(temp1)
  temp1 <- temp1[!is.na(temp1$temp1),]
  temp1 <- as.data.frame(temp1)
  return(temp1)
}  

# Country-level
create_temp2 <- function(graph) {
  temp1 <- unique(graph$from)
  temp1 <- as.character(temp1)
  temp1 <- as.data.frame(temp1)

  temp2 <- unique(graph$to)
  temp2 <- as.character(temp2)
  temp2 <- as.data.frame(temp2)
  colnames(temp2) <-  colnames(temp1)
  
  temp1 <- rbind(temp1,temp2)
  temp1 <- unique(temp1)
  temp1$temp1 <- as.character(temp1$temp1)
  temp1 <- as.data.frame(temp1)
  temp1 <- temp1[!is.na(temp1$temp1),]
  temp1 <- as.data.frame(temp1)
  temp1$temp1 <- as.character(temp1$temp1)
  return(temp1)
}  

if (country_or_institution_view == "i") {
  tempm1 <- create_temp1(grapha1)
  tempm2 <- create_temp1(grapha2)
  tempm3 <- create_temp1(grapha3)
  tempm4 <- create_temp1(grapha4)
  tempm5 <- create_temp1(grapha5)
  tempm6 <- create_temp1(grapha6)

  } else if (country_or_institution_view == "c") {
  tempm1 <- create_temp2(grapha1)
  tempm2 <- create_temp2(grapha2)
  tempm3 <- create_temp2(grapha3)
  tempm4 <- create_temp2(grapha4)
  tempm5 <- create_temp2(grapha5)
  tempm6 <- create_temp2(grapha6)
}

# Sort node list in alphabetical order
#
tempm1$temp1 <- sort(tempm1$temp1, decreasing = FALSE)
tempm2$temp1 <- sort(tempm2$temp1, decreasing = FALSE)
tempm3$temp1 <- sort(tempm3$temp1, decreasing = FALSE)
tempm4$temp1 <- sort(tempm4$temp1, decreasing = FALSE)
tempm5$temp1 <- sort(tempm5$temp1, decreasing = FALSE)
tempm6$temp1 <- sort(tempm6$temp1, decreasing = FALSE)

# Merge node list with transparency index
#
merge_with_transp <- function(transp, node_list) {
  temp <- transp
  temp <- temp[temp$Code %in% node_list$temp1,]
  
  temp2 <- merge(node_list, temp, by.x = "temp1", by.y = "Code", all = TRUE)
  temp2[is.na(temp2)] <- 0
  rm(temp)
  return(temp2)
}

# Generate merged node lists and their networks:
#
if (country_or_institution_view == "c") {
  if (color_using_index == "yes") {
  tempm1 <- merge_with_transp(transparency, tempm1)
  tempm2 <- merge_with_transp(transparency, tempm2)
  tempm3 <- merge_with_transp(transparency, tempm3)
  tempm4 <- merge_with_transp(transparency, tempm4)
  tempm5 <- merge_with_transp(transparency, tempm5)
  tempm6 <- merge_with_transp(transparency, tempm6)
  }
  
  networkm1 <- graph.data.frame(grapha1, vertices = tempm1, directed = TRUE)
  networkm2 <- graph.data.frame(grapha2, vertices = tempm2, directed = TRUE)
  networkm3 <- graph.data.frame(grapha3, vertices = tempm3, directed = TRUE)
  networkm4 <- graph.data.frame(grapha4, vertices = tempm4, directed = TRUE)
  networkm5 <- graph.data.frame(grapha5, vertices = tempm5, directed = TRUE)
  networkm6 <- graph.data.frame(grapha6, vertices = tempm6, directed = TRUE)
} else 
  if (country_or_institution_view == "i") {
  networkm1 <- graph.data.frame(grapha1, vertices = tempm1, directed = TRUE)
  networkm2 <- graph.data.frame(grapha2, vertices = tempm2, directed = TRUE)
  networkm3 <- graph.data.frame(grapha3, vertices = tempm3, directed = TRUE)
  networkm4 <- graph.data.frame(grapha4, vertices = tempm4, directed = TRUE)
  networkm5 <- graph.data.frame(grapha5, vertices = tempm5, directed = TRUE)
  networkm6 <- graph.data.frame(grapha6, vertices = tempm6, directed = TRUE)
}

# Generate PageRank scores for existing networks
#
prm1 <- page.rank(networkm1)$vector
prm2 <- page.rank(networkm2)$vector
prm3 <- page.rank(networkm3)$vector
prm4 <- page.rank(networkm4)$vector
prm5 <- page.rank(networkm5)$vector
prm6 <- page.rank(networkm6)$vector

# SELECT NETWORK TO PLOT
#
if(selected_period == 1) {
  selected_network <- networkm1
} else if (selected_period == 2)
  {
    selected_network <- networkm2
  } else if (selected_period == 3)
    {
      selected_network <- networkm3
    } else if (selected_period == 4)
      {
        selected_network <- networkm4
      } else if (selected_period == 5)
        {
          selected_network <- networkm5
        } else if (selected_period == 6)
          {
            selected_network <- networkm6
          }

# Country vertex coloring
#
# If the index data file is loaded and if
# index-based coloring is turned on (see above)
# the code below will assign colors to nodes
# both outside (frame) and inside (fill)
#
bottom_quintile <- 15
second_lowest_quintile <- 30.25

if(country_or_institution_view == "c") {
  if (color_using_index == "yes") {
  V(selected_network)$color <- ifelse(V(selected_network)$name == selected_country, "white", 
                       ifelse(V(selected_network)[,selected_period] < bottom_quintile & V(selected_network)[,selected_period] > 1, "red",
                       ifelse(V(selected_network)[,selected_period] < second_lowest_quintile & V(selected_network)[,selected_period] > 1,"orange", "gray")))
  V(selected_network)$frame.color <- ifelse(V(selected_network)[,2*selected_period-1] == "-1","red", 
                                     ifelse(V(selected_network)$ch2016 == "1","green",
                                            "black"))
    } else if (color_using_index == "no") {
      V(selected_network)$color <- ifelse(V(selected_network)$name == selected_country, "white","gray")
    }
}

# Draw Network Map
#
if(country_or_institution_view == "c") {
  tm1 <- tkplot(selected_network,
                canvas.width = 1000,
                canvas.height = 1000,
                vertex.size = ifelse(V(selected_network)$name == selected_country, 12, sqrt(page_rank(selected_network)$vector)*110),
                vertex.label.dist = 1,
                edge.curved = T,
                edge.color = ifelse(E(selected_network)$edge_multiplier == 1, "green",
                             ifelse(E(selected_network)$edge_multiplier == 0.5,"gray", "gray")),
                # This scales arrow thickness proportionately to the flow's position in the dataset
                edge.width = sqrt(E(selected_network)$weight/median(E(selected_network)$weight))/max(sqrt(E(selected_network)$weight/median(E(selected_network)$weight)))*20
)
tkconfigure(igraph:::.tkplot.get(tm1)$canvas, "bg"="white")
}

if (country_or_institution_view == "i") {
  tm1 <- tkplot(selected_network,
                canvas.width = 1000,
                canvas.height = 1000,
                vertex.label.cex = 0.5,
                vertex.size = sqrt(page_rank(selected_network)$vector)*110,
                vertex.color = ifelse(substr(V(selected_network)$name, nchar(V(selected_network)$name)-3,nchar(V(selected_network)$name)-2) == "GT","orange", "white"),
                edge.curved = T,
                edge.color = ifelse(E(selected_network)$edge_multiplier == 1, "green",
                             ifelse(E(selected_network)$edge_multiplier == 0.5,"gray", "gray")),
                # This scales arrow thickness proportionately to the flow's position in the dataset
                edge.width = sqrt(E(selected_network)$weight/median(E(selected_network)$weight))/max(sqrt(E(selected_network)$weight/median(E(selected_network)$weight)))*20
  )
  
  tkconfigure(igraph:::.tkplot.get(tm1)$canvas, "bg"="white")
}

