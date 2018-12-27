# MarketingAttribution
Markov model for marketing multi-channel attribution


Multi Channel Attribution Model in R

channel <- read.csv(file = "C:/Users/yfei/Documents/MultichannelR.csv", header = T, sep = ",")
head(channel)


#add conversion tag#
   for(row in 1:nrow(channel))
{if(90 %in% channel[row,]){channel$convert[row] = 1}}
column = colnames(channel)
channel$path = do.call(paste, c(channel[column], sep = " > "))
head(channel$path)

#split conversion tag with channel numbers#

for(row in 1:nrow(channel))
(channel$path[row] = strsplit(channel$path[row], " > 90")[[1]][1]}
 head(channel$path)

 channel_fin = channel[,c(34,33)]
 head(channel_fin)
 channel_fin1 = ddply(channel_fin, ~path, summarise, conversion = sum(convert))
 Data=channel_fin1

#Build up heuristic model#

 H<- heuristic_models(Data,'path','conversion', var_value = 'conversion')

 #Build up Markov Model#
 
 M<- markov_model(Data, 'path', 'conversion',var_value = 'conversion', order = 1)

 #Combine models together#
 
 R<- merge(H, M, by = 'channel_name')
 R1 <- R[, (colnames(R) %in% c('channel_name', 'first_touch_conversions', 'last_touch_conversions', 'linear_touch_conversions', 'total_conversion'))]
 
 R1 <- melt(R1, id='channel_name')

 #Plot to compaire four different models#
 
 ggplot(R1, aes(channel_name, value, fill = variable)) +
   geom_bar(stat='identity', position='dodge') +
   ggtitle('TOTAL CONVERSIONS') +
   theme(axis.title.x = element_text(vjust = -2)) +
   theme(axis.title.y = element_text(vjust = +2)) +
   theme(title = element_text(size = 16)) +
   theme(plot.title=element_text(size = 20)) +
   ylab("")
