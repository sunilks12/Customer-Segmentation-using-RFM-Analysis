library(sqldf)
library(ggplot2)
library(dplyr)
library(caret)
library(PRROC)
library(ggthemes)
library(RColorBrewer)
# First, we import data

data = read.csv("data/Online_Retail.csv")



data$CustomerID = as.factor(data$CustomerID)



# Add headers and interpret the last column as a date, extract year of purchase

# First format into correct POSIXct 
Invoice_Date_formatted <- strptime(data$InvoiceDate,format="%m/%d/%Y")



data$InvoiceDate = as.Date(Invoice_Date_formatted, "%Y-%m-%d")
data$year_of_purchase = as.numeric(format(data$InvoiceDate, "%Y"))
data$days_since       = as.numeric(difftime(time1 = "2011-12-10",
                                            time2 = data$InvoiceDate,
                                            units = "days"))


# Split data into quarters

q4_date = as.Date("2011-12-10")-90
q3_date = q4_date - 90
q2_date = q3_date - 90
data$quarter_of_purchase = 1
data$quarter_of_purchase[which(data$InvoiceDate>=q4_date)] = 4
data$quarter_of_purchase[which(data$InvoiceDate<q4_date & data$InvoiceDate>=q3_date)] = 3
data$quarter_of_purchase[which(data$InvoiceDate<q3_date & data$InvoiceDate>=q2_date)] = 2


# How many days worth of data do we have?
max(data$InvoiceDate) - min(data$InvoiceDate)


# How many records in just the UK?
nrow(data[data$Country=='United Kingdom',])

# What percentage of records is that?
nrow(data[data$Country=='United Kingdom',])/nrow(data)*100


# Let's only look at records from the UK
data_uk = data[data$Country=='United Kingdom',]
summary(data_uk)



# Let's add a purchase amount column
data_uk$purchase_amount = data_uk$UnitPrice*data_uk$Quantity



# How many records with valid customer IDs?
nrow(data_uk[!is.na(data_uk$CustomerID),])


# What percentage of records is that?
nrow(data_uk[!is.na(data_uk$CustomerID),])/nrow(data_uk)*100


# New dataframe with cust
data_uk_cust = data_uk[!is.na(data_uk$CustomerID),]

# Negative purchase amounts/refunds? Lets check -
nrow(data_uk_cust[data_uk_cust$purchase_amount < 0,])/nrow(data_uk_cust)*100

# Total amount thats refunded?
sum(data_uk_cust[data_uk_cust$purchase_amount < 0,]$purchase_amount)/sum(data_uk_cust[data_uk_cust$purchase_amount >= 0,]$purchase_amount)*100



cust_agg_invoice <- data_uk_cust %>%
  group_by(InvoiceNo) %>%
  summarize(CustomerID = max(CustomerID), purchase_amount = sum(purchase_amount),
            days_since = max(days_since), InvoiceDate = max(InvoiceDate), quarter_of_purchase=max(quarter_of_purchase))





# Compute RFM variables as of today
customers_total = sqldf("SELECT CustomerID,
                               MIN(days_since) AS 'recency',
                               MAX(days_since) AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'avg_amount',
                               MAX(purchase_amount) AS 'max_amount',
                               SUM(purchase_amount) AS 'revenue'
                        FROM cust_agg_invoice
                        GROUP BY 1")




# How many unique customers does this give us?
nrow(customers_total)
# 3950


# Histogram with density plot of recency for cust
ggplot(customers_total, aes(x=recency)) + 
  geom_histogram(aes(y = ..density..),color="darkblue", fill="lightblue",binwidth = 5)+
  geom_density( alpha=.2, fill="#FF6666")+
  ylab("No. of Customers") +
  xlab("Recency (No. of days since last purchase)")+
  ggtitle("Distribution of Recency")+
  theme_stata()


# Boxplot
ggplot(customers_total, aes(y=log(recency))) +
  geom_boxplot(width=0.4) +
  ylab("No. of Customers") +
  xlab("Recency (No. of purchases)")+
  ggtitle("Distribution of Recency")+
  theme_stata()



# Histogram with density plot of frequency for cust
ggplot(customers_total, aes(x=frequency)) + 
  geom_histogram(aes(y = ..density..),color="darkblue", fill="lightblue",binwidth = 5)+
  geom_density( alpha=.2, fill="#FF6666")+
  ylab("No. of Customers") +
  xlab("Frequency (No. of purchases)")+
  ggtitle("Distribution of Frequency")+
  theme_stata()

# Boxplot
ggplot(customers_total, aes(y=log(frequency))) +
  geom_boxplot(width=0.4) +
  ylab("No. of Customers") +
  xlab("Log of Frequency (No. of purchases)")+
  ggtitle("Log Distribution of Frequency")+
  theme_stata()



# Histogram with density plot of frequency for cust
ggplot(customers_total, aes(x=frequency)) + 
  stat_bin(binwidth=10,color="darkblue", fill="lightblue") +
  stat_bin(binwidth=10, geom="text", aes(label=after_stat(count)), vjust=0)+
  ylab("No. of Customers") +
  xlab("Frequency(No. of purchases")+
  ggtitle("Distribution of Frequency")+
  theme_stata()


# Monetary Value
ggplot(customers_total, aes(x = log10(revenue))) +
  geom_density(fill='#add8e6',alpha=0.6) +
  geom_vline(aes(xintercept = log10(summary(customers_total$revenue[customers_total$revenue!=0]))[4]), 
             linetype = "dashed", size = 1,fill='black')+
  ylab("Density (% of customers)") +
  xlab("Log of Revenue (Monetary Value")+
  ggtitle("Distribution of Revenue")+
  theme_stata()


# Let's look at purchase cycles -
purchase_total = sqldf("SELECT InvoiceDate,
                               MIN(days_since) AS 'recency',
                               MAX(days_since) AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               SUM(purchase_amount) AS 'purchase_total'
                        FROM cust_agg_invoice
                        GROUP BY 1")




summary(customers_total)

# ----- Deciding segments ------


# Let's choose segments for recency as follows-
# Active - today(10th Dec) to median
# Warm - median to 3rd quantile
# Cold - more than 3rd Quantile

# Let's choose segments for frequency as follows-
# Guest - Lesser than equal to mean
# Recurring - More than mean


# Let's choose segments for monetary value as follows-
# Average Spender - Less than or equal to Mean
# Big spender - More than mean



# Let's create following 6 segments -
# 1) Active Guest Big (The Haileys Comet)- Ideal prospects you want to capture
# 2) Active Recurring Big (The Golden Apples)- Ideal prospects you want to retain
# 3) Active Average spender (Bread and butter) - Main consumer base
# 4) Warm Average spender (The masses) - Part of main consumer base who still support
# 5) Warm Big spender (George Clooney from Gravity) - Ideal Customer base thats slipping out of your hands 
# 6) Cold (Jack from Titanic) - Little prospect of bringing them back, can try a few hail marys


# Let's first divide up data -

customers_q4 <- customers_total


# ---- More complex 6-segment solution using which ------
customers_q4$segment = "NA"
customers_q4$segment[which(customers_q4$recency > 144)] = "cold"
customers_q4$segment[which(customers_q4$recency <= 144 & customers_q4$recency > 52)] = "warm"
customers_q4$segment[which(customers_q4$recency <= 52)] = "active"
customers_q4$segment[which(customers_q4$segment == "active" & customers_q4$avg_amount <= 293)] = "active average"
customers_q4$segment[which(customers_q4$segment == "active" & customers_q4$avg_amount > 293)] = "active big"
customers_q4$segment[which(customers_q4$segment == "active big" & customers_q4$frequency <= 5)] = "active guest big"
customers_q4$segment[which(customers_q4$segment == "active big" & customers_q4$frequency > 5)] = "active recurring big"


customers_q4$segment[which(customers_q4$segment == "warm" & customers_q4$avg_amount <= 293)] = "warm average"
customers_q4$segment[which(customers_q4$segment == "warm" & customers_q4$avg_amount > 293)] = "warm big"

# ----- Re-order factor in a way that makes sense -----
customers_q4$segment = factor(x = customers_q4$segment, levels = c("cold",
                                                                       "warm big", "warm average",
                                                                       "active guest big", "active recurring big", "active average"))

table(customers_q4$segment)
pie(table(customers_q4$segment), col = rainbow(24))

aggregate(x = customers_q4[, 2:5], by = list(customers_q4$segment), mean)

dfq4 <- customers_q4 %>% 
  group_by(segment) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(dfq4, aes(x = "", y = perc, fill = segment)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Segment")) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette="Set3")+
  theme_void()+
  theme(plot.background = element_rect(fill="#efeeed",color="#efeeed"))

barplot(r4$x, names.arg = r4$Group.1)

# --- retrospective segmentation Q3 -----

cust_agg_q3 = cust_agg_invoice[cust_agg_invoice$InvoiceDate < (as.Date("2011-12-10")-90),]
customers_q3 = sqldf("SELECT CustomerID,
                               MIN(days_since) -90 AS 'recency',
                               MAX(days_since) -90 AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'avg_amount',
                               MAX(purchase_amount) AS 'max_amount',
                               SUM(purchase_amount) AS 'revenue'
                        FROM cust_agg_q3
                        GROUP BY 1")



# ---- Q3 More complex 6-segment solution using which ------
customers_q3$segment = "NA"
customers_q3$segment[which(customers_q3$recency > 144)] = "cold"
customers_q3$segment[which(customers_q3$recency <= 144 & customers_q3$recency > 52)] = "warm"
customers_q3$segment[which(customers_q3$recency <= 52)] = "active"
customers_q3$segment[which(customers_q3$segment == "active" & customers_q3$avg_amount <= 293)] = "active average"
customers_q3$segment[which(customers_q3$segment == "active" & customers_q3$avg_amount > 293)] = "active big"
customers_q3$segment[which(customers_q3$segment == "active big" & customers_q3$frequency <= 5)] = "active guest big"
customers_q3$segment[which(customers_q3$segment == "active big" & customers_q3$frequency > 5)] = "active recurring big"


customers_q3$segment[which(customers_q3$segment == "warm" & customers_q3$avg_amount <= 293)] = "warm average"
customers_q3$segment[which(customers_q3$segment == "warm" & customers_q3$avg_amount > 293)] = "warm big"

# ----- Q3 Re-order factor in a way that makes sense -----
customers_q3$segment = factor(x = customers_q3$segment, levels = c("cold",
                                                                   "warm big", "warm average",
                                                                   "active guest big", "active recurring big", "active average"))

table(customers_q3$segment)
pie(table(customers_q3$segment), col = rainbow(24))

aggregate(x = customers_q3[, 2:5], by = list(customers_q3$segment), mean)




dfq3 <- customers_q3 %>% 
  group_by(segment) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(dfq3, aes(x = "", y = perc, fill = segment)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Segment")) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette="Set3")+
  theme_void()+
  theme(plot.background = element_rect(fill="#efeeed",color="#efeeed"))



# --- retrospective segmentation Q2 -----

cust_agg_q2 = cust_agg_invoice[cust_agg_invoice$InvoiceDate < (as.Date("2011-12-10")-180),]
customers_q2 = sqldf("SELECT CustomerID,
                               MIN(days_since) -180 AS 'recency',
                               MAX(days_since) -180 AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'avg_amount',
                               MAX(purchase_amount) AS 'max_amount',
                               SUM(purchase_amount) AS 'revenue'
                        FROM cust_agg_q2
                        GROUP BY 1")

# ---- Q2 More complex 6-segment solution using which ------
customers_q2$segment = "NA"
customers_q2$segment[which(customers_q2$recency > 144)] = "cold"
customers_q2$segment[which(customers_q2$recency <= 144 & customers_q2$recency > 52)] = "warm"
customers_q2$segment[which(customers_q2$recency <= 52)] = "active"
customers_q2$segment[which(customers_q2$segment == "active" & customers_q2$avg_amount <= 293)] = "active average"
customers_q2$segment[which(customers_q2$segment == "active" & customers_q2$avg_amount > 293)] = "active big"
customers_q2$segment[which(customers_q2$segment == "active big" & customers_q2$frequency <= 5)] = "active guest big"
customers_q2$segment[which(customers_q2$segment == "active big" & customers_q2$frequency > 5)] = "active recurring big"


customers_q2$segment[which(customers_q2$segment == "warm" & customers_q2$avg_amount <= 293)] = "warm average"
customers_q2$segment[which(customers_q2$segment == "warm" & customers_q2$avg_amount > 293)] = "warm big"

# ----- Q2 Re-order factor in a way that makes sense -----
customers_q2$segment = factor(x = customers_q2$segment, levels = c("cold",
                                                                   "warm big", "warm average",
                                                                   "active guest big", "active recurring big", "active average"))

table(customers_q2$segment)
pie(table(customers_q2$segment), col = rainbow(24))

aggregate(x = customers_q2[, 2:7], by = list(customers_q2$segment), mean)



dfq2 <- customers_q2 %>% 
  group_by(segment) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

ggplot(dfq2, aes(x = "", y = perc, fill = segment)) +
  geom_col(color = "black") +
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Segment")) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette="Set3")+
  theme_void()+
  theme(plot.background = element_rect(fill="#efeeed",color="#efeeed"))




# Show average revenue per customer and per segment
r2 = aggregate(x = customers_q2$revenue, by = list(customers_q2$segment), mean)
r3 = aggregate(x = customers_q3$revenue, by = list(customers_q3$segment), mean)
r4 = aggregate(x = customers_q4$revenue, by = list(customers_q4$segment), mean)

# Re-order and display results
r2 = r2[order(r2$x, decreasing = TRUE), ]
r3 = r3[order(r3$x, decreasing = TRUE), ]
r4 = r4[order(r4$x, decreasing = TRUE), ]

barplot(r3$x, names.arg = r3$Group.1)



#----- Transition expectation from Q3 to Q4--------

revenue_q4 = sqldf("SELECT CustomerID, SUM(purchase_amount) AS 'revenue_q4'
                      FROM cust_agg_invoice
                      WHERE quarter_of_purchase = 4
                      GROUP BY 1")
summary(revenue_q4)

# Merge Q3 customers and Q4 revenue
forward_q3 = merge(customers_q3, revenue_q4, all.x = TRUE)
forward_q3$revenue_q4[is.na(forward_q3$revenue_q4)] = 0

# Show average revenue per customer and per segment
r3_4 = aggregate(x = forward_q3$revenue_q4, by = list(customers_q3$segment), mean)
print(r3_4)

# Re-order and display results
r3_4 = r3_4[order(r3_4$x, decreasing = TRUE), ]
print(r3_4)

ggplot(r3_4, aes(y=x,x=factor(Group.1, levels = Group.1))) + 
  geom_col(color="darkblue", fill="lightblue")+
  ggtitle("Revenues in Quarter 4 for Quarter 3 Customers")+
  ylab("Average Purchase amount(Sterling Pounds)") +
  xlab("Customer segment")



#----- Transition expectation from Q2 to Q3--------

revenue_q3 = sqldf("SELECT CustomerID, SUM(purchase_amount) AS 'revenue_q3'
                      FROM cust_agg_invoice
                      WHERE quarter_of_purchase = 3
                      GROUP BY 1")
summary(revenue_q3)

# Merge Q2 customers and Q3 revenue
forward_q2 = merge(customers_q2, revenue_q3, all.x = TRUE)
forward_q2$revenue_q3[is.na(forward_q2$revenue_q3)] = 0


# Show average revenue per customer and per segment
r2_3 = aggregate(x = forward_q2$revenue_q3, by = list(customers_q2$segment), mean)
print(r2_3)

# Re-order and display results
r2_3 = r2_3[order(r2_3$x, decreasing = TRUE), ]
print(r2_3)


ggplot(r2_3, aes(y=x,x=factor(Group.1, levels = Group.1))) + 
  geom_col(color="darkblue", fill="lightblue")+
  ggtitle("Revenues in Quarter 3 for Quarter 2 Customers")+
  ylab("Average Purchase amount(Sterling Pounds)") +
  xlab("Customer segment")


# ------ Lets look at transitions --------

# Q3 to Q4
transition3_4 = merge(x=customers_q3,y=customers_q4,by="CustomerID")
#transition3_4 = transition3_4[transition3_4$segment.x!=transition3_4$segment.y,]
tr_3_4_counts = table(paste(transition3_4$segment.x, " to ",transition3_4$segment.y))
write.csv(data.frame(tr_3_4_counts),"transition_q3_q4.csv")

# Q2 to Q3
transition2_3 = merge(x=customers_q2,y=customers_q3,by="CustomerID")
#transition2_3 = transition2_3[transition2_3$segment.x!=transition2_3$segment.y,]
tr_2_3_counts = table(paste(transition2_3$segment.x, " to ",transition2_3$segment.y))
tr_2_3_counts
write.csv(data.frame(tr_2_3_counts),"transition_q2_q3.csv")


### Maybe we could create a diagram that shows arrows from each segment to the other with the
### number of customers transitioning above them and the color of the arrow indicating whether
### the transition is favourable or not


# --- COMPUTING PREDICTORS AND TARGET VARIABLES (PROBABILITY TO BUY NEXT QUARTER) ------------

# Target column indicating whether the customer is active in the next quarter
forward_q3$active_q4 = as.numeric(forward_q3$revenue_q4 > 0)

# Let's also add the segment of the customer from the previous quarter
prev_quart_segment <- merge(x=customers_q3,y=customers_q2,by="CustomerID",all.x=TRUE)$segment.y
prev_quart_segment = as.character(prev_quart_segment)
prev_quart_segment[is.na(prev_quart_segment)] = "None"
prev_quart_segment = as.factor(prev_quart_segment)

forward_q3$prev_segment = prev_quart_segment

# Train-Test split

set.seed(101) # Set Seed so that same sample can be reproduced in the future
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(forward_q3), size = floor(.75*nrow(forward_q3)), replace = F)
X_train <- forward_q3[sample,]
X_test  <- forward_q3[-sample, ]
y_train = forward_q3[sample,]$active_q4
y_test = forward_q3[-sample,]$active_q4

# Calibrate probability model (logit)

prob.logit.model = glm(data = X_train, formula = active_q4 ~ recency + frequency + avg_amount
                       + revenue + prev_segment, family=binomial(link='logit'))

summary(prob.logit.model)

fitted.results <- predict(prob.logit.model,newdata=subset(X_test,select=c(2,3,4,5,6,7,8,9,10,11)),type='response')


# Plot Boxplot

X_test$fitted_results = fitted.results


p<-ggplot(X_test, aes(x=segment,y=fitted_results,color=segment)) +
  geom_boxplot() +
  theme_stata()+
  ylab("Probability to make a purchase in next quarter")+
  xlab("Segments")+
  ggtitle("Probability DIstribution per Segment")
p


fitted.results <- ifelse(fitted.results > 0.5,1,0)

# ----- Confusion Matrix ---------

conf <- confusionMatrix(data=as.factor(fitted.results), reference = as.factor(y_test))

conf

ctable <- as.table(conf)
fourfoldplot(ctable, color = c("#CC6666", "#99CC99"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")


draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, "Won't Buy", cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, "Will buy", cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, "Won't Buy", cex=1.2, srt=90)
  text(140, 335, "Will buy", cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
} 

draw_confusion_matrix(conf)




# -------- Predict  probabilities for current quarter--------

# Calibrate probability model (logit)


# Let's also add the segment of the customer from the previous quarter
prev_quart_segment_q4 <- merge(x=customers_q4,y=customers_q3,by="CustomerID",all.x=TRUE)$segment.y
prev_quart_segment_q4 = as.character(prev_quart_segment_q4)
prev_quart_segment_q4[is.na(prev_quart_segment_q4)] = "None"
prev_quart_segment_q4 = as.factor(prev_quart_segment_q4)

customers_q4$prev_segment = prev_quart_segment_q4



fitted.results_q4 <- predict(prob.logit.model,newdata=subset(customers_q4),type='response')


summary(fitted.results_q4)
customers_q4$probability = fitted.results_q4


p_pred<-ggplot(customers_q4, aes(x=segment,y=probability,color=segment)) +
  geom_boxplot() +
  theme_stata()+
  ylab("Probability to make a purchase in next quarter")+
  xlab("Segments")+
  ggtitle("Probability DIstribution per Segment")
p_pred


temp1 = customers_q4[customers_q4$probability>0.42031,]

temp2 = temp1[temp1$probability<0.76826,]
nrow(temp2)

list_cust = temp2$CustomerID

data_new = read.csv("data/Online_Retail.csv")

summary(data_new)

data_new$CustomerID = as.factor(data_new$CustomerID)

write.csv(data.frame(data_new[data_new$CustomerID %in% list_cust,]),"Online_Retail_targeted.csv")


