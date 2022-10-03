rm(list = ls())

# Importing libraries
library(ggplot2)

# Loading the data
df = read.csv('http://bit.ly/w-data')
View(df)
summary(df)

# Key observations :
# The average hrs of study is 5.012 hr.
# The average achieved score is 51.48

# Investigate for outliers :
par(mar = c(3,5,3,3))
par(mfrow = c(1,2))
boxplot(df$Hours, col = 7,
        main = 'Boxplot of hours of study',
        ylab = 'Study hrs.', las = 1, lwd = 2,
        cex.main = 2, cex.lab = 1.2)
boxplot(df$Scores, col = 3,
        main = 'Boxplot of exam scores',
        ylab = 'Score', las = 1, lwd = 2,
        cex.main = 2, cex.lab = 1.2)
# Comment : Thus, from the plots, it is clear that there is outlier in the data.

# Plot to guess the mathematical relationship between the variables
ggplot(df, aes(x = Hours, y = Scores)) +
        geom_point(colour = 'red') +
        theme_fivethirtyeight() +
        labs(title = 'Scatterplot of Study hours vs. Exam score \n',
             x = 'Hours of study', y = 'Percentage score') +
        theme(plot.title = element_text(face = 'bold',
                                        size = 20, 
                                        colour = 'darkblue'),
              axis.title = element_text(face = 'bold'),
              axis.text = element_text(face = 'bold'),
              axis.title.x = element_text(vjust = -3),
              axis.title.y = element_text(vjust = 5),
              plot.margin = unit(c(1,1.5,1,1.4),'cm')) +
        scale_y_continuous(n.breaks = 10)
# Comment : A clear positive linear relationship is present.

# Preparing the data for model fitting :
# We will split the data into train and test data
# by 75-25 division :

set.seed(123)
rs = sample(c('train','test'),
            replace = T,
            size = nrow(df),
            prob = c(0.75,0.25)); rs
table(rs)

# train data :
df_train = df[rs == 'train',]; df_train
df_test = df[rs == 'test',]; df_test

# fitting linear model to the train data :
l = lm(Scores ~ Hours, data = df_train); l

# model summary :
summary.lm(l)

# Comment : from the summary table, we see that
# 'p-value' of 'Hours' is very much less than 0.05
# So, the regression coefficient is significant.
# And, the R-square is 0.9615, which is very high,
# thus, the it is a very good fit.
# Interpretation : Almost 96.15% variation in the 
# 'percentage score' can be explained by the 
# fitted linear regression model of 'percentage score' 
# on 'study hours'.


# plot with regression line :
ggplot(df_train, aes(x = Hours, y = Scores)) +
        geom_point(colour = 'red') +
        geom_smooth(method = 'lm', colour = 'blue') +
        theme_fivethirtyeight() +
        labs(title = 'Fitting of linear regression on the train data',
             subtitle = 'Predictor : Study hours | Response : Percentage score \n',
             x = 'Hours of study', y = 'Percentage score') +
        theme(plot.title = element_text(face = 'bold',
                                        size = 20, 
                                        colour = 'darkblue'),
              plot.subtitle = element_text(face = 'bold',
                                           colour = 'darkred',
                                           size = 12),
              axis.title = element_text(face = 'bold'),
              axis.text = element_text(face = 'bold'),
              axis.title.x = element_text(vjust = -3),
              axis.title.y = element_text(vjust = 5),
              plot.margin = unit(c(1,1.5,1,1.4),'cm')) +
        scale_y_continuous(n.breaks = 10)


# Now, we will see how well the model predicts score
# (by using the test data)
pred_func = function(x) (as.numeric(l$coefficients[1] + x*l$coefficients[2]))

score_pred = pred_func(df_test$Hours)
data.frame('Original score' = df_test$Scores,
                 'Predicted score' = score_pred)
# the predicted scores are more or less near to the originals.
df2 = data.frame("Hours" = rep(df_test$Hours,2),
                 'Scores' = c(df_test$Scores, score_pred),
                 'type' = rep(c('Original','Predicted'),
                              each = nrow(df_test)))
View(df2)

ggplot(df2, aes(x = Hours, y = Scores, colour = type)) +
        geom_point(aes(colour = type)) +
        theme_minimal() +
        labs(x = 'Hours of study', y = 'Percentage score',
             colour = "Score types") +
        theme(axis.title = element_text(face = 'bold'),
              axis.text = element_text(face = 'bold'),
              axis.title.x = element_text(vjust = -3),
              axis.title.y = element_text(vjust = 5),
              legend.title = element_text(face = 'bold'),
              legend.text = element_text(face = 'bold'),
              plot.margin = unit(c(1,1.5,1,1.4),'cm')) +
        scale_y_continuous(n.breaks = 10) +
        scale_x_continuous(n.breaks = 10)


# Estimated percentage score of a student who studies 9.25
# hrs per day :-
pred_func(9.25)



