# QUESTION 1

  # Import Data
  onlineNews <- read.csv2("alldata_onlinenews_33.csv", header = TRUE) 
  # Data pre-processing 
     # Cleaning 
       trimws(names(onlineNews), which = c("both", "left", "right"))
       
       if (sum(onlineNews$n_unique_tokens)==0) {
         onlineNews$n_unique_tokens<-NULL  
       } 
       
       if (sum(onlineNews$n_non_stop_words)==0) {
         onlineNews$n_non_stop_words<-NULL
       }
       
       if (sum(onlineNews$n_non_stop_unique_tokens)==0) {
         onlineNews$n_non_stop_unique_tokens<-NULL
       } 
       
     # Preparing 
       # Duplicating days of week
       onlineNews$is_weekend = NULL
       # URL and timedelta columns have been omitted since they are meta-data and cannot be treated as features
       onlineNews$url<-NULL
       onlineNews$timedelta <- NULL
       # The X column and the articles with no words included have no meaning and should not be included in the analysis
       onlineNews$X<-NULL
       onlineNews<-onlineNews[!onlineNews$n_tokens_content==0,]

       # Checking for null/NaN values
       for (i in 1:ncol(onlineNews))
       {
         na <- is.na(onlineNews[,i])
         inf <- is.infinite(onlineNews[,i])
         nan <- is.nan(onlineNews[,i])
       }
       any(na)
       any(nan)
       any(inf)
       
       str(onlineNews)
      
      # Factor variables
      onlineNews$weekday_is_monday <- factor(onlineNews$weekday_is_monday) 
      onlineNews$weekday_is_wednesday <- factor(onlineNews$weekday_is_wednesday) 
      onlineNews$weekday_is_thursday <- factor(onlineNews$weekday_is_thursday) 
      onlineNews$weekday_is_friday <- factor(onlineNews$weekday_is_friday) 
      onlineNews$weekday_is_tuesday <- factor(onlineNews$weekday_is_tuesday) 
      onlineNews$weekday_is_saturday <- factor(onlineNews$weekday_is_saturday) 
      onlineNews$weekday_is_sunday <- factor(onlineNews$weekday_is_sunday) 
      onlineNews$data_channel_is_lifestyle <- factor(onlineNews$data_channel_is_lifestyle) 
      onlineNews$data_channel_is_entertainment <- factor(onlineNews$data_channel_is_entertainment) 
      onlineNews$data_channel_is_bus <- factor(onlineNews$data_channel_is_bus) 
      onlineNews$data_channel_is_socmed <- factor(onlineNews$data_channel_is_socmed) 
      onlineNews$data_channel_is_tech <- factor(onlineNews$data_channel_is_tech) 
      onlineNews$data_channel_is_world <- factor(onlineNews$data_channel_is_world)
      
      summary(onlineNews)
      
      # Create numeric variable dataset
      library(psych)
      index <- sapply(onlineNews, class) !='factor'  
      newsNumeric <- onlineNews[,index]
      round(t(describe(newsNumeric <- onlineNews[,index])),2)
      n <- nrow(newsNumeric)
      
      # Create factor variable dataset
      newsFactors <- onlineNews[,!index]
      
# Visualization of numeric variables
      par(mfrow=c(2,4))
      for (i in c(1,2,3,4,5,6,8,9))
      {
        h1 <- hist(newsNumeric[,i], main=names(newsNumeric)[i], border='pink', col='purple')
      }
      par(mfrow=c(2,4))
      for (i in c(24,25,26,27,28,41,42,45))
      {
        h2 <- hist(newsNumeric[,i], main=names(newsNumeric)[i], border='pink', col='purple')
      }
      par(mfrow=c(2,3))
      for (i in c(29,30,31,32,33,34))
      {
        h3 <- hist(newsNumeric[,i], main=names(newsNumeric)[i], border='pink', col='purple')
      }
      
      # Analysis of factor variables
      barplot(sapply(newsFactors[,c(1:6)],table)/n, names.arg = c("Lfst", "Entrt", "Bus", "Soc", "Tech", "World"), horiz=T, las=1, col=2:3, ylim=c(0,9), cex.names=1.3)
      legend('topleft', fil=2:3, legend=c('No','Yes'), ncol=2, bty='n',cex=1.5)
      barplot(sapply(newsFactors[,c(7:13)],table)/n, names.arg = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"), horiz=T, las=1, col=2:3, ylim=c(0,11), cex.names=1.3)
      legend('topleft', fil=2:3, legend=c('No','Yes'), ncol=2, bty='n',cex=1.5)

 # Data exploration of shares
    # Outliers for shares exist
    outlier_values_shares <- boxplot.stats(newsNumeric[,45])$out  # outlier values
    print("Outliers of shares")
    print(length(outlier_values_shares))
    boxplot(newsNumeric[,45], main=names(newsNumeric)[45], boxwex=0.1)
  
    # Distribution of shares
    onlineNews$shares <- log(onlineNews$shares)
    newsNumeric$shares<- log(newsNumeric$shares)
    hist(onlineNews$shares, freq = FALSE, col="blue", main = "Distribution of log_shares", xlab = "Log_shares")
    curve(dnorm(x, mean = mean(onlineNews$shares), sd = sd(onlineNews$shares)), col = 2, lty = 1, lwd = 2, add = TRUE)
   
  # Pairs of shares and other numerical variables
  pairs(newsNumeric[,c(45,1,2,3,5,6,8,9)])
  pairs(newsNumeric[,c(45,24,25,26,27,28)])
  pairs(newsNumeric[,c(45,29,30,31,32,33,43,42)])

  # Shares on each factor variables
  par(mfrow=c(2,3))
  for(j in 1:6){
    boxplot(newsNumeric[,1]~newsFactors[,j], xlab=names(newsFactors)[j], ylab='Shares',cex.lab=1.5)
    abline(lm(newsNumeric[,1]~newsFactors[,j]),col=2)
  }
  par(mfrow=c(2,4))
  for(j in 7:13){
    boxplot(newsNumeric[,1]~newsFactors[,j], xlab=names(newsFactors)[j], ylab='Shares',cex.lab=1.5)
    abline(lm(newsNumeric[,1]~newsFactors[,j]),col=2)
  }
  
  # Correlation visualization of shares and calculations
  install.packages('corrplot')
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  library(corrplot)
  corrplot(cor(newsNumeric[,c(45,1,2,3,5,6,8,9)]), method= "color", col = col(200),
           type = "upper", order = "hclust", number.cex = .7,
           addCoef.col = "black", # Add coefficient of correlation
           tl.col = "black", tl.srt = 90, # Text label color and rotation
           # Combine with significance
           sig.level = 0.05, insig = "blank", 
           # hide correlation coefficient on the principal diagonal
           diag = FALSE)
  corrplot(cor(newsNumeric[,c(45,24,25,26,27,28)]), method= "color", col = col(200),
           type = "upper", order = "hclust", number.cex = .7,
           addCoef.col = "black", # Add coefficient of correlation
           tl.col = "black", tl.srt = 90, # Text label color and rotation
           # Combine with significance
           sig.level = 0.05, insig = "blank", 
           # hide correlation coefficient on the principal diagonal
           diag = FALSE)
  corrplot(cor(newsNumeric[,c(45,29,30,31,32,33,43,42)]), method= "color", col = col(200),
           type = "upper", order = "hclust", number.cex = .7,
           addCoef.col = "black", # Add coefficient of correlation
           tl.col = "black", tl.srt = 90, # Text label color and rotation
           # Combine with significance
           sig.level = 0.05, insig = "blank", 
           # hide correlation coefficient on the principal diagonal
           diag = FALSE)

# QUESTION 2
  
  # Initial regression model
  fit_full <- lm(shares ~ ., data = onlineNews)
  summary(fit_full)
    # Remove the variables that have multicolinearity
    onlineNews$weekday_is_sunday<-NULL
    onlineNews$LDA_04<-NULL
    onlineNews$n_non_stop_words <-NULL

  # Second regression model
  fit_full2 <- lm(shares ~ ., data = onlineNews)
  summary(fit_full2)
  # Final regression model
    # AIC method
    selected_aic_model <- step(fit_full2, direction='both')
    summary(selected_aic_model)
    length(coef(selected_aic_model))
    
    # BIC method
    nRegres = length(resid(fit_full))
    selected_bic_model = step(fit_full2, direction = "both", k = log(nRegres))
    summary(selected_bic_model)
    length(coef(selected_bic_model))
  
# QUESTION 3  
  
# Residual Analysis and Diagnostics
  par(mfrow=c(2,4))
  # Normality of residuals   
      # Visualization      
      plot(selected_bic_model, which=2, col=c("red"))  # Q-Q Plot
      hist(selected_bic_model$residuals,probability = T, main = 'Residuals')
      x0<- seq (min(selected_bic_model$residuals), max(selected_bic_model$residuals), length.out = 100)
      y0 <- dnorm(x0, mean(selected_bic_model$residuals), sd(selected_bic_model$residuals))
      lines(x0,y0, col=2,lty=2)
      # Testing normality
      install.packages("nortest")
      library(nortest)
      lillie.test(selected_bic_model$res)
      shapiro.test(selected_bic_model$res)
  # Data linearity 
      # Visualization
      plot(selected_bic_model, which=1, col=c("blue")) # Residuals vs Fitted Plot
      # Testing linearity 
      library(car)
      residualPlots(selected_bic_model, plot=F, type = "rstudent")
  # Homoscedasticity of residuals variance  
      # Visualization
      plot(selected_bic_model, which=3, col=c("blue"))  # Scale-Location Plot
      studentResiduals <- rstudent(selected_bic_model)
      yhat <- fitted(selected_bic_model)
      plot(yhat, studentResiduals, main = "Residuals",col=c("blue"))
      abline(h=c(-2,2), col=2, lty=2)
      plot(yhat, studentResiduals^2, main = "Residuals",col=c("blue"))
      abline(h=4, col=2, lty=2)
      # Testing homoscedasticity
      qyhat.quantiles <- cut(yhat, breaks=quantile(yhat,probs = seq(0,1,0.25), dig.lab=6))
      library(car)
      leveneTest(rstudent(selected_bic_model)~qyhat.quantiles)
      ncvTest(selected_bic_model)
      
  # Outliers and high levarage points
      # Visualization
      plot(selected_bic_model, which=4, col=c("blue"))  
      plot(selected_aic_model, which=5, col=c("blue"))  
      leveragePlots(selected_bic_model,col="blue")

  # Multicollinearity    
      library(car)
      round(vif(selected_bic_model),2)

  # Fixing assumptions' problems
  
      # (1) Polynominals 
      polynominal <- lm(shares ~ poly(n_tokens_title,5, raw=TRUE)
                        + poly(n_tokens_content,5, raw=TRUE) 
                        + poly(num_hrefs, 5, raw=TRUE) 
                        + poly(kw_min_min,5, raw=TRUE) 
                        + poly(kw_min_avg,5, raw=TRUE)
                        + poly(kw_max_avg,5, raw=TRUE)
                        + poly(kw_avg_avg,5, raw=TRUE) 
                        + poly(LDA_00,5, raw=TRUE)
                        + poly(global_subjectivity,5, raw=TRUE)
                        + poly(avg_negative_polarity,5, raw=TRUE)
                        + poly(min_negative_polarity,5, raw=TRUE)
                        + data_channel_is_entertainment
                        + data_channel_is_tech
                        + weekday_is_tuesday
                        + weekday_is_wednesday
                        + weekday_is_thursday,
                        data = onlineNews)
      summary(polynominal)
      length(coef(polynominal))
      
      # AIC method
      polyn_selected_aic_model <- step(polynominal, direction='both')
      summary(polyn_selected_aic_model)
      length(coef(polyn_selected_aic_model))
      
      # BIC method
      polyn_nRegres = length(resid(polynominal))
      poly_selected_bic_model = step(polynominal, direction = "both", k = log(polyn_nRegres))
      summary(poly_selected_bic_model)
      length(coef(poly_selected_bic_model))
    
    # (2) Logarithms
    
    logarithms <- lm(shares ~ log(n_tokens_title)
                     + log(n_tokens_content)
                     + num_hrefs 
                     + kw_min_min
                     + kw_min_avg
                     + kw_max_avg
                     + kw_avg_avg
                     + log(LDA_00)
                     + global_subjectivity
                     + avg_negative_polarity
                     + min_negative_polarity
                     + data_channel_is_entertainment
                     + data_channel_is_tech
                     + weekday_is_tuesday
                     + weekday_is_wednesday
                     + weekday_is_thursday,
                     data = onlineNews)
    
    
    summary(logarithms)
    length(coef(logarithms))
    
    # AIC method
    log_selected_aic_model <- step(logarithms, direction='both')
    summary(log_selected_aic_model)
    length(coef(log_selected_aic_model))
    
    # BIC method
    log_nRegres = length(resid(logarithms))
    log_selected_bic_model = step(logarithms, direction = "both", k = log(log_nRegres))
    summary(log_selected_bic_model)
    length(coef(log_selected_bic_model))

    
# QUESTION 4: 10-fold cross validation
    
    testOnlineNews <- read.csv2("OnlineNewsPopularity_test.csv", header = TRUE) 
    # Data pre-processing 
        # Cleaning 
        trimws(names(testOnlineNews), which = c("both", "left", "right"))
        
        if (sum(testOnlineNews$n_unique_tokens)==0) {
          testOnlineNews$n_unique_tokens<-NULL  
        } 
        
        if (sum(testOnlineNews$n_non_stop_words)==0) {
          testOnlineNews$n_non_stop_words<-NULL
        }
        
        if (sum(testOnlineNews$n_non_stop_unique_tokens)==0) {
          testOnlineNews$n_non_stop_unique_tokens<-NULL
        } 
    
    # Preparing 
        # Duplicating days of week
        testOnlineNews$is_weekend = NULL
        # URL and timedelta columns have been omitted since they are meta-data and cannot be treated as features
        testOnlineNews$url<-NULL
        testOnlineNews$timedelta <- NULL
        # The X column and the articles with no words included have no meaning and should not be included in the analysis
        testOnlineNews$X<-NULL
        testOnlineNews<-testOnlineNews[!testOnlineNews$n_tokens_content==0,]
        
        # Checking for null/NaN values
        for (i in 1:ncol(testOnlineNews))
        {
          na1 <- is.na(testOnlineNews[,i])
          inf1 <- is.infinite(testOnlineNews[,i])
          nan1 <- is.nan(testOnlineNews[,i])
        }
        any(na1)
        any(nan1)
        any(inf1)
        
        # Converting shares to log_shares
        testOnlineNews$shares <- log(testOnlineNews$shares)
    
    str(testOnlineNews)
    
    wilcox.test(onlineNews[,55],testOnlineNews[,55])
    
    # Full model
    library("caret")
    model_full <- train(
      shares ~., testOnlineNews, 
      method = "lm", trControl = trainControl(
      method = "cv", 
      number = 10, 
      verboseIter = TRUE))
    
    summary(model_full)
      # Remove columns with multicolinearity problem
       testOnlineNews$n_non_stop_words <-NULL
       testOnlineNews$weekday_is_sunday <- NULL
       testOnlineNews$LDA_04 <- NULL
       
       library("caret")
       model_full_new <- train(
         shares ~., testOnlineNews, 
         method = "lm", trControl = trainControl(
           method = "cv", 
           number = 10, 
           verboseIter = TRUE))
       summary(model_full_new)
       
    # Full BIC model
    library("caret")
    model_bic_full <- train(
      shares ~ n_tokens_title + n_tokens_content + num_hrefs + data_channel_is_entertainment +
        data_channel_is_tech + kw_min_min + kw_min_avg + kw_max_avg + kw_avg_avg + weekday_is_tuesday 
      + weekday_is_wednesday + weekday_is_thursday + LDA_00 + global_subjectivity + avg_negative_polarity
      + min_negative_polarity, testOnlineNews, 
        method = "lm", trControl = trainControl(
        method = "cv", 
        number = 10, 
        verboseIter = TRUE))
    
    summary(model_bic_full)
    
    # Full AIC model
    library("caret")
    model_aic_full <- train(
      shares ~ n_tokens_title + n_tokens_content + num_hrefs + data_channel_is_entertainment 
      + data_channel_is_tech + data_channel_is_socmed + data_channel_is_tech + kw_min_min 
      + kw_min_max + kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_min_shares 
      + weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday
      + weekday_is_friday + LDA_00 + global_subjectivity + min_positive_polarity + avg_negative_polarity
      + min_negative_polarity + title_sentiment_polarity + global_rate_positive_words, testOnlineNews, 
        method = "lm", trControl = trainControl(
        method = "cv", 
        number = 10, 
        verboseIter = TRUE))
    
    summary(model_aic_full)
  
    # AIC polynomial model
    library("caret")
    model_aic_polyn <- train(
      shares ~ poly(n_tokens_title, 5, raw = TRUE) + poly(num_hrefs, 5, raw = TRUE) 
      + data_channel_is_tech + weekday_is_tuesday + weekday_is_wednesday 
      + poly(kw_min_min, 5, raw = TRUE) + poly(kw_min_avg, 5, raw = TRUE)
      + poly(kw_max_avg, 5, raw = TRUE) + poly(kw_avg_avg, 5, raw = TRUE) + poly(LDA_00, 5, raw = TRUE)
      + poly(global_subjectivity, 5, raw = TRUE)
      + data_channel_is_entertainment
      + data_channel_is_tech
      + weekday_is_tuesday
      + weekday_is_wednesday
      + weekday_is_thursday, testOnlineNews, 
        method = "lm", trControl = trainControl(
        method = "cv", 
        number = 10, 
        verboseIter = TRUE))
    
    summary(model_aic_polyn)
    
    # BIC polynomial model
    library("caret")
    model_bic_polyn <- train(
      shares ~ poly(kw_min_avg, 5, raw = TRUE) + data_channel_is_entertainment + data_channel_is_tech
      + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday, testOnlineNews, 
        method = "lm", trControl = trainControl(
        method = "cv", 
        number = 10, 
        verboseIter = TRUE))
    
    summary(model_bic_polyn)
    
# QUESTION 5: TEST DATA FILE
    
 # Selecting model
    
    # Initial regression model
    fit_full_3 <- lm(shares ~ ., data = testOnlineNews)
    summary(fit_full_3)

    # Final regression model
    # AIC method
    test_selected_aic_model <- step(fit_full_3, direction='both')
    summary(test_selected_aic_model)
    length(test_selected_aic_model$coefficients)

    # BIC method
    test_nRegres = length(resid(fit_full_3))
    test_selected_bic_model = step(fit_full_3, direction = "both", k = log(test_nRegres))
    summary(test_selected_bic_model)
    length(test_selected_bic_model$coefficients)
    
    # 10-fold cross validation of full model
    library("caret")
    model_full_test <- train(
      shares ~., onlineNews, 
      method = "lm", trControl = trainControl(
        method = "cv", 
        number = 10, 
        verboseIter = TRUE))
    summary(model_full_test)
    
    # 10-fold cross validation of AIC model
    library("caret")
    model_full_aic_test_2 <- train(
      shares ~ n_tokens_title + n_tokens_content + n_unique_tokens + n_non_stop_unique_tokens + num_hrefs 
      + num_self_hrefs + num_imgs + average_token_length + data_channel_is_lifestyle 
      + data_channel_is_entertainment + data_channel_is_bus + data_channel_is_socmed + data_channel_is_tech 
      + kw_min_min + kw_max_min + kw_avg_min + kw_max_max + kw_avg_max + kw_min_avg + kw_max_avg + kw_avg_avg
      + self_reference_min_shares
      + weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday
      + weekday_is_friday + LDA_00 + LDA_01 + LDA_02 + LDA_03
      + global_subjectivity + global_rate_positive_words + avg_negative_polarity
      + title_subjectivity + title_sentiment_polarity + abs_title_subjectivity, onlineNews, 
      method = "lm", trControl = trainControl(
        method = "cv", 
        number = 10, 
        verboseIter = TRUE))
    
    summary(model_full_aic_test_2)
    
    # 10-fold cross validation of BIC model
    library("caret")
    model_bic_full_test_2 <- train(
      shares ~ n_non_stop_unique_tokens +
      + data_channel_is_entertainment + data_channel_is_socmed + data_channel_is_tech 
      + kw_min_min + kw_max_min + kw_avg_min + kw_avg_max + kw_min_avg + kw_max_avg + kw_avg_avg
      + self_reference_min_shares
      + weekday_is_monday + weekday_is_tuesday + weekday_is_wednesday + weekday_is_thursday
      + weekday_is_friday + LDA_00 +
      + global_subjectivity, onlineNews, 
      method = "lm", trControl = trainControl(
        method = "cv", 
        number = 10, 
        verboseIter = TRUE))
    
    summary(model_bic_full_test_2)
    
    
    
   

     
