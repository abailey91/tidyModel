library(tidyverse)

test_data <- read_csv("data/ad_sales.csv") %>%
  drop_na() %>%
  mutate(Year = c(rep(2016,12),rep(2017,12),rep(2018,12))) %>%
  mutate(Date = as.Date(paste0(Month,"-",Year),"%d-%b-%Y")) %>%
  mutate(Month = factor(lubridate::month(Date,label = T))) %>%
  mutate(Trend = 1:nrow(.))

#create binary month vars
month_vars <- data.frame(model.matrix(~test_data$Month+0))
colnames(month_vars) <- gsub("test_data.Month","d_",colnames(month_vars))

#add to testing dataset
test_data <- test_data %>%
  bind_cols(month_vars) %>%
  mutate(store = "Store 1")

#duplicate test data with some random noise
test_data_2 <- test_data %>%
  mutate(store = "Store 2") %>%
  mutate(Sales = jitter(Sales,100))

#duplicate test data with some random noise
test_data_3 <- test_data %>%
  mutate(store = "Store 3") %>%
  mutate(Sales = jitter(Sales,100))


test_data_final <- bind_rows(test_data,test_data_2,test_data_3)

#look at data
ggplot(test_data_final,aes(x = Date, y = Sales)) +
  geom_line() +
  facet_wrap(~store)


model_pred_vars <- colnames(test_data_final)[!colnames(test_data_final) %in% c("Date","Sales","Month","Year","store","d_Dec")]


#test output
debug(relative_weights_importance)
tidy_model <- build_tidy_model_lm(data = test_data_final,
                                  y_var = "Sales",
                                  predictors = model_pred_vars,
                                  standardise = FALSE,
                                  store)

tidy_model[1,] %>% unnest(summary)

model_coefs <- tidy_model %>%
  unnest(summary) %>%
  ggplot(aes(x = term,y = statistic)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept = 1.96,linetype = "dotted",color = "red",size = 1) +
  geom_hline(yintercept = -1.96,linetype = "dotted",color = "red",size = 1) +
  facet_wrap(~store)

#get a view of variable importance
rel_weight <- relative_weights_importance(tidy_model$model[[2]])


