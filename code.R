library("gh")
library("lubridate")
library("ggplot2")
library("viridis")
library("dplyr")
library("tidyr")

# get data, loop because pagination
stardates <- NULL

geht <- TRUE
page <- 1
while(geht){
  print(page)
  stars <- try(gh("/repos/robjhyndman/forecast/stargazers",
              .token = Sys.getenv("GITHUB_TOKEN"),
              .send_headers = c("Accept" = 'application/vnd.github.v3.star+json'),
              page = page))
  
  geht <- stars != ""
  
  if(geht){
    stardates <- c(stardates, vapply(stars, "[[", "", "starred_at"))
    page <- page + 1
    }
  
  
}

stardates <- lubridate::ymd_hms(stardates)

# make table of counts per day
star_table <- data.frame(time = stardates)
star_table <- mutate(star_table, date = as.Date(time))
star_table <- group_by(star_table, date) %>%
  summarize(n = n()) 
star_table <- mutate(star_table, 
                     cum_n = cumsum(n))

ggplot(star_table,
       aes(date, cum_n)) +
  geom_point()

model1 <- lm("log(cum_n) ~ date", data = star_table)
model2 <- lm("cum_n ~ date", data = star_table) 
newdata <- data.frame(date = seq(from = Sys.Date(), by = "1 day", length = 1000))
pred1 <- predict(model1, newdata = newdata)
pred2 <- exp(predict(model2, newdata = newdata))

pred_table <- mutate(newdata,
                     exponential = pred1,
                     linear = pred2)
pred_table <- gather(pred_table, "model", "prediction", 2:3)

pred_table <- bind_rows(star_table, pred_table)
ggplot(pred_table) +
  geom_point(aes(date, cum_n)) +
  geom_line(aes(date, prediction, col = model))