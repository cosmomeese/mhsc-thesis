### Start teramonagi Script ####

# optional - store token

# Get activity intraday time series
# You have to use a personal key and secret.

#### for each date in range

fetchThisDate <- "2016-05-12"
df_steps <- get_activity_intraday_time_series(token, 
                                              "steps",
                                              fetchThisDate,
                                              detail_level="1min")
#ggplot(df, aes(x=time, y=value)) + geom_line()



df_heart <- get_heart_rate_intraday_time_series(token, 
                                                date=fetchThisDate, 
                                                detail_level="1min")


#ggplot(df, aes(x=time, y=value)) + geom_line()

# merge frames

## END

df_together <- dplyr::inner_join(df_steps,df_heart,by="time")
colnames(df_together) <- c("time","steps","hr")

p <- ggplot(df_together, aes(x = time))
p <- p + geom_line(aes(y = steps, colour = "Step Count"))

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = hr, colour = "Heart Rate"))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Heart Rate [bpm]"))

# modifying colours and theme options
p <- p + scale_colour_manual(values = c("red", "blue"))
p <- p + labs(y = "Step Count [/minute]",
              x = "Date and time",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.8, 0.9))
print(p)

### Start avsecz Script ####

## NOT YET WORKING

token <- get_fitbit_token(file_path = "./fitbit/fitbitr")
token <- get_fitbit_token(appname = "studyScript-FitR",
                          key = FITBIT_KEY,
                          secret = FITBIT_SECRET,
                          scope = scope)
fitbit_token <- token

attr(fitbit_token, "rate_limit") <- list(total = 150,
                                         remains = 150,
                                         next_reset = NA,
                                         current_time = Sys.time())

dt <- get_activity(from_date = "2017-12-06", token = fitbit_token)