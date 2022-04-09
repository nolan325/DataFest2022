#downloading packages
install.packages("stringr")
library(stringr)
library(plyr)
library(tibble)
install.packages("S$Vectors")
library(S4Vectors)
install.packages("lubridate")
library(lubridate)
library(ggplot2)

#creates copy of logs to safely use data
logs_copy <- logs
#creates list of lists of data based on player_id (i.e. based on each kid that played it)
sorted_by_players <- split(new_log, new_log$player_id)
str(sorted_by_players$`6427041`)
#looking at the finishing time and final actions of whatever player is desired
finishing_times_by_player <- c()


#Cleaning up DATA

df1 <- cbind(df1, new_log[age_gender_row, c('avatar_age', 'avatar_gender','player_id')])


#Converting weird time-string format to hours
df1$time_clean <- as.POSIXlt(df1$`new_log$event_time[cumsum_no_events_per_player]`,
                             format="%%H %%M %%S")
df1$time_clean <- hms(df1$`new_log$event_time[cumsum_no_events_per_player]`)
df1$hours <- hour(df1$time_clean)
df1$minutes <- minute(df1$time_clean)
new_time <- data.frame()


convert_to_hours <- function(time) {
  new_time <- hour(time)+minute(time)/60 + second(time)/3600
}
df1$time_clean3 <- convert_to_hours(df1$time_clean)
df2 <- data.frame(df1$player_ids, df1$avatar_age, df1$avatar_gender, df1$time_clean3)



#individually adding "0H" to values that played for less than 1 hour
fix(df2)
fix(df1)

#Extract unique player_ids
player_ids <- unique(new_log$player_id, nmax =166)

#creating array with rows containing corresponding to the last action executed (i.e. the time
#they finished) and the 3rd action each player executed corresponding (mostly) to the action of
#confirming gender and age selection
age_gender_row <- array(NA, dim=167)
age_gender_row[1] <- 3
for(i in 1:166) {
  age_gender_row[i+1] <- 3 + cumsum_no_events_per_player[i]
}
age_gender_row <- age_gender_row[-167]
df1 <- data.frame(player_ids)
df1 <- cbind(df1, new_log$event_time[cumsum_no_events_per_player])
cumsum_no_events_per_player
finishing_times_by_player <- append(finishing_times_by_player,
                                      new_log$event_time[cumsum_no_events_per_player])

print(finishing_times_by_player)
cumsum_no_events_per_player <- cumsum(no_of_events_per_player)
print(finishing_times_by_player)

times_as_hours <- as.data.frame(strsplit(finishing_times_by_player, " "),
                               make.names = T)
fix(times_as_hours)
i <- 1
for(i in 1:166){
  if(length(times_as_hours[[i]]) != 4) {
    paste0('true', i)
  }
}


#for loop through each player to discover the number of events for each player
#store in vector hopefully of length 166
no_of_events_per_player <- c()
player <- 1
dummy <- data.frame()
for(player in 1:166) {
  dummy <- as.data.frame(sorted_by_players[player])
  no_of_events_per_player <- append(no_of_events_per_player, 
                                    value=nrow(dummy))
}


#creates smaller dataset, contained in folder, named "new_log"
new_log <- subset(logs_copy, select = c("player_id","event_id","event_description","event_time","avatar_age","avatar_gender"))

#attempting to look at the final entries in event_time for specific players,
#thereby learning how long they played the game
sorted_by_players$`6427040`$event_time[length(sorted_by_players$`6427040`$event_time)]
X$`6427011`$avatar_age = 14
X$`6427001`$event_time[14997]
X[1]$avatar_age
for(i in 1:166) {
  print(X[i]$avatar_age)
}

#Finding frequency of certain events#
time_spent_selecting_avatar <- subset(new_log, subset=(new_log$event_id == 605 ))
sorted_by_players2 <- split(time_spent_selecting_avatar, f=time_spent_selecting_avatar$player_id)
time_spent_selecting_avatar$time_clean <- hms(time_spent_selecting_avatar$event_time)
fix(time_spent_selecting_avatar)
time_spent_selecting_avatar <- time_spent_selecting_avatar[,2:5]

#Analyzing the DATA
attach(beautifuldataframe)
plot1 <- hist(finish)
plot2 <- hist(no_outliers$finish)
plot3 <- hist(no_outliers2$finish)
summary(finish)
summary(no_outliers$finish)
summary(no_outliers2$finish)

#Plots
ggplot(data=no_outliers3, aes(x=gender, y=finish, group=age)) +
  geom_jitter(aes(color=gender))
mydata<-morebeautifuldf

#Bar graph
ggplot(no_outliers3,                                  
       aes(x = age,
           y = finish,
           fill = gender)) +
  geom_bar(stat = "summary",
           position = "dodge")+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#Slide " " Plot
boxplot(data=no_outliers3, finish~age*gender,
        main="Completion Time by Age and Gender",
        ylab="Completion Time (hours)",
        xlab="Age crossed with gender",
        col=(c("cornflowerblue","lightgreen","indianred2","yellow")))


#Subsetting the beautiful data
male11 <- subset(no_outliers3, subset=(gender=='Male' & age == 11))
male12 <- subset(no_outliers3, subset=(gender=='Male' & age == 12))
male13 <- subset(no_outliers3, subset=(gender=='Male' & age == 13))
male14 <- subset(no_outliers3, subset=(gender=='Male' & age == 14))
female11 <- subset(no_outliers3, subset=(gender=='Female' & age == 11))
female12 <- subset(no_outliers3, subset=(gender=='Female' & age == 12))
female13 <- subset(no_outliers3, subset=(gender=='Female' & age == 13))
female14 <- subset(no_outliers3, subset=(gender=='Female' & age == 14))
male <- subset(no_outliers3, subset=(gender=='Male'))
female <- subset(no_outliers3, subset=(gender=='Female'))


names_ <- c('male11','male12','male13','male14','female11', 'female12', 'female13','female14')
names_
i=1
for(i in 1:length(names_)) {                              # Head of for-loop
  write.csv2(get(names_[i]),                              # Write CSV files to folder
             paste0("C:/Users/nhauc/OneDrive/Documents/R",
                    names_[i],
                    ".csv"),
             row.names = T)
}

female11.12 <- rbind(female11, female12)
female13.14 <- rbind(female13, female14)
male11.12 <- rbind(male11, male12)
male13.14 <- rbind(male13, male14)


age_factor<-array(dim = nrow(female))


for(i in 1:59){
  if(female$age[i] == 11 | female$age[i] == 12) {
    age_factor[i] <- 0
  } else if(female$age[i] == 13 | female$age[i] == 14) {
    age_factor[i] <- 1
  }

}
female$age_factor <- age_factor

nrow(male)
age_factor1<-array(dim = nrow(male))
for(i in 1:69){
  if(male$age[i] == 11 | male$age[i] == 12) {
    age_factor1[i] <- 0
  } else if(male$age[i] == 13 | male$age[i] == 14) {
    age_factor1[i] <- 1
  }
  
}
male$age_factor <- age_factor1
age_factor1



nrow(female)
#Bartlett test for equality of variances between age groups within gender groups
bartlett.test(male$finish ~ male$age_factor) #EQUAL VARIANCES P=.941
bartlett.test(female$finish ~ female$age_factor) #...not equal... p=.05186

#T-Testing for equality of means between age groups within gender groups
t.test(finish ~ age_factor, var.equal=T, data=male) #p=.343
t.test(finish ~ age_factor, var.equal=F, data=female, level=.9) #p=.6002

#Bartlett test for equality of variances between gender within age groups
female.male11.12 <- rbind(male11.12, female11.12)
female.male13.14 <- rbind(male13.14, female13.14)
female.male11.12$gender <- as.factor(female.male11.12$gender)
female.male13.14$gender <- as.factor(female.male13.14$gender)
bartlett.test(female.male11.12$finish ~ female.male11.12$gender) #EQUAL VARIANCES P=.2212
bartlett.test(female.male13.14$finish ~ female.male13.14$gender) #Eqaul p=.4556

#T-Testing for equality of means between genders within age groups
t.test(finish ~ gender, var.equal = T, data=female.male11.12) #p=.7829
t.test(finish ~ gender, var.equal = T, data=female.male13.14) #p=.5373

#Bartlett test for equality of variances between genders w/o respect to age
no_outliers3$gender <- as.factor(no_outliers3$gender)
bartlett.test(no_outliers3$finish ~ no_outliers3$gender) #equal p=.9563
#T-Test for equality of means
t.test(finish ~ gender, data=no_outliers3) #ayyy p=.5239

detach(beautifuldataframe)

#Table to display p-values
p-values <- c()
mtx<-matrix()


#Outliers based on time played: under 100 hours played, under 50, under 25 etc.
no_outliers <- subset(beautifuldataframe, subset = (beautifuldataframe$finish < 100) )
no_outliers2 <- subset(beautifuldataframe, subset = (beautifuldataframe$finish < 50) )
no_outliers3 <- subset(beautifuldataframe, subset = (beautifuldataframe$finish < 25) )

#Manually fixing missing values that weren't captured in our algorithm: players who selected
#multiple ages or genders before settling on a final choice
fix(beautifuldataframe)



  

#exporting the data. You would have to change your filepath
write.csv(new_log, "C:\\Users\\nhauc\\OneDrive\\Documents\\R\\new_log.csv", row.names = T)
write.csv(df2, "C:\\Users\\nhauc\\OneDrive\\Documents\\R\\beautifuldataframe.csv",row.names =T)
write.csv(beautifuldataframe, "C:\\Users\\nhauc\\OneDrive\\Documents\\R\\morebeautifuldf.csv", row.names = T)
