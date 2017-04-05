library(dplyr) # dplyr is library written by Hadley Wickham for easy manipulation of data. See https://cran.r-project.org/web/packages/dplyr/index.html to download or http://dplyr.tidyverse.org/
library(readr)
library(ggplot2) # This is a plotting system for R, also by Hadley Wickham. See http://ggplot2.org/.

#a data frame is a data structure like a spreadsheet that uses code instead of a graphical user interface.


d <- read_csv("~/Downloads/reddit_diabetes.csv") #This data set was accessed through the reddit API and is publicly available.


d %>%
  mutate(label = ifelse(grepl("pump", author_flair_text, ignore.case=TRUE) &
                          grepl("MDI", author_flair_text, ignore.case=TRUE), "pump & MDI", 
                        ifelse(grepl("pump", author_flair_text), "pumper",
                               ifelse(grepl("MDI", author_flair_text, ignore.case=TRUE), "MDI", NA)))) %>%
  select(author, author_flair_text, label) %>%
  filter(label == "pump & MDI" & !duplicated(author)) %>%
  as.data.frame() %>%
  nrow()


#Based on flair, how many authors have changed their flair from MDI to pump or pump to MDI?


d %>%
  mutate(label = ifelse(grepl("pump", author_flair_text, ignore.case=TRUE) &
                          grepl("MDI", author_flair_text, ignore.case=TRUE), "pump & MDI", 
                        ifelse(grepl("pump", author_flair_text), "pumper",
                               ifelse(grepl("MDI", author_flair_text, ignore.case=TRUE), "MDI", NA)))) %>%
  select(author, author_flair_text, label, created_utc) %>%
  # mutate(created_utc = as.POSIXct(created_utc, origin = "1970-01-01")) %>%
  # arrange(author, created_utc)
  filter(!is.na(label)) %>%
  group_by(author) %>%
  filter(!duplicated(label)) %>%
  count(author, sort = TRUE)



#The filter_by_anything function allows me to filter by key word(s).
names(d)
filter_by_anything <- function(pattern) {
  d %>%
    filter(grepl(pattern, body)) # %>%
  #     filter(!duplicated(author)) %>% 
  #     count(author_flair_text, sort=TRUE)
}
filter_by_anything("open source")

# Here I filter by keyword "cyborg" and use mutate to create a new column with reddit comments that contain cyborg
# and the date and time of their creation. I then use select to look at the posts that are archived as well as when they were
# created. Next I use arrange to order them from oldest to newest and finally use as.data.frame() to read the comments.
filter_by_anything("cyborg") %>%
  mutate(created_utc = as.POSIXct(created_utc, origin = "1970-01-01")) %>%
  select(created_utc, archived) %>%
  arrange(created_utc) %>%
  as.data.frame()
# How do people with diabetes who wear insulin pumps (cyborgs) access their own data?
filter_by_anything("data") %>%
  filter(grepl("open source", body)) %>%
  as.data.frame()
# One of the comments this yielded was interesting for answering our question. ``8  There is so much going on in this
#field right now. In the past, people were pretty limited in what they could do with technology to manage their diabetes,
#because the type of technology wasn't considered in the FDA approval process (i.e. it had to jump through all the 
#same hoops that a new pump model would, even if it was just an app that was reading data, not writing it). Even at 
#that time, people were still coming up with ideas. In particular, I can think of a user here who set up an IFTTT 
#program that would let him text his sugar to a number and then it would be automatically logged in a Google spreadsheet.
#He shared the code here and I was able to set it up for myself, too. I probably still have the code snippet somewhere.
#\n\nNow there has been a proliferation of diabetes related apps. There are some that gamify your care. An example is 
#[MySugr](https://mysugr.com/) where you make very detailed log entries to get more points to "feed the monster" and 
#if you hit a certain number ... <truncated>""

filter_by_anything("csv")%>%
  filter(!duplicated(author)) %>%
  select(author_flair_text) %>%
  pluck("author_flair_text") %>%
  length()

filter_by_anything("malfunction")

filter_by_anything("API")

filter_by_anything("hack")


summary(d)

d

top <- function(.data, n) { .data %>% top_n(n) %>% as.data.frame()  }

#%>% means "pipe"

head(head(d, 3),1)


head(d, 1)

d %>%
  head(3) %>%
  head(1)



cyborg <- d %>%
  filter(grepl("cyborg", body))

cyborg %>% 
  filter(!duplicated(author)) %>% 
  count(author_flair_text, sort=TRUE) %>% 
  filter(grepl("t2", author_flair_text, ignore.case = TRUE)) %>%
  mutate(t2_n = ifelse(grepl("t2", author_flair_text, ignore.case = TRUE), n, 0)) %>%
  summarize(t2s = sum(t2_n)) %>%
  
  d %>%
  count(author_flair_text, sort = T) %>%
  mutate(total = sum(n),
         share = n / total)

d2 <- read_csv("~/Downloads/reddit_diabetes.csv") %>%
  select(created_utc, author, author_flair_text) %>%
  mutate(author_flair_text = ifelse(grepl("T1 ", author_flair_text), "T1", author_flair_text)) %>%
  mutate(author_flair_text = ifelse(grepl("T2 ", author_flair_text), "T2", author_flair_text)) %>%
  mutate(author_flair_text = ifelse(grepl("T1, ", author_flair_text), "T1", author_flair_text)) %>%
  mutate(author_flair_text = ifelse(grepl("T2, ", author_flair_text), "T2", author_flair_text)) %>%
  mutate(author_flair_text = ifelse(grepl("T1.5 ", author_flair_text), "T1.5", author_flair_text)) %>%
  arrange(created_utc) %>%
  filter(!is.na(author_flair_text)) %>%
  filter(!duplicated(author)) %>%
  group_by(author_flair_text) %>%
  mutate(id = 1:n())

d2 %>%
  semi_join(
    d2 %>%
      ungroup %>%
      arrange(desc(id)) %>%
      filter(!duplicated(author_flair_text)) %>%
      select(author_flair_text) %>%
      head(10),
    by = "author_flair_text"
  ) %>%
  mutate(created_utc = as.POSIXct(created_utc, origin = "1970-01-01")) %>%
  ggplot(aes(x = created_utc, y = id)) +
  geom_step(aes(colour = factor(author_flair_text))) +
  scale_x_datetime()


ggplot(aes(x = created_utc, y = id, colour = factor(author_flair_text))) +
  geom_step(size = 2)



%>%
  count(author, diabetes_type_by_flair) %>%
  ungroup %>%
  filter(!is.na(diabetes_type_by_flair)) %>%
  group_by(author) %>%
  mutate(diabetes_type_id = 1:n()) %>%
  ungroup %>%
  filter(author == "anniesplay699")
arrange(desc(diabetes_type_id))

# What's the main order of diabetes flow based flair changes?

names(d)




names(d)

head(d, 1)

d








