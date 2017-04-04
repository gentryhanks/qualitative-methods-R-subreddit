library(dplyr)
library(readr)
library(ggplot2)

d <- read_csv("~/Downloads/reddit_diabetes.csv")




filter_by_anything <- function(pattern) {
  d %>%
    filter(grepl(pattern, body)) # %>%
#     filter(!duplicated(author)) %>% 
#     count(author_flair_text, sort=TRUE)
}
filter_by_anything("open source")
filter_by_anything("cyborg") %>%
  mutate(created_utc = as.POSIXct(created_utc, origin = "1970-01-01")) %>%
  select(created_utc, archived) %>%
  arrange(created_utc) %>%
  as.data.frame()

pluck <- function(.data, column) { .data[[column]] }

filter_by_anything("data") %>%
  filter(grepl("open source", body)) %>%
  pluck("body")
  
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








