library(methods)
library(dplyr)
src <- src_sqlite(path = "iPinYou/ipinyou.db")
user_history_2nd <- tbl(src, "user_history_2nd")
get_numeric <- function(x) {
  class(x) <- "numeric"
  x
}
# threshold.value <- get_numeric(ISOdatetime(2013, 6, 6, 0, 1, 1))
threshold.value <- get_numeric(ISOdatetime(2013, 6, 12, 0, 0, 0))
df.train <- filter(user_history_2nd, Timestamp < threshold.value, iPinyouID != "null") %>%
  select(iPinyouID, Domain) %>%
  collect()

df.test <- filter(user_history_2nd, Timestamp >= threshold.value, iPinyouID != "null") %>%
  select(iPinyouID, Domain) %>%
  collect()

sprintf("nrow(df.train): %d nrow(df.test): %d\n", 
        nrow(df.train), nrow(df.test)) %>%
  cat

# df.test <- filter(user_history_2nd, Timestamp >= threshold.value, iPinyouID != "null")
library(BWPMF)
encode(df.train)
history.train <- encode_data(df.train)
sprintf("training history: user: %d item: %d\n", 
        count_cookie_history(history.train),
        count_hostname_history(history.train)) %>%
  cat

history.test <- encode_data(df.test)
sprintf("testing history: user: %d item: %d\n", 
        count_cookie_history(history.test),
        count_hostname_history(history.test)) %>%
  cat
serialize_cookie("cookie1.bin")
serialize_hostname("hostname1.bin")
serialize_history(history.train, "history1.train.bin")
serialize_history(history.test, "history1.test.bin")

