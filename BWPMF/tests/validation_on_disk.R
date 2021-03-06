library(BWPMF)
src.path <- system.file("2015-10-01-100.txt", package = "BWPMF")
encode(src.path)
history <- encode_data(src.path)
count_cookie()
count_hostname()
history_size <- check_history(history)
stopifnot(history_size == 3970)
history_non_zero_size <- count_non_zero_of_history(history)
stopifnot(history_non_zero_size == 885)
testing_id <- c(154, 397, 513, 818, 273, 3, 862, 635)
testing_history <- extract_history(training_history <- history, testing_id)

stopifnot(check_history(training_history) + check_history(testing_history) == history_size)
stopifnot(count_non_zero_of_history(training_history) + count_non_zero_of_history(testing_history) == history_non_zero_size)


m <- init_model(.1, .1, .1, .1, .1, .1, 10, training_history)

phi <- init_phi(m, training_history, tempfile())
print(attributes(phi))
# print_phi_index(phi)
# print_history_index(training_history)
# test_phi(phi, training_history)
n <- 1000
pmf <- list(
  time = numeric(n),
  training_logloss = numeric(n),
  testing_logloss = numeric(n)#,
#   training_mae = numeric(n),
  # testing_mae = numeric(n)
)
print_history(history)
if (interactive()) pb <- txtProgressBar(max = n, style = 3)
for(i in 1:n) {
  if (interactive()) setTxtProgressBar(pb, i)
  pmf$time[i] <- system.time(train_once(m, training_history, phi, function(msg) {
    # cat(msg);cat("\n")
  }))[3]
#   cat(sprintf("training logloss: %f mae: %f testing logloss: %f mae: %f\n", 
              pmf$training_logloss[i] <- pmf_logloss(m, training_history)#, 
              # pmf$training_mae[i] <- pmf_mae(m, training_history)#,
              pmf$testing_logloss[i] <- pmf_logloss(m, testing_history)#, 
              # pmf$testing_mae[i] <- pmf_mae(m, testing_history)#))
}
if (interactive()) close(pb)
stopifnot(diff(tail(pmf$training_logloss, 800)) < 1e-5)
for(i in (4:8 * 100)) stopifnot(sum(diff(tail(pmf$training_logloss, i))) < 0)
head(pmf$training_logloss)
pmf$training_logloss
