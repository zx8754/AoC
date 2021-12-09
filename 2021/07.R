d <- scan("07_data.txt", sep = ",", what = integer())

#part 1 -----------------------------------------------------------------------
min(sapply(0:max(d), function(i) sum(abs(d - i))))
# [1] 355764

#part 2 -----------------------------------------------------------------------
min(
  sapply(0:max(d), function(i) {
    sum(sapply(abs(d - i), function(j) sum(seq(length.out = j))))
  })
  )
# [1] 99634572
