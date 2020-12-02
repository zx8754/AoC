x <- readLines("02_data.txt")

# part 1
sum(
  sapply(strsplit(x, "[-: ]"), function(i){
    cnt <- nchar(i[ 5 ]) - nchar(gsub(i[ 3 ], "", i[ 5 ]))
    cnt >= as.integer(i[ 1 ]) & cnt <= as.integer(i[ 2 ])
    })
  )
#[1] 569

# part 2
sum(
  sapply(strsplit(x, "[-: ]"), function(i){
    ix <- as.integer(i[1:2])
    sum(substring(i[ 5 ], ix, ix) == i[ 3 ]) == 1
    })
  )
#[1] 346
