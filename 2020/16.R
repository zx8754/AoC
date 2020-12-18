x <- readLines("16_data.txt")
#x <- readLines("16_sample.txt")

# part 1 ------------------------------------------------------------------
r <- unlist(strsplit(x[ grepl("-", x) ], " "))
r <- lapply(strsplit(r[ grepl("-", r) ], "-"), as.integer)

nt <- x[ (which(grepl("nearby", x)) + 1):length(x)]
nt <- lapply(strsplit(nt, ","), as.integer)

sum(unlist(
  lapply(nt, function(i){
    i[ !sapply(i, function(j){
      sum(sapply(r, function(k){
        any(j >= k[ 1 ] & j <= k[ 2 ])
      })) > 0
    }) ]
  })))
# [1] 20058

# part 2 ------------------------------------------------------------------
yt <- unlist(lapply(strsplit(x[ (which(grepl("your", x)) + 1) ], ","), as.integer))

r <- x[ 1:(which(grepl("your", x)) - 2) ]
r <- strsplit(r, ": ", fixed = TRUE)
names(r) <- make.names(sapply(r, "[[", 1))

r <- lapply(r, function(i){
  as.integer(unlist(strsplit(gsub(" or ", "-", i[2]), "-")))
})

#valid nt
ix <- sapply(nt, function(i){
  sum(sapply(i, function(j){
    sum(sapply(r, function(k){
      any((j >= k[ 1 ] & j <= k[ 2 ]) | 
            (j >= k[ 3 ] & j <= k[ 4 ]))
    })) > 0
  })) == lengths(nt)[ 1 ]
}) 
ntv <- t(unname(as.matrix(data.frame(nt[ ix ]))))

cols <- setNames(rep(NA_integer_, 20), names(r))
rs <- r
cntV = sum(ix)
ntvs = ntv
for(s in 1:20){
  #s=5
  rs <- rs[ names(rs) %in% names(cols)[ is.na(cols) ] ]
  res <- sapply(rs, function(i){
    mm <- which(colSums((ntvs >= i[1] & ntvs <= i[2]) | 
                          (ntvs >= i[3] & ntvs <= i[4])) == cntV)
    if(length(mm) == 1) mm else NA
  })
  cols[ names(res)[ !is.na(res) ] ] <- res[ !is.na(res) ]
  ntvs[, res[ !is.na(res)]] <- NA
}

print(prod(yt[ cols[ grep("departure", names(cols)) ] ]), digits = 22)
# [1] 366871907221
