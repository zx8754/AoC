x <- readLines("04_data.txt")

# part1
sum(
  sapply(split(x, cumsum(x == "")), function(i){
    d <- unlist(strsplit(trimws(paste(i, collapse = " ")), "[ :]"))
    all(c("byr","iyr","eyr","hgt","hcl","ecl","pid") %in% d)
  }))
#[1] 250

# part2
sum(
  sapply(split(x, cumsum(x == "")), function(i){
    d <- unlist(strsplit(trimws(paste(i, collapse = " ")), "[ :]"))
    
    s1 <- as.integer(d[ which(d == "byr") + 1 ]) 
    byr <- s1 >= 1920 & s1 <= 2002
    
    s2 <- as.integer(d[ which(d == "iyr") + 1 ]) 
    iyr <- s2 >= 2010 & s2 <= 2020
    
    s3 <- as.integer(d[ which(d == "eyr") + 1 ])
    eyr <- s3 >= 2020 & s3 <= 2030
    
    s4 <- d[ which(d == "hgt") + 1 ]
    if(length(s4) == 0){ hgt <- NA
    } else if(grepl("*cm$", s4)){ 
      ss <- as.numeric(gsub("cm", "", s4, fixed = TRUE))
      hgt <- ss >= 150 & ss <= 193
    } else if(grepl("*in$", s4)){
      ss <- as.numeric(gsub("in", "", s4, fixed = TRUE))
      hgt <- ss >= 59 & ss <= 76
    } else {hgt <- NA}
    
    s5 <- d[ which(d == "hcl") + 1 ]
    hcl <- grepl("^#[0-9a-f]{6}$", s5)
    
    s6 <- d[ which(d == "ecl") + 1 ]
    ecl <- s6 %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
    
    s7 <- d[ which(d == "pid") + 1 ]
    pid <- grepl("^[0-9]{9}$", s7)
    
    sum(c(byr, iyr, eyr, hgt, hcl, ecl, pid), na.rm = TRUE) >=7
  })
)
#[1] 158