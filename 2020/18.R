x <- readLines("18_data.txt")
#x <- readLines("18_sample.txt")

evil <- function(s, part = 1){ 
  if(part == 1){
    nD <- sum(grepl("[0-9]", unlist(strsplit(s, " "))))
    sB <- paste(paste(rep("(", nD), collapse = ""),
                gsub("([0-9]+)", "\\1)", s), collapse = "")  
  } else if(part == 2){
    
    if(!grepl(" * ", s, fixed = TRUE)){
      sB <- s
    } else { 
      sB = paste0("(", gsub(" * ", ") * (", s, fixed = TRUE), ")")
    }
  }
  eval(parse(text = sB))  
}

calc <- function(s, part = 1){
  ss <- gsub("(", "( ", s, fixed = TRUE)
  ss <- gsub(")", " )", ss, fixed = TRUE)
  ss <- unlist(strsplit(ss, " "))
  
  if(sum(ss == "(") > 0){ 
    nCol <- sum(ss != " ")
    nRow <- sum(ss %in% c("(", ")", "+", "*"))
    m <- matrix("", nrow = nRow, ncol = nCol)
    
    r = 1; c = 1
    for(i in seq(ss)){
      #i=1
      if(i == 1){ #nothing
      } else if(ss[ i ] == "("){
        r <- r + 1
      } else if(ss[ i - 1] == ")") { r <- r + 1 }
      m[r, c] <- ss[ i ]
      c <- c + 1
    }#m
    
    out <- c(apply(m, 1, function(j){
      if(sum(grepl("[()]", j)) == 2){
        res <- rep("", length(j))
        res[ which(grepl("[(]", j)) ] <- evil(paste(j[ !j %in% c("", "(", ")")], 
                                                    collapse = " "), part = part)
        res
      } else {j}
    }))
    
    out <- paste(out[ out != ""], collapse = " ")
    calc(out, part = part) 
  } else {
    evil(s, part = part)
  }
}

# part 1  -----------------------------------------------------------------
print(sum(sapply(x, calc, part = 1)), digits = 22)
#[1] 3885386961962

# part 2 ------------------------------------------------------------------
print(sum(sapply(x, calc, part = 2)), digits = 22)
#[1] 112899558798666
