# target area: x=25..67, y=-260..-200
target <- setNames(c(25, 67, -260, -200), c("x1", "x2", "y1", "y2"))

#target area: x=20..30, y=-10..-5
target <- setNames(c(20, 30, -10, -5), c("x1", "x2", "y1", "y2"))

#part 1 -----------------------------------------------------------------------
# The probe's x position increases by its x velocity.
# The probe's y position increases by its y velocity.
# Due to drag, the probe's x velocity changes by 1 toward the value 0; that is,
#   it decreases by 1 if it is greater than 0, increases by 1 if it is 
#   less than 0, or does not change if it is already 0.
# Due to gravity, the probe's y velocity decreases by 1.

#part 2 -----------------------------------------------------------------------
S = c(0, 0)
# example velocity x=7, y=2
