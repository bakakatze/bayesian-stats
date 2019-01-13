
require(rgl) # package to draw 3d function

## Intro to drawing 3D plot
f = function(x,y){
  r = x^2 + ((y^2)/36)
}

open3d()

plot3d(f, xlim = c(-100,100), ylim = c(-100,100), alpha = 0.8,
       xlab = "X", ylab = "Y", zlab = "f(x,y)")


# Learn about Jeffrey's Prior (uninformative prior) here:
# https://www.youtube.com/watch?v=S42N_6pQ5TA

# chapter 2