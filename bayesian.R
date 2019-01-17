
require(rgl) # package to draw 3d function

## Intro to drawing 3D plot
f = function(x,y){
  r = x^2 + ((y^2)/36)
}

open3d()

plot3d(f, xlim = c(-100,100), ylim = c(-100,100), alpha = 0.8,
       xlab = "X", ylab = "Y", zlab = "f(x,y)")


# page 27 -- 4.2 Prediction in the Bus Problem
