library(sp)http://127.0.0.1:20839/graphics/plot_zoom_png?width=531&height=809
data("meuse")
class(meuse)

names(meuse)

coordinates(meuse) <- ~x + y
class(meuse)

summary(meuse)
coordinates(meuse)[1:5,]

bubble(meuse, "zinc",main = "Zinc Concentration")

data("meuse.grid")
summary(meuse.grid)

class(meuse.grid)

coordinates(meuse.grid) <- ~x+y
class(meuse.grid)

gridded(meuse.grid) <- TRUE
class(meuse.grid)

image(meuse.grid["dist"])
title("Distance To river (red = 0")
library(gstat)
# log(zinc)~1 means that we assume a constant trend for the variable log(zinc)
zinc.idw <- idw(zinc~1 , meuse, meuse.grid)
class(zinc.idw)
spplot(zinc.idw["var1.pred"],main = "Zinc IDW Interpolations")

plot(log(zinc) ~ sqrt(dist), meuse)
abline(lm(log(zinc) ~ sqrt(dist),meuse))

lzn.vgm <- variogram(log(zinc) ~ 1, meuse)
lzn.vgm

# the partial sill, nugget and model type of the model are equal to those of the omnidirectional model fitted above; 
#the range is that in the direction with the largest range (45o )
# the anisotropy ratio, the range in the 135 direction and the range in the 45 direction, estimated “by eye” by comparing the 45 and 135 degrees sample variograms.

lzn.fit <- fit.variogram(lzn.vgm, model = vgm(1,"Sph",900,1))       
lzn.fit
plot(lzn.vgm,lzn.fit)

lznr.vgm <- variogram(log(zinc)~sqrt(zinc),meuse)
lznr.fit <- fit.variogram(lznr.vgm, model = vgm(1,"Exp",300,1))
lznr.fit
plot(lznr.vgm,lznr.fit)

## Kriging
lzn.krined <- krige(log(zinc) ~ 1 , meuse , meuse.grid, model = lzn.fit)
spplot(lzn.krined["var1.pred"])

## Condition Simulation
lzn.condsim <- krige(log(zinc) ~ 1,meuse,meuse.grid,model = lzn.fit, nmax =30, nsim =4)
spplot(lzn.condsim, main = "Four Condition Simulations")

lzn.condism2 <- krige(log(zinc)~sqrt(dist) , meuse, meuse.grid,model = lznr.fit,nmax = 30,nsim = 4)
spplot(lzn.condism2, main = "Four UK conditional simulations")

## Anisotropic variograms/ Directional Variograms

lzn.dir = variogram(log(zinc)~1, meuse, alpha = c(0,45,90,135))
lzndir.fit = vgm(0.59, "Sph", 1200, 0.05, anis = c(45, 0.25))  # Changed anis ratio from 4 to 0.25
plot(lznr.dir, lzndir.fit, as.table = TRUE)

lznr.dir = variogram(log(zinc)~sqrt(dist),meuse,alpha = c(0,45,90,135))
plot(lznr.dir,lzn.fit, as.table = TRUE)


## Variogram maps

## The threshold assures that only semivariogram map values based on at least 5 point pairs are shown,
# removing too noisy estimation.

vgm.map <- variogram(log(zinc)~sqrt(zinc),meuse,cutoff = 2000, width = 100,map = TRUE)
plot(vgm.map, threshold = 5)

## Cross variography

g = gstat(NULL,"log(zn)",log(zinc)~sqrt(dist),meuse)
g = gstat(g,"log(cd)",log(cadmium)~sqrt(dist),meuse)
g = gstat(g,"log(pb)",log(lead)~sqrt(dist),meuse)
g = gstat(g,"log(cu)",log(copper)~sqrt(dist),meuse)
v = variogram(g)
g = gstat(g,model = vgm(1,"Exp",300,1), fill.all = TRUE)
g.fit = fit.lmc(v,g)
g.fit

plot(v,g.fit)
vgm.map = variogram(g, cutoff =1500 ,width = 100, map = TRUE)
plot(vgm.map, threshold = 5, col.regions = bpy.colors(), xlab = "", ylab = "")
