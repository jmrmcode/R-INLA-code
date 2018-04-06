m3 <- inla.mesh.2d(loc = coordsTra.df, max.edge = c(500000, 5000000),cutoff=50000)
# SPDE model
spde_m3 <- inla.spde2.matern(m3, alpha = 2)
# to assist with keeping track of which elements relate to what effects, I can create an index
s.index_m3 <- inla.spde.make.index("spatial.field", n.spde = spde_m3$n.spde)
# to create a projector matrix (links the m mesh vertices to the n responses)
A_m3 <- inla.spde.make.A(m3, loc = coordsTra)

# to create the stack
stackm3 <- inla.stack(data = list(y = trainingData$PA), A = list(A_m3, 1), effects = list(s.index_m3,
list(b0 = rep(1, nrow(coordsTra.df)),data.frame(NOF=trainingData$NOF), data.frame(grid=trainingData$grid), data.frame(CWD=trainingData$CWD),
data.frame(MAT=trainingData$MAT),data.frame(slope=trainingData$slope),data.frame(PRECSEAS=trainingData$PRECSEAS),data.frame(TEMPSEAS=trainingData$TEMPSEAS)
,data.frame(AET=trainingData$AET),data.frame(PREC=trainingData$PREC))), tag = "prueba1")

prueba2 <- inla(y ~ 0 + b0 + NOF*PREC + slope + I(NOF^2) + I(PREC^2) + PREC + TEMPSEAS + f(spatial.field, model = spde_m3)
, data = inla.stack.data(stackm3),
control.predictor = list(A = inla.stack.A(stackm3),compute=TRUE),
control.fixed = list(mean.intercept=0.5),family = "logistic",verbose = T)

str(prueba2$marginals.fixed)[[1]]
