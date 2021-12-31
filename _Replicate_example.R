## --------------------------
### REPLICATE ARTICLE EXAMPLE
## --------------------------

## specify class-conditional probabilities in poLCA format
## ncol = N of responses (e.g., 0,1)
## nrow = nClasses
prbs <- list(matrix(c(0.3,0.7,
                      0.6, 0.4),ncol=2,byrow=TRUE), # P(L=1|class)
             matrix(c(0.2,0.8,
                      0.9, 0.1),ncol=2,byrow=TRUE), # P(L=2|class)
             matrix(c(0.1,0.9,
                      0.6, 0.4),ncol=2,byrow=TRUE)) # P(L=3|class)

## Convert the class condition probability matrices (prbs)
## from poLCA format to the format provided in van der Ark and Smits article.
pA <- conPr(pR = prbs) 
L <- 3 # number of items
R <- Response.pat(L = L, isc = 1:2) # for item scores 1:2 
Pw <- c(.2, .8)   # Class weights 

#compute the density of the item-score vectors (p)
pis <- pv(Pw = Pw, L = L, R = R, prbs = pA, print.patterns = FALSE)  
