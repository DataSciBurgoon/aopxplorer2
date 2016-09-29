## ------------------------------------------------------------------------
library(aopxplorer)
x <- get_aopn("Steatosis")
plot(x)

## ------------------------------------------------------------------------
list_adverse_outcomes()

## ------------------------------------------------------------------------
x <- get_aopn("Steatosis")
V(x)$name
your_data <- list()
your_data[[1]] <- c(1,1,1,1)
your_data[[2]] <- c(11, 12, 13, 14)
your_data[[3]] <- c(15.4, 1.1, 17.2, 4.1)
your_data[[4]] <- c(NA, NA, NA, NA)
your_data[[5]] <- c(NA, NA, NA, NA)
your_data[[6]] <- c(NA, NA, NA, NA)
your_data[[7]] <- c(NA, NA, NA, NA)
your_data[[8]] <- c(NA, NA, NA, NA)
your_data[[9]] <- c(NA, NA, NA, NA)
your_data[[10]] <- c(NA, NA, NA, NA)
your_data[[11]] <- c(NA, NA, NA, NA)
data_steatosis_net <- associate_data_aopn(your_data, x)
cytoscape_aopn <- toCytoscape(data_steatosis_net)
send_aopn(cytoscape_aopn)

## ------------------------------------------------------------------------
mat_dat <- matrix(runif(60), nrow=15, ncol=4, byrow=TRUE, 
                  dimnames = list(c("PPARalpha", "HSD17B4", "blahx", "FXR", 
                                    "steatosis", "blahy", "SREBP1", "Rheb1",
                                    "mTORC1", "IRS1", "Tsc1", "GSK3",
                                    "blahq", "blahs", "Akt2"), c("C1", "C2", "C3", "C4")))

x <- get_aopn("Steatosis")
V(x)$name
mat_names_match <- rownames(mat_dat)[which(rownames(mat_dat) %in% V(x)$name)]
mat_dat_pared <- mat_dat[mat_names_match, ]
mat_dat_pared_rearranged <- mat_dat_pared[match(V(x)$name, mat_names_match),]

mat_dat_pared_list <- setNames(split(mat_dat_pared_rearranged, 
                                     seq(nrow(mat_dat_pared_rearranged))), 
                               rownames(mat_dat_pared_rearranged))


data_steatosis_net <- associate_data_aopn(mat_dat_pared_list, x)
cytoscape_aopn <- toCytoscape(data_steatosis_net)
send_aopn(cytoscape_aopn)

