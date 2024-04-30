#!/usr/bin/env Rscript
# Author: Katie Bickerton kbickerton@glos.ac.uk
# Script: MX_skomer_cjs.R
# Desc: 
# Arguments: 
# Date: March 2024

rm(list = ls())
graphics.off()


require(tidyverse)
require(RMark)
require(R2ucare)



mx.sk <- read.csv("../Data/MX_skomer_1999_2022_ch.csv", skip = 1, header = TRUE)

# remove individuals ringed but never observed breeding
mx.sk <- mx.sk[mx.sk$n.encs != 0,]
# format data into capture histories
mx.ch <- mx.sk[,4:50]
mx.ch <- unite(mx.ch, "ch", 2:47, remove = FALSE, sep = "")

mx.ch1 <- data.frame("ch" = mx.ch$ch)



# process data for RMark
mx.proc <- process.data(mx.ch1, model = "CJS", time.intervals = rep(1, 45))
mx.ddl <- make.design.data(mx.proc)

# specify models to run
Phi.1 <- list(formula=~1)
Phi.t <- list(formula=~time)

p.1 <- list(formula=~1)
p.t <- list(formula=~time)

# create model list
mx.cml <- create.model.list("CJS")
# run model
mx.fits <- mark.wrapper(model.list = mx.cml, data = mx.proc, ddl = mx.ddl, run = TRUE, silent = TRUE)

# model comparison table
mx.fits$model.table

# calculate survival and capture probabilities
mx.sk.phi <- get.real(mx.fits[[4]], "Phi", se = TRUE)
mx.sk.phi1 <- select(mx.sk.phi, time, estimate, se, lcl, ucl) %>% unique()

mx.sk.p <- get.real(mx.fits[[4]], "p", se = TRUE)
mx.sk.p1 <- select(mx.sk.p, time, estimate, se, lcl, ucl) %>% unique()

mx.sk.pars <- rbind(mx.sk.phi1, mx.sk.p1)
mx.sk.pars$parameter <- c(rep("Phi",45), rep("p", 45))

# goodness of fit tests

mx.test <- as.data.frame(table(mx.ch1))


mx.hist <- matrix(as.numeric(unlist(strsplit(as.character(mx.test$mx.ch1),""))),
                  nrow = length(mx.test$mx.ch1),byrow=T)
mx.freq <- mx.test$Freq


mx.3sr <- test3sr(mx.hist, mx.freq)
mx.3sm <- test3sm(mx.hist, mx.freq)
mx.2ct <- test2ct(mx.hist, mx.freq)
mx.2cl <- test2cl(mx.hist, mx.freq)
mx.all <- overall_CJS(mx.hist, mx.freq)



