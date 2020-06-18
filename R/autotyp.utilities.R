library(maps)
library(maptools)
library(mapproj)

# register <- read.csv('data/Register.csv', na.strings='')
#
# alignment <- read.csv('data/Alignment_per_language.csv', na.strings="")
#
# synthesis <- read.csv('data/Synthesis.csv', na.strings="")


###############################################################################
# A convenience vector listing the taxonomic levels recognized in the register:
###############################################################################

autotyp.taxa <- c('Stock', 'MajorBranch', 'SubBranch', 'SubSubBranch', 'LowestSubBranch', 'Language')


#############################################################################################################
# A function for quickly adding the register information to a data table, 
# assuming it was read in and named `register`; if not, specify the name:
##############################################################################################################

add.register <- function(df, gg=register) merge(df, gg, by ='LID')


# alignment.g <- add.register(alignment)
# synthesis.g <- add.register(synthesis)

##############################################################################################################
# Function for plotting maps, assuming the data contain the register and are available
# 
# Usage:
# 
# for one language or a set of languages in the same color:
# 
# > autotyp.map(subset(register, grepl('Chintang', Language) | grepl('Mandarin', Language)), color='red')
# 
# or for a set of languages differentially colored according to the values of some variable:
# 
# > autotyp.map(add.register(alignment), variable = 'AlignmentCaseErgDegree.binned3', color = c(low='white', medium='orange', high='red'))
#
# This will print red latest, i.e. above the others when there languages at (nearly) the same place. To reverse this order, write c(high='red', medium='orange', low='white')
#
# Note: if a name contains spaces and other special characters, use backticks, e.g. `A ≺ P`='blue'
#
# The color vector accepts any named vector, e.g. for synthesis one could do
# > synthesis.g <- add.register(synthesis)
# > color <- rainbow(length(unique(synthesis.g$VInflMax.n)),end=.00, start=.70)
# > names(color) <- sort(unique(synthesis.g$VInflMax.n))
# > autotyp.map(synthesis.g, variable = 'VInflMax.n', color = color)
# 
# which prints synthesis degrees on a scale from blue (.70) to red (.00)
# 
# Adding a legend:
# > colors <- c(low='yellow', medium='orange', high='red')
# > autotyp.map(add.register(alignment), variable = 'AlignmentCaseErgDegree.binned3', color = colors)
# > legend('bottom', ncol=3, legend=names(colors), pch=21, pt.bg=colors, pt.cex=1.5, bty='n')
##############################################################################################################

autotyp.map <- function (data, variable, color='red', map.line.col='darkgrey', cex = 0.8, lwd = .5, pch=21, pch.variable, border='black', close.up='all', xlim=NULL, ylim=NULL, orientation=c(90,150,0), labels=NULL, label.pos=4, label.offset=.2, label.cex=.5, label.font=1, label.col='black', label.crt=0, country.borders=F, margins=c(0,0,0,0), 
projection='cylequalarea', parameters=0, 
...) {
        if (grepl('pap|png|guin', close.up, ignore.case=T)) {xlim = c(132, 180); ylim = c(-10, 5)} 
        if (grepl('cauc', close.up, ignore.case=T)) {xlim = c(35, 70); ylim = c(35, 50)} else {xlim=NULL; ylim=NULL}
    
    if (any(grepl('tbl', class(data)))) {data <- as.data.frame(data)}        
                        
     if (missing(variable)) {
        data[, "col"] <- color
        par(mar=margins)
        map("world", projection = projection, param=parameters, wrap=T, orientation=orientation,
             interior = country.borders, xlim=xlim, ylim=ylim, col=map.line.col)
            
        points(mapproject(x = data[, "Longitude"], y = data[, "Latitude"]), cex = cex, pch = pch, lwd = lwd, col=border, bg = as.character(data[, 
            "col"]))
           if (!is.null(labels)) {text(mapproject(x = data[, "Longitude"], y = data[, "Latitude"]), labels=data[,labels], cex=label.cex, font=label.font, pos=label.pos, offset=label.offset, col=label.col, crt=label.crt)}
    }
    else {  
            if(length(color)==1) {stop('Color should be a vector of named elements, such as c(value1="blue", value2="red")')}
            else {
                 data <- data[!is.na(data[,variable]),]   
                 data <- do.call(rbind, by(data, data[,variable], function(i) {
                         i[,'col'] <- color[names(color) %in% i[,variable]]
                         i[,'requested.order'] <- which(names(color) %in% i[,variable])
                         return(i)
                         }))
                 data <- data[order(data[, 'requested.order']),]
                 par(mar=margins)
                 map("world", projection = projection, param=parameters, orientation=c(90,150,0), wrap=T,
                        interior = country.borders, xlim=xlim, ylim=ylim, col=map.line.col)
                if(missing(pch.variable)) {data[,'plot.symbol'] <- pch} else {data[,'plot.symbol'] <- data[,pch.variable]}
                        if(any(grepl('[0-9]',data[,'plot.symbol']))) {data[,'plot.symbol'] <- as.numeric(data[,'plot.symbol'])} # correct reading of what's given in the pch variable 
                        points(mapproject(x = data[, "Longitude"], y = data[, "Latitude"]), cex = cex, pch = data[,'plot.symbol'], lwd = lwd, col=border,
                        bg = as.character(data[, "col"]))
                   if (!is.null(labels)) {text(mapproject(x = data[, "Longitude"], y = data[, "Latitude"]), labels=data[,labels], cex=label.cex, font=label.font, pos=label.pos, offset=label.offset, col=label.col, crt=label.crt)}
        }
        }
}        