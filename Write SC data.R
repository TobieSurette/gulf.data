library(gulf.utils)

load("~/Desktop/gulf.data/SCSET.rdata")
load("~/Desktop/gulf.data/SCBIO.rdata")

years <- sort(unique(b$year))

b$tag.number[b$tag.number == "00000000"] <- ""

tmp <- aggregate(b["samplers"], 
                 by = b[c("year", "month", "day", "tow.id")], 
                 function(x) paste(unique(x), collapse = "; "))

x <- x[, -which(names(x) %in% c("x", "y"))]
x$project <- "scs"
x <- x[c("project", setdiff(names(x), "project"))]   
key(x) <- c("year", "month", "day", "tow.id")
x$samplers <- tmp$samplers[match(x[key(x)], tmp[key(x)])]

b$project <- "scs"
vars <- c("project", "day", "month", "year", "tow.id", "species", "zone")
b <- b[c(vars, setdiff(names(b), c(vars, "samplers")))]
   
vars <- c("data.type", "samplers", "subzone", "trap.code", "position.type", "latitude.start", "longitude.start", "depth", "soak.days")

b <- b[, -which(names(b) %in% vars)]

x$comment[x$comment == "*"] <- ""
b$comments[b$comments == "*"] <- ""
b$comment <- b$comments 
b <- b[, -which(names(b) %in% c("maturity", "comments"))]

for (i in 1:length(years)){
   write.table(b[b$year == years[i], ], file = paste0("inst/extdata/scs.bio.", years[i], ".csv"), row.names = FALSE, na = "", sep= ",")
   write.table(x[x$year == years[i], ], file = paste0("inst/extdata/scs.set.", years[i], ".csv"), row.names = FALSE, na = "", sep= ",")
}
   