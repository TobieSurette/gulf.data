#' Field Samplers
#' 
#' @name sampler
#' 
#' @description These are functions to clean up or standardize the names of field samplers from 
#'              various projects, such as research projects or sea-sampling.
#'              
#' @param x Object containing field sampler information.
#' @param project Research project (see \code{\link{project}}).            
#' @param ... Unused.  
#' 
#' @section Functions and Methods:
#' \describe{
#'     \item{\code{sampler}}{Generic \code{sampler} method.}
#'     \item{\code{sampler.default}}{Default sampler method.}
#'     \item{\code{sampler.scs}}{Field sampler clean-up function for snow crab survey.}
#'     \item{\code{sampler.sco}}{Field sampler clean-up function for snow crab at-sea and port 
#'                               observer data.}
#' } 
#' 

#' @rdname sampler
#' @export sampler
sampler <- function(x, ...) UseMethod("sampler")

#' @rdname sampler
#' @export
sampler.default <- function(x, project, ...){
   # Determine research project:
   if (missing(project)) project <- gulf.metadata::project(x)
   project <- gulf.metadata::project(project)
   if (is.null(project)) stop("Unable to determine research project identifier.")
   
   # Extract sampler information:
   names(x) <- gsub("samplers", "sampler", tolower(names(x)))
   names(x) <- gsub("observers", "observer", tolower(names(x)))
   if ("sampler" %in% names(x)) x <- x$sampler
   if ("observer" %in% names(x)) x <- x$observer
   x <- as.character(x)
   
   # Apply proper function by reasearch project:
   if (project == "scs") return(sampler.scs(x))
   if (project == "sco") return(sampler.sco(x))
   
   return(NULL)
}

#' @rdname sampler
#' @export sampler.scs
sampler.scs <- function(x){
   v <- toupper(x)
   v <- gsub("?", "E", v, fixed = TRUE)
   v <- gsub("^ ", "", v)
   v <- gsub(" $", "", v)
   v <- gsub(" +", " ", v)
   v <- gsub("&", "/", v, fixed = TRUE)
   v <- gsub("\\ ", " / ", v, fixed = TRUE)
   v <- gsub("\\", " / ", v, fixed = TRUE)
   v <- gsub(" / ", "/", v, fixed = TRUE)             
   v <- gsub("/ ", "/", v, fixed = TRUE)             
   v <- gsub(" /", "/", v, fixed = TRUE)             
   v <- gsub(" .", " ", v, fixed = TRUE)
   v <- gsub(". ", " ", v, fixed = TRUE)                   
   v <- gsub(" ", ".", v, fixed = TRUE) 
   
   # Marcel Hebert:
   v <- gsub("M.HEBERT", "MARCEL HEBERT", v, fixed = TRUE)
   v <- gsub("M.H.", "MARCEL HEBERT", v, fixed = TRUE)
   v[v == "MARCEL.HEBERT"] <- "MARCEL HEBERT"   
   v <- gsub("/HEBERT/", "/MARCEL HEBERT/", v, fixed = TRUE)
   
   # Claude Albert:
   v <- gsub("CLAU$", "CLAUDE ALBERT", v)
   v <- gsub("C.A.", "CLAUDE ALBERT", v, fixed = TRUE)    
   v <- gsub("C.ALBERT", "CLAUDE ALBERT", v, fixed = TRUE)
   v <- gsub("/ALBERT/", "/CLAUDE ALBERT/", v, fixed = TRUE)  
   v <- gsub("^C[/]", "CLAUDE ALBERT/", v, fixed = TRUE)
   
   # Stephane Albert:
   v <- gsub("STEPHANE/", "STEPHANE ALBERT/", v, fixed = TRUE) 
   v <- gsub("STEPHANE.A.", "STEPHANE ALBERT", v, fixed = TRUE)
   v <- gsub("S.ALBERT", "STEPHANE ALBERT", v, fixed = TRUE)
   v <- gsub("STEPHANE.A", "STEPHANE ALBERT", v, fixed = TRUE)
   v <- gsub("STEPHAUE", "STEPHANE ALBERT", v, fixed = TRUE)
   
   v[v == "C/STEPHANE ALBERT"] <- "CLAUDE ALBERT/STEPHANE ALBERT" 
   v[v == "CLAUDE/STEPHANE ALBERTLB"] <- "CLAUDE ALBERT/STEPHANE ALBERT" 
   
   # Alain Hebert:
   v <- gsub("A.HEBERT", "ALAIN HEBERT", v, fixed = TRUE)
   v <- gsub("ALLAIN.H.", "ALAIN HEBERT", v, fixed = TRUE)
   v <- gsub("AL.HEBERT", "ALAIN HEBERT", v, fixed = TRUE)
   
   # Bobby Campbell:
   v <- gsub("R.CAMPBELL", "ROBERT CAMPBELL", v, fixed = TRUE)
   v <- gsub("BOBBY.H.", "ROBERT CAMPBELL", v, fixed = TRUE)
   v <- gsub("B.CAMPBELL", "ROBERT CAMPBELL", v, fixed = TRUE)
   v <- gsub("B.CAMBELL", "ROBERT CAMPBELL", v, fixed = TRUE)
   v <- gsub("R.CAMBPELL", "ROBERT CAMPBELL", v, fixed = TRUE)
   v <- gsub("B.CAMBPELL", "ROBERT CAMPBELL", v, fixed = TRUE)
   v <- gsub("R.C.", "ROBERT CAMPBELL", v, fixed = TRUE)
   
   # Yvon Chiasson:
   v <- gsub("Y.C.", "YVON CHIASSON", v, fixed = TRUE)  
   v <- gsub("Y.CHIASSON", "YVON CHIASSON", v, fixed = TRUE)
   
   v <- gsub("FRANCOIS", "FRANCOIS PLANTE", v, fixed = TRUE)
   v <- gsub("/FRANCOI$", "FRANCOIS PLANTE", v, fixed = TRUE)
   v[v == "CLAUDE/PIERRE/FRANCOI"] <- "CLAUDE ALBERT/PIERRE DEGRACE/FRANCOIS PLANTE"                          
   
   # Pierre Degrace:
   v <- gsub("P.DEGRACE", "PIERRE DEGRACE", v, fixed = TRUE)
   v <- gsub("^PIERRE[/]", "PIERRE DEGRACE/", v)  
   v <- gsub("PIERRE.DEGRACE", "PIERRE DEGRACE/", v, fixed = TRUE) 
   v <- gsub("P.D.G.", "PIERRE DEGRACE", v, fixed = TRUE) 
   
   # Christine Sabean:
   v <- gsub("CHRISTINE", "CHRISTINE SABEAN", v)
   v <- gsub("/CHRISTI/", "/CHRISTINE SABEAN/", v, fixed = TRUE)
   
   # David Giard:
   v <- gsub("D.GIARD", "DAVID GIARD", v, fixed = TRUE)
   v <- gsub("/DAVE/", "/DAVID GIARD/", v, fixed = TRUE)
   v <- gsub("DAVE GIARD", "DAVID GIARD", v)
   v <- gsub("P[.]DAVID GIARD", "DAVID GIARD", v)
   v <- gsub("S.GIARD", "DAVID GIARD", v, fixed = TRUE)
   
   # Miscellaneous:
   v <- gsub("M.OUELLET", "MAXIME OUELLET", v, fixed = TRUE)
   v <- gsub("RITA", "RITA CORMIER", v)
   v <- gsub("^LUC", "LUC SAVOIE", v)    
   v <- gsub("P.M.", "PIERRE MALLET", v, fixed = TRUE)  
   v <- gsub("M-A.MCCAIE", "MARC-ANDRE MCCAIE", v, fixed = TRUE)
   v <- gsub("C.VITOUX", "CHRISTOPHE VITOUX", v, fixed = TRUE)
   v <- gsub("S.CHIASSON", "STEPHANE CHIASSON", v, fixed = TRUE)
   
   v <- gsub("T.TURBIDE", "TOMMY TURBIDE", v, fixed = TRUE)
   v <- gsub("D.LAPIERRE", "DANNY LAPIERRE", v, fixed = TRUE)
   v <- gsub("J.BOURGEOIS", "JULES BOURGEOIS", v, fixed = TRUE)
   
   v <- gsub("A.DUGUAY", "ALBAN DUGUAY", v, fixed = TRUE)  
   v <- gsub("AD$", "ALBAN DUGUAY", v)  
   v <- gsub("D.G.", "PIERRE DEGRACE", v, fixed = TRUE) 
   v <- gsub("A.MALLET", "ALAIN MALLET", v, fixed = TRUE) 
   v <- gsub("J.CHIASSON", "JULES CHIASSON", v, fixed = TRUE) 
   v <- gsub("BOBBY.H", "BOBBY HACHE", v, fixed = TRUE)      
   v <- gsub("F.PAULIN", "FRANCOIS PAULIN", v, fixed = TRUE)  
   
   v[v == "*"] <- ""
   v <- gsub("[.]", " ", v)
   v <- gsub("[;]", ",", v)
   
   # Ad hoc corrections:
   v <- gsub("ALBERT, GIONET, D.GIONET", "ALBERT, DANIEL GIONET", v, fixed = TRUE)
   v <- gsub("ALBERT, GIONET", "ALBERT, DANIEL GIONET", v, fixed = TRUE)
   v <- gsub("S.HEBERT, B.HEBERT", "STEPHANE ALBERT, BOBBY HACHE", v, fixed = TRUE) 
   v <- gsub("GIONET, D.GIONET,", "DANIEL GIONET,", v, fixed = TRUE)
   
   # Re-order samplers:
   v <- unlist(lapply(lapply(strsplit(v, ", "), sort), paste, collapse = ", "))

   return(v)
}  

#' @rdname sampler
#' @export sampler.sco
sampler.sco <- function(x){
   x <- gsub("?", "E", x)
   x <- gsub("?", "E", x)
   x <- gsub("?", "E", x)
   x <- gsub("?", "C", x)
   x <- gsub("[,']", " ", x)
   x <- as.character(x)
   x <- gsub("(^[ ]+)|([ ]+$)", "", x)
   x <- gsub(" - ", "-", x)
   
   # Observer name corrections:
   x <- gsub("AALEXANDRE KENNY", "ALEXANDRE KENNY", x)
   x <- gsub("ANDRE LAROCQUE", "AUDRE LAROCQUE", x)
   x <- gsub("NATASHA CAISSIE", "NATACHA CAISSIE", x)
   
   x <- gsub("JOSIANNE ARESNEAU", "JOSIANNE ARSENEAU", x) 
   x <- gsub("JOSLANE ARSENEAU", "JOSIANNE ARSENEAU", x) 
   x <- gsub("JOSIANE ARSENEAU", "JOSIANNE ARSENEAU", x) 
   
   x <- gsub("SABRINA AUCET", "SABRINA AUDET", x) 
   
   x <- gsub("ABOELLAH BACHA", "ABDELLAH BACHA", x) 
   x <- gsub("SEBASTION BEAUCHAMP", "SEBASTIEN BEAUCHAMP", x)  
   
   x <- gsub("REMI P BEAUCHEMIN", "REMI P. BEAUCHEMIN", x)  
   x <- gsub("REMI P.BEAUCHEMIN", "REMI P. BEAUCHEMIN", x)  
   
   x <- gsub("STEVE BENAIT", "STEVE BENOIT", x)
   x <- gsub("BENOIT STEVE", "STEVE BENOIT", x)
   x <- gsub("BENAIT, STEVE", "STEVE BENOIT", x)
   
   x <- gsub("BENOIT SUSIE", "SUSIE BENOIT", x)
   x <- gsub("BENOIT, SUSIE", "SUSIE BENOIT", x)
   
   x <- gsub("SUZIE OKIZOLE-BERNIER", "SUZIE OLAIZOLA BERNIER", x)
   x <- gsub("SUZIE OLAIZDA BERNIER", "SUZIE OLAIZOLA BERNIER", x)
   x <- gsub("SUZIE OLAIZOLA-BERNIER", "SUZIE OLAIZOLA BERNIER", x)
   x <- gsub("SUZIE OLAIZOLA-REINIER", "SUZIE OLAIZOLA BERNIER", x)   
   
   x <- gsub("SEAN-PHILIPPE BERTIN", "JEAN-PHILIPPE BERTIN", x)
   x <- gsub("JEAN PHILIPPE BERTIN", "JEAN-PHILIPPE BERTIN", x)
   
   x <- gsub("BRANCH BRIAN", "BRIAN BRANCH", x)
   
   x <- gsub("MARIE MICHELE BOURASSA", "MARIE-MICHELE BOURASSA", x)
   
   x <- gsub("ANTHONY GALLIER" , "ANTHONY CALLIER", x)
   x <- gsub("ANTHONY GALLIN", "ANTHONY CALLIER", x)
   x <- gsub("ANTHONY GALLUM", "ANTHONY CALLIER", x)  
   x <- gsub("ANTHONY GALLIEN", "ANTHONY CALLIER", x)
   x <- gsub("ANTHONY CALLIN", "ANTHONY CALLIER", x)
   
   x <- gsub("VANESSA CARYDRAS", "VANESSA CARYDIAS", x)
   
   x <- gsub("JOEL CHAINOT", "JOEL CHANUT", x)
   x <- gsub("JOEL CHANOT", "JOEL CHANUT", x)
   
   x <- gsub("PAUL J. CHIASSON", "PAUL JR. CHIASSON", x)
   x <- gsub("PAUL JR CHAISSON", "PAUL JR. CHIASSON", x)
   x <- gsub("PAUL JR CHIASSON", "PAUL JR. CHIASSON", x)
   x <- gsub("CHIASSON PAUL", "PAUL JR. CHIASSON", x)
   x <- gsub("CHIASSON PAUL JR", "PAUL JR. CHIASSON", x)   
   
   x <- gsub("CHENARD J.P", "J.P. CHENARD", x)
   x <- gsub("CHENARD J.P.", "J.P. CHENARD", x)
   x <- gsub("CHENARD JP", "J.P. CHENARD", x)
   
   x <- gsub("MICHAIL CHIASSON", "MICHAEL CHIASSON", x) 
   x <- gsub("STEVE CHOINARD", "STEVE CHOUINARD", x)  
   
   x <- gsub("STEVE CHEVANE", "STEVE CHEVARIE", x)  
   x <- gsub("STEVE CHEVAINE", "STEVE CHEVARIE", x)  
   x <- gsub("STEVE CHEVAIRE", "STEVE CHEVARIE", x)  
   x <- gsub("STEVE CHEVERIE", "STEVE CHEVARIE", x)                 
   
   x <- gsub("DANIEL CYR-GAGNON", "DANIEL CYR GAGNON", x)
   
   x <- gsub("MORGAN DAUVEGNE", "MORGAN DAUVERGNE", x)
   
   x <- gsub("JULIEN DESLAURIES", "JULIEN DESLAURIERS", x)
   
   x <- gsub("NICOLAS ALLEN DAMERS", "NICOLAS ALLEN DEMERS", x)
   x <- gsub("NICOLA ALLEN DAMERS", "NICOLAS ALLEN DEMERS", x)
   
   x <- gsub("MARCO DE SANTIS", "MARCO DESANTIS", x) 
   
   x <- gsub("VERONIQUE DERESPE", "VERONIQUE DERASPE", x)  
   x <- gsub("VERONIQUE DERRAPE", "VERONIQUE DERASPE", x) 
   
   x <- gsub("JESSICA DORION", "JESSICA DOIRON", x)
   x <- gsub("JESSICA DARRA", "JESSICA DOIRON", x)   
   
   x <- gsub("ALEX DUBOURGUOIS", "ALEX DUBOURGUAIS", x) 
   
   x <- gsub("CHRISTINE DUFRESRE", "CHRISTINE DUFRESNE", x)  
   x <- gsub("CHRISTINE DUTRESNE", "CHRISTINE DUFRESNE", x)  
   x <- gsub("CHRISTINE DUTRESRE", "CHRISTINE DUFRESNE", x)  
   
   x <- gsub("MARIO-FERRON", "MARIO FERRON", x)  
   
   x <- gsub("MIGUEL FILLION", "MIGUEL FICCION", x)
   x <- gsub("MIGUES FICCION", "MIGUEL FICCION", x)
   
   x <- gsub("JEAN PHILIPPE FLEURY", "JEAN-PHILIPPE FLEURY", x)
   x <- gsub("JEAN - PHILIPPE FLEURY", "JEAN-PHILIPPE FLEURY", x)
   
   x <- gsub("MARINA MARCAGGI-FOUCAR", "MARINA MARCAGGI-FOUCARD", x)
   x <- gsub("MARINA MARCAGGI F", "MARINA MARCAGGI-FOUCARD", x)
   x <- gsub("MARINA MARCAGGI FOUCAR", "MARINA MARCAGGI-FOUCARD", x)
   x <- gsub("MARINA MARCAGGIE", "MARINA MARCAGGI-FOUCARD", x)
   x <- gsub("MARINA MARCAGGIF", "MARINA MARCAGGI-FOUCARD", x)
   x <- gsub("MARINA MARGAGGI FOUCAR", "MARINA MARCAGGI-FOUCARD", x)
   
   x <- gsub("AUDRE GAGNON", "ANDRE GAGNON", x) 
   
   x <- gsub("MARTIN GALLUCHION", "MARTIN GALLUCHON", x)
   x <- gsub("MARTIN GALLUCTION", "MARTIN GALLUCHON", x)
   x <- gsub("GALLUCHAN MARTIN", "MARTIN GALLUCHON", x)
   x <- gsub("GALLUCHION MARTIN", "MARTIN GALLUCHON", x)
   x <- gsub("GALLUCHON MARTIN", "MARTIN GALLUCHON", x)
   
   x <- gsub("JEAN-FRANCOIS GAUTHIER", "JEAN-FRANCIS GAUTHIER", x)
   
   x <- gsub("MYLENE GIONEST", "MYLENE GIONET", x)
   
   x <- gsub("SIMON GOSSEAN", "SIMON GOSSELIN", x)
   x <- gsub("SIMON GOSSEUX", "SIMON GOSSELIN", x)
   
   x <- gsub("DAMIEN CRELON", "DAMIEN GRELON", x)
   x <- gsub("DAMIEN GULON", "DAMIEN GRELON", x)
   x <- gsub("DAMION CRELON", "DAMIEN GRELON", x)
   
   x <- gsub("ABIGAEL GUENETTE", "ABIGAEL GUERETTE", x)
   x <- gsub("ABIGAEL QUENETTE", "ABIGAEL GUERETTE", x)
   x <- gsub("ABIGAIL GUEIETTE", "ABIGAEL GUERETTE", x)
   
   x <- gsub("MICHALAS HACHE", "NICHOLAS HACHE", x)
   
   x <- gsub("WADII HADUJ", "WADII HADAJ", x)
   
   x <- gsub("MICKE VAN DER HAYDE", "MIEKE VAN DER HEYDE", x)
   x <- gsub("MICKE VAN DER HEYDE", "MIEKE VAN DER HEYDE", x)
   x <- gsub("MIEKE VAN DER HAYDE", "MIEKE VAN DER HEYDE", x)
   x <- gsub("MIEKE VANDER HEYDE", "MIEKE VAN DER HEYDE", x)
   
   x <- gsub("MAUDE JAMPHE", "MAUDE JOMPHE", x)
   
   x <- gsub("JARED JUCKIEWICZ", "JARED JUCHIEWICZ", x)
   x <- gsub("JARED JUCHIGWICZ", "JARED JUCHIEWICZ", x)
   
   x <- gsub("DENIS LABBERTE", "DENIS LALIBERTE", x)
   
   x <- gsub("FRANCOIS LANDY", "FRANCOIS LANDRY", x)
   
   x <- gsub("MARTIN LANGLAIS", "MARTIN LANGLOIS", x)
   
   x <- gsub("YVES L LAROCQUE", "YVES LAROCQUE", x) 
   x <- gsub("YVES LLAROCQUE", "YVES LAROCQUE", x) 
   x <- gsub("YVES L LARACQUE", "YVES LAROCQUE", x)
   x <- gsub("YVES L. LAROCQUE", "YVES LAROCQUE", x)  
   x <- gsub("YVES L LAROQUE", "YVES LAROCQUE", x)
   x <- gsub("YVES L.LAROCQUE", "YVES LAROCQUE", x)
   
   x <- gsub("PATRICK LE BRETON", "PATRICK LEBRETON", x)    
   
   x <- gsub("MYLENE LEFEBURE", "MYLENE LEFEBVRE", x)   
   x <- gsub("MYLENE LE FEBURE", "MYLENE LEFEBVRE", x)   
   x <- gsub("MYLENE LEFEBRE", "MYLENE LEFEBVRE", x)   
   
   x <- gsub("VINCENT DEMANDE", "VINCENT LEMONDE", x)
   x <- gsub("VINCENT SEMANDE", "VINCENT LEMONDE", x)
   x <- gsub("VINCENT SEMONDE", "VINCENT LEMONDE", x)
   
   x <- gsub("LEVESQUE CHRIS", "CHRISTOPHER LEVESQUE", x) 
   x <- gsub("LE VESQUE CHRIS", "CHRISTOPHER LEVESQUE", x) 
   x <- gsub("CHRIS LEVESQUE", "CHRISTOPHER LEVESQUE", x)   
   
   x <- gsub("FRANCOIS MATHUNIN", "FRANCOIS MATHURIN", x)
   
   x <- gsub("KESLIE MATTHEWS", "LESLIE MATTHEWS", x)
   x <- gsub("LESLIE MATTHEW", "LESLIE MATTHEWS", x)
   x <- gsub("LESLIE MATTHEWSS", "LESLIE MATTHEWS", x)
   x <- gsub("LESLIE MATTHEWSSS", "LESLIE MATTHEWS", x)
   
   x <- gsub("MC WILLIAMS MURRAY", "MURRAY MCWILLIAMS", x)
   x <- gsub("MCWILLIAMS MURRAY", "MURRAY MCWILLIAMS", x)
   x <- gsub("MURRAY MC WILLIAMS", "MURRAY MCWILLIAMS", x)
   
   x <- gsub("TANIA MIASSE", "TANIA MIOUSSE",  x)
   
   x <- gsub("LUKE NAVILLE", "LUKE NEVILLE",  x)
   
   
   x <- gsub("ANDRE PARACHIS", "ANDRE PARADIS", x)
   x <- gsub("ANDRE PARADES", "ANDRE PARADIS", x)                           
   
   x <- gsub("VINCENT PAREBOOM", "VINCENT PEREBOOM", x)
   x <- gsub("VINCENT PEMBOOM", "VINCENT PEREBOOM", x)
   x <- gsub("VINCENT PERBOOM", "VINCENT PEREBOOM", x)
   x <- gsub("VINCENT PEREBOON", "VINCENT PEREBOOM", x)
   x <- gsub("VINCENT PERNBOOM", "VINCENT PEREBOOM", x)
   
   x <- gsub("CARDINE PELLETIER" , "CAROLINE PELLETIER", x)
   
   x <- gsub("STEPHANIE-C PIEDDESAUX", "STEPHANIE C PIEDDESAUX", x)
   x <- gsub("STEPHANIE C PIEDDERAUX", "STEPHANIE C PIEDDESAUX", x)
   x <- gsub("STEPHANIE C PIEODESAUX", "STEPHANIE C PIEDDESAUX", x)
   
   x <- gsub("LUC QUENNEUILLE", "LUC QUENNEVILLE", x)
   
   x <- gsub("MICHEL - ALVIN ROUSSEL", "MICHEL-ALVIN ROUSSEL", x)
   x <- gsub("MICHEL - ALVIN ROUSSEL", "MICHEL-ALVIN ROUSSEL", x)
   x <- gsub("MATTHEWS SMITH", "MATTHEW SMITH", x)
   
   x <- gsub("CHARLES E RAYNAULT", "CHARLES E. RAYNAULT", x)
   x <- gsub("CHARLES-E. RAYNAULT", "CHARLES E. RAYNAULT", x)
   x <- gsub("CHARLES-ETIEM RAYNAULT", "CHARLES E. RAYNAULT", x)
   x <- gsub("CHARLES-ETIEN RAYNAULT", "CHARLES E. RAYNAULT", x)
   
   x <- gsub("AMELIE ROBILLERD", "AMELIE ROBILLARD", x)
   
   x <- gsub("CLAUDIE ROMERO", "CLAUDIA ROMERO", x)
   
   x <- gsub("BRIAN ROUSSEI", "BRIAN ROUSSEL", x)
   
   x <- gsub("YVAN RVEST", "YVAN RUEST", x)
   
   
   x <- gsub("VERONIQUE SAUCIER", "VERONIQUE SOUCIER", x)
   
   x <- gsub("MATTHEWS SMITH", "MATTHEW SMITH", x) 
   
   x <- gsub("STEPHEN  SMITH", "STEPHEN SMITH", x) 
   x <- gsub("STEPHEN L. SMITH", "STEPHEN SMITH", x) 
   
   
   x <- gsub("MAMADDU HADY SOW", "MAMADOU HADY SOW", x)
   x <- gsub("SOW MAMADOU HADY", "MAMADOU HADY SOW", x)
   
   x <- gsub("AHMED TOUJANI", "AHMED TOUGANI", x)  
   
   x <- gsub("PIERRE LUC TREMBLEY", "PIERRE LUC TREMBLAY", x)  
   
   x <- gsub("FRANCIS VALIQUELLE", "FRANCIS VALIQUETTE", x)
   x <- gsub("FRANCIS VOLIQUETTE", "FRANCIS VALIQUETTE", x)
   
   x <- gsub("DOMINIC VINGEUX", "DOMINIC VIGNEUX", x)
   
   x <- gsub("DANWIESE", "DAN WIESE", x)  
   
   x <- gsub("MARILYN LAROCQUE", "MARILYNE LAROCQUE", x) 
   x <- gsub("MARIO ELEMENT", "MARIO CLEMENT", x)  
   x <- gsub("RENE LEBOUTHILIER", "RENE LEBOUTHILLIER", x) 
   
   x <- gsub("ROBERT WILISON", "ROBERT WILSON", x)
   x <- gsub("ROBERT WILLSON", "ROBERT WILSON", x)
   x <- gsub("ROBERT WINSON", "ROBERT WILSON", x)
   
   return(x)
}
