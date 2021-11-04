#' @title Correct Observer Names
#' 
#' @description Functions to correct the spelling of at-sea observer names.

#' @export
observer <- function(x){
   # OBSERVER - Standardize and correct observer names object.

   ux <- unique(x)
   cx <- ux
   
   # Remove leading and trailing spaces and change special characters:
   cx <- gsub("[,']", " ", cx)
   cx <- as.character(cx)
   cx <- gsub("(^[ ]+)|([ ]+$)", "", cx)
   cx <- gsub(" - ", "-", cx)

   # Observer name corrections:
   cx <- gsub("AALEXANDRE KENNY", "ALEXANDRE KENNY", cx)
   cx <- gsub("ANDRE LAROCQUE", "AUDRE LAROCQUE", cx)
   cx <- gsub("NATASHA CAISSIE", "NATACHA CAISSIE", cx)
                   
   cx <- gsub("JOSIANNE ARESNEAU", "JOSIANNE ARSENEAU", cx) 
   cx <- gsub("JOSLANE ARSENEAU", "JOSIANNE ARSENEAU", cx) 
   cx <- gsub("JOSIANE ARSENEAU", "JOSIANNE ARSENEAU", cx) 
         
   cx <- gsub("SABRINA AUCET", "SABRINA AUDET", cx) 

   cx <- gsub("ABOELLAH BACHA", "ABDELLAH BACHA", cx) 
   cx <- gsub("SEBASTION BEAUCHAMP", "SEBASTIEN BEAUCHAMP", cx)  

   cx <- gsub("REMI P BEAUCHEMIN", "REMI P. BEAUCHEMIN", cx)  
   cx <- gsub("REMI P.BEAUCHEMIN", "REMI P. BEAUCHEMIN", cx)  

   cx <- gsub("STEVE BENAIT", "STEVE BENOIT", cx)
   cx <- gsub("BENOIT STEVE", "STEVE BENOIT", cx)
   cx <- gsub("BENAIT, STEVE", "STEVE BENOIT", cx)
   
   cx <- gsub("DANIEL LE BLANC", "DANIEL LEBLANC", cx) 
                      
   cx <- gsub("BENOIT SUSIE", "SUSIE BENOIT", cx)
   cx <- gsub("BENOIT, SUSIE", "SUSIE BENOIT", cx)
   
   cx <- gsub("SUZIE OKIZOLE-BERNIER", "SUZIE OLAIZOLA BERNIER", cx)
   cx <- gsub("SUZIE OLAIZDA BERNIER", "SUZIE OLAIZOLA BERNIER", cx)
   cx <- gsub("SUZIE OLAIZOLA-BERNIER", "SUZIE OLAIZOLA BERNIER", cx)
   cx <- gsub("SUZIE OLAIZOLA-REINIER", "SUZIE OLAIZOLA BERNIER", cx)   
       
   cx <- gsub("SEAN-PHILIPPE BERTIN", "JEAN-PHILIPPE BERTIN", cx)
   cx <- gsub("JEAN PHILIPPE BERTIN", "JEAN-PHILIPPE BERTIN", cx)
   
   cx <- gsub("BRANCH BRIAN", "BRIAN BRANCH", cx)

   cx <- gsub("MARIE MICHELE BOURASSA", "MARIE-MICHELE BOURASSA", cx)
   
   cx <- gsub("ANTHONY GALLIER" , "ANTHONY CALLIER", cx)
   cx <- gsub("ANTHONY GALLIN", "ANTHONY CALLIER", cx)
   cx <- gsub("ANTHONY GALLUM", "ANTHONY CALLIER", cx)  
   cx <- gsub("ANTHONY GALLIEN", "ANTHONY CALLIER", cx)
   cx <- gsub("ANTHONY CALLIN", "ANTHONY CALLIER", cx)
   
   cx <- gsub("VANESSA CARYDRAS", "VANESSA CARYDIAS", cx)

   cx <- gsub("JOEL CHAINOT", "JOEL CHANUT", cx)
   cx <- gsub("JOEL CHANOT", "JOEL CHANUT", cx)

   cx <- gsub("PAUL J. CHIASSON", "PAUL JR. CHIASSON", cx)
   cx <- gsub("PAUL JR CHAISSON", "PAUL JR. CHIASSON", cx)
   cx <- gsub("PAUL JR CHIASSON", "PAUL JR. CHIASSON", cx)
   cx <- gsub("CHIASSON PAUL", "PAUL JR. CHIASSON", cx)
   cx <- gsub("CHIASSON PAUL JR", "PAUL JR. CHIASSON", cx)   

   cx <- gsub("CHENARD J.P", "J.P. CHENARD", cx)
   cx <- gsub("CHENARD J.P.", "J.P. CHENARD", cx)
   cx <- gsub("CHENARD JP", "J.P. CHENARD", cx)
   
   cx <- gsub("MICHAIL CHIASSON", "MICHAEL CHIASSON", cx) 
   cx <- gsub("STEVE CHOINARD", "STEVE CHOUINARD", cx)  
  
   cx <- gsub("STEVE CHEVANE", "STEVE CHEVARIE", cx)  
   cx <- gsub("STEVE CHEVAINE", "STEVE CHEVARIE", cx)  
   cx <- gsub("STEVE CHEVAIRE", "STEVE CHEVARIE", cx)  
   cx <- gsub("STEVE CHEVERIE", "STEVE CHEVARIE", cx)                 
  
   cx <- gsub("DANIEL CYR-GAGNON", "DANIEL CYR GAGNON", cx)

   cx <- gsub("MORGAN DAUVEGNE", "MORGAN DAUVERGNE", cx)
   
   cx <- gsub("JULIEN DESLAURIES", "JULIEN DESLAURIERS", cx)
             
   cx <- gsub("NICOLAS ALLEN DAMERS", "NICOLAS ALLEN DEMERS", cx)
   cx <- gsub("NICOLA ALLEN DAMERS", "NICOLAS ALLEN DEMERS", cx)

   cx <- gsub("MARCO DE SANTIS", "MARCO DESANTIS", cx) 
 
   cx <- gsub("VERONIQUE DERESPE", "VERONIQUE DERASPE", cx)  
   cx <- gsub("VERONIQUE DERRAPE", "VERONIQUE DERASPE", cx) 
   
   cx <- gsub("JESSICA DORION", "JESSICA DOIRON", cx)
   cx <- gsub("JESSICA DARRA", "JESSICA DOIRON", cx)   
   
   cx <- gsub("ALEX DUBOURGUOIS", "ALEX DUBOURGUAIS", cx) 
   
   cx <- gsub("CHRISTINE DUFRESRE", "CHRISTINE DUFRESNE", cx)  
   cx <- gsub("CHRISTINE DUTRESNE", "CHRISTINE DUFRESNE", cx)  
   cx <- gsub("CHRISTINE DUTRESRE", "CHRISTINE DUFRESNE", cx)  

   cx <- gsub("MARIO-FERRON", "MARIO FERRON", cx)  

   cx <- gsub("MIGUEL FILLION", "MIGUEL FICCION", cx)
   cx <- gsub("MIGUES FICCION", "MIGUEL FICCION", cx)
   
   cx <- gsub("JEAN PHILIPPE FLEURY", "JEAN-PHILIPPE FLEURY", cx)
   cx <- gsub("JEAN - PHILIPPE FLEURY", "JEAN-PHILIPPE FLEURY", cx)

   cx <- gsub("MARINA MARCAGGI-FOUCAR", "MARINA MARCAGGI-FOUCARD", cx)
   cx <- gsub("MARINA MARCAGGI F", "MARINA MARCAGGI-FOUCARD", cx)
   cx <- gsub("MARINA MARCAGGI FOUCAR", "MARINA MARCAGGI-FOUCARD", cx)
   cx <- gsub("MARINA MARCAGGIE", "MARINA MARCAGGI-FOUCARD", cx)
   cx <- gsub("MARINA MARCAGGIF", "MARINA MARCAGGI-FOUCARD", cx)
   cx <- gsub("MARINA MARGAGGI FOUCAR", "MARINA MARCAGGI-FOUCARD", cx)

   cx <- gsub("AUDRE GAGNON", "ANDRE GAGNON", cx) 
   
   cx <- gsub("MARTIN GALLUCHION", "MARTIN GALLUCHON", cx)
   cx <- gsub("MARTIN GALLUCTION", "MARTIN GALLUCHON", cx)
   cx <- gsub("GALLUCHAN MARTIN", "MARTIN GALLUCHON", cx)
   cx <- gsub("GALLUCHION MARTIN", "MARTIN GALLUCHON", cx)
   cx <- gsub("GALLUCHON MARTIN", "MARTIN GALLUCHON", cx)
   
   cx <- gsub("JEAN-FRANCOIS GAUTHIER", "JEAN-FRANCIS GAUTHIER", cx)
   
   cx <- gsub("MYLENE GIONEST", "MYLENE GIONET", cx)
   
   cx <- gsub("SIMON GOSSEAN", "SIMON GOSSELIN", cx)
   cx <- gsub("SIMON GOSSEUX", "SIMON GOSSELIN", cx)

   cx <- gsub("DAMIEN CRELON", "DAMIEN GRELON", cx)
   cx <- gsub("DAMIEN GULON", "DAMIEN GRELON", cx)
   cx <- gsub("DAMION CRELON", "DAMIEN GRELON", cx)
   
   cx <- gsub("ABIGAEL GUENETTE", "ABIGAEL GUERETTE", cx)
   cx <- gsub("ABIGAEL QUENETTE", "ABIGAEL GUERETTE", cx)
   cx <- gsub("ABIGAIL GUEIETTE", "ABIGAEL GUERETTE", cx)

   cx <- gsub("MICHALAS HACHE", "NICHOLAS HACHE", cx)

   cx <- gsub("WADII HADUJ", "WADII HADAJ", cx)

   cx <- gsub("MICKE VAN DER HAYDE", "MIEKE VAN DER HEYDE", cx)
   cx <- gsub("MICKE VAN DER HEYDE", "MIEKE VAN DER HEYDE", cx)
   cx <- gsub("MIEKE VAN DER HAYDE", "MIEKE VAN DER HEYDE", cx)
   cx <- gsub("MIEKE VANDER HEYDE", "MIEKE VAN DER HEYDE", cx)

   cx <- gsub("MAUDE JAMPHE", "MAUDE JOMPHE", cx)
    
   cx <- gsub("JARED JUCKIEWICZ", "JARED JUCHIEWICZ", cx)
   cx <- gsub("JARED JUCHIGWICZ", "JARED JUCHIEWICZ", cx)

   cx <- gsub("DENIS LABBERTE", "DENIS LALIBERTE", cx)
   
   cx <- gsub("FRANCOIS LANDY", "FRANCOIS LANDRY", cx)
  
   cx <- gsub("MARTIN LANGLAIS", "MARTIN LANGLOIS", cx)
   
   cx <- gsub("YVES L LAROCQUE", "YVES LAROCQUE", cx) 
   cx <- gsub("YVES LLAROCQUE", "YVES LAROCQUE", cx) 
   cx <- gsub("YVES L LARACQUE", "YVES LAROCQUE", cx)
   cx <- gsub("YVES L. LAROCQUE", "YVES LAROCQUE", cx)  
   cx <- gsub("YVES L LAROQUE", "YVES LAROCQUE", cx)
   cx <- gsub("YVES L.LAROCQUE", "YVES LAROCQUE", cx)

   cx <- gsub("PATRICK LE BRETON", "PATRICK LEBRETON", cx)    

   cx <- gsub("MYLENE LEFEBURE", "MYLENE LEFEBVRE", cx)   
   cx <- gsub("MYLENE LE FEBURE", "MYLENE LEFEBVRE", cx)   
   cx <- gsub("MYLENE LEFEBRE", "MYLENE LEFEBVRE", cx)   

   cx <- gsub("VINCENT DEMANDE", "VINCENT LEMONDE", cx)
   cx <- gsub("VINCENT SEMANDE", "VINCENT LEMONDE", cx)
   cx <- gsub("VINCENT SEMONDE", "VINCENT LEMONDE", cx)

   cx <- gsub("LEVESQUE CHRIS", "CHRISTOPHER LEVESQUE", cx) 
   cx <- gsub("LE VESQUE CHRIS", "CHRISTOPHER LEVESQUE", cx) 
   cx <- gsub("CHRIS LEVESQUE", "CHRISTOPHER LEVESQUE", cx)   

   cx <- gsub("FRANCOIS MATHUNIN", "FRANCOIS MATHURIN", cx)
 
   cx <- gsub("KESLIE MATTHEWS", "LESLIE MATTHEWS", cx)
   cx <- gsub("LESLIE MATTHEW", "LESLIE MATTHEWS", cx)
   cx <- gsub("LESLIE MATTHEWSS", "LESLIE MATTHEWS", cx)
   cx <- gsub("LESLIE MATTHEWSSS", "LESLIE MATTHEWS", cx)
                     
   cx <- gsub("MC WILLIAMS MURRAY", "MURRAY MCWILLIAMS", cx)
   cx <- gsub("MCWILLIAMS MURRAY", "MURRAY MCWILLIAMS", cx)
   cx <- gsub("MURRAY MC WILLIAMS", "MURRAY MCWILLIAMS", cx)

   cx <- gsub("TANIA MIASSE", "TANIA MIOUSSE",  cx)

   cx <- gsub("LUKE NAVILLE", "LUKE NEVILLE",  cx)
   

   cx <- gsub("ANDRE PARACHIS", "ANDRE PARADIS", cx)
   cx <- gsub("ANDRE PARADES", "ANDRE PARADIS", cx)                           

   cx <- gsub("VINCENT PAREBOOM", "VINCENT PEREBOOM", cx)
   cx <- gsub("VINCENT PEMBOOM", "VINCENT PEREBOOM", cx)
   cx <- gsub("VINCENT PERBOOM", "VINCENT PEREBOOM", cx)
   cx <- gsub("VINCENT PEREBOON", "VINCENT PEREBOOM", cx)
   cx <- gsub("VINCENT PERNBOOM", "VINCENT PEREBOOM", cx)
                   
   cx <- gsub("CARDINE PELLETIER" , "CAROLINE PELLETIER", cx)

   cx <- gsub("STEPHANIE-C PIEDDESAUX", "STEPHANIE C PIEDDESAUX", cx)
   cx <- gsub("STEPHANIE C PIEDDERAUX", "STEPHANIE C PIEDDESAUX", cx)
   cx <- gsub("STEPHANIE C PIEODESAUX", "STEPHANIE C PIEDDESAUX", cx)
   
   cx <- gsub("LUC QUENNEUILLE", "LUC QUENNEVILLE", cx)
    
   cx <- gsub("MICHEL - ALVIN ROUSSEL", "MICHEL-ALVIN ROUSSEL", cx)
   cx <- gsub("MICHEL - ALVIN ROUSSEL", "MICHEL-ALVIN ROUSSEL", cx)
   cx <- gsub("MATTHEWS SMITH", "MATTHEW SMITH", cx)

   cx <- gsub("CHARLES E RAYNAULT", "CHARLES E. RAYNAULT", cx)
   cx <- gsub("CHARLES-E. RAYNAULT", "CHARLES E. RAYNAULT", cx)
   cx <- gsub("CHARLES-ETIEM RAYNAULT", "CHARLES E. RAYNAULT", cx)
   cx <- gsub("CHARLES-ETIEN RAYNAULT", "CHARLES E. RAYNAULT", cx)

   cx <- gsub("AMELIE ROBILLERD", "AMELIE ROBILLARD", cx)

   cx <- gsub("CLAUDIE ROMERO", "CLAUDIA ROMERO", cx)

   cx <- gsub("BRIAN ROUSSEI", "BRIAN ROUSSEL", cx)

   cx <- gsub("YVAN RVEST", "YVAN RUEST", cx)


   cx <- gsub("VERONIQUE SAUCIER", "VERONIQUE SOUCIER", cx)
   
   cx <- gsub("MATTHEWS SMITH", "MATTHEW SMITH", cx) 
   
   cx <- gsub("STEPHEN  SMITH", "STEPHEN SMITH", cx) 
   cx <- gsub("STEPHEN L. SMITH", "STEPHEN SMITH", cx) 

   
   cx <- gsub("MAMADDU HADY SOW", "MAMADOU HADY SOW", cx)
   cx <- gsub("SOW MAMADOU HADY", "MAMADOU HADY SOW", cx)
      
   cx <- gsub("AHMED TOUJANI", "AHMED TOUGANI", cx)  

   cx <- gsub("PIERRE LUC TREMBLEY", "PIERRE LUC TREMBLAY", cx)  
   
   cx <- gsub("FRANCIS VALIQUELLE", "FRANCIS VALIQUETTE", cx)
   cx <- gsub("FRANCIS VOLIQUETTE", "FRANCIS VALIQUETTE", cx)
    
   cx <- gsub("DOMINIC VINGEUX", "DOMINIC VIGNEUX", cx)
   
   cx <- gsub("DANWIESE", "DAN WIESE", cx)  
    
   cx <- gsub("MARILYN LAROCQUE", "MARILYNE LAROCQUE", cx) 
   cx <- gsub("MARIO ELEMENT", "MARIO CLEMENT", cx)  
   cx <- gsub("RENE LEBOUTHILIER", "RENE LEBOUTHILLIER", cx) 
   
   cx <- gsub("ROBERT WILISON", "ROBERT WILSON", cx)
   cx <- gsub("ROBERT WILLSON", "ROBERT WILSON", cx)
   cx <- gsub("ROBERT WINSON", "ROBERT WILSON", cx)
   
   ix <- match(x, ux)
   x <- cx[ix]
   
   return(x)
}
