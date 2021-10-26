observer.scobs <- function(x){
   # OBSERVER.SCOBS - Standardize and correct observer names from an 'scobs' object.
   
   # Remove leading and trailing spaces and change special characters:
   x$observer <- gsub("É", "E", x$observer)
   x$observer <- gsub("È", "E", x$observer)
   x$observer <- gsub("É", "E", x$observer)
   x$observer <- gsub("Ç", "C", x$observer)
   x$observer <- gsub("[,']", " ", x$observer)
   x$observer <- as.character(x$observer)
   x$observer <- gsub("(^[ ]+)|([ ]+$)", "", x$observer)
   x$observer <- gsub(" - ", "-", x$observer)

   # Observer name corrections:
   x$observer <- gsub("AALEXANDRE KENNY", "ALEXANDRE KENNY", x$observer)
   x$observer <- gsub("ANDRE LAROCQUE", "AUDRE LAROCQUE", x$observer)
   x$observer <- gsub("NATASHA CAISSIE", "NATACHA CAISSIE", x$observer)
                   
   x$observer <- gsub("JOSIANNE ARESNEAU", "JOSIANNE ARSENEAU", x$observer) 
   x$observer <- gsub("JOSLANE ARSENEAU", "JOSIANNE ARSENEAU", x$observer) 
   x$observer <- gsub("JOSIANE ARSENEAU", "JOSIANNE ARSENEAU", x$observer) 
         
   x$observer <- gsub("SABRINA AUCET", "SABRINA AUDET", x$observer) 

   x$observer <- gsub("ABOELLAH BACHA", "ABDELLAH BACHA", x$observer) 
   x$observer <- gsub("SEBASTION BEAUCHAMP", "SEBASTIEN BEAUCHAMP", x$observer)  

   x$observer <- gsub("REMI P BEAUCHEMIN", "REMI P. BEAUCHEMIN", x$observer)  
   x$observer <- gsub("REMI P.BEAUCHEMIN", "REMI P. BEAUCHEMIN", x$observer)  

   x$observer <- gsub("STEVE BENAIT", "STEVE BENOIT", x$observer)
   x$observer <- gsub("BENOIT STEVE", "STEVE BENOIT", x$observer)
   x$observer <- gsub("BENAIT, STEVE", "STEVE BENOIT", x$observer)
                      
   x$observer <- gsub("BENOIT SUSIE", "SUSIE BENOIT", x$observer)
   x$observer <- gsub("BENOIT, SUSIE", "SUSIE BENOIT", x$observer)
   
   x$observer <- gsub("SUZIE OKIZOLE-BERNIER", "SUZIE OLAIZOLA BERNIER", x$observer)
   x$observer <- gsub("SUZIE OLAIZDA BERNIER", "SUZIE OLAIZOLA BERNIER", x$observer)
   x$observer <- gsub("SUZIE OLAIZOLA-BERNIER", "SUZIE OLAIZOLA BERNIER", x$observer)
   x$observer <- gsub("SUZIE OLAIZOLA-REINIER", "SUZIE OLAIZOLA BERNIER", x$observer)   
       
   x$observer <- gsub("SEAN-PHILIPPE BERTIN", "JEAN-PHILIPPE BERTIN", x$observer)
   x$observer <- gsub("JEAN PHILIPPE BERTIN", "JEAN-PHILIPPE BERTIN", x$observer)
   
   x$observer <- gsub("BRANCH BRIAN", "BRIAN BRANCH", x$observer)

   x$observer <- gsub("MARIE MICHELE BOURASSA", "MARIE-MICHELE BOURASSA", x$observer)
   
   x$observer <- gsub("ANTHONY GALLIER" , "ANTHONY CALLIER", x$observer)
   x$observer <- gsub("ANTHONY GALLIN", "ANTHONY CALLIER", x$observer)
   x$observer <- gsub("ANTHONY GALLUM", "ANTHONY CALLIER", x$observer)  
   x$observer <- gsub("ANTHONY GALLIEN", "ANTHONY CALLIER", x$observer)
   x$observer <- gsub("ANTHONY CALLIN", "ANTHONY CALLIER", x$observer)
   
   x$observer <- gsub("VANESSA CARYDRAS", "VANESSA CARYDIAS", x$observer)

   x$observer <- gsub("JOEL CHAINOT", "JOEL CHANUT", x$observer)
   x$observer <- gsub("JOEL CHANOT", "JOEL CHANUT", x$observer)

   x$observer <- gsub("PAUL J. CHIASSON", "PAUL JR. CHIASSON", x$observer)
   x$observer <- gsub("PAUL JR CHAISSON", "PAUL JR. CHIASSON", x$observer)
   x$observer <- gsub("PAUL JR CHIASSON", "PAUL JR. CHIASSON", x$observer)
   x$observer <- gsub("CHIASSON PAUL", "PAUL JR. CHIASSON", x$observer)
   x$observer <- gsub("CHIASSON PAUL JR", "PAUL JR. CHIASSON", x$observer)   

   x$observer <- gsub("CHENARD J.P", "J.P. CHENARD", x$observer)
   x$observer <- gsub("CHENARD J.P.", "J.P. CHENARD", x$observer)
   x$observer <- gsub("CHENARD JP", "J.P. CHENARD", x$observer)
   
   x$observer <- gsub("MICHAIL CHIASSON", "MICHAEL CHIASSON", x$observer) 
   x$observer <- gsub("STEVE CHOINARD", "STEVE CHOUINARD", x$observer)  
  
   x$observer <- gsub("STEVE CHEVANE", "STEVE CHEVARIE", x$observer)  
   x$observer <- gsub("STEVE CHEVAINE", "STEVE CHEVARIE", x$observer)  
   x$observer <- gsub("STEVE CHEVAIRE", "STEVE CHEVARIE", x$observer)  
   x$observer <- gsub("STEVE CHEVERIE", "STEVE CHEVARIE", x$observer)                 
  
   x$observer <- gsub("DANIEL CYR-GAGNON", "DANIEL CYR GAGNON", x$observer)

   x$observer <- gsub("MORGAN DAUVEGNE", "MORGAN DAUVERGNE", x$observer)
   
   x$observer <- gsub("JULIEN DESLAURIES", "JULIEN DESLAURIERS", x$observer)
             
   x$observer <- gsub("NICOLAS ALLEN DAMERS", "NICOLAS ALLEN DEMERS", x$observer)
   x$observer <- gsub("NICOLA ALLEN DAMERS", "NICOLAS ALLEN DEMERS", x$observer)

   x$observer <- gsub("MARCO DE SANTIS", "MARCO DESANTIS", x$observer) 
 
   x$observer <- gsub("VERONIQUE DERESPE", "VERONIQUE DERASPE", x$observer)  
   x$observer <- gsub("VERONIQUE DERRAPE", "VERONIQUE DERASPE", x$observer) 
   
   x$observer <- gsub("JESSICA DORION", "JESSICA DOIRON", x$observer)
   x$observer <- gsub("JESSICA DARRA", "JESSICA DOIRON", x$observer)   
   
   x$observer <- gsub("ALEX DUBOURGUOIS", "ALEX DUBOURGUAIS", x$observer) 
   
   x$observer <- gsub("CHRISTINE DUFRESRE", "CHRISTINE DUFRESNE", x$observer)  
   x$observer <- gsub("CHRISTINE DUTRESNE", "CHRISTINE DUFRESNE", x$observer)  
   x$observer <- gsub("CHRISTINE DUTRESRE", "CHRISTINE DUFRESNE", x$observer)  

   x$observer <- gsub("MARIO-FERRON", "MARIO FERRON", x$observer)  

   x$observer <- gsub("MIGUEL FILLION", "MIGUEL FICCION", x$observer)
   x$observer <- gsub("MIGUES FICCION", "MIGUEL FICCION", x$observer)
   
   x$observer <- gsub("JEAN PHILIPPE FLEURY", "JEAN-PHILIPPE FLEURY", x$observer)
   x$observer <- gsub("JEAN - PHILIPPE FLEURY", "JEAN-PHILIPPE FLEURY", x$observer)

   x$observer <- gsub("MARINA MARCAGGI-FOUCAR", "MARINA MARCAGGI-FOUCARD", x$observer)
   x$observer <- gsub("MARINA MARCAGGI F", "MARINA MARCAGGI-FOUCARD", x$observer)
   x$observer <- gsub("MARINA MARCAGGI FOUCAR", "MARINA MARCAGGI-FOUCARD", x$observer)
   x$observer <- gsub("MARINA MARCAGGIE", "MARINA MARCAGGI-FOUCARD", x$observer)
   x$observer <- gsub("MARINA MARCAGGIF", "MARINA MARCAGGI-FOUCARD", x$observer)
   x$observer <- gsub("MARINA MARGAGGI FOUCAR", "MARINA MARCAGGI-FOUCARD", x$observer)

   x$observer <- gsub("AUDRE GAGNON", "ANDRE GAGNON", x$observer) 
   
   x$observer <- gsub("MARTIN GALLUCHION", "MARTIN GALLUCHON", x$observer)
   x$observer <- gsub("MARTIN GALLUCTION", "MARTIN GALLUCHON", x$observer)
   x$observer <- gsub("GALLUCHAN MARTIN", "MARTIN GALLUCHON", x$observer)
   x$observer <- gsub("GALLUCHION MARTIN", "MARTIN GALLUCHON", x$observer)
   x$observer <- gsub("GALLUCHON MARTIN", "MARTIN GALLUCHON", x$observer)
   
   x$observer <- gsub("JEAN-FRANCOIS GAUTHIER", "JEAN-FRANCIS GAUTHIER", x$observer)
   
   x$observer <- gsub("MYLENE GIONEST", "MYLENE GIONET", x$observer)
   
   x$observer <- gsub("SIMON GOSSEAN", "SIMON GOSSELIN", x$observer)
   x$observer <- gsub("SIMON GOSSEUX", "SIMON GOSSELIN", x$observer)

   x$observer <- gsub("DAMIEN CRELON", "DAMIEN GRELON", x$observer)
   x$observer <- gsub("DAMIEN GULON", "DAMIEN GRELON", x$observer)
   x$observer <- gsub("DAMION CRELON", "DAMIEN GRELON", x$observer)
   
   x$observer <- gsub("ABIGAEL GUENETTE", "ABIGAEL GUERETTE", x$observer)
   x$observer <- gsub("ABIGAEL QUENETTE", "ABIGAEL GUERETTE", x$observer)
   x$observer <- gsub("ABIGAIL GUEIETTE", "ABIGAEL GUERETTE", x$observer)

   x$observer <- gsub("MICHALAS HACHE", "NICHOLAS HACHE", x$observer)

   x$observer <- gsub("WADII HADUJ", "WADII HADAJ", x$observer)

   x$observer <- gsub("MICKE VAN DER HAYDE", "MIEKE VAN DER HEYDE", x$observer)
   x$observer <- gsub("MICKE VAN DER HEYDE", "MIEKE VAN DER HEYDE", x$observer)
   x$observer <- gsub("MIEKE VAN DER HAYDE", "MIEKE VAN DER HEYDE", x$observer)
   x$observer <- gsub("MIEKE VANDER HEYDE", "MIEKE VAN DER HEYDE", x$observer)

   x$observer <- gsub("MAUDE JAMPHE", "MAUDE JOMPHE", x$observer)
    
   x$observer <- gsub("JARED JUCKIEWICZ", "JARED JUCHIEWICZ", x$observer)
   x$observer <- gsub("JARED JUCHIGWICZ", "JARED JUCHIEWICZ", x$observer)

   x$observer <- gsub("DENIS LABBERTE", "DENIS LALIBERTE", x$observer)
   
   x$observer <- gsub("FRANCOIS LANDY", "FRANCOIS LANDRY", x$observer)
  
   x$observer <- gsub("MARTIN LANGLAIS", "MARTIN LANGLOIS", x$observer)
   
   x$observer <- gsub("YVES L LAROCQUE", "YVES LAROCQUE", x$observer) 
   x$observer <- gsub("YVES LLAROCQUE", "YVES LAROCQUE", x$observer) 
   x$observer <- gsub("YVES L LARACQUE", "YVES LAROCQUE", x$observer)
   x$observer <- gsub("YVES L. LAROCQUE", "YVES LAROCQUE", x$observer)  
   x$observer <- gsub("YVES L LAROQUE", "YVES LAROCQUE", x$observer)
   x$observer <- gsub("YVES L.LAROCQUE", "YVES LAROCQUE", x$observer)

   x$observer <- gsub("PATRICK LE BRETON", "PATRICK LEBRETON", x$observer)    

   x$observer <- gsub("MYLENE LEFEBURE", "MYLENE LEFEBVRE", x$observer)   
   x$observer <- gsub("MYLENE LE FEBURE", "MYLENE LEFEBVRE", x$observer)   
   x$observer <- gsub("MYLENE LEFEBRE", "MYLENE LEFEBVRE", x$observer)   

   x$observer <- gsub("VINCENT DEMANDE", "VINCENT LEMONDE", x$observer)
   x$observer <- gsub("VINCENT SEMANDE", "VINCENT LEMONDE", x$observer)
   x$observer <- gsub("VINCENT SEMONDE", "VINCENT LEMONDE", x$observer)

   x$observer <- gsub("LEVESQUE CHRIS", "CHRISTOPHER LEVESQUE", x$observer) 
   x$observer <- gsub("LE VESQUE CHRIS", "CHRISTOPHER LEVESQUE", x$observer) 
   x$observer <- gsub("CHRIS LEVESQUE", "CHRISTOPHER LEVESQUE", x$observer)   

   x$observer <- gsub("FRANCOIS MATHUNIN", "FRANCOIS MATHURIN", x$observer)
 
   x$observer <- gsub("KESLIE MATTHEWS", "LESLIE MATTHEWS", x$observer)
   x$observer <- gsub("LESLIE MATTHEW", "LESLIE MATTHEWS", x$observer)
   x$observer <- gsub("LESLIE MATTHEWSS", "LESLIE MATTHEWS", x$observer)
   x$observer <- gsub("LESLIE MATTHEWSSS", "LESLIE MATTHEWS", x$observer)
                     
   x$observer <- gsub("MC WILLIAMS MURRAY", "MURRAY MCWILLIAMS", x$observer)
   x$observer <- gsub("MCWILLIAMS MURRAY", "MURRAY MCWILLIAMS", x$observer)
   x$observer <- gsub("MURRAY MC WILLIAMS", "MURRAY MCWILLIAMS", x$observer)

   x$observer <- gsub("TANIA MIASSE", "TANIA MIOUSSE",  x$observer)

   x$observer <- gsub("LUKE NAVILLE", "LUKE NEVILLE",  x$observer)
   

   x$observer <- gsub("ANDRE PARACHIS", "ANDRE PARADIS", x$observer)
   x$observer <- gsub("ANDRE PARADES", "ANDRE PARADIS", x$observer)                           

   x$observer <- gsub("VINCENT PAREBOOM", "VINCENT PEREBOOM", x$observer)
   x$observer <- gsub("VINCENT PEMBOOM", "VINCENT PEREBOOM", x$observer)
   x$observer <- gsub("VINCENT PERBOOM", "VINCENT PEREBOOM", x$observer)
   x$observer <- gsub("VINCENT PEREBOON", "VINCENT PEREBOOM", x$observer)
   x$observer <- gsub("VINCENT PERNBOOM", "VINCENT PEREBOOM", x$observer)
                   
   x$observer <- gsub("CARDINE PELLETIER" , "CAROLINE PELLETIER", x$observer)

   x$observer <- gsub("STEPHANIE-C PIEDDESAUX", "STEPHANIE C PIEDDESAUX", x$observer)
   x$observer <- gsub("STEPHANIE C PIEDDERAUX", "STEPHANIE C PIEDDESAUX", x$observer)
   x$observer <- gsub("STEPHANIE C PIEODESAUX", "STEPHANIE C PIEDDESAUX", x$observer)
   
   x$observer <- gsub("LUC QUENNEUILLE", "LUC QUENNEVILLE", x$observer)
    
   x$observer <- gsub("MICHEL - ALVIN ROUSSEL", "MICHEL-ALVIN ROUSSEL", x$observer)
   x$observer <- gsub("MICHEL - ALVIN ROUSSEL", "MICHEL-ALVIN ROUSSEL", x$observer)
   x$observer <- gsub("MATTHEWS SMITH", "MATTHEW SMITH", x$observer)

   x$observer <- gsub("CHARLES E RAYNAULT", "CHARLES E. RAYNAULT", x$observer)
   x$observer <- gsub("CHARLES-E. RAYNAULT", "CHARLES E. RAYNAULT", x$observer)
   x$observer <- gsub("CHARLES-ETIEM RAYNAULT", "CHARLES E. RAYNAULT", x$observer)
   x$observer <- gsub("CHARLES-ETIEN RAYNAULT", "CHARLES E. RAYNAULT", x$observer)

   x$observer <- gsub("AMELIE ROBILLERD", "AMELIE ROBILLARD", x$observer)

   x$observer <- gsub("CLAUDIE ROMERO", "CLAUDIA ROMERO", x$observer)

   x$observer <- gsub("BRIAN ROUSSEI", "BRIAN ROUSSEL", x$observer)

   x$observer <- gsub("YVAN RVEST", "YVAN RUEST", x$observer)


   x$observer <- gsub("VERONIQUE SAUCIER", "VERONIQUE SOUCIER", x$observer)
   
   x$observer <- gsub("MATTHEWS SMITH", "MATTHEW SMITH", x$observer) 
   
   x$observer <- gsub("STEPHEN  SMITH", "STEPHEN SMITH", x$observer) 
   x$observer <- gsub("STEPHEN L. SMITH", "STEPHEN SMITH", x$observer) 

   
   x$observer <- gsub("MAMADDU HADY SOW", "MAMADOU HADY SOW", x$observer)
   x$observer <- gsub("SOW MAMADOU HADY", "MAMADOU HADY SOW", x$observer)
      
   x$observer <- gsub("AHMED TOUJANI", "AHMED TOUGANI", x$observer)  

   x$observer <- gsub("PIERRE LUC TREMBLEY", "PIERRE LUC TREMBLAY", x$observer)  
   
   x$observer <- gsub("FRANCIS VALIQUELLE", "FRANCIS VALIQUETTE", x$observer)
   x$observer <- gsub("FRANCIS VOLIQUETTE", "FRANCIS VALIQUETTE", x$observer)
    
   x$observer <- gsub("DOMINIC VINGEUX", "DOMINIC VIGNEUX", x$observer)
   
   x$observer <- gsub("DANWIESE", "DAN WIESE", x$observer)  
    
   x$observer <- gsub("MARILYN LAROCQUE", "MARILYNE LAROCQUE", x$observer) 
   x$observer <- gsub("MARIO ELEMENT", "MARIO CLEMENT", x$observer)  
   x$observer <- gsub("RENE LEBOUTHILIER", "RENE LEBOUTHILLIER", x$observer) 
   
   x$observer <- gsub("ROBERT WILISON", "ROBERT WILSON", x$observer)
   x$observer <- gsub("ROBERT WILLSON", "ROBERT WILSON", x$observer)
   x$observer <- gsub("ROBERT WINSON", "ROBERT WILSON", x$observer)
   
   return(x$observer)
}
