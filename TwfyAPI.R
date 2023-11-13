install.packages("twfy")
library(twfy)

#set_api_key("C7md57FJUqRLBN6DiQD5XY8G") #only need to run once
get_api_key() #should be the above key

#https://www.theyworkforyou.com/api/getDebates?key=C7md57FJUqRLBN6DiQD5XY8G&type=commons&search=climate+20170621..20221201&output=xml
#https://www.theyworkforyou.com/api/getDebates?key=C7md57FJUqRLBN6DiQD5XY8G&type=commons&search=climate+20170621..20221201&output=xml&num=1000&page=1

keywords_filter = c("climate change", "climate emergency", "climate crisis", "global warming", "global heating", "climate breakdown", "net zero", "energy", "transport", "emissions", "carbon", "CO2", "GHG", "greenhouse gas","methane", "fossil fuel", "fossil fuels", "gas", "oil", "coal", "fracking", "heat pump", "insulation", "aviation", "green business", "land use", "green jobs", "decarbonisation", "decarbonise", "decarbonising", "hydrogen", "solar", "PV", "wind energy", "offshore wind", "onshore wind", "photovoltaic", "nuclear energy", "nuclear power", "renewable", "EV", "electric vehicle", "cycling", "active travel", "sustainable", "just transition", "fair transition", "flood", "drought", "heat", "extreme temperature", "extreme weather", "environment", "nature", "peatland", "ecological", "ecocide","cop23", "cop24", "cop25","cop26", "cop27", "Paris Agreement", "UNFCCC", "1.5", "1.5 degrees", "climate protests", "future generations", "climate justice", "loss and damage", "mitigation", "adaptation", "climate finance", "nature restoration", "reforestation", "climate education", "ecology", "climate resilience", "climate resilient", "food security", "food waste", "climate leader", "climate leadership", "climate policy", "IPCC", "frequent flyer", "zero-emission", "petrol", "low emission", "LEZ", "ULEZ", "clean air", "idling", "circular economy", "green building", "green construction", "sea level rise", "pollution", "CCUS", "climate engineering", "geoengineering", "radiative cooling", "PDRC", "zero-energy", "green finance", "CCL", "feed-in-tariffs", "CBAM", "low-carbon", "climate commitments", "build back greener", "high-emitting", "NDC", "LULUCF", "demand reduction", "electrification", "phase-out", "IAM", "integrated assessment model", "zero-carbon", "agrivoltaics", "agrophotovoltaics", "agrisolar", "climate action", "geothermal", "tidal energy", "tidal power", "wildfire", "storm", "hydropower",  "energy-efficient", "climate technologies",  "climate tech",  "climate solution", "climate refugees", "climate-friendly", "zero-pollution", "climate fund", "climate aid", "climate investment", "divestment", "climate-smart", "climate impacts", "forestry", "agri-climate", "climate risk") 

#regex pattern g=for keyword list
keyword_pattern <- paste(keywords_filter, collapse = "|")

#keyword list for apis
all_keywords = chartr(" ", "+", keywords_filter)


### Parliamentary Debates----

get_debs= function(type, term){
  search = paste0(term, " 20170621..20221201")
  print(paste("search term is:", search))
  file = getDebates(type = type, search = search, num = 1000, page = 1)
  
  x = file$rows
  xbod = data.frame(date = x$hdate, text = x$body,  relevance = x$relevance, id = x$epobject_id)
  xpar = data.frame(title = x$parent$body)
  xspeaker = data.frame(speaker = x$speaker$name, speaker_id = x$speaker$member_id, constituency = x$speaker$constituency, party = x$speaker$party, house = x$speaker$house)
  
  merged = cbind(xbod, xpar, xspeaker)
  merged = merged[merged$relevance >= 50, ]
  
  if(file$info$total_results <= 1000){
    print(paste(term, "has no more pages"))
    return(merged)
  } else if(file$info$total_results >1000){
    print(paste(term, "has", file$info$total_results, "results"))

    file2 = getDebates(type = type, search = search , num = 1000, page = 2)
    
    x2 = file2$rows
    xbod2 = data.frame(date = x2$hdate, text = x2$body,  relevance = x2$relevance, id = x2$epobject_id)
    xpar2 = data.frame(title = x2$parent$body)
    xspeaker2 = data.frame(speaker = x2$speaker$name, speaker_id = x2$speaker$member_id, constituency = x2$speaker$constituency, party = x2$speaker$party, house = x2$speaker$house)
    
    merged2 = cbind(xbod2, xpar2, xspeaker2)
    merged2 = merged2[merged2$relevance >= 50, ]
    
    merged3 = rbind(merged, merged2)
    
    return(merged3)
    
  }
  
}

get_all_debs = function(type,words){
  all = data.frame()
  for(i in words){
    search = paste0(i, " 20170621..20221201")
    file = getDebates(type = type, search = search, num = 1000, page = 1)
      
    if(file$info[["total_results"]] < 2) {
      warning(paste("Missing data detected for" ,i, "- terminating function and moving to next keyword"))
      next}
    
    new = get_debs(type,i)
    
    all = rbind(all, new)
  }
  return(all)
}

debs = get_all_debs("commons", all_keywords)
debsL = get_all_debs("lords", all_keywords)
debsWMH = get_all_debs("westminsterhall", all_keywords)



#remove duplicates
debs = distinct(debs, id, .keep_all = TRUE)
debsL = distinct(debsL, id, .keep_all = TRUE)
debsWMH = distinct(debsWMH, id, .keep_all = TRUE)

debates = rbind(debs, debsL, debsWMH)
#try to remove wierd bits and empty speaker rows
debates = debates[!is.na(debates$speaker),]
debates$text = gsub("<.*?>", "", debates$text) 
debates$text = gsub("&.*?;", "", debates$text)

#keep just ones with relevant title
debates = debates[grep(keyword_pattern, debates$title, ignore.case = TRUE),]


climdebs = get_debs("lords", "climate+change")
climdebs = climdebs[!is.na(climdebs$speaker),]
v = climdebs[grep(keyword_pattern, climdebs$title, ignore.case = TRUE),]
vl = climdebs[grepl(keyword_pattern, climdebs$title, ignore.case = TRUE),]



write.csv(debates, "ParlyDebates.csv")

### EDMs----
#install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)

get_edms = function(term){
  edm = GET(paste0("https://oralquestionsandmotions-api.parliament.uk/EarlyDayMotions/list?parameters.searchTerm=", term, "&parameters.tabledStartDate=2017-06-17&parameters.tabledEndDate=2022-12-01&parameters.take=100"))
  
  data = fromJSON(rawToChar(edm$content))
  total = data$PagingInfo$Total
  skip = data$PagingInfo$Skip
  take = data$PagingInfo$Take
  
  x = data$Response
  xbod = data.frame(date = x$DateTabled, id = x$Id, title = x$Title, text = x$MotionText, sponsors = x$SponsorsCount)
  xspons = data.frame(author = x$PrimarySponsor$Name, constituency = x$PrimarySponsor$Constituency, party = x$PrimarySponsor$Party)
  
  merged = cbind(xbod, xspons)
  
  if(total <=100) {
    print(paste(term, "has no extra pages"))
    return(merged)
  } else if(total > 100){
      print(paste(term, "has extra pages"))
      skip = 100
      take = total - 100
      get_2nd = GET(paste0("https://oralquestionsandmotions-api.parliament.uk/EarlyDayMotions/list?parameters.searchTerm=", term, "&parameters.tabledStartDate=2017-06-17&parameters.tabledEndDate=2022-12-01&parameters.skip=", as.character(skip), "&parameters.take=", as.character(take)))
      data2 = fromJSON(rawToChar(get_2nd$content))
      x2 = data2$Response
    
      xbod2 = data.frame(date = x2$DateTabled, id = x2$Id, title = x2$Title, text = x2$MotionText, sponsors = x2$SponsorsCount)
      xspons2 = data.frame(author = x2$PrimarySponsor$Name, constituency = x2$PrimarySponsor$Constituency, party = x2$PrimarySponsor$Party)
    
      merged2 = cbind(xbod2, xspons2)
    
      merged3 = rbind(merged, merged2)
    
      return(merged3)
  }
  
  
}

climateedms = get_edms("climate")

edmsub = climateedms[grep(paste(str_to_title(keywords_filter), collapse="|"), str_to_title(climateedms$title)),]

library(dplyr)

get_all_edms = function(words){
  all = data.frame()
  for(i in words){
    new = get_edms(i)
    
    all = rbind(all, new)
  }
  return(all)
}


testing8 = get_all_edms(all_keywords)
#remove duplicates
edms = distinct(testing8, id, .keep_all = TRUE)
#keep just ones with relevant title
edms = edms[grep(keyword_pattern, edms$title, ignore.case = TRUE),]
#reformat date
edms$date = as.Date(edms$date)
write.csv(edms, "ParlyEDMs.csv")


###Petitions----

#current parliament
get_petitions = function(term){
  firstcall = fromJSON(paste0("https://petition.parliament.uk/petitions.json?page=1&q=", term, "&state=all"))
  
  frame = firstcall[["data"]][["attributes"]]
  frame = data.frame(id = firstcall$data[["id"]], created = frame$created_at,  petition_title = frame$action, petiton_text = paste(frame$background, frame$additional_details), signature_count =  frame$signature_count, state = frame$state, keyword = term)
  
  npages = as.numeric(gsub(".*page=|&q.*", "",firstcall$links$last))
  
  if(is.na(npages)){
    print(paste(term, "has no extra pages"))
    return(frame)
  } else if (npages > 1){
      for(i in 2:npages){
        print(paste(term,"has", npages, "pages"))
        othercalls = paste0("https://petition.parliament.uk/petitions.json?page=",i, "&q=", term, "&state=all")
        get_it2 = fromJSON(othercalls)
        newframe = get_it2[["data"]][["attributes"]]
        newframe = data.frame(id = get_it2$data[["id"]], created = newframe$created_at, petition_title = newframe$action, petiton_text = paste(newframe$background, newframe$additional_details), signature_count =  newframe$signature_count, state = newframe$state, keyword = term)
      
        frame = rbind(frame, newframe)
    }
    return(frame)
  }
}

get_all_ps = function(words){
  all = data.frame()
  for(i in words){
    firstcall = fromJSON(paste0("https://petition.parliament.uk/petitions.json?page=1&q=", i, "&state=all"))
    
    if(length(firstcall$data) < 1) {
      warning(paste("Missing data detected for" ,i, "- terminating function and moving to next keyword"))
      next
    }
    new = get_petitions(i)
    
    all = rbind(all, new)
  }
  return(all)
}

testing2 = get_all_ps(all_keywords)

#archived (2017-2019 parliament)
get_ARCpetitions = function(term){
  firstcall = fromJSON(paste0("https://petition.parliament.uk/archived/petitions.json?&page=1&parliament=3&q=", term, "&state=all"))
  
  frame = firstcall[["data"]][["attributes"]]
  frame = data.frame( id = firstcall$data[["id"]], created = frame$created_at,  petition_title = frame$action, petiton_text = paste( frame$background, frame$additional_details), signature_count =  frame$signature_count, state = frame$state, keyword = term)
  
  npages = as.numeric(gsub(".*page=|&q.*", "",firstcall$links$last))
  
  print(paste(term, npages, "pages done"))
  
  if(is.na(npages)){
    print(paste(term, "has no extra pages"))
    return(frame)
  } else if (npages > 1){
      for(i in 2:npages){
        othercalls = paste0("https://petition.parliament.uk/archived/petitions.json?page=",i, "&parliament=3&q=", term, "&state=all")
        get_it2 = fromJSON(othercalls)
        newframe = get_it2[["data"]][["attributes"]]
        newframe = data.frame(id = get_it2$data[["id"]], created = newframe$created_at,  petition_title = newframe$action, petiton_text = paste(newframe$background, newframe$additional_details), signature_count =  newframe$signature_count, state = newframe$state, keyword = term)
      
        frame = rbind(frame, newframe)
      }
    return(frame)
  } 
}
get_all_ARCps = function(words){
  all = data.frame()
  for(i in words){
    firstcall = fromJSON(paste0("https://petition.parliament.uk/archived/petitions.json?&page=1&parliament=3&q=", i, "&state=all"))
    if(length(firstcall$data) < 1) {
      warning(paste("Missing data detected for" ,i, "- terminating function and moving to next keyword"))
      next
    }
    new = get_ARCpetitions(i)
    
    all = rbind(all, new)
  }
  return(all)
}
testing4 = get_all_ARCps(all_keywords)

#combined current and archived
petitions = rbind(testing2, testing4)
#reformat date column
petitions$created = as.Date(petitions$created)
#filter dates
petitions = petitions %>% filter(created < "2022-12-02")
#keep just ones with relevant title
petitions = petitions[grep(keyword_pattern, petitions$petition_title, ignore.case = TRUE),]
#remove duplicates
petitions = distinct(petitions, id, .keep_all = TRUE)

write.csv(petitions, "ParlyEpetitions.csv")


gov_response = petitions[petitions$signature_count>10000,]
rownames(gov_response) = NULL

get_response = function(id){
  call = fromJSON(paste0("https://petition.parliament.uk/petitions/", id, ".json"))
  frame = call$data[["attributes"]]
  df = data.frame(id = id , gov_response = paste(frame$government_response[["summary"]], frame$government_response[["details"]]))
  return(df)
  
}

get_all_responses = function(ids){
  all = data.frame()
  for(i in ids){
    print(paste("getting id", i))
    firstcall = fromJSON(paste0("https://petition.parliament.uk/petitions/", i, ".json"))
    
    if(length(firstcall$data[["attributes"]][['government_response']]) < 1) {
      warning(paste("No response detected for" ,i, "- terminating function and moving to next id"))
      next}
    new = get_response(as.character(i))
    
    all = rbind(all, new)
  }
  return(all)
}
  
responses = get_all_responses(gov_response$id)

gov_response = merge(gov_response,responses, by = "id")
write.csv(gov_response, "ParlyEpetitionsResponse.csv")
