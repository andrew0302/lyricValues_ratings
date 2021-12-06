
#load packages
library('data.table') # data manipulation
library('here')       # file logistics
library('dplyr')      # logistics
library('stringr')    # manipulating strings
library('purrr')      # iteration


#data reshaping function
data_file_reshape <- function(dt, file_name){

   #get question wording for each value:
questions <- as.data.frame(colnames(fread(here(data_file_path, file_name), skip=1)))[41:50,] %>%
  gsub("Please, rate the importance of the following values as a life-guiding principle for the SPEAKER of the lyrics. Use the slider in which 100 indicates that the value is of supreme importance for them, and -100 indicates that the value is completely opposed to their principles. - ", "", .)
  
  #remove junk on first two rows
  responses_dt <- dt[3:.N][`Participant Consent`=='agree', ]
  
  # logical vector indicating: 
  # 1) value ratings of songs
  # 2) participant ID
  songs <- grepl("-1_|PROLIFIC_PID", colnames(responses_dt)) 
  
  # keep only columns in logical vector
  responses_dt <- responses_dt[, ..songs]
  
  # pivot to long format
  responses_dt <- melt(responses_dt, id.var="PROLIFIC_PID")
  
  # create vector of patterns representing each value
  pattern <- paste0("-1_", seq(1, 10, 1))
  #create vector of song IDs
  songs <- as.character(responses_dt$variable)

  #remove value pattern from each song in vector
  songs <- gsub(pattern[10], "", songs)
  songs <- gsub(pattern[9], "", songs)
  songs <- gsub(pattern[8], "", songs) 
  songs <- gsub(pattern[7], "", songs) 
  songs <- gsub(pattern[6], "", songs) 
  songs <- gsub(pattern[5], "", songs) 
  songs <- gsub(pattern[4], "", songs) 
  songs <- gsub(pattern[3], "", songs) 
  songs <- gsub(pattern[2], "", songs) 
  songs <- gsub(pattern[1], "", songs) 

  #create new variable for song_ID, convert 'variable' to character
  responses_dt[, item_ID := ..songs][, variable := as.character(variable)]
  
  # create a vector of only unique song IDs
  songs <- unique(responses_dt$item_ID)

  # remove song ID from character string
  # this will leave the extension, which denotes which value is rated
  for(i in 1:length(songs)) {
    responses_dt$variable <- gsub(songs[i], "", responses_dt$variable)
  }
  
  # get the first word of a string
  pull_out_word <- function(question) {
    stringr::str_extract(question, '\\w*')
  }

  # convert the value in the cell to the value name
  to_value <- function(x) {
           if (x== "-1_1") {x = pull_out_word(questions[1])    # power
    } else if (x== "-1_2") {x = pull_out_word(questions[2])    # achievement
    } else if (x== "-1_3") {x = pull_out_word(questions[3])    # hedonism
    } else if (x== "-1_4") {x = pull_out_word(questions[4])    # stimulation
    } else if (x== "-1_5") {x = pull_out_word(questions[5])    # self-direction
    } else if (x== "-1_6") {x = pull_out_word(questions[6])    # universalism
    } else if (x== "-1_7") {x = pull_out_word(questions[7])    # benevolence
    } else if (x== "-1_8") {x = pull_out_word(questions[8])    # tradition
    } else if (x== "-1_9") {x = pull_out_word(questions[9])    # conformity
    } else if (x== "-1_10"){x = pull_out_word(questions[10])   # security
    } else {x = NA}
  }
  
  #execute recode function on relevant rows
  responses_dt <- responses_dt[, variable := as.character(lapply(variable, to_value))]

  # create a list of unique values
  values <- unique(responses_dt$variable)
  
  #replace blank responses with explicit "0"
  responses_dt$value <- sub("^$", "0", responses_dt$value)
  responses_dt$value <- as.numeric(responses_dt$value)

  #rename participant ID
  setnames(responses_dt, "PROLIFIC_PID", "participant_ID")
  
  # pivot to wide format, where each value is now its own column
  responses_dt <- dcast(responses_dt, participant_ID + item_ID ~ variable, value.var = "value")
}