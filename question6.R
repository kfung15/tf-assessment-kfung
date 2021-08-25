
confirmYYYYMMDD = function(object){
  object = str_split(object, pattern = "/")
  # print(object)
  tryCatch({
    if(length(object[[1]]) == 3){
      # Check for valid year
      if(as.numeric(object[[1]][1]) >= 0 & as.numeric(object[[1]][1]) <= 9999){
        # print("Valid Year")
        # Check for valid month
        if(as.numeric(object[[1]][2]) >= 1 & as.numeric(object[[1]][2]) <= 12){
          # print("")
          # Check for valid day
          if(as.numeric(object[[1]][3]) >= 1 & as.numeric(object[[1]][3]) <= 31){
            # print("Valid YYYYMMDD")
            return(TRUE)
          }
        }
      }
    }
    return(FALSE)
  }, warning = function(w){
    # print(w)
    return(FALSE)
  }, error = function(e){
    # print(e)
    return(FALSE)
  })
}

confirmMMDDYYYY = function(object){
  object = str_split(object, pattern = "/")
  # print(object[[1]])
  # print(as.numeric(object[[1]][1]))
  # print(as.numeric(object[[1]][2]))
  # print(as.numeric(object[[1]][3]))
  tryCatch({
    if(length(object[[1]]) == 3){
      # Check for valid month
      if(as.numeric(object[[1]][1]) >= 1 & as.numeric(object[[1]][1]) <= 12){
        # print("Valid Month")
        # Check for valid day
        if(as.numeric(object[[1]][2]) >= 1 & as.numeric(object[[1]][2]) <= 31){
          # print("Valid Day")
          # Check for valid year
          if(as.numeric(object[[1]][3]) >= 0 & as.numeric(object[[1]][3]) <= 9999){
            # print("Valid MMDDYYYY")
            return(TRUE)
          }
        }
      }
    }
    return(FALSE)
  }, warning = function(w){
    # print(w)
    return(FALSE)
  }, error = function(e){
    # print(e)
    return(FALSE)
  })
}

confirmDDMMYYYY = function(object){
  object = str_split(object, pattern = "/")
  tryCatch({
    if(length(object[[1]]) == 3){
      # Check for valid month
      if(as.numeric(object[[1]][1]) >= 1 & as.numeric(object[[1]][1]) <= 31){
        # Check for valid day
        if(as.numeric(object[[1]][2]) >= 1 & as.numeric(object[[1]][2]) <= 12){
          # print("")
          # Check for valid year
          if(as.numeric(object[[1]][3]) >= 0 & as.numeric(object[[1]][3]) <= 9999){
            # print("Valid DDMMYYYY")
            return(TRUE)
          }
        }
      }
    }
    return(FALSE)
  }, warning = function(w){
    # print(w)
    return(FALSE)
  }, error = function(e){
    # print(e)
    return(FALSE)
  })
}

confirmDDmmmYYYY = function(object){
  # print(object)
  month_list = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  # print(object)
  tryCatch({
      # Check for valid day
      if(as.numeric(object[1]) >= 1 & as.numeric(object[1]) <= 31){
        # Check for valid month (as character)
        if(tolower(object[2]) %in% month_list){
          # Check for valid year
          if(as.numeric(object[3]) >= 0 & as.numeric(object[3]) <= 9999){
            return(TRUE)
          }
        }
      }
    
    return(FALSE)
  }, warning = function(w){
    # print(w)
    return(FALSE)
  }, error = function(e){
    # print(e)
    return(FALSE)
  })
}

# Pattern 1: YYYY/MM/DD (2021/12/31) - Solved
# Pattern 2; MM/DD/YYYY (12/31/2021) - Solved
# Pattern 3: DD/MM/YYYY (31/12/2021) - Solved
# Pattern 4: DD mmm YYYY (31 dec 2021) - Solved

count_dates = function(string){
  
  total_date_count = 0
  
  split_up = str_split(string, pattern=" ")
  
  # Check for Patterns 1-3
  for(x in 1:length(split_up[[1]])){
    sub_date_count = 0
    string_to_observe = split_up[[1]][x]
    # print(string_to_observe)
    for(y in 1:(nchar(string_to_observe) - 9)){
      sub_string_to_observe = substr(string_to_observe,y,(y+9)) # Patterns 1-3 have 10 chars
      # print(sub_string_to_observe)
      if((confirmMMDDYYYY(sub_string_to_observe) | confirmDDMMYYYY(sub_string_to_observe)) | confirmYYYYMMDD(sub_string_to_observe)){
        sub_date_count = sub_date_count + 1
      } else {
        # print("No Match!")
      }
    }
    if(sub_date_count == 1){
      total_date_count = total_date_count + 1
    }
  }
  
  # Check for Pattern 4
  # Three splits at a time
  for(y in 1:(length(split_up[[1]]) - 3)){
    if(confirmDDmmmYYYY(split_up[[1]][y:(y+2)])){
      total_date_count = total_date_count + 1
    }
  }
  return(total_date_count)
}

# --------------------------- Test Cases --------------------------- 

test_string = "12/31/2021/12/31/2021/31/12/2021 16 dec 2021 is the day that my 2021/12/07/12/2021birthday is at!2021/12/05 24/12/2021"
count_dates(test_string) # 3

test_string_2 = "12/41/10000"
count_dates(test_string_2) # 0

test_string_3 = "12/41/3009"
count_dates(test_string_3) # 0

test_string_4 = "12/31/2021/12/31/2021/31/12/202116 dec 2021 is the day that my 2021/12/07/12/2021birthday is at!2021/12/05 24/12/2021"
count_dates(test_string_4) # 2




