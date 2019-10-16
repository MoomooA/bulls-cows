
generate_computer_vector <- function(){
  comp<-sample(1:10,4)
  return(comp)
}

get_guess <- function(num){
  check<-0
  numbers_string<-readline("Please enter four numbers (seperated by spaces)> ")
  input<-unlist(strsplit(numbers_string, " "))
#  numeric<-check_numeric(input)
  for(i in 1:length(input)){#check numeric
    if(!grepl("^[0-9]+$",input[i]))
    {
      check<-check+1
      print("You entered non-numeric input")
      return(get_guess(num))
    }
  }
  user<-as.integer(input)
  length<-length(user)
  if(length !=4){#check length
    check<-check+1
    print(paste("You entered", length, "numbers"))
    return(get_guess(num))
  }
  
  #the input is perfect
  check<-0
  num<-num-1
  result<-c(user, num)
  return(result)
}

check_length<-function(user){
  l<-length(user)
  if(l==4){
    return(1)
  }
  return(0)
}

check_numeric<-function(user){
  for(i in 1:4){
    if(is.na(user[i])||user[i]<1||user[i]>10){
      return(0)
    }
    return(1)
  }
}

number_bulls <- function(comp, user){
  B<-0
  for(i in 1:4){#go through computer's choice
    for(j in 1:4){#go through user's choice
      if(i==j && comp[i]==user[j]){#case of bull
        B<-B+1
        break
      }
    }
  }
  return(B)
}

number_cows <- function(comp, user){
  C<-0
  for(i in 1:4){#go through computer's choice
    for(j in 1:4){#go through user's choice
      if( i!=j && comp[i]==user[j]){#case of cow
        C<-C+1
        break
      }
    }
  }
  return(C)
}

number_bulls_and_cows <- function(comp, user){
  bulls<-number_bulls(comp, user)
  cows<-number_cows(comp, user)
  result<-c(bulls, cows)
  return(result)
}

do_response <- function(result, B, C, guessnum){
  if(result == 0){
    print("There are 4 bulls. YOU WIN!")
  }
  else if(result == 1){
    print(paste("There are", B, "bulls and", C, "cows. And you have", guessnum, "guess(es) left"))
  }
  else if(result == 2){
    print("You have run out of guesses. And the answer is:")
  }
}



comp <- generate_computer_vector()
#print(comp)
num <- 10
while(num>=0){
  guess <- get_guess(num)
  user<-guess[1:4]
#  print(paste("user=", user))
  num<-guess[5]
  #number_bulls_and_cows(comp, user)
  answer <- number_bulls_and_cows(comp,user)
  bulls <- answer[1]
  cows <- answer[2]
  
  if(bulls == 4){
    do_response(0, 0, 0, 0)
    break
  }
  else if(num == 0){
    do_response(2, 0, 0, 0)
#    print("You have run out of guesses. And the answer is:")
    print(comp)
    break
  }
  else{
    do_response(1, bulls, cows, num)
  }
  #num <- num -1
}


