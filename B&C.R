
generate_computer_vector <- function(){
  comp<-sample(1:10,4)
  return(comp)
}

get_guess <- function(num, check){
  #check<-0
  numbers_string<-readline("Please enter four numbers (seperated by spaces)> ")
  input<-unlist(strsplit(numbers_string, " "))
#  numeric<-check_numeric(input)
  for(i in 1:length(input)){#check numeric
    if(!grepl("^[0-9]+$",input[i]))
    {
      check<-check+1
      print("You entered non-numeric input")
      error_check(check)
      return(get_guess(num, check))
    }
  }
  user<-as.integer(input)
  length<-length(user)
  if(length !=4){#check length
    check<-check+1
    print(paste("You entered", length, "number(s)"))
    error_check(check)
    return(get_guess(num, check))
  }
  #check duplication
  for(i in 1:3){
    for(j in (i+1):4){
      if(user[i]==user[j]){
        check<-check+1
        print("Please enter distinct numbers")
        error_check(check)
        return(get_guess(num, check))
      }
    }
  }
  #check size of the numbers
  for(i in 1:4)
    if(user[i]<1 || user[i]>10){
      check<-check+1
      print("Please enter numbers between 1-10")
      error_check(check)
      return(get_guess(num, check))
    }
  #the input is perfect
  num<-num-1
  result<-c(user, num)
  return(result)
}

error_check<-function(num){
  if(num==3){
    stop("Three incorrect inputs in a row. GAME OVER")
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
num <- 10
while(num>=0){
  check <- 0
  guess <- get_guess(num, check)
  user<-guess[1:4]
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
    print(comp)
    break
  }
  else{
    do_response(1, bulls, cows, num)
  }
  #num <- num -1
}


