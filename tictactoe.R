# Tic-Tac-Toe
# Zhubo Deng

triples <- list(
  c(1,2,3),
  c(4,5,6),
  c(7,8,9),
  c(1,4,7),
  c(2,5,8),
  c(3,6,9),
  c(1,5,9),
  c(3,5,7)
)


display = function(state){
  res = 1:9
  for (i in 1:9) {
    if ( !is.na(state[i]) ) {
      if (state[i] == 'x') {
        res[i] = 'x'
      } else if (state[i] == 'o') {
        res[i] = 'o'
      }
    }
  }
  cat("", res[1], "|", res[2], "|", res[3], "\n")
  cat("---+---+---\n")
  cat("", res[4], "|", res[5], "|", res[6], "\n")
  cat("---+---+---\n")
  cat("", res[7], "|", res[8], "|", res[9], "\n")
}


prompt_user = function(who, state) {
  flag = TRUE
  while (flag) {
    if(who == 'x') {
      pos = readline("Where should x play: ")
      pos = as.integer(pos)
    } else if (who == 'o') {
      pos = readline("Where should o play: ")
      pos = as.integer(pos)
    }
    if ( !(pos %in% 1:9) ) {
      cat("Please enter a valid move")
    } else if ( !is.na(state[pos]) && state[pos] == 'x' ) {
      cat("Position", pos, "was taken. Choose again.\n")
    } else if ( !is.na(state[pos]) && state[pos] == 'o' ) {
      cat("Position", pos, "was taken. Choose again.\n")
    } else {
      flag = FALSE
    }
  }
  return(pos)
}


update = function(state, who, pos) {
  if(who == 'x') {
    state[pos] = 'x'
  } else if (who == 'o') {
    state[pos] = 'o'
  }
  return(state)
}



computer_turn = function(state) {
  
  for (i in 1:length(triples)) {
    if(sum(triples[[i]] %in% which(state == 'o')) == 2) {
      # if the computer can win by playing a spot, it must play in that spot and win
      pos = triples[[i]][!(triples[[i]] %in% which(state == 'o'))]
      if(is.na(state[pos])) {
        # check that place was taken or not
        return(pos)
      } else {
        i = i + 1
      }
    } else if(sum(triples[[i]] %in% which(state == 'x')) == 2) {
      # if the computer can win by playing a spot, it must play in that spot and win
      pos = triples[[i]][!(triples[[i]] %in% which(state == 'x'))]
      if(is.na(state[pos])) {
        return(pos)
      } else {
        i = i + 1
      }
    }
  }
  pos = sample(1:length(triples), 1)
  #print(pos)
  while ( !is.na(state[pos]) && ( state[pos] == 'x' || state[pos] == 'o' ) ) {
    pos = sample(1:length(triples), 1)
    #print(pos)
  }
  return(pos)
  
}



check_winner = function(state, who) {
  flag = FALSE
  for (i in 1:length(triples)) {
    if( sum(triples[[i]] %in% which(state == who)) == 3 ) {
        flag = TRUE
    }
  }
  return(flag)
}

play = function(){
  state = 1:9
  num.player = readline("How many human players? 1 or 2: ")
  while (num.player != 1 && num.player != 2) {
    cat("number should be 1 or 2")
    num.player = readline("How many human players? 1 or 2: ")
  }
  
  if(num.player == 1) {
    player.order = readline("Should the computer play first or second? 1 or 2: ")
    while (player.order != 1 && player.order != 2) {
      cat("order should be 1 or 2")
      player.order = readline("Should the computer play first or second? 1 or 2: ")
    }
    if(player.order == 1) {
      
      # pc vs human
      state = rep(NA, 9)
      winner = FALSE
      who = 'o'
      while (!winner) {
        if(who == 'o') {
          # computer's turn
          display(state)
          pos = computer_turn(state)
          #print(pos)
          state = update(state, who, pos)
          #print(state)
          cat("Computer played.\n")
          winner = check_winner(state, who)
          if(winner) {
            cat(who, "wins\n")
            display(state)
            break
          }
          if(sum(is.na(state)) == 0) {
            cat("Draw.\n")
            display(state)
            break
          }
          who = 'x'     # switch to o
        }
        if(who == 'x') {
          display(state)
          pos = prompt_user(who, state)
          state = update(state, who, pos)
          winner = check_winner(state, who)
          if(winner) {
            cat(who, "wins\n")
            display(state)
            break
          }
          if(sum(is.na(state)) == 0) {
            cat("Draw.\n")
            display(state)
            break
          }
          who = 'o'     # switch to o
        }
      }

    } else {
      
      # human vs pc
      state = rep(NA, 9)
      winner = FALSE
      who = 'x'
      while (!winner) {
        if(who == 'x') {
          # x's turn
          display(state)
          pos = prompt_user(who, state)
          state = update(state, who, pos)
          winner = check_winner(state, who)
          if(winner) {
            cat(who, "wins\n")
            display(state)
            break
          }
          if(sum(is.na(state)) == 0) {
            cat("Draw.\n")
            display(state)
            break
          }
          who = 'o'     # switch to o
        }
        if(who == 'o') {
          # computer's turn
          display(state)
          pos = computer_turn(state)
          #print(pos)
          state = update(state, who, pos)
          #print(state)
          cat("Computer played.\n")
          winner = check_winner(state, who)
          if(winner) {
            cat(who, "wins\n")
            display(state)
            break
          }
          if(sum(is.na(state)) == 0) {
            cat("Draw.\n")
            display(state)
            break
          }
          who = 'x'     # switch to o
        }
      }
    }
    
  } else {
    
    # player vs player
    state = rep(NA, 9)
    winner = FALSE
    who = 'x'
    while (!winner) {
      if(who == 'x') {
        # x's turn
        display(state)
        pos = prompt_user(who, state)
        state = update(state, who, pos)
        winner = check_winner(state, who)
        if(winner){
          cat(who, "wins\n")
          display(state)
          break
        }
        if(sum(is.na(state)) == 0) {
          cat("Draw.\n")
          display(state)
          break
        }
        who = 'o'     # switch to o
      }
      if(who == 'o') {
        # o's turn
        display(state)
        pos = prompt_user(who, state)
        state = update(state, who, pos)
        winner = check_winner(state, who)
        if(winner) {
          cat(who, "wins\n")
          display(state)
          break
        }
        if(sum(is.na(state)) == 0) {
          cat("Draw.\n")
          display(state)
          break
        }
        who = 'x'     # switch to o
      }
    }
  } 
}

play()

