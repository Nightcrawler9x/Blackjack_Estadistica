#---------------------------------- LIBRERÍAS ----------------------------------#
#install.packages("ggplot2")
library(ggplot2)
set.seed(101451)
#---------------------------------- VARIABLES ----------------------------------#
gameEnv <<- new.env()

deck <<- rep(c(1:10, 10, 10, 10), 4)  

initializeGame <- function(config) {
  gameEnv$stack <- config$defaultStack
  gameEnv$currentBet <- config$defaultBet
  gameEnv$stats <- numeric(0)
  gameEnv$cardCount <- 0
  createShoe(config$defaultDecks)
}

gameConfig <- list(
  defaultBet = 100,
  defaultStack = 100000,
  defaultDecks = 6
)

DEBUG <<- FALSE

#-------------------------------- FUNCIONES ------------------------------------#

createShoe <- function(n) {
  gameEnv$shoe <- sample(rep(deck, n))
}

updateCardCount <- function(cards) {
  count <- 0
  
  for (card in cards) {
    if (card >= 2 && card <= 6) {
      count <- count + 1
    } else if (card >= 7 && card <= 9) {
      count <- count + 0
    } else if (card == 10 || card == 1) {
      count <- count - 1
    }
  }
  
  gameEnv$cardCount <- gameEnv$cardCount + count
}

getCards <- function(m = 1) {
  if (length(gameEnv$shoe) < m) stop("No hay suficientes cartas en el zapato.")
  
  shownCards <- gameEnv$shoe[1:m]
  gameEnv$shoe <- gameEnv$shoe[-(1:m)]
  updateCardCount(shownCards)
  
  return(shownCards)
}

handValue <- function(cards) {
  value <- sum(cards)
  
  if (any(cards == 1) && value <= 11){
    value <- value + 10
  }
  if (value > 21){
    value <- 0 
  }  
  if (value == 21 && length(cards) == 2){
    value <- 21.5
  } 
  
  return(value)
}

newHand <- function(bet = gameEnv$currentBet) {
  cards <- getCards(2)
  list(bet = bet, cards = cards)
}

dealerCards <- function(cards) {
  
  while (handValue(cards) < 17 && handValue(cards) > 0) {
    cards <- c(cards, getCards(1))
  }
  
  return(cards)
}

hit <- function(hand) {
  newCard <- getCards(1)
  hand$cards <- c(hand$cards, newCard)
  return(hand)
}

stand <- function(hand) {
  return(hand)
}

doubleDown <- function(hand) {
  gameEnv$currentBet <- gameEnv$currentBet * 2
  hand <- hit(hand)
  return(stand(hand)) 
}

splitPair <- function(hand) {
  
  card1 <- getCards(1)
  card2 <- getCards(1)
  
  hand1 <- list(bet = gameEnv$currentBet, cards = c(hand$cards[1], card1))
  hand2 <- list(bet = gameEnv$currentBet, cards = c(hand$cards[2], card2))
  
  return(list(hand1, hand2))
}


winnings <- function(dealer, player) {
  result <- 0
  
  if (dealer == 21.5 && player != 21.5){
    result <- -1
  } 
  else if (player == 21.5 && dealer != 21.5){
    result <- 1.5
  } 
  else if (player < dealer && dealer <= 21){
    result <- -1
  } 
  else if (player > dealer && player <= 21){
    result <- 1
  } 
  else if (player == dealer){
    result <- 0
  } 
  
  gameEnv$stats <- c(gameEnv$stats, result)
  
  return(result)
}

playHand <- function(strategy, hand = newHand(gameEnv$currentBet), dealer = getCards(2)) {
  
  action <- strategy(hand$cards, dealer[1], gameEnv$currentBet,  updateBet = TRUE)
  
  if (DEBUG) {
    cat("Inicio de la mano:\n")
    cat("Initial Bet: ", gameEnv$currentBet, "\n")
    cat(" Dealer: ", paste(dealer, collapse = "-"), " (", 
        handValue(dealer), ")\n", sep = "")
    cat(" Player: ", paste(hand$cards, collapse = "-"), "\n")
  }
  
  while (TRUE) {
    playerValue <- handValue(hand$cards)
    if (playerValue == 0 || action == "S"){
      break
    } 
    if (action == "H"){
      hand <- hit(hand)
    }
    if (action == "D"){
      hand <- doubleDown(hand)
    } 
    if (action == "SP") {
      splitHands <- splitPair(hand)
      totalResult <- 0
      
      for (subHand in splitHands) {
        totalResult <- totalResult + playHand(strategy, subHand, dealer)
      }
      
      return(totalResult)  
    }
    action <- strategy(hand$cards, dealer[1], gameEnv$currentBet)
    if(DEBUG){
      cat("Accion: ", action, " -> ", paste(hand$cards, collapse = "-"), " (", handValue(hand$cards), ")", sep = "", "\n")
    }
  }
  
  dealer <- dealerCards(dealer)
  
  result <- winnings(handValue(dealer), handValue(hand$cards)) * gameEnv$currentBet
  gameEnv$stack <- gameEnv$stack + result
  
  if (DEBUG) {
    cat("Fin de la mano:\n")
    cat(" Player: ", paste(hand$cards, collapse = "-"), " (", handValue(hand$cards), ")\n")
    cat(" Dealer: ", paste(dealer, collapse = "-"), " (", handValue(dealer), ")\n")
    cat(" Ganancias: ", result, "\n")
  }
  
  return(result)
}

#-------------------------------- ESTRATEGIAS ---------------------------------#

simpleStrategy <- function(playerCards, dealerFaceCard, ...) {
  
  playerValue <- handValue(playerCards)
  
  if (handValue(dealerFaceCard) > 6 && playerValue < 17){
    action <- "H"
  }else{
    action <- "S"
  }
  
  return(action)
}


testStrategy <- function(playerCards, ...) {
  
  playerValue <- handValue(playerCards)
  
  if (length(playerCards) == 2 && playerCards[1] == playerCards[2]) {
    action <- "SP"  
  } else if (playerValue == 11) {
    action <- "D" 
  } else if (playerValue >= 17 && playerValue <= 21) {
    action <- "S"  
  } else if (playerValue < 12) {
    action <- "H"  
  } else {
    action <- "H"  
  }
  
  return(action)
}


ourLuckStrategy <- function(playerCards, dealerFaceCard, bet, 
                            updateBet = FALSE , 
                            previousResults = gameEnv$stats, ...) 
{
  
  currentBet <- bet
  
  if (updateBet) {
    if (length(previousResults) >= 2) {
      lastTwoResults <- tail(previousResults, 2)
      if (all(lastTwoResults < 0)) {
        currentBet <- min(currentBet + 10, gameEnv$stack)
      } else if (all(lastTwoResults > 0)) {
        currentBet <- max(currentBet - 10, 10)
      }
    }
  }
  
  gameEnv$currentBet <- currentBet
  action <- simpleStrategy(playerCards, dealerFaceCard)
  if(DEBUG){
    cat("Nueva apuesta:", currentBet, "\n")
  }
  return(action)
}

cardCountingStrategy <- function(playerCards, dealerFaceCard, bet, 
                                 updateBet = FALSE,...) {
  
  currentBet <- bet
  
  if (updateBet) {
    if (gameEnv$cardCount > 2) {
      currentBet <- min(bet * 2, gameEnv$stack) 
    } else if (gameEnv$cardCount < -2) {
      currentBet <- max(bet / 2, 10)  
    }
  }
  
  gameEnv$currentBet <- currentBet
  action <- simpleStrategy(playerCards, dealerFaceCard)
  if(DEBUG){
    cat("Nueva apuesta: ", currentBet, 
        " | Conteo de cartas: ", gameEnv$cardCount, "\n")
  }
  
  return(action)
}


optimalStrategy <- function(playerCards, dealerFaceCard, bet, 
                            updateBet = FALSE, ...) {
  
  playerValue <- handValue(playerCards)
  
  locAce <- playerCards == 1
  
  if (length(playerCards) == 2 && playerCards[1] == playerCards[2]) {
    type <- "pair"
    if (playerCards[1] == 1) {
      playerValue <- 2
    }
  } else if (sum(locAce) > 0 && (playerValue - sum(locAce)) > handValue(playerCards[!locAce])) {
    type <- "soft"
  } else {
    type <- "hard"
  }
  
  decision <- strategyTable[[type]][as.character(playerValue), as.character(dealerFaceCard)]
  
  if (length(decision) == 0 || is.na(decision)) {
    decision <- simpleStrategy(playerCards, dealerFaceCard)
  }
  
  action <- switch(decision,
                   "S" = "S",
                   "H" = "H",
                   "Ds" = if(length(playerCards) > 2) "S" else "D",
                   "SP" = if(length(playerCards) == 2) "SP" else "H",
                   "S")
  
  if (updateBet) {
    gameEnv$currentBet <- bet
  }
  
  return(action)
}


strategyTable <<- list(
  hard = data.frame(
    row.names = as.character(20:18),
    "2" = c("S", "S", "S"),
    "3" = c("S", "S", "S"),
    "4" = c("S", "S", "S"),
    "5" = c("S", "S", "S"),
    "6" = c("S", "S", "S"),
    "7" = c("S", "S", "S"),
    "8" = c("S", "S", "S"),
    "9" = c("S", "S", "S"),
    "10" = c("S", "S", "S"),
    "11" = c("S", "S", "S")
  ),
  soft = data.frame(
    row.names = as.character(20:18),
    "2" = c("S", "S", "S"),
    "3" = c("S", "S", "Ds"),
    "4" = c("S", "S", "Ds"),
    "5" = c("S", "S", "Ds"),
    "6" = c("S", "S", "Ds"),
    "7" = c("S", "S", "S"),
    "8" = c("S", "S", "S"),
    "9" = c("S", "S", "H"),
    "10" = c("S", "S", "H"),
    "11" = c("S", "S", "H")
  ),
  pair = data.frame(
    row.names = as.character(c(20, 18)),
    "2" = c("S", "SP"),
    "3" = c("S", "SP"),
    "4" = c("S", "SP"),
    "5" = c("S", "SP"),
    "6" = c("S", "SP"),
    "7" = c("S", "S"),
    "8" = c("S", "SP"),
    "9" = c("S", "SP"),
    "10" = c("S", "S"),
    "11" = c("S", "S")
  )
)

#-------------------------------- SIMULACION ----------------------------------#
simulateGames <- function(strategy, numShoes, colorp){
  initializeGame(gameConfig)
  
  startingStack <- gameConfig$defaultStack
  startingBet <- gameConfig$defaultBet
  
  allStacks <- numeric(0)
  
  cat("Shoes:", numShoes, "\n")
  for (shoe in 1:numShoes){
    createShoe(gameConfig$defaultDecks)
    
    for (hand in 1:40) {
      
      if(DEBUG){
        cat("Hand:", hand, "\n")
      }
      
      if(length(gameEnv$shoe)>45){
        playHand(strategy)
      }else{
        break 
      }
      
      if(DEBUG){
        cat("Stats: ", gameEnv$stats, "\n")
      }
      
      allStacks <- c(allStacks, gameEnv$stack)
      if(DEBUG){
        cat("Stacks: ", allStacks, "\n")
      }
    }
  }
  
  meanStack <- mean(allStacks)
  sdStack <- sd(allStacks)
  minStack <- min(allStacks)
  maxStack <- max(allStacks)
  impliedHouseEdge <- round((startingStack - meanStack) / ((40*numShoes) * startingBet) * 100, 5)
  
  cat("\n--- Reporte ---\n")
  cat("Media de stacks finales:", meanStack, "\n")
  cat("Desviación estándar:", sdStack, "\n")
  cat("Rango:",minStack, "-", maxStack, "\n")
  cat("Implied House Edge:", impliedHouseEdge, "%\n")
  
  
  ggplot(data.frame(allStacks), aes(x = allStacks)) +
    geom_histogram(bins = 20, fill = colorp, color = "black") +
    labs(title = "Distribución de Stacks", x = "Stack Final", y = "Frecuencia") +
    theme_minimal()
}
