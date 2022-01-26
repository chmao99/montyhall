#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   'create_game()'
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#'   Contestant selects a door for the first guess.
#'
#' @description
#'   `select_door` select a door randomly from doors
#'    numbered 1,2,3.
#'
#' @details
#'   The function simulates the process when contestant selects
#'   a door randomly.
#'
#' @param ... no arguments are used by the function.
#'
#' @return  The function returns a number between 1 and 3
#'   indicating the door initially opened by contestant.
#'
#' @examples
#'   'select_door()'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host opens goat door
#'
#' @description
#'   `open_goat_door(game, a.pick)` reveals a goat door from the
#'   2 doors not opened yet.
#'
#' @details
#'   The function is to reveal a goat door. If the contestant
#'   selected a car at the first guess, the rest 2 doors will be
#'   opened randomly. If a goat door was selected, the rest goat
#'   door will be opened as a hint.
#'
#' @param
#'   game:   a vector of 3 characters(2 "goats" and 1 "car")
#'           created by 'create_game()';
#'   a.pick: a number (between 1 to 3) for the door
#'           opened by the contestant at the first guess.
#' @return
#'   The function returns a number between 1 and 3 indicating
#'   the goat door revealed by host.
#'
#' @examples
#'   this.game <- create_game()
#'   this.game
#'   my.initial.pick <- select_door()
#'   my.initial.pick
#'   open_goat_door( this.game, my.initial.pick)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Contestant makes the final pick
#'
#' @description
#'   `change_door(stay=T, opened.door, a.pick )` makes the final pick depending upon
#'   if you intend to stay or switch.
#'
#' @details
#'   The function provides the contestant an opportunity
#'   to stay with their original selection or switch to
#'   the other unopened door.
#'
#' @param
#'   stay:        logical flag to determine if you intend to stay
#'                or switch;
#'   opened.door: a number (between 1 to 3) for the door revealed
#'                by host;
#'   a.pick:      a number (between 1 to 3) for the door opened by
#'                the contestant at the first guess.
#'
#' @return
#'   The function returns a number between 1 and 3 indicating
#'   the contestant's final pick.
#'
#' @examples
#'opened.door <- open_goat_door( this.game, my.initial.pick )
#'change_door( stay=T,
#'             opened.door=opened.door,
#'             a.pick=my.initial.pick )
#'change_door( stay=F,
#'             opened.door=opened.door,
#'             a.pick=my.initial.pick )
#'my.final.pick <- change_door( stay=F,
#'                              opened.door=opened.door,
#'                              a.pick=my.initial.pick )

#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine if the contestant has won.
#' @description
#'   `determine_winner( final.pick, game )`decides if you win
#'   the car or not
#' @details
#'   The function is to test if the contestant's final pick is
#'   the car or not. If it is the car, display "WIN", otherwise,
#'   display "LOSE".
#' @param
#'   final.pick: a number (between 1 to 3) for the door selected
#'               by the contestant finally;
#'   game:       a vector of 3 characters(2 "goats" and 1 "car")
#'               created by 'create_game()';
#' @return
#'   The function returns the result ("WIN" or "LOSE").
#' @examples
#'this.game
#'my.initial.pick
#'my.final.pick <- change_door( stay=T,
#'                              opened.door=opened.door,
#'                              a.pick=my.initial.pick )
#'determine_winner( final.pick=my.final.pick,
#'                  game=this.game )
#'my.final.pick <- change_door( stay=F,
#'                              opened.door=opened.door,
#'                              a.pick=my.initial.pick )
#'determine_winner( final.pick=my.final.pick,
#'                  game=this.game )

#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Play single round of Monty Hall Problem game
#' @description
#'   `play_game()`generates a Monty Hall Problem game, play it
#'   randomly and display the result of the game.
#' @details
#'   The function integrates all the functions of playing a
#'   Monty Hall Problem game, and plays a single game in order.
#' @param ... no arguments are used by the function.
#' @return
#'   The function returns the result ("WIN" or "LOSE") of both
#'   strategies (stay or switch) for a single round of play.
#' @examples
#'   "play_game()"
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Play multiple rounds of Monty Hall Problem game
#' @description
#'   `play_n_game()`generates the results of multiple rounds
#'    of random Monty Hall Problem game.
#' @details
#'   The function plays n rounds of Monty Hall Problem games.
#'   It reports the proportion of WIN and LOSE for both strategies
#'   (stay or switch) and lists the results of each game.
#' @param
#'   n:  a positive integer indicating number of times the
#'       game played.
#' @return
#'   a list of each game's results of both strategies (stay or switch) .
#' @examples
#'  "play_n_game( n = 1000)"
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
