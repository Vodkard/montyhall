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
#'   create_game()
#'
#' @export

create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#'   Selects a door.
#'
#' @description
#'   `select_door` picks a number from 1 to 3,
#'   representing the three doors.
#'
#' @details
#'   First, the variable `doors` is created, holding the
#'   numbers 1 through 3. Then one of the numbers is
#'   selected randomly and put into the variable `a.pick`
#'   which is returned when the function is called.
#'
#' @param
#'   ... no arguments are used by the function.
#'
#' @return
#'   The function returns a numerical value between 1 and 3.
#'
#' @examples
#'   select_door()
#'
#' @export

select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Opens an unselected door with a goat behind it.
#'
#' @description
#'   `open_goat_door()` generates a number between 1 and 3
#'   representing a door with a goat behind it and not the
#'   door that was selected by the contestant.
#'
#' @details
#'   This function uses `if` statements to first determine
#'   if the contestant selected the car. If he did select
#'   the car, then a random goat door is opened. If the
#'   contestant chose a goat door, then the other goat door
#'   is opened.
#'
#' @param
#'   `'game` and `a.pick` are passed into the function.
#'   `game` holds the info on which door hold either a
#'   goat or car. `a.pick` is the chosen door by the
#'   contestant.
#'
#' @return
#'   The function returns numerical value representing an
#'   open door with a goat behind it.
#'
#' @examples
#'   open_goat_door( new.game, first.pick )
#'   open_goat_door( c("goat", "car", "goat"), 2 )
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
#'   Contestant chooses to stay or change doors.
#'
#' @description
#'   `change_door()` generates the contestant's final pick.
#'   They can iether stay with their first pick, or change
#'   with the last unopened door.
#'
#' @details
#'   This function uses if statements that determine if the
#'   contestant stayed or changed doors. If the contested
#'   stayed, then it turns the contestants inital door pick into
#'   a final pick that is returned. If the contestant did not
#'   stay, then it passes the final unopened door into the
#'   final pick and returns that door instead.
#'
#' @param
#'   `stay` - This tells the function if the contestant stayed
#'   with their initial pick or not.
#'   `opened.door` - This tells the function which door was opened.
#'   `a.pick` - This tells the function what the contestant's
#'   inital door pick was.
#'
#' @return
#'   The functions returns a number between 1 and 3 that represents
#'   the contestants final door pick.
#'
#' @examples
#'   change_door( stay=T, opened.door, first.pick )
#'   change_door( stay=F, 3, 1)
#'
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
#'   Contestant is determined either a winner or not.
#' @description
#'   `determine_winner` uses the contestant's final door pick
#'   along with the knowledge of where the car is to generate
#'   whether the contestant won or lost.
#'
#' @details
#'  The function uses if statements, along with the passed
#'  paramenters of the game setup and the final pick. It checks
#'  to see if the conetsant's final pick was the car or the
#'  final goat. If it is the car, it returns thjew string `WIN`.
#'  If it was the goat, it reuturns the string `LOSE`.
#'
#' @param
#'   `final.pick` - This is the contestant's final door choice,
#'   represnted by the number 1 through 3.
#'   `game` - This is a dataset that holds the order of where
#'   the two goats and one car are.
#'
#' @return
#'   The function returns either the string `WIN` or `LOSE` based
#'   on whether the contestant chose the door with the car or the
#'   door with a goat.
#'
#' @examples
#'   determine_winner( final.pick.stay, new.game  )
#'   determine_winner( 3, c("car", "goat", "goat"))
#'
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
#'   Plays a single game
#'
#' @description
#'   `play_game` generates a full game with both outcomes from either
#'   staying with the intial door, or switching doors.
#'
#' @details
#'   This function first creates a new game along with the
#'   contestant's first door pick. It then opens a goat door.
#'   Next it creates a final pick for poss possiblities of
#'   staying vs not staying. Afterwards, it determines which
#'   of the two final picks was the winner and loser. Finally,
#'   it creates the results in a data frame and retuns the results.
#'
#' @param
#'   ... no arguments are used by the function.
#'
#' @return
#'   The function returns a data frame showing the results of
#'   a single game with the winning and losing outcomes from
#'   staying or not staying.
#'
#' @examples
#'   play_game()
#'
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
#'   Runs the Monty Hall game a given amount of times and shows results.
#'
#' @description
#'   `play_n_games` plays the Monty Hall game a number of
#'   times based on the numerical parameter you pass to it.
#'   It then shows the proportional results.
#'
#' @details
#'   The function uses a `for` loop that continues based on
#'   the parameter passed into the function. It records each
#'   outcome in a results list. When the loop finishes, it
#'   creates a table showing the proportional results.
#'
#' @param
#'   `n` - This determines the number of times the game is run.
#'
#' @return
#'   The function returns a results table showing the proportional
#'   results of `n` runs of the game.
#'
#' @examples
#'   play_n_games(30)
#'   play_n_games(100)
#'
#' @export

play_n_games <- function( n )
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
