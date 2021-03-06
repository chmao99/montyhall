% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{open_goat_door}
\alias{open_goat_door}
\title{Host opens goat door}
\usage{
open_goat_door(game, a.pick)
}
\arguments{
\item{game:}{a vector of 3 characters(2 "goats" and 1 "car")
created by 'create_game()';
a.pick: a number (between 1 to 3) for the door
opened by the contestant at the first guess.}
}
\value{
The function returns a number between 1 and 3 indicating
the goat door revealed by host.
}
\description{
\code{open_goat_door(game, a.pick)} reveals a goat door from the
2 doors not opened yet.
}
\details{
The function is to reveal a goat door. If the contestant
selected a car at the first guess, the rest 2 doors will be
opened randomly. If a goat door was selected, the rest goat
door will be opened as a hint.
}
\examples{
  this.game <- create_game()
  this.game
  my.initial.pick <- select_door()
  my.initial.pick
  open_goat_door( this.game, my.initial.pick)

}
