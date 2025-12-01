\name{glicko}
\alias{glicko}
\title{The Glicko Rating System}
\description{
  Implements the Glicko rating system for estimating the relative 
  skill level of players in two-player games such as chess. It
  extends the Elo method by including a deviation parameter for
  each player, representing uncertainty on the rating.   
}
\usage{
glicko(x, status = NULL, init = c(2200,300), gamma = 0, cval = 15, 
    history = FALSE, sort = TRUE, rdmax = 350, \dots)
}
\arguments{
  \item{x}{A data frame containing four variables: (1) a numeric
    vector denoting the time period in which the game took place
    (2) a numeric or character identifier for player one (3)
    a numeric or character identifier for player two and (4)
    the result of the game expressed as a number, typically
    equal to one for a player one win, zero for a player two
    win and one half for a draw.}
  \item{status}{A data frame with the current status of the
    system. If not \code{NULL}, this needs to be a data frame 
    in the form of the \code{ratings} component of the returned 
    list, containing variables named \code{Player}, \code{Rating},
    \code{Deviation}, and optionally \code{Games}, \code{Win}, 
	\code{Draw}, \code{Loss} and \code{Lag}, which are set to zero 
    if not given.}
  \item{init}{The rating vector at which to initialize a new player 
    not appearing in \code{status}. Must be a vector of length two
    giving the initial rating and initial deviation respectively. 
    If different initializations for different players are 
    required, this can be done using \code{status}. The initial
	deviation cannot be greater than \code{rdmax}.}
  \item{gamma}{A player one advantage parameter; either a single
    value or a numeric vector equal to the number of rows in 
	\code{x}. Positive values favour player one, while negative 
	values favour player two. This could represent the advantage 
	of playing at home, or the advantage of playing white for chess. 
	Note that this is not passed to \code{\link{predict.rating}}, 
	which has its own \code{gamma} parameter.}
  \item{cval}{The c parameter, which controls the increase in the
    player deviations across time. Must be a single non-negative 
    number.}
  \item{history}{If \code{TRUE} returns the entire history for each 
    period in the component \code{history} of the returned list.}
  \item{sort}{If \code{TRUE} sort the results by rating (highest
    to lowest). If \code{FALSE} sort the results by player.}
  \item{rdmax}{The maximum value allowed for the rating deviation.}
  \item{\dots}{Not used.}
}
\details{
  The Glicko rating system is a method for evaluating the skill 
  of players. It is more complex than Elo but typically yields 
  better predictions. 
  Default values are roughly optimized for the chess data analyzed 
  in the file doc/ChessRatings.pdf, using the binomial deviance 
  criterion. A player one advantage parameter has been added to 
  the original definition in the reference. A player one advantage 
  parameter is also used for prediction purposes in 
  \code{\link{predict.rating}}.
  In this implementation, rating deviances increase at the 
  beginning of the updating period, and decrease at the end.
  This is slightly different from the Glicko-2 implementation, 
  where deviance increases for active players may occur at the end 
  of the previous period. In both implementations there will be
  an initial increase for existing but previously inactive players. 
}
\value{
  A list object of class \code{"rating"} with the following 
  components

  \item{ratings}{A data frame of the results at the end of the 
    final time period. The variables are self explanatory except
    for \code{Lag}, which represents the number of time periods
    since the player last played a game. This is equal to zero
    for players who played in the latest time period, and is
    also zero for players who have not yet played any games.}
  \item{history}{A three dimensional array, or \code{NULL} if 
    \code{history} is \code{FALSE}. The row dimension is the 
    players, the column dimension is the time periods. 
    The third dimension gives different parameters.}
  \item{gamma}{The player one advantage parameter.}
  \item{cval}{The c parameter.}
  \item{type}{The character string \code{"Glicko"}.}
}
\references{
  Glickman, M.E. (1999) 
  Parameter estimation in large dynamic paired comparison experiments. 
  J. R. Stat. Soc. Ser. C: Applied Statistics, 48(3), 377-394.
}

\seealso{\code{\link{elo}}, \code{\link{glicko2}}, \code{\link{steph}}}

\examples{
afl <- aflodds[,c(2,3,4,7)]
robj <- glicko(afl)
robj

robj <- glicko(afl[afl$Week==1,])
for(i in 2:max(afl$Week)) robj <- glicko(afl[afl$Week==i,], robj$ratings)
robj
}
\keyword{models}




