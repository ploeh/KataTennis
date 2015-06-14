module Ploeh.Katas.Tennis

type Player = PlayerOne | PlayerTwo
type Point = Love | Fifteen | Thirty

type PointsData = { PlayerOnePoint : Point; PlayerTwoPoint : Point }
type FortyData = { Player : Player; OtherPlayerPoint : Point }

type Score =
| Points of PointsData
| Forty of FortyData
| Deuce
| Advantage of Player
| Game of Player

let other = function PlayerOne -> PlayerTwo | PlayerTwo -> PlayerOne

// Transitions

let scoreWhenGame winner = Game winner

let scoreWhenAdvantage advantagedPlayer winner =
    if advantagedPlayer = winner
    then Game winner
    else Deuce

let scoreWhenDeuce winner = Advantage winner

let scoreWhenForty current winner =
    if current.Player = winner
    then Game winner
    else Deuce