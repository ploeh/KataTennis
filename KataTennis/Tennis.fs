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

let incrementPoint = function
    | Love -> Some Fifteen
    | Fifteen -> Some Thirty
    | Thirty -> None

let pointTo player point current =
    match player with
    | PlayerOne -> { current with PlayerOnePoint = point }
    | PlayerTwo -> { current with PlayerTwoPoint = point }

let pointFor player current =
    match player with
    | PlayerOne -> current.PlayerOnePoint
    | PlayerTwo -> current.PlayerTwoPoint

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
    else
        match incrementPoint current.OtherPlayerPoint with
        | Some p -> Forty { current with OtherPlayerPoint = p }
        | None -> Deuce

let scoreWhenPoints current winner = Forty {
    Player = winner
    OtherPlayerPoint = pointFor (other winner) current }