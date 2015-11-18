#load "Tennis.fs"

open Ploeh.Katas.Tennis

let firstBall = score newGame PlayerTwo
let secondBall = score firstBall PlayerOne

let simpleGame =
    scoreSeq [
        PlayerTwo; PlayerTwo; PlayerOne; PlayerTwo; PlayerOne; PlayerOne;
        PlayerTwo; PlayerTwo]

let displayGame = simpleGame |> scoreToString "Björn Borg" "John McEnroe"