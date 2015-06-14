module Ploeh.Katas.TennisProperties

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open Tennis

[<Property>]
let ``Given game is over then it stays over`` (winner : Player) =
    let actual : Score = scoreWhenGame winner

    let expected = Game winner
    expected =! actual

[<Property>]
let ``Given advantage when advantaged player wins then score is correct``
    (advantagedPlayer : Player) =

    let actual : Score = scoreWhenAdvantage advantagedPlayer advantagedPlayer

    let expected = Game advantagedPlayer
    expected =! actual
