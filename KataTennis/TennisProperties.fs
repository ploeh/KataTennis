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

[<Property>]
let ``Given advantage when other player wins then score is correct``
    (advantagedPlayer : Player) =

    let actual = scoreWhenAdvantage advantagedPlayer (other advantagedPlayer)
    Deuce =! actual

[<Property>]
let ``Given deuce when player wins then score is correct``
    (winner : Player) =

    let actual : Score = scoreWhenDeuce winner

    let expected = Advantage winner
    expected =! actual

[<Property>]
let ``Given player: 40 when player wins then score is correct``
    (current : FortyData) =

    let actual = scoreWhenForty current current.Player

    let expected = Game current.Player
    expected =! actual

[<Property>]
let ``Given player: 40 - other: 30 when other wins then score is correct``
    (current : FortyData) =

    let current = { current with OtherPlayerPoint = Thirty }
    let actual = scoreWhenForty current (other current.Player)
    Deuce =! actual

[<Property>]
let ``Given player: 40 - other: < 30 when other wins then score is correct``
    (current : FortyData) =

    let opp = Gen.elements [Love; Fifteen] |> Arb.fromGen
    Prop.forAll opp (fun otherPlayerPoint ->
        let current = { current with OtherPlayerPoint = otherPlayerPoint }

        let actual = scoreWhenForty current (other current.Player)

        let expected =
            incrementPoint current.OtherPlayerPoint
            |> Option.map (fun np -> { current with OtherPlayerPoint = np })
            |> Option.map Forty
        expected =! Some actual)

[<Property>]
let ``Given player: 30 when player wins then score is correct``
    (current : PointsData)
    (winner : Player) = 

    let current = (Thirty |> pointTo winner) current

    let actual : Score = scoreWhenPoints current winner

    let expected = Forty {
        Player = winner
        OtherPlayerPoint = pointFor (other winner) current }
    expected =! actual

[<Property>]
let ``Given player: <30 when player wins then score is correct``
    (current : PointsData)
    (winner : Player) =

    let pp = Gen.elements [Love; Fifteen] |> Arb.fromGen
    Prop.forAll pp (fun playerPoint ->
        let current = pointTo winner playerPoint current

        let actual = scoreWhenPoints current winner

        let expectedPlayerPoint =
            current
            |> pointFor winner
            |> incrementPoint
        let expected =
            expectedPlayerPoint
            |> Option.map (fun p -> current |> pointTo winner p |> Points)
        expected =! Some actual)

[<Property>]
let ``score returns a value`` (current : Score) (winner : Player) =
    let actual : Score = score current winner
    true // Didn't crash - this is mostly a boundary condition test

// The following five properties aren't particularly useful, because they
// mainly reproduce the implementation details of the score function. I've
// added them here in order to demonstrate that this can be done, but unless
// you're in an extremely fault-sensitive setting (medical equipment, guidance
// software for vehicles, etc.) they most likely add little to negative value.
// After all, the tests contain more code than the implementation itself.
// Related: http://blog.ploeh.dk/2013/04/02/why-trust-tests

[<Property>]
let ``score Points returns correct result`` points winner =
    let actual = score (Points points) winner

    let expected = scoreWhenPoints points winner
    expected =! actual

[<Property>]
let ``score Forty returns correct result`` forty winner =
    let actual = score (Forty forty) winner

    let expected = scoreWhenForty forty winner
    expected =! actual

[<Property>]
let ``score Deuce returns correct result`` winner =
    let actual = score Deuce winner

    let expected = scoreWhenDeuce winner
    expected =! actual

[<Property>]
let ``score Advantage returns correct result`` advantagedPlayer winner =
    let actual = score (Advantage advantagedPlayer) winner

    let expected = scoreWhenAdvantage advantagedPlayer winner
    expected =! actual

[<Property>]
let ``score Game returns correct result`` gameWinner pseudoWinner =
    let actual = score (Game gameWinner) pseudoWinner

    let expected = scoreWhenGame gameWinner
    expected =! actual

let isPoints =    function Points _    -> true | _ -> false
let isForty =     function Forty  _    -> true | _ -> false
let isDeuce =     function Deuce       -> true | _ -> false
let isAdvantage = function Advantage _ -> true | _ -> false
let isGame =      function Game _      -> true | _ -> false

[<Property>]
let ``A game with less then four balls isn't over`` (wins : Player list) =
    let actual : Score = wins |> Seq.truncate 3 |> scoreSeq
    test <@ actual |> (not << isGame) @>

[<Property>]
let ``A game with less than six balls can't be Deuce`` (wins : Player list) =
    let actual = wins |> Seq.truncate 5 |> scoreSeq
    test <@ actual |> (not << isDeuce) @>

[<Property>]
let ``A game with less than seven balls can't have any player with advantage``
    (wins : Player list) =

    let actual = wins |> Seq.truncate 6 |> scoreSeq
    test <@ actual |> (not << isAdvantage) @>

let genListLongerThan n =
    let playerGen = Arb.generate<Player>
    let nPlayers = playerGen |> Gen.listOfLength (n + 1)
    let morePlayers = playerGen |> Gen.listOf
    Gen.map2 (@) nPlayers morePlayers

[<Property>]
let ``A game with more than four balls can't be Points`` () =
    let moreThanFourBalls = genListLongerThan 4 |> Arb.fromGen
    Prop.forAll moreThanFourBalls (fun wins ->

        let actual = scoreSeq wins

        test <@ actual |> (not << isPoints) @>)

[<Property>]
let ``A game with more than five balls can't be Forty`` () =
    let moreThanFiveBalls = genListLongerThan 5 |> Arb.fromGen
    Prop.forAll moreThanFiveBalls (fun wins ->

        let actual = scoreSeq wins

        test <@ actual |> (not << isForty) @>)

[<Property>]
let ``A game where one player wins all balls is over in four balls`` (player) =
    let fourWins = Seq.init 4 (fun _ -> player)
    
    let actual = scoreSeq fourWins

    let expected = Game player
    expected =! actual

[<Property>]
let ``A game where players alternate never ends`` firstWinner =
    let alternateWins = 
        firstWinner
        |> Gen.constant
        |> Gen.map (fun p -> [p; other p])
        |> Gen.listOf
        |> Gen.map List.concat
        |> Arb.fromGen
    Prop.forAll alternateWins (fun wins ->
        
        let actual = scoreSeq wins
        
        test <@ actual |> (not << isGame) @>)

[<Property>]
let ``other returns a different player`` player = player <>! other player

[<Property>]
let ``other other returns same player`` player = player =! other (other player)