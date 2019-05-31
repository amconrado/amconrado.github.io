{-- Game.elm: contains the model-view-controller for
     running and displaying the actual game. --}

--module Game exposing (main)
module Game exposing (..)
import Browser
import Browser.Events as Events
import Html exposing (Html, button)
import Html.Events exposing (onClick)
import Random exposing (Generator)
import Time
import Debug
import Blocks exposing (..)
import Json.Decode as Decode
import Color
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Update.Extra as Update

{--- Game board:
        - 10x20 squares (width x height)
 --}

main : Program Flags Model Msg
main =
     Browser.element
        {init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    {activePiece : Tetromino
    , nextPiece : Tetromino
    , inactiveBlocks : List (List Block)
    , score : Int
    , activePiece2 : Tetromino
    , nextPiece2 : Tetromino
    , inactiveBlocks2 : List (List Block)
    , score2 : Int
    , speed : Float
    , gameState : GameState
    , difficulty : Difficulty
    , gameType : GameType
    , style : Style
    }

type Style = Vapor
            | Lofi

type Msg = Tick
           |DropOneSquare 
           | DropCompletely
           | RotateLeft
           | RotateRight
           | MoveLeft
           | MoveRight
           | RandomTetromino Tetromino
           | DropOneSquareP2
           | DropCompletelyP2
           | RotateLeftP2
           | RotateRightP2
           | MoveLeftP2
           | MoveRightP2
           | RandomTetrominoP2 Tetromino
           | EndGame
           | InvalidKey 
           | ResetGame
           | ChangeDifficulty
           | ChangeGameType
           | ChangeStyle


type GameState = Playing
                 | Ended

type Difficulty = Easy
                 | Medium
                 | Hard

type GameType = SinglePlayer
                | TwoPlayer

type alias Flags = ()

modelToString : Model -> String
modelToString model =
    case model.gameState of
        Playing ->
            "Playing"
        Ended ->
            "Ended"

init : Flags -> (Model, Cmd Msg)
init () =
    (initModel, Random.generate RandomTetromino tetrominoGenerator)

initModel : Model
initModel =
    {activePiece = E
    , nextPiece = E
    , inactiveBlocks = List.repeat 20 []
    , activePiece2 = E
    , nextPiece2 = E
    , inactiveBlocks2 = List.repeat 20 []
    , score = 0
    , score2 = 0
    , speed = 1000
    , gameState = Playing
    , difficulty = Medium
    , gameType = SinglePlayer
    , style = Lofi
    }

-- Keyboard decoding code/logic pulled from RandomNumbersUI.elm in lecture notes
keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string

keyParser : String -> Msg
keyParser key =
    if key == "a" then
        MoveLeft
    else if key == "w" then
        DropCompletely
    else if key == "d" then
        MoveRight
    else if key == "s" then
        DropOneSquare
    else if key == "q" then
        RotateLeft
    else if key == "e" then
        RotateRight
    else if key == "j" then
        MoveLeftP2
    else if key == "i" then
        DropCompletelyP2
    else if key == "l" then
        MoveRightP2
    else if key == "k" then
        DropOneSquareP2
    else if key == "u" then
        RotateLeftP2
    else if key == "o" then
        RotateRightP2
    else
        InvalidKey

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch 
    [
        Time.every model.speed (\_ -> Tick)
        , Events.onKeyDown (Decode.map keyParser keyDecoder)
    ]

{-- tetrominoGenerator : generate a random Tetromino, located
at the middle top of the board --}
tetrominoGenerator : Generator Tetromino
tetrominoGenerator =
    let
        makeTetromino i =
            case i of
                0 -> initO
                1 -> initI
                2 -> initJ
                3 -> initL
                4 -> initT
                5 -> initZ
                6 -> initS
                _ -> Debug.todo "tetrominoGenerator invalid case"
     in
        Random.map makeTetromino (Random.int 0 6)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model.gameState of
        Ended -> 
            case msg of
                ResetGame -> (initModel, Cmd.none)
                _ -> (model, Cmd.none)
        _ ->
            case msg of
                Tick -> -- On Tick, drop the active piece(s) one square
                    if (model.gameState == Playing && model.gameType == SinglePlayer) then
                        case model.difficulty of
                            Easy -> update DropOneSquare {model | speed = model.speed}
                            Medium -> update DropOneSquare {model | speed = (Basics.max (model.speed - 1) 500)}
                            Hard -> update DropOneSquare {model | speed = (Basics.max (model.speed - 10) 200)}
                    else if (model.gameState == Playing && model.gameType == TwoPlayer) then
                        case model.difficulty of
                            Easy -> 
                                (update DropOneSquare {model | speed = model.speed})
                                |> Update.andThen update DropOneSquareP2
                            Medium -> 
                                (update DropOneSquare {model | speed = (Basics.max (model.speed - 1) 500)})
                                |> Update.andThen update DropOneSquareP2

                            Hard -> 
                                (update DropOneSquare {model | speed = (Basics.max (model.speed - 10) 200)})
                                |> Update.andThen update DropOneSquareP2
                    else
                        (model, Cmd.none)
                DropOneSquare ->
                    case attemptOneSquareDrop model.activePiece model.inactiveBlocks of
                        (Nothing, updatedRows) -> -- Collision occured
                            ({model | inactiveBlocks = updatedRows, activePiece = E}, 
                             Random.generate RandomTetromino tetrominoGenerator)
                        (Just newTet, _) ->
                            ({model | activePiece = newTet}, Cmd.none)
                DropCompletely -> 
                    ({model | inactiveBlocks = (attemptFullDrop model.activePiece model.inactiveBlocks), 
                                               activePiece = E},
                    Random.generate RandomTetromino tetrominoGenerator)
                RotateLeft -> 
                    case rotateLeft model.activePiece of
                        E -> (model, Random.generate RandomTetromino tetrominoGenerator)
                        Piece blocks color orient -> 
                            if checkCollisions blocks model.inactiveBlocks then
                                (model, Cmd.none)
                            else
                                ({model | activePiece = Piece blocks color orient}, Cmd.none)
                RotateRight ->
                    case rotateLeft model.activePiece of
                        E -> (model, Random.generate RandomTetromino tetrominoGenerator)
                        Piece blocks color orient -> 
                            if checkCollisions blocks model.inactiveBlocks then
                                (model, Cmd.none)
                            else
                                ({model | activePiece = Piece blocks color orient}, Cmd.none)
                MoveLeft ->
                    case shiftLeft model.activePiece of
                        E -> (model, Random.generate RandomTetromino tetrominoGenerator)
                        Piece blocks color orient -> 
                            if checkCollisions blocks model.inactiveBlocks then
                                (model, Cmd.none)
                            else
                                ({model | activePiece = Piece blocks color orient}, Cmd.none)
                MoveRight -> 
                    case shiftRight model.activePiece of
                        E -> (model, Random.generate RandomTetromino tetrominoGenerator)
                        Piece blocks color orient -> 
                            if checkCollisions blocks model.inactiveBlocks then
                                (model, Cmd.none)
                            else
                                ({model | activePiece = Piece blocks color orient}, Cmd.none)
                RandomTetromino piece ->
                    case (piece, model.nextPiece) of
                        (Piece blocks _ _, Piece _ _ _) -> 
                            let newscore = model.score + 10 in
                            if checkCollisions blocks model.inactiveBlocks then
                                update EndGame {model | activePiece = piece}
                            else
                                ({model | activePiece = model.nextPiece, nextPiece = piece, score= newscore}, Cmd.none)
                        (Piece blocks _ _, E) -> -- this case happens when game just started out
                            ({model | nextPiece = piece}, Random.generate RandomTetromino tetrominoGenerator)
                        (_,_) -> ({model | activePiece = piece}, Cmd.none)
                DropOneSquareP2 ->
                    case attemptOneSquareDrop model.activePiece2 model.inactiveBlocks2 of
                        (Nothing, updatedRows) -> -- Collision occured
                            ({model | inactiveBlocks2 = updatedRows, activePiece = E}, 
                             Random.generate RandomTetrominoP2 tetrominoGenerator)
                        (Just newTet, _) ->
                            ({model | activePiece2 = newTet}, Cmd.none)
                DropCompletelyP2 -> 
                    ({model | inactiveBlocks2 = (attemptFullDrop model.activePiece2 model.inactiveBlocks2), 
                                               activePiece2 = E},
                    Random.generate RandomTetrominoP2 tetrominoGenerator)
                RotateLeftP2 -> 
                    case rotateLeft model.activePiece2 of
                        E -> (model, Random.generate RandomTetrominoP2 tetrominoGenerator)
                        Piece blocks color orient -> 
                            if checkCollisions blocks model.inactiveBlocks2 then
                                (model, Cmd.none)
                            else
                                ({model | activePiece2 = Piece blocks color orient}, Cmd.none)
                RotateRightP2 ->
                    case rotateLeft model.activePiece2 of
                        E -> (model, Random.generate RandomTetrominoP2 tetrominoGenerator)
                        Piece blocks color orient -> 
                            if checkCollisions blocks model.inactiveBlocks2 then
                                (model, Cmd.none)
                            else
                                ({model | activePiece2 = Piece blocks color orient}, Cmd.none)

                MoveLeftP2 ->
                    case shiftLeft model.activePiece2 of
                        E -> (model, Random.generate RandomTetrominoP2 tetrominoGenerator)
                        Piece blocks color orient -> 
                            if checkCollisions blocks model.inactiveBlocks2 then
                                (model, Cmd.none)
                            else
                                ({model | activePiece2 = Piece blocks color orient}, Cmd.none)
                MoveRightP2 -> 
                    case shiftRight model.activePiece2 of
                        E -> (model, Random.generate RandomTetrominoP2 tetrominoGenerator)
                        Piece blocks color orient -> 
                            if checkCollisions blocks model.inactiveBlocks2 then
                                (model, Cmd.none)
                            else
                                ({model | activePiece2 = Piece blocks color orient}, Cmd.none)
                RandomTetrominoP2 piece ->
                    case (piece, model.nextPiece2) of
                        (Piece blocks _ _, Piece _ _ _) -> 
                            let newscore2 = model.score2 + 10 in
                            if checkCollisions blocks model.inactiveBlocks2 then
                                update EndGame {model | activePiece2 = piece}
                            else
                                ({model | activePiece2 = model.nextPiece2, nextPiece2 = piece, score2 = newscore2}, Cmd.none)
                        (Piece blocks _ _, E) -> -- this case happens when game just started out
                            ({model | nextPiece2 = piece}, Random.generate RandomTetrominoP2 tetrominoGenerator)
                        (_,_) -> ({model | activePiece2 = piece}, Cmd.none)
                EndGame ->
                    ({model | gameState = Ended}, Cmd.none)
                InvalidKey ->
                    (model, Cmd.none)
                ResetGame ->
                    ({initModel | gameType = model.gameType}, Cmd.none)
                ChangeDifficulty ->
                    case model.difficulty of
                        Easy -> ({model | difficulty = Medium}, Cmd.none)
                        Medium -> ({model | difficulty = Hard}, Cmd.none)
                        Hard -> ({model | difficulty = Easy}, Cmd.none)
                ChangeGameType ->
                    case model.gameType of
                        SinglePlayer -> ({initModel | gameType = TwoPlayer}, Cmd.none)
                        TwoPlayer -> ({initModel | gameType = SinglePlayer}, Cmd.none)
                ChangeStyle ->
                    case model.style of
                        Vapor -> ({model | style = Lofi}, Cmd.none)
                        Lofi -> ({model | style = Vapor}, Cmd.none)

resetButton: Html Msg
resetButton = button [onClick ResetGame
                                     , Html.Attributes.style "background-color" "red"
                                     , Html.Attributes.style "height" "60px"
                                     , Html.Attributes.style "text-align" "center"
                                     , Html.Attributes.style "position" "absolute"
                                     , Html.Attributes.style "left" "50px"
                                     , Html.Attributes.style "top" "650px"] [ text "R E S E T  G A M E" ]

view : Model -> Html Msg
view model =
    case model.gameState of
        Ended ->
            let
                messages = renderActivePiece model.style model.activePiece
                svgMessage = 
                    svg
                    [ width "1500"
                    , height "700"
                    , viewBox "0 0 1200 700"
                    ]
                        ((renderGameBoard model) ++
                        (renderGameOver model) ++
                        [resetButton])

                difficultyButton = renderDifficultyButton model.difficulty
                gameTypeButton = renderGameTypeButton model.gameType
                gameStyleButton = renderGameStyleButton model.style
            in
                Html.div [Html.Attributes.style "background-image" (imageChooser model.style), 
                  Html.Attributes.style "background-position" "center center",
                  Html.Attributes.style "background-size" "cover",
                  Html.Attributes.style "margin" "0",
                  Html.Attributes.style "padding" "0"] [svgMessage, resetButton, difficultyButton, gameTypeButton, gameStyleButton]
        Playing ->
            let
                messages = renderActivePiece model.style model.activePiece
                svgMessage = 
                    svg
                    [ width "1500"
                    , height "700"
                    , viewBox "0 0 1200 700"
                    ]
                    (renderGameBoard model)
                difficultyButton = renderDifficultyButton model.difficulty
                gameTypeButton = renderGameTypeButton model.gameType
                gameStyleButton = renderGameStyleButton model.style
            in
                Html.div [Html.Attributes.style "background-image" (imageChooser model.style), 
                  Html.Attributes.style "background-position" "center center",
                  Html.Attributes.style "background-size" "cover",
                  Html.Attributes.style "margin" "0",
                  Html.Attributes.style "padding" "0"] [svgMessage, resetButton, difficultyButton, gameTypeButton, gameStyleButton]


{-- Functions to render buttons --}

renderDifficultyButton : Difficulty -> Html Msg
renderDifficultyButton difficulty =
    case difficulty of 
        Easy ->
            button [onClick ChangeDifficulty
                   , Html.Attributes.style "background-color" "rgb(181,243,233)"
                   , Html.Attributes.style "height" "60px"
                   , Html.Attributes.style "width" "225px"
                   , Html.Attributes.style "text-align" "center"
                   , Html.Attributes.style "position" "absolute"
                   , Html.Attributes.style "left" "200px"
                   , Html.Attributes.style "top" "650px"] [ text "D I F F I C U L T Y :  E A S Y" ]
        Medium ->
            button [onClick ChangeDifficulty
                   , Html.Attributes.style "background-color" "rgb(172,189,233)"
                   , Html.Attributes.style "height" "60px"
                   , Html.Attributes.style "width" "225px"
                   , Html.Attributes.style "text-align" "center"
                   , Html.Attributes.style "position" "absolute"
                   , Html.Attributes.style "left" "200px"
                   , Html.Attributes.style "top" "650px"] [ text "D I F F I C U L T Y :  M E D I U M" ]
        Hard ->
            button [onClick ChangeDifficulty
                   , Html.Attributes.style "background-color" "rgb(185,172,233)"
                   , Html.Attributes.style "height" "60px"
                   , Html.Attributes.style "width" "225px"
                   , Html.Attributes.style "text-align" "center"
                   , Html.Attributes.style "position" "absolute"
                   , Html.Attributes.style "left" "200px"
                   , Html.Attributes.style "top" "650px"] [ text "D I F F I C U L T Y :  H A R D" ]

renderGameTypeButton : GameType -> Html Msg
renderGameTypeButton gameType = 
    case gameType of 
        SinglePlayer ->
            button [onClick ChangeGameType
                   , Html.Attributes.style "background-color" "rgb(255,255,255)"
                   , Html.Attributes.style "height" "60px"
                   , Html.Attributes.style "width" "200px"
                   , Html.Attributes.style "text-align" "center"
                   , Html.Attributes.style "position" "absolute"
                   , Html.Attributes.style "left" "450px"
                   , Html.Attributes.style "top" "650px"] [ text "M U L T I P L A Y E R" ]
        TwoPlayer ->
            button [onClick ChangeGameType
                   , Html.Attributes.style "background-color" "rgb(255,255,255)"
                   , Html.Attributes.style "height" "60px"
                   , Html.Attributes.style "width" "200px"
                   , Html.Attributes.style "text-align" "center"
                   , Html.Attributes.style "position" "absolute"
                   , Html.Attributes.style "left" "450px"
                   , Html.Attributes.style "top" "650px"] [ text "S I N G L E P L A Y E R" ]

renderGameStyleButton : Style -> Html Msg
renderGameStyleButton s = 
    case s of 
        Lofi ->
            button [onClick ChangeStyle
                   , Html.Attributes.style "background-color" "rgb(135,95,96)"
                   , Html.Attributes.style "height" "60px"
                   , Html.Attributes.style "width" "200px"
                   , Html.Attributes.style "text-align" "center"
                   , Html.Attributes.style "position" "absolute"
                   , Html.Attributes.style "left" "675px"
                   , Html.Attributes.style "top" "650px"] [ text "V A P O R W A V E" ]
        Vapor ->
            button [onClick ChangeStyle
                   , Html.Attributes.style "background-color" "rgb(255, 153, 255)"
                   , Html.Attributes.style "height" "60px"
                   , Html.Attributes.style "width" "200px"
                   , Html.Attributes.style "text-align" "center"
                   , Html.Attributes.style "position" "absolute"
                   , Html.Attributes.style "left" "675px"
                   , Html.Attributes.style "top" "650px"] [ text "L O F I" ]


{-- Functions to render game board, scoreboard, and next piece --}

renderGameBoard : Model -> List (Svg msg)
renderGameBoard model =
    case model.gameType of
        SinglePlayer ->
            renderGameBoardSinglePlayer model
        TwoPlayer ->
            renderGameBoardsTwoPlayer model

renderGameBoardSinglePlayer : Model -> List (Svg msg)
renderGameBoardSinglePlayer model =
    [ rect
        [ x "600"
        , y "0"
        , width "300"
        , height "600"
        , rx "0"
        , ry "0"
        , fill "gray"
        , stroke "black"
        , strokeWidth "1"
        ][]] ++ 
      (renderActivePiece model.style model.activePiece) ++
      (renderAllRows model.style model.inactiveBlocks) ++
      [ rect
        [ x "400"
        , y "0"
        , width "200"
        , height "150"
        , rx "0"
        , ry "0"
        , fill "gray"
        , stroke "black"
        , strokeWidth "1"][],
        Svg.text_
        [ x "425"
        , y "20"
        , fontFamily "monospace"
        , fontSize "15"
        , fill "white"]
        [Svg.text "N E X T    P I E C E"]] ++
        (renderNextPiece model.style model.nextPiece) ++
        [rect
        [ x "1000"
        , y "300"
        , width "200"
        , height "150"
        , rx "0"
        , ry "0"
        , fill "gray"
        , stroke "black"
        , strokeWidth "1"][],
        Svg.text_
        [ x "1035"
        , y "350"
        , fontFamily "monospace"
        , fontSize "26"]
        [Svg.text "S C O R E"],
        Svg.text_
        [ x "1060"
        , y "400"
        , fontFamily "monospace"
        , fontSize "26"]
        [Svg.text (String.fromInt model.score)]]++
        [rect
        [ x "-100"
        , y "100"
        , width "200"
        , height "200"
        , rx "0"
        , ry "0"
        , fill "gray"
        , stroke "black"
        , strokeWidth "1"][],
        Svg.text_
        [ x "-80"
        , y "150"
        , fontFamily "monospace"
        , fontSize "26"]
        [Svg.text "T E T R I S"],
         Svg.text_
        [ x "-80"
        , y "170"
        , fontFamily "monospace"
        , fontSize "14"]
        [Svg.text "SHIFT L/R = A & D"],
         Svg.text_
        [ x "-80"
        , y "190"
        , fontFamily "monospace"
        , fontSize "14"]
        [Svg.text "SOFT DROP = S"],
         Svg.text_
        [ x "-80"
        , y "210"
        , fontFamily "monospace"
        , fontSize "14"]
        [Svg.text "HARD DROP = W"],
         Svg.text_
        [ x "-80"
        , y "230"
        , fontFamily "monospace"
        , fontSize "14"]
        [Svg.text "ROTATE L/R = Q & E"],
         Svg.text_
        [ x "-80"
        , y "250"
        , fontFamily "monospace"
        , fontSize "14"]
        [Svg.text "P2 CONTROLS = IJKL"]
        ]

renderGameBoardsTwoPlayer : Model -> List (Svg msg)
renderGameBoardsTwoPlayer model =
    [ rect -- P1 Board
        [ x "250" -- 600
        , y "0"
        , width "300"
        , height "600"
        , rx "0"
        , ry "0"
        , fill "gray"
        , stroke "black"
        , strokeWidth "1"
        ][]] ++ 
      (renderActivePieceP1 model.style model.activePiece) ++
      (renderAllRowsP1 model.style model.inactiveBlocks) ++
      [ rect
        [ x "50" -- 400
        , y "0"
        , width "200"
        , height "150"
        , rx "0"
        , ry "0"
        , fill "gray"
        , stroke "black"
        , strokeWidth "1"][],
        Svg.text_
        [ x "75" -- 425
        , y "20"
        , fontFamily "monospace"
        , fontSize "15"
        , fill "white"]
        [Svg.text "N E X T    P I E C E"]] ++
        (renderNextPieceP1 model.style model.nextPiece) ++
        [rect -- Score Box
        [ x "50"
        , y "300"
        , width "200"
        , height "150"
        , rx "0"
        , ry "0"
        , fill "gray"
        , stroke "black"
        , strokeWidth "1"][],
        Svg.text_ -- Score Text
        [ x "85"
        , y "350"
        , fontFamily "monospace"
        , fontSize "26"]
        [Svg.text "S C O R E"],
        Svg.text_ -- Score Number
        [ x "110"
        , y "400"
        , fontFamily "monospace"
        , fontSize "26"]
        [Svg.text (String.fromInt model.score)],
        rect -- P2 Board
        [ x "800"
        , y "0"
        , width "300"
        , height "600"
        , rx "0"
        , ry "0"
        , fill "gray"
        , stroke "black"
        , strokeWidth "1"
        ][]] ++ 
      (renderActivePieceP2 model.style model.activePiece2) ++
      (renderAllRowsP2 model.style model.inactiveBlocks2) ++
      [ rect
        [ x "1100" -- Next Piece Box
        , y "0"
        , width "200"
        , height "150"
        , rx "0"
        , ry "0"
        , fill "gray"
        , stroke "black"
        , strokeWidth "1"][],
        Svg.text_ -- Next Piece Text
        [ x "1125"
        , y "20"
        , fontFamily "monospace"
        , fontSize "15"
        , fill "white"]
        [Svg.text "N E X T    P I E C E"]] ++
        (renderNextPieceP2 model.style model.nextPiece2) ++
        [rect -- Score Box
        [ x "1100"
        , y "300"
        , width "200"
        , height "150"
        , rx "0"
        , ry "0"
        , fill "gray"
        , stroke "black"
        , strokeWidth "1"][],
        Svg.text_ -- Score Text
        [ x "1135"
        , y "350"
        , fontFamily "monospace"
        , fontSize "26"]
        [Svg.text "S C O R E"],
        Svg.text_ -- Score Number
        [ x "1160"
        , y "400"
        , fontFamily "monospace"
        , fontSize "26"]
        [Svg.text (String.fromInt model.score2)]]

renderGameOver: Model -> List (Svg msg)
renderGameOver model =
    case model.gameType of
        SinglePlayer ->
            [rect
            [ x "600"
            , y "0"
            , width "300"
            , height "600"
            , rx "0"
            , ry "0"
            , fillOpacity "0.4"
            , fill "gray"
            , stroke "black"
            , strokeWidth "1"
            ][],
            Svg.text_
            [ x "615"
            , y "200"
            , fontFamily "monospace"
            , fill "white"
            , fontSize "30"]
            [Svg.text "G A M E O V E R"]]         
        TwoPlayer ->
            [rect
            [ x "250"
            , y "0"
            , width "300"
            , height "600"
            , rx "0"
            , ry "0"
            , fillOpacity "0.4"
            , fill "gray"
            , stroke "black"
            , strokeWidth "1"
            ][],
            Svg.text_
            [ x "265"
            , y "200"
            , fontFamily "monospace"
            , fill "white"
            , fontSize "30"]
            [Svg.text "G A M E O V E R"],
            rect
            [ x "800"
            , y "0"
            , width "300"
            , height "600"
            , rx "0"
            , ry "0"
            , fillOpacity "0.4"
            , fill "gray"
            , stroke "black"
            , strokeWidth "1"
            ][],
            Svg.text_
            [ x "815"
            , y "200"
            , fontFamily "monospace"
            , fill "white"
            , fontSize "30"]
            [Svg.text "G A M E O V E R"]]         

{-- Functions to render blocks on screen (different functions for single and 
multiplayer modes --}

renderActivePiece: Style -> Tetromino -> List(Svg msg)
renderActivePiece s thispiece =
    case thispiece of
        Piece blocks _ _ -> renderOneRow s blocks
        E -> []

renderNextPiece: Style -> Tetromino -> List (Svg msg)
renderNextPiece s thispiece =
    let
        helper block =
            let
                thisx= String.fromInt (350+(30*block.point.x))
                thisy = String.fromInt (60+(30*block.point.y))
                thisColor= (colorChooser s block.color) 
            in 
                rect [x thisx, y thisy, fill thisColor, width "30", height "30", stroke "black", strokeWidth "1"] []
    in
        case thispiece of
            Piece blocks _ _ -> List.map helper blocks
            E -> []

renderAllRows: Style -> List(List Block) -> List (Svg msg)
renderAllRows s rlist =
    case rlist of
        []-> []
        firstlist :: restlists -> (renderOneRow s firstlist) ++ (renderAllRows s restlists)

renderOneRow : Style -> List Block -> List (Svg msg)
renderOneRow s blist =
    case blist of
        [] -> []
        {point, color} :: rest -> 
            let
                thisx= String.fromInt (600+(30*point.x))
                thisy = String.fromInt (30*point.y)
                thisColor= colorChooser s color 
            in 
                [rect [x thisx, y thisy, fill thisColor, width "30", height "30", stroke "black", strokeWidth "1"] []] ++ (renderOneRow s rest)

colorChooser: Style -> Blocks.Color -> String
colorChooser s c=
    case s of
        Vapor -> vaporColorChooser c
        Lofi -> lofiColorChooser c

renderActivePieceP1: Style -> Tetromino -> List(Svg msg)
renderActivePieceP1 s thispiece =
    case thispiece of
        Piece blocks _ _ -> renderOneRowP1 s blocks
        E -> []

renderNextPieceP1: Style -> Tetromino -> List (Svg msg)
renderNextPieceP1 s thispiece =
    let
        helper block =
            let
                thisx= String.fromInt ((30*block.point.x))
                thisy = String.fromInt (60+(30*block.point.y))
                thisColor= (colorChooser s block.color) 
            in 
                rect [x thisx, y thisy, fill thisColor, width "30", height "30", stroke "black", strokeWidth "1"] []
    in
        case thispiece of
            Piece blocks _ _ -> List.map helper blocks
            E -> []


renderAllRowsP1: Style -> List(List Block) -> List (Svg msg)
renderAllRowsP1 s rlist =
    case rlist of
        []-> []
        firstlist :: restlists -> (renderOneRowP1 s firstlist) ++ (renderAllRowsP1 s restlists)

renderOneRowP1 : Style -> List Block -> List (Svg msg)
renderOneRowP1 s blist =
    case blist of
        [] -> []
        {point, color} :: rest -> 
            let
                thisx= String.fromInt (250+(30*point.x))
                thisy = String.fromInt (30*point.y)
                thisColor= colorChooser s color 
            in 
                [rect [x thisx, y thisy, fill thisColor, width "30", height "30", stroke "black", strokeWidth "1"] []] ++ (renderOneRowP1 s rest)


renderActivePieceP2: Style -> Tetromino -> List(Svg msg)
renderActivePieceP2 s thispiece =
    case thispiece of
        Piece blocks _ _ -> renderOneRowP2 s blocks
        E -> []

renderNextPieceP2: Style -> Tetromino -> List (Svg msg)
renderNextPieceP2 s thispiece =
    let
        helper block =
            let
                thisx= String.fromInt (1050+(30*block.point.x))
                thisy = String.fromInt (60+(30*block.point.y))
                thisColor= (colorChooser s block.color) 
            in 
                rect [x thisx, y thisy, fill thisColor, width "30", height "30", stroke "black", strokeWidth "1"] []
    in
        case thispiece of
            Piece blocks _ _ -> List.map helper blocks
            E -> []

  
renderAllRowsP2: Style -> List(List Block) -> List (Svg msg)
renderAllRowsP2 s rlist =
    case rlist of
        []-> []
        firstlist :: restlists -> (renderOneRowP2 s firstlist) ++ (renderAllRowsP2 s restlists)

renderOneRowP2 : Style -> List Block -> List (Svg msg)
renderOneRowP2 s blist =
    case blist of
        [] -> []
        {point, color} :: rest -> 
            let
                thisx= String.fromInt (800+(30*point.x))
                thisy = String.fromInt (30*point.y)
                thisColor= colorChooser s color 
            in 
                [rect [x thisx, y thisy, fill thisColor, width "30", height "30", stroke "black", strokeWidth "1"] []] ++ (renderOneRowP2 s rest)

lofiColorChooser: Blocks.Color -> String
lofiColorChooser c=
    case c of
        Green -> "rgb(153, 210, 164)"
        Yellow -> "rgb(233, 196, 79)"
        Purple -> "rgb(135, 95, 96)"
        Pink -> "rgb(242, 129, 148)"
        Blue -> "rgb(40, 105, 119)"
        DarkBlue -> "rgb(229, 81, 78)"
        Orange -> "rgb(217, 192, 132)"
        _ -> Debug.todo "fu"

vaporColorChooser: Blocks.Color -> String
vaporColorChooser c =
    case c of
        Green -> "rgb(204, 255, 204)"
        Yellow -> "rgb(255, 255, 128)"
        Purple -> "rgb(204, 0, 255)"
        Pink -> "rgb(255, 153, 255)"
        Blue -> "rgb(204, 255, 255)"
        DarkBlue -> "rgb(0, 51, 153)"
        Orange -> "rgb(255, 153, 102)"
        _ -> Debug.todo "fu"

imageChooser: Style -> String
imageChooser s =
    case s of
        Vapor -> "url(vaporwave.jpg)"
        Lofi -> "url(lofi.jpg)"


{-- Functions that complete actual Tetris logic (dropping pieces, checking
for collisions, etc. --}


{-- attemptOneSquareDrop : try to drop the Tetromino by one square vertically. 
Returns the (Just updated Tetromino, rowList) if successful, 
(Nothing, updated rowList) otherwise. --}
attemptOneSquareDrop : Tetromino -> List (List Block) -> (Maybe Tetromino, List (List Block))
attemptOneSquareDrop piece rowList =
    case piece of
        E -> (Nothing, rowList)
        Piece blocks shape orient ->
            let
                droppedBlocks = List.map (\x -> (Block (Point x.point.x (x.point.y + 1)) x.color)) blocks
            in
                if ((checkCollisions droppedBlocks rowList)
                    || (List.any (\x -> x.point.y >= 20) droppedBlocks))then
                    -- Collision on attempted drop
                    (Nothing, (addToRowList blocks rowList) |> removeClearedRows)
                else
                    -- No collision
                    (Just (Piece droppedBlocks shape orient), rowList)

{-- removeClearedRows : remove any rows that have been 'cleared' (i.e.
are completely full of blocks, or that are completely empty) and shift 
rows down as needed. For example, there should be no rows "floating 
in space" after cleared rows are removed --}
removeClearedRows : List (List Block) -> List (List Block)
removeClearedRows rowList =
    let
        -- Helper that removes rows if they are completely full or empty
        clearRows rows = 
            case rows of
                [] -> rows
                head::tail ->
                    if (List.length head >= 10) || (List.length head == 0) then
                        clearRows tail
                    else
                        head::(clearRows tail)

        -- Helper to make sure that y values in points correspond to correct
        -- rows after potentially being shifted down
        updateRows currRow rows =
            case rows of
                [] -> rows
                head::tail ->
                    case head of
                        [] -> []::(updateRows (currRow + 1) tail)
                        firstElem::_ ->
                            if firstElem.point.y /= currRow then
                                (List.map (\x -> (Block (Point x.point.x currRow) x.color)) head)::(updateRows (currRow + 1) tail)
                            else
                                head::(updateRows (currRow + 1) tail)

        clearedList = clearRows rowList
        listLength = List.length clearedList
    in
        -- If rows were remove, add 'empty' rows to top of board and 
        -- update the y coordinates of the shifted rows
        if listLength < 20 then
            updateRows 0 <| (List.repeat (20 - listLength) []) ++ clearedList
        else
            clearedList

{-- checkCollisions : checks if there are any collisions (i.e. point with
same x and y as another point) between the piece and the rowList.
Returns True if there is a collision, False if no collisions --}
checkCollisions : List Block -> List (List Block) -> Bool
checkCollisions piece rowList =
    case rowList of
        [] -> False
        head::tail -> 
            case head of
                [] -> checkCollisions piece tail
                _ ->
                   if checkRowForCollision piece head then
                        True
                    else
                        checkCollisions piece tail

{-- checkRowForCollision : checks if list of pieces collides with any blocks
in other list of pieces. Returns True on collision, False otherwise --}
checkRowForCollision : List Block -> List Block -> Bool
checkRowForCollision piece list =
    case list of
        [] -> False
        {point, color}::tail ->
            if (List.any (\x -> (x.point.x == point.x) && (x.point.y == point.y)) piece)  then
                True
            else
                checkRowForCollision piece tail


{-- addToRowList : adds the piece's blocks to the rowList. Assumes
that there are no collisions. Add call to checkCollisions here
if you don't want to assume that. --}
addToRowList : List Block -> List (List Block) -> List (List Block)
addToRowList piece rowList =
    addToRowListHelper piece 0 rowList

addToRowListHelper : List Block -> Int -> List (List Block) -> List (List Block)
addToRowListHelper piece currRow rowList =
    case (piece, rowList) of
        ([], _) -> rowList
        (_, head::tail) -> 
            let
                (blocksInRow, otherBlocks) = List.partition (\x -> x.point.y == currRow) piece
                updatedRow = List.sortWith (\p1 p2 -> compare p1.point.x p2.point.x) (head ++ blocksInRow)
            in
                [updatedRow] ++ (addToRowListHelper otherBlocks (currRow + 1) tail)
        (_, []) -> Debug.todo "addToRowList: piece outside of game bounds"


{-- attemptFullDrop : drop the Tetromino all the way down the board,
until it hits either the bottom or other pieces, and return the 
updated rowList with the Tetromino in it --}
attemptFullDrop : Tetromino -> List (List Block) -> List (List Block)
attemptFullDrop piece rowList =
    let
        (updatedTetromino, updatedRowList) = attemptOneSquareDrop piece rowList
    in
        case updatedTetromino of
            Nothing ->
                updatedRowList
            Just newTet -> 
                attemptFullDrop newTet updatedRowList          

