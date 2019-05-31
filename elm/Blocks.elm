{-- Blocks.elm : contains the custom data structures to
    represent and manipulate the Tetris blocks --}

module Blocks exposing (..)
import Collage exposing (..)
import Collage.Render exposing (..)
import Color
import Html.Attributes exposing (style) 

type alias Point = { x : Int, y : Int}

--type alias Green = Color.rgb 204 255 204

--type alias Yellow = Color.rgb 255 255 128

--type alias Pink = Color.rgb 255 153 255

--type alias Purple = Color.rgb 204 0 255

--type alias Blue = Color.rgb 204 255 255

--type alias DarkBlue = Color.rgb 0 51 153

--type alias Orange = Color.rgb 255 153 102

type Color = Green | Yellow | Pink | Purple | Blue | DarkBlue | Orange |C

type Shape = I | O | T | J | L | S | Z 


-- define these explicitly in documentation
type Orientation = One | Two | Three | Four

type alias Block = {point : Point, color : Color}

type Tetromino = E
                 | Piece (List Block) Shape Orientation
--type Tetromino = E | Piece { blocks : List Block, shape : Shape, orientation : Orientation}

--MAIN FUNCTIONS

initO = let
            pointA = Point 4 0
            pointB = Point 5 0
            pointC = Point 4 1
            pointD = Point 5 1
            blockA = Block pointA Green
            blockB = Block pointB Green
            blockC = Block pointC Green
            blockD = Block pointD Green
            blocklist = [blockA, blockB, blockC, blockD]
        in
            Piece blocklist O One

initI = let
            pointA = Point 3 0
            pointB = Point 4 0
            pointC = Point 5 0
            pointD = Point 6 0
            blockA = Block pointA DarkBlue
            blockB = Block pointB DarkBlue
            blockC = Block pointC DarkBlue
            blockD = Block pointD DarkBlue
            blocklist = [blockA, blockB, blockC, blockD]
        in
             Piece blocklist I One

initJ = let
            pointA = Point 3 0
            pointB = Point 4 0
            pointC = Point 5 0
            pointD = Point 5 1
            blockA = Block pointA Yellow
            blockB = Block pointB Yellow
            blockC = Block pointC Yellow
            blockD = Block pointD Yellow
            blocklist = [blockA, blockB, blockC, blockD]
        in
             Piece blocklist J One

initL = let
            pointA = Point 5 0
            pointB = Point 4 0
            pointC = Point 3 0
            pointD = Point 3 1
            blockA = Block pointA Orange
            blockB = Block pointB Orange 
            blockC = Block pointC Orange
            blockD = Block pointD Orange
            blocklist = [blockA, blockB, blockC, blockD]
        in
             Piece blocklist L One

initT = let
            pointA = Point 3 0
            pointB = Point 4 0
            pointC = Point 5 0
            pointD = Point 4 1
            blockA = Block pointA Blue
            blockB = Block pointB Blue
            blockC = Block pointC Blue
            blockD = Block pointD Blue
            blocklist = [blockA, blockB, blockC, blockD]
        in
             Piece blocklist T One

initZ = let
            pointA = Point 3 0
            pointB = Point 4 0
            pointC = Point 4 1
            pointD = Point 5 1
            blockA = Block pointA Purple
            blockB = Block pointB Purple
            blockC = Block pointC Purple
            blockD = Block pointD Purple
            blocklist = [blockA, blockB, blockC, blockD]
        in
             Piece blocklist Z One

initS = let
            pointA = Point 3 1
            pointB = Point 4 1
            pointC = Point 4 0
            pointD = Point 5 0
            blockA = Block pointA Pink
            blockB = Block pointB Pink
            blockC = Block pointC Pink
            blockD = Block pointD Pink
            blocklist = [blockA, blockB, blockC, blockD]
        in
             Piece blocklist S One



shiftRight: Tetromino -> Tetromino
shiftRight tet =
    case tet of
        E -> E
        Piece blocks shape orientation ->
             let
                newList = shiftRListRunner blocks
                isOk = isValidList newList
            in       
                case isOk of
                    False -> tet
                    True ->
                        Piece newList shape orientation

shiftLeft: Tetromino -> Tetromino
shiftLeft tet =
    case tet of
        E -> E
        Piece blocks shape orientation ->
             let
                newList = shiftLListRunner blocks
                isOk = isValidList newList
            in       
                case isOk of
                    False -> tet
                    True ->
                        Piece newList shape orientation


rotateRight: Tetromino -> Tetromino
rotateRight tet =
    case tet of
        E -> E
        Piece blocks shape orientation ->
            case shape of
                O -> tet
                I -> let
                        potential = rotateRightI blocks orientation
                        plist = Tuple.first potential
                        newOri= Tuple.second potential
                        ok= isValidList plist
                    in
                        if ok
                        then Piece plist shape newOri
                        else wallKickRight tet
                Z -> let
                        potential = rotateRightZ blocks orientation
                        plist = Tuple.first potential
                        newOri= Tuple.second potential
                        ok= isValidList plist
                    in
                        if ok
                        then Piece plist shape newOri
                        else wallKickRight tet
                S -> let
                        potential = rotateRightS blocks orientation
                        plist = Tuple.first potential
                        newOri= Tuple.second potential
                        ok= isValidList plist
                    in
                        if ok
                        then Piece plist shape newOri
                        else wallKickRight tet
                T -> let
                        potential = rotateRightT blocks orientation
                        plist = Tuple.first potential
                        newOri= Tuple.second potential
                        ok= isValidList plist
                    in
                        if ok
                        then Piece plist shape newOri
                        else wallKickRight tet
                J -> let
                        potential = rotateRightJ blocks orientation
                        plist = Tuple.first potential
                        newOri= Tuple.second potential
                        ok= isValidList plist
                    in
                        if ok
                        then Piece plist shape newOri
                        else wallKickRight tet
                L -> let
                        potential = rotateRightL blocks orientation
                        plist = Tuple.first potential
                        newOri= Tuple.second potential
                        ok= isValidList plist
                    in
                        if ok
                        then Piece plist shape newOri
                        else wallKickRight tet

rotateLeft: Tetromino -> Tetromino
rotateLeft tet =
    case tet of
        E -> E
        Piece blocks shape orientation ->
            case shape of
                O -> tet
                I -> let
                        potential = rotateRightI blocks orientation
                        plist = Tuple.first potential
                        newOri= Tuple.second potential
                        ok= isValidList plist
                    in
                        if ok
                        then Piece plist shape newOri
                        else wallKickLeft tet
                Z -> let
                        potential = rotateRightZ blocks orientation
                        plist = Tuple.first potential
                        newOri= Tuple.second potential
                        ok= isValidList plist
                    in
                        if ok
                        then Piece plist shape newOri
                        else wallKickLeft tet
                S -> let
                        potential = rotateRightS blocks orientation
                        plist = Tuple.first potential
                        newOri= Tuple.second potential
                        ok= isValidList plist
                    in
                        if ok
                        then Piece plist shape newOri
                        else wallKickLeft tet
                T -> let
                        potential = rotateLeftT blocks orientation
                        plist = Tuple.first potential
                        newOri= Tuple.second potential
                        ok= isValidList plist
                    in
                        if ok
                        then Piece plist shape newOri
                        else wallKickLeft tet
                J -> let
                        potential = rotateLeftJ blocks orientation
                        plist = Tuple.first potential
                        newOri= Tuple.second potential
                        ok= isValidList plist
                    in
                        if ok
                        then Piece plist shape newOri
                        else wallKickLeft tet
                L -> let
                        potential = rotateLeftL blocks orientation
                        plist = Tuple.first potential
                        newOri= Tuple.second potential
                        ok= isValidList plist
                    in
                        if ok
                        then Piece plist shape newOri
                        else wallKickLeft tet
--HELPER FUNCTIONS

wallKickRight: Tetromino -> Tetromino
wallKickRight tet =
    case tet of
        E -> Debug.todo "y u do dis (wallKickRight)"
        Piece blist shape orient ->
            case blist of
                []-> Debug.todo "Oh no cannot wall kick nothingness"
                b1::rest->
                    let
                        thisx = b1.point.x
                        thisy = b1.point.y 
                        newblocs = List.map (\x -> (Block (Point x.point.x (x.point.y + 1)) x.color)) blist
                        newtet= Piece newblocs shape orient      
                     in
                        if (thisy<=0) then
                            if (thisx>5) 
                                then rotateRight(shiftLeft newtet)
                                else rotateRight(shiftRight newtet)
                        else if (thisx>5)
                                then rotateRight(shiftLeft tet)
                                else rotateRight(shiftRight tet)

wallKickLeft: Tetromino -> Tetromino
wallKickLeft tet =
    case tet of
        E -> Debug.todo "y u do dis (wallkick)"
        Piece blist shape orient ->
            case blist of
                []-> Debug.todo "Oh no cannot wall kick nothingness"
                b1::rest->
                    let
                        thisx = b1.point.x
                        thisy = b1.point.y 
                        newblocs = List.map (\x -> (Block (Point x.point.x (x.point.y + 1)) x.color)) blist
                        newtet= Piece newblocs shape orient      
                     in
                        if (thisy<=0) then
                            if (thisx>5) 
                                then rotateLeft(shiftLeft newtet)
                                else rotateLeft(shiftRight newtet)
                        else if (thisx>5)
                                then rotateLeft(shiftLeft tet)
                                else rotateLeft(shiftRight tet)
                    
            


shiftRListRunner: List Block -> List Block
shiftRListRunner bs=
    case bs of
        [] -> []
        b1 :: rest -> let
                        newPoint = Point (b1.point.x+1) b1.point.y
                    in
                        (Block newPoint b1.color) :: shiftRListRunner rest


shiftLListRunner: List Block -> List Block
shiftLListRunner bs=
    case bs of
        [] -> []
        b1 :: rest -> let
                        newPoint = Point (b1.point.x-1) b1.point.y
                    in
                        (Block newPoint b1.color) :: shiftLListRunner rest

isValidList: List Block -> Bool
isValidList bs=
    case bs of
        [] -> True
        b1 :: rest -> let 
                        thisx = b1.point.x
                        thisy = b1.point.y
                    in
                        (thisx<10) && (thisx>=0) && (thisy<20) && (thisy>=0) && (isValidList rest)

rotateRightI: List Block -> Orientation -> (List Block, Orientation)
rotateRightI bs ori=
    case bs of
        [] -> Debug.todo "yikes empty block list in rotateRightI"
        head :: rest ->
            let
                thisx = head.point.x
                thisy = head.point.y 
                thisc = head.color      
            in case ori of
                One -> let 
                            new1 = Block (Point (thisx+2) (thisy+2)) thisc
                            new2 = Block (Point (thisx+2) (thisy+1)) thisc
                            new3 = Block (Point (thisx+2) thisy) thisc
                            new4 = Block (Point (thisx+2) (thisy-1)) thisc
                            newList = [new1, new2, new3, new4]
                        in (newList, Two)
                Two -> let 
                            new5 = Block (Point (thisx-2) (thisy-2)) thisc
                            new6 = Block (Point (thisx-1) (thisy-2)) thisc
                            new7 = Block (Point thisx (thisy-2)) thisc
                            new8 = Block (Point (thisx+1) (thisy-2)) thisc
                            newLista = [new5, new6, new7, new8]
                        in (newLista, One)
                _ -> Debug.todo "only two orientations for I piece"


rotateRightZ: List Block -> Orientation -> (List Block, Orientation)
rotateRightZ bs ori=
    case bs of
        [] -> Debug.todo "yikes empty block list in rotateRightZ"
        head :: rest ->
            let
                thisx = head.point.x
                thisy = head.point.y 
                thisc = head.color      
            in case ori of
                One -> let 
                            new1 = Block (Point (thisx+2) (thisy-1)) thisc
                            new2 = Block (Point (thisx+2) (thisy)) thisc
                            new3 = Block (Point (thisx+1) thisy) thisc
                            new4 = Block (Point (thisx+1) (thisy+1)) thisc
                            newList = [new1, new2, new3, new4]
                        in (newList, Two)
                Two -> let 
                            new5 = Block (Point (thisx-2) (thisy+1)) thisc
                            new6 = Block (Point (thisx-1) (thisy+1)) thisc
                            new7 = Block (Point (thisx-1) (thisy+2)) thisc
                            new8 = Block (Point (thisx) (thisy+2)) thisc
                            newLista = [new5, new6, new7, new8]
                        in (newLista, One)
                _ -> Debug.todo "only two orientations for Z piece"
            
rotateRightS: List Block -> Orientation -> (List Block, Orientation)
rotateRightS bs ori=
    case bs of
        [] -> Debug.todo "yikes empty block list in rotateRightS"
        head :: rest ->
            let
                thisx = head.point.x
                thisy = head.point.y 
                thisc = head.color      
            in case ori of
                One -> let 
                            new1 = Block (Point (thisx+1) (thisy-2)) thisc
                            new2 = Block (Point (thisx+1) (thisy-1)) thisc
                            new3 = Block (Point (thisx+2) (thisy-1)) thisc
                            new4 = Block (Point (thisx+2) (thisy)) thisc
                            newList = [new1, new2, new3, new4]
                            hardnew1 = Block (Point 5 0) thisc
                            hardnew2 = Block (Point 5 1) thisc
                            hardnew3 = Block (Point 6 1) thisc
                            hardnew4 = Block (Point 6 2) thisc
                            newHardList = [hardnew1, hardnew2, hardnew3, hardnew4]
                        in if (thisy<=1)
                            then (newHardList, Two)
                            else (newList, Two)
                Two -> let 
                            new5 = Block (Point (thisx-1) (thisy+2)) thisc
                            new6 = Block (Point (thisx) (thisy+2)) thisc
                            new7 = Block (Point (thisx) (thisy+1)) thisc
                            new8 = Block (Point (thisx+1) (thisy+1)) thisc
                            newLista = [new5, new6, new7, new8]
                        in (newLista, One)
                _ -> Debug.todo "only two orientations for S piece"

rotateRightT: List Block -> Orientation -> (List Block, Orientation)
rotateRightT bs ori=
    case bs of
        [] -> Debug.todo "yikes empty block list in rotateRightT"
        head :: rest ->
            let
                thisx = head.point.x
                thisy = head.point.y 
                thisc = head.color      
            in case ori of
                One -> let 
                            new1 = Block (Point (thisx+1) (thisy-1)) thisc
                            new2 = Block (Point (thisx+1) (thisy)) thisc
                            new3 = Block (Point (thisx+1) (thisy+1)) thisc
                            new4 = Block (Point (thisx) (thisy)) thisc
                            newList = [new1, new2, new3, new4]
                        in (newList, Two)
                Two -> let 
                            new5 = Block (Point (thisx+1) (thisy+1)) thisc
                            new6 = Block (Point (thisx) (thisy+1)) thisc
                            new7 = Block (Point (thisx-1) (thisy+1)) thisc
                            new8 = Block (Point (thisx) (thisy)) thisc
                            newLista = [new5, new6, new7, new8]
                        in (newLista, Three)
                Three -> let 
                            new9 = Block (Point (thisx-1) (thisy+1)) thisc
                            new10 = Block (Point (thisx-1) (thisy)) thisc
                            new11= Block (Point (thisx-1) (thisy-1)) thisc
                            new12= Block (Point (thisx) (thisy)) thisc
                            newListb = [new9, new10, new11, new12]
                        in (newListb, Four)
                Four -> let 
                            new13 = Block (Point (thisx-1) (thisy-1)) thisc
                            new14 = Block (Point (thisx) (thisy-1)) thisc
                            new15= Block (Point (thisx+1) (thisy-1)) thisc
                            new16= Block (Point (thisx) (thisy)) thisc
                            newListc = [new13, new14, new15, new16]
                        in (newListc, One)

rotateLeftT: List Block -> Orientation -> (List Block, Orientation)
rotateLeftT bs ori=
    case bs of
        [] -> Debug.todo "yikes empty block list in rotateLeftT"
        head :: rest ->
            let
                thisx = head.point.x
                thisy = head.point.y 
                thisc = head.color      
            in case ori of
                One -> let 
                            new1 = Block (Point (thisx+1) (thisy+1)) thisc
                            new2 = Block (Point (thisx+1) (thisy)) thisc
                            new3 = Block (Point (thisx+1) (thisy-1)) thisc
                            new4 = Block (Point (thisx+2) (thisy)) thisc
                            newList = [new1, new2, new3, new4]
                        in (newList, Four)
                Two -> let 
                            new5 = Block (Point (thisx-1) (thisy+1)) thisc
                            new6 = Block (Point (thisx) (thisy+1)) thisc
                            new7 = Block (Point (thisx+1) (thisy+1)) thisc
                            new8 = Block (Point (thisx) (thisy+2)) thisc
                            newLista = [new5, new6, new7, new8]
                        in (newLista, One)
                Three -> let 
                            new9 = Block (Point (thisx-1) (thisy-1)) thisc
                            new10 = Block (Point (thisx-1) (thisy)) thisc
                            new11= Block (Point (thisx-1) (thisy+1)) thisc
                            new12= Block (Point (thisx-2) (thisy)) thisc
                            newListb = [new9, new10, new11, new12]
                        in (newListb, Two)
                Four -> let 
                            new13 = Block (Point (thisx+1) (thisy-1)) thisc
                            new14 = Block (Point (thisx) (thisy-1)) thisc
                            new15= Block (Point (thisx-1) (thisy-1)) thisc
                            new16= Block (Point (thisx) (thisy-2)) thisc
                            newListc = [new13, new14, new15, new16]
                        in (newListc, Three)

rotateRightJ: List Block -> Orientation -> (List Block, Orientation)
rotateRightJ bs ori=
    case bs of
        [] -> Debug.todo "yikes empty block list in rotateRightJ"
        head :: rest ->
            let
                thisx = head.point.x
                thisy = head.point.y 
                thisc = head.color      
            in case ori of
                One -> let 
                            new1 = Block (Point (thisx+1) (thisy-1)) thisc
                            new2 = Block (Point (thisx+1) (thisy)) thisc
                            new3 = Block (Point (thisx+1) (thisy+1)) thisc
                            new4 = Block (Point (thisx) (thisy+1)) thisc
                            newList = [new1, new2, new3, new4]
                        in (newList, Two)
                Two -> let 
                            new5 = Block (Point (thisx+1) (thisy+1)) thisc
                            new6 = Block (Point (thisx) (thisy+1)) thisc
                            new7 = Block (Point (thisx-1) (thisy+1)) thisc
                            new8 = Block (Point (thisx-1) (thisy)) thisc
                            newLista = [new5, new6, new7, new8]
                        in (newLista, Three)
                Three -> let 
                            new9 = Block (Point (thisx-1) (thisy+1)) thisc
                            new10 = Block (Point (thisx-1) (thisy)) thisc
                            new11= Block (Point (thisx-1) (thisy-1)) thisc
                            new12= Block (Point (thisx) (thisy-1)) thisc
                            newListb = [new9, new10, new11, new12]
                        in (newListb, Four)
                Four -> let 
                            new13 = Block (Point (thisx-1) (thisy-1)) thisc
                            new14 = Block (Point (thisx) (thisy-1)) thisc
                            new15= Block (Point (thisx+1) (thisy-1)) thisc
                            new16= Block (Point (thisx+1) (thisy)) thisc
                            newListc = [new13, new14, new15, new16]
                        in (newListc, One)

rotateLeftJ: List Block -> Orientation -> (List Block, Orientation)
rotateLeftJ bs ori=
    case bs of
        [] -> Debug.todo "yikes empty block list in rotateLeftJ"
        head :: rest ->
            let
                thisx = head.point.x
                thisy = head.point.y 
                thisc = head.color      
            in case ori of
                One -> let 
                            new1 = Block (Point (thisx+1) (thisy+1)) thisc
                            new2 = Block (Point (thisx+1) (thisy)) thisc
                            new3 = Block (Point (thisx+1) (thisy-1)) thisc
                            new4 = Block (Point (thisx+2) (thisy-1)) thisc
                            newList = [new1, new2, new3, new4]
                        in (newList, Four)
                Two -> let 
                            new5 = Block (Point (thisx-1) (thisy+1)) thisc
                            new6 = Block (Point (thisx) (thisy+1)) thisc
                            new7 = Block (Point (thisx+1) (thisy+1)) thisc
                            new8 = Block (Point (thisx+1) (thisy+2)) thisc
                            newLista = [new5, new6, new7, new8]
                        in (newLista, One)
                Three -> let 
                            new9 = Block (Point (thisx-1) (thisy-1)) thisc
                            new10 = Block (Point (thisx-1) (thisy)) thisc
                            new11= Block (Point (thisx-1) (thisy+1)) thisc
                            new12= Block (Point (thisx-2) (thisy+1)) thisc
                            newListb = [new9, new10, new11, new12]
                        in (newListb, Two)
                Four -> let 
                            new13 = Block (Point (thisx+1) (thisy-1)) thisc
                            new14 = Block (Point (thisx) (thisy-1)) thisc
                            new15= Block (Point (thisx-1) (thisy-1)) thisc
                            new16= Block (Point (thisx-1) (thisy-2)) thisc
                            newListc = [new13, new14, new15, new16]
                        in (newListc, Three)

rotateRightL: List Block -> Orientation -> (List Block, Orientation)
rotateRightL bs ori=
    case bs of
        [] -> Debug.todo "yikes empty block list in rotateRightL"
        head :: rest ->
            let
                thisx = head.point.x
                thisy = head.point.y 
                thisc = head.color      
            in case ori of
                One -> let 
                            new1 = Block (Point (thisx-1) (thisy+1)) thisc
                            new2 = Block (Point (thisx-1) (thisy)) thisc
                            new3 = Block (Point (thisx-1) (thisy-1)) thisc
                            new4 = Block (Point (thisx-2) (thisy-1)) thisc
                            newList = [new1, new2, new3, new4]
                        in (newList, Two)
                Two -> let 
                            new5 = Block (Point (thisx-1) (thisy-1)) thisc
                            new6 = Block (Point (thisx) (thisy-1)) thisc
                            new7 = Block (Point (thisx+1) (thisy-1)) thisc
                            new8 = Block (Point (thisx+1) (thisy-2)) thisc
                            newLista = [new5, new6, new7, new8]
                        in (newLista, Three)
                Three -> let 
                            new9 = Block (Point (thisx+1) (thisy-1)) thisc
                            new10 = Block (Point (thisx+1) (thisy)) thisc
                            new11= Block (Point (thisx+1) (thisy+1)) thisc
                            new12= Block (Point (thisx+2) (thisy+1)) thisc
                            newListb = [new9, new10, new11, new12]
                        in (newListb, Four)
                Four -> let 
                            new13 = Block (Point (thisx+1) (thisy+1)) thisc
                            new14 = Block (Point (thisx) (thisy+1)) thisc
                            new15= Block (Point (thisx-1) (thisy+1)) thisc
                            new16= Block (Point (thisx-1) (thisy+2)) thisc
                            newListc = [new13, new14, new15, new16]
                        in (newListc, One)

rotateLeftL: List Block -> Orientation -> (List Block, Orientation)
rotateLeftL bs ori=
    case bs of
        [] -> Debug.todo "yikes empty block list in rotateLeftL"
        head :: rest ->
            let
                thisx = head.point.x
                thisy = head.point.y 
                thisc = head.color      
            in case ori of
                One -> let 
                            new1 = Block (Point (thisx-1) (thisy-1)) thisc
                            new2 = Block (Point (thisx-1) (thisy)) thisc
                            new3 = Block (Point (thisx-1) (thisy+1)) thisc
                            new4 = Block (Point (thisx) (thisy+1)) thisc
                            newList = [new1, new2, new3, new4]
                        in (newList, Four)
                Two -> let 
                            new5 = Block (Point (thisx+1) (thisy-1)) thisc
                            new6 = Block (Point (thisx) (thisy-1)) thisc
                            new7 = Block (Point (thisx-1) (thisy-1)) thisc
                            new8 = Block (Point (thisx-1) (thisy)) thisc
                            newLista = [new5, new6, new7, new8]
                        in (newLista, One)
                Three -> let 
                            new9 = Block (Point (thisx+1) (thisy+1)) thisc
                            new10 = Block (Point (thisx+1) (thisy)) thisc
                            new11= Block (Point (thisx+1) (thisy-1)) thisc
                            new12= Block (Point (thisx) (thisy-1)) thisc
                            newListb = [new9, new10, new11, new12]
                        in (newListb, Two)
                Four -> let 
                            new13 = Block (Point (thisx-1) (thisy+1)) thisc
                            new14 = Block (Point (thisx) (thisy+1)) thisc
                            new15= Block (Point (thisx+1) (thisy+1)) thisc
                            new16= Block (Point (thisx+1) (thisy)) thisc
                            newListc = [new13, new14, new15, new16]
                        in (newListc, Three)

                    
                            
