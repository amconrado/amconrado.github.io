module Tests exposing (test_suite, test_checkShifters, r)
import Game exposing (..)
import Blocks exposing (..)

test_suite =
--    test_checkCollisions 
--    test_removeClearedRows
--    test_addToRowList
--    test_attemptOneSquareDrop
--    test_attemptFullDrop
    test_checkRowForCollision


test_checkCollisions =
    let 
        point1 = Point 2 2 
        piece1 = [Block (Point 1 1) C, Block (Point 2 2) C]
        block1 = Block point1 C
        block2 = Block (Point 1 1) C
        block3 = Block (Point 3 4) C
        rowList1 = [[block3], [block2, block1]]
    in
    Game.checkCollisions piece1 rowList1


test_checkShifters =
    let
        pointA = Point 0 2
        pointB = Point 1 2
        pointC = Point 0 3
        pointD = Point 1 3
        blockA = Block pointA Yellow
        blockB = Block pointB Yellow
        blockC = Block pointC Yellow
        blockD = Block pointD Yellow
        blocklist = [blockA, blockB, blockC, blockD]
        tetrominoo= Piece blocklist O One
            
    in
        Blocks.shiftLeft tetrominoo

r =
    let
        pointA = Point 3 3
        pointB = Point 4 3
        pointC = Point 5 3
        pointD = Point 4 4
        blockA = Block pointA Yellow
        blockB = Block pointB Yellow
        blockC = Block pointC Yellow
        blockD = Block pointD Yellow
        blocklist = [blockA, blockB, blockC, blockD]
        tetrominoo= Piece blocklist T One
            
    in
        Game.renderOneRow blocklist
        
            
test_removeClearedRows =
    let
        lastRow = [Block (Point 0 19) C
                 ,Block (Point 1 19) C
                 , Block (Point 2 19) C
                 , Block (Point 3 19) C
                 , Block (Point 4 19) C
                 , Block (Point 5 19) C
                 , Block (Point 6 19) C
                 , Block (Point 7 19) C
                 , Block (Point 8 19) C
                 , Block (Point 9 19) C]
        rowList = [[Block (Point 11 0) C]] ++ (List.repeat 17 []) ++ [[Block (Point 10 18) C]] ++ [lastRow] 
    in
    Game.removeClearedRows rowList

test_addToRowList =
    let
        lastRow = [Block (Point 0 0) C
                 ,Block (Point 1 1) C
                 , Block (Point 2 2) C
                 , Block (Point 3 3) C
                 , Block (Point 4 4) C
                 , Block (Point 5 5) C
                 , Block (Point 6 6) C
                 , Block (Point 7 7) C
                 , Block (Point 8 8) C
                 , Block (Point 9 9) C]
        rowList = [[Block (Point 11 11) C]] ++ (List.repeat 17 lastRow) ++ [[Block (Point 10 10) C]] ++ [lastRow]
        rowList1 = (List.repeat 20 [])
        pieces = [Block (Point 8 1) C
                 ,Block (Point 1 1) C
                 , Block (Point 5 1) C
                 , Block (Point 3 3) C]
    in
        Game.addToRowList pieces rowList1

test_attemptOneSquareDrop =
    let
        tetromino = Piece [Block (Point 9 19) C] I One
        lastRow = [Block (Point 0 19) C
                 ,Block (Point 1 19) C
                 , Block (Point 2 19) C
                 , Block (Point 3 19) C
                 , Block (Point 4 19) C
                 , Block (Point 5 19) C
                 , Block (Point 6 19) C
                 , Block (Point 7 19) C
                 , Block (Point 8 19) C]
        rowList = List.repeat 19 [] ++ [lastRow]

    in
        Game.attemptOneSquareDrop tetromino rowList

test_attemptFullDrop =
    let
        tetromino = Piece [Block (Point 8 19) C] I One
        lastRow = [Block (Point 0 19) C
                 ,Block (Point 1 19) C
                 , Block (Point 2 19) C
                 , Block (Point 3 19) C
                 , Block (Point 4 19) C
                 , Block (Point 5 19) C
                 , Block (Point 6 19) C
                 , Block (Point 7 19) C
                 , Block (Point 8 19) C]
        rowList = List.repeat 19 [] ++ [lastRow]

    in
        Game.attemptFullDrop tetromino rowList

test_checkRowForCollision =
    let 
        point1 = Point 2 2 
        piece1 = [Block (Point 1 1) C, Block (Point 2 2) C]
        block1 = Block point1 C
        block2 = Block (Point 1 1) C
        block3 = Block (Point 3 4) C
        rowList1 = [block2, block3]
    in
    Game.checkRowForCollision piece1 rowList1

