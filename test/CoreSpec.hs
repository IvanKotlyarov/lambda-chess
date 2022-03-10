module CoreSpec where

import Test.Hspec
import Core
import Core (Move(DoubleSquare), knightMovesSquares)

spec :: Spec
spec = do
    describe "whitePawnMoveSquares" $ do
        it "returns correct values if pawn is on the initial row" $ do
            whitePawnMoveSquares ('b', 2) `shouldBe` [('b', 3), ('b', 4)]

        it "returns valid squares on not initial position" $ do
            whitePawnMoveSquares ('c', 3) `shouldBe` [('c', 4)]

    describe "blackPawnMoveSquares" $ do
        it "returns correct values if pawn is on the initial row" $ do
            blackPawnMoveSquares ('b', 7) `shouldBe` [('b', 6), ('b', 5)]

        it "returns valid squares on not initial position" $ do
            blackPawnMoveSquares ('c', 6) `shouldBe` [('c', 5)]

    describe "pawnMoveFreeSquares" $ do
        it "white can move forward if next square is free" $ do
            pawnMoveFreeSquares ('d', 4) emptyBoard White  `shouldBe` [('d', 5)]
        it "black can move forward if next square is free" $ do
            pawnMoveFreeSquares ('d', 4) emptyBoard Black `shouldBe` [('d', 3)]
        it "white can move forward from initial position if next squares are free" $ do
            pawnMoveFreeSquares ('d', 2) emptyBoard  White  `shouldBe` [('d', 3), ('d', 4)]
        it "black can move forward from initial position if next squares are free" $ do
            pawnMoveFreeSquares ('d', 7) emptyBoard  Black  `shouldBe` [('d', 6), ('d', 5)]
        it "white can move forward from initial position to just one square if second one is taken" $ do
            pawnMoveFreeSquares ('d', 2) (placePiece ('d', 4) whitePawn emptyBoard)  White  `shouldBe` [('d', 3)]
        it "black can move forward from initial position to just one square if second one is taken" $ do
            pawnMoveFreeSquares ('d', 7) (placePiece ('d', 5) whitePawn emptyBoard)  Black  `shouldBe` [('d', 6)]
        it "white cant move forward from initial position if next square is taken" $ do
            pawnMoveFreeSquares ('d', 2) (placePiece ('d', 3) whitePawn emptyBoard)  White  `shouldBe` []
        it "black cant move forward from initial position if next square is taken" $ do
            pawnMoveFreeSquares ('d', 7) (placePiece ('d', 6) whitePawn emptyBoard)  Black  `shouldBe` []
        it "white cant move forward if next square is taken" $ do
            pawnMoveFreeSquares ('d', 3) (placePiece ('d', 4) whitePawn emptyBoard)  White  `shouldBe` []
        it "black cant move forward if next square is taken" $ do
            pawnMoveFreeSquares ('d', 5) (placePiece ('d', 4) whitePawn emptyBoard)  Black  `shouldBe` []

    describe "whitePawnMoves" $ do
        it "moves forward by two squares from initial posision" $ do
            whitePawnMoves ('b', 2) emptyBoard `shouldBe` [Move whitePawn ('b', 2) ('b', 3), DoubleSquare ('b', 2) ('b', 4)]
        it "captures black piece" $ do
            whitePawnMoves ('b', 2) $ placePiece ('c', 3) blackPawn emptyBoard
            `shouldBe`
            [Capture whitePawn ('b', 2) ('c', 3), Move whitePawn ('b', 2) ('b', 3), DoubleSquare ('b', 2) ('b', 4)]

        it "does not capture white piece" $ do
            whitePawnMoves ('b', 2) $ placePiece ('c', 3) whitePawn emptyBoard
            `shouldBe`
            [Move whitePawn ('b', 2) ('b', 3), DoubleSquare ('b', 2) ('b', 4)]

        it "promotes with and without captures from 7th row" $ do
            whitePawnMoves ('b', 7) $ placePiece ('c', 8) blackRook emptyBoard
            `shouldBe`
            [CapturePromotion ('b', 7) ('c', 8) whiteQueen,
            CapturePromotion ('b', 7) ('c', 8) whiteRook,
            CapturePromotion ('b', 7) ('c', 8) whiteBishop,
            CapturePromotion ('b', 7) ('c', 8) whiteKnight,
            Promotion ('b', 7) ('b', 8) whiteQueen,
            Promotion ('b', 7) ('b', 8) whiteRook,
            Promotion ('b', 7) ('b', 8) whiteBishop,
            Promotion ('b', 7) ('b', 8) whiteKnight]
        
        it "returns correct en-passant move" $ do 
            whitePawnMoves ('b', 5) $ movePiece (DoubleSquare ('a', 7) ('a', 5)) emptyBoard 
            `shouldBe` 
            [Move whitePawn ('b', 5) ('b', 6), EnPassant whitePawn ('b', 5) ('a', 6)]
        
        it "doesnt returns en-passant move" $ do 
            whitePawnMoves ('h', 5) $ movePiece (DoubleSquare ('a', 2) ('a', 4)) emptyBoard
            `shouldBe`
            [Move whitePawn ('h', 5) ('h', 6)]

    describe "blackPawnMoves" $ do
        it "moves forward by two squares from initial posision" $ do
            blackPawnMoves ('b', 7) emptyBoard `shouldBe` [Move blackPawn ('b', 7) ('b', 6), DoubleSquare ('b', 7) ('b', 5)]
        it "captures white piece" $ do
            blackPawnMoves ('b', 7) $ placePiece ('c', 6) whitePawn emptyBoard
            `shouldBe`
            [Capture blackPawn ('b', 7) ('c', 6), Move blackPawn ('b', 7) ('b', 6), DoubleSquare ('b', 7) ('b', 5)]

        it "does not capture black piece" $ do
            blackPawnMoves ('b', 7) $ placePiece ('c', 6) blackPawn emptyBoard
            `shouldBe`
            [Move blackPawn ('b', 7) ('b', 6), DoubleSquare ('b', 7) ('b', 5)]

        it "promotes with and without captures from 2th row" $ do
            blackPawnMoves ('b', 2) $ placePiece ('c', 1) whiteRook emptyBoard
            `shouldBe`
            [CapturePromotion ('b', 2) ('c', 1) blackQueen,
            CapturePromotion ('b', 2) ('c', 1) blackRook,
            CapturePromotion ('b', 2) ('c', 1) blackBishop,
            CapturePromotion ('b', 2) ('c', 1) blackKnight,
            Promotion ('b', 2) ('b', 1) blackQueen,
            Promotion ('b', 2) ('b', 1) blackRook,
            Promotion ('b', 2) ('b', 1) blackBishop,
            Promotion ('b', 2) ('b', 1) blackKnight]
        
        it "returns correct en-passant move" $ do 
            blackPawnMoves ('b', 4) $ movePiece (DoubleSquare ('a', 2) ('a', 4)) emptyBoard 
            `shouldBe` 
            [Move blackPawn ('b', 4) ('b', 3), EnPassant blackPawn ('b', 4) ('a', 3)]
        

    describe "allPossibleMoves" $ do
        it "returns white pawn possible moves" $ do
            allPossibleMoves (placePiece ('e', 2) whitePawn emptyBoard) White
            `shouldBe` [Move whitePawn ('e', 2) ('e', 3), DoubleSquare ('e', 2) ('e', 4)]
    
    describe "knightMovesSquares" $ do 
        it "returns knight geometric moves for center square" $ do 
            knightMovesSquares ('d', 5) `shouldBe` [('e',7),('f',6),('f',4),('e',3),('c',3),('b',4),('b',6),('c',7)]

        it "returns correct geometric knight moves a1" $ do
            knightMovesSquares ('a', 1) `shouldBe` [('b', 3), ('c', 2)]

        it "returns correct geometric knight moves for h8" $ do
            knightMovesSquares ('h', 8) `shouldBe` [('g', 6), ('f', 7)]

        it "returns correct geometric knight moves for e1" $ do
            knightMovesSquares ('e', 1) `shouldBe` [('f', 3), ('g', 2), ('c', 2), ('d', 3)]