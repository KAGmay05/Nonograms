module Types where

data Cell = Filled | Empty | Unknown
    deriving (Eq, Show)

type Row = [Cell]
type Board = [Row]

data Game = Game
    { board :: Board
    , rowClues :: [[Int]]
    , colClues :: [[Int]]
    }
    deriving (Eq, Show)
