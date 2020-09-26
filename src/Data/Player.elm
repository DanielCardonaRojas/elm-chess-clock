module Data.Player exposing (..)


type Player
    = Player1
    | Player2


switch : Player -> Player
switch player =
    if player == Player1 then
        Player2

    else
        Player1
