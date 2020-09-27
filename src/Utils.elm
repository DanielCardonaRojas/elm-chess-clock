module Utils exposing (..)


iff : Bool -> a -> a -> a
iff condition trueValue falseValue =
    if condition then
        trueValue

    else
        falseValue
