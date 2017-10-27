module Helpers exposing (..)

import Maybe exposing (withDefault)


seqM : List (Maybe a) -> Maybe (List a)
seqM xs =
    let
        go m accum =
            if m == Nothing || accum == Nothing then
                Nothing
            else
                case m of
                    Nothing ->
                        Nothing

                    Just x ->
                        Maybe.map ((::) x) accum
    in
        List.foldr go (Just []) xs


or : a -> Maybe a -> a
or =
    withDefault


zipWith =
    List.map2
