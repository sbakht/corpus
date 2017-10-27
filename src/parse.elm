module Parse exposing (..)

import Regex exposing (..)


type alias PossibleWord =
    List (Maybe String)


wordRegex : Regex
wordRegex =
    regex <|
        String.concat <|
            [ "<td class=\"c1\"><span class=\"l\">(.+?)</span>.*?<i class=\"ab\">(.+?)</i></td>"
            , "<td class=\"c2\"><a .*?>(.+?)</a></td>"
            , "<td class=\"c3\">(.*?)<span class=\"auu\">(.+?)</span>(.*?)</td>"
            ]


titles : String -> List Match
titles data =
    find All (regex "<h4 class=\"dxe\">(.+?)</h4>") data


tables : String -> List Match
tables data =
    Debug.log "tables" <| find All (regex "<table class=\"taf\".*?>.*?</table>?") data


matches : String -> List Match
matches x =
    find All wordRegex x


parse : List Match -> List PossibleWord
parse =
    List.map .submatches
