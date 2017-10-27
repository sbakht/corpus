module Main exposing (..)

import Html exposing (text, div, p, span, Html, li, ul)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import List exposing (map, foldr)
import String exposing (concat)
import Parser exposing (Parser)
import Http exposing (..)
import Helpers exposing (..)
import Parse exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Word =
    { location : Location
    , transliteration : String
    , translation : String
    , arabic : Arabic
    }


type alias Location =
    ( String, String, String )


type alias Arabic =
    ( String, String, String )



--------------------------------------------------


getHTML : Cmd Msg
getHTML =
    Http.send Got req


req : Request String
req =
    Http.getString "arabic.html"



--Http.getString "http://corpus.quran.com/qurandictionary.jsp?"


mkWord : List String -> Word
mkWord s =
    case s of
        [ loc, lit, tran, ar1, ar2, ar3 ] ->
            Word (parseLoc loc) lit tran ( ar1, ar2, ar3 )

        otherwise ->
            Word ( "", "", "" ) "" "" ( "", "", "" )


parseLoc : String -> Location
parseLoc x =
    case String.split ":" x of
        [ a, b, c ] ->
            ( a, b, c )

        otherwise ->
            ( "", "", "" )


toWord : List (Maybe String) -> Maybe Word
toWord =
    Maybe.map mkWord << seqM


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "", getHTML )


type Msg
    = Getting
    | Got (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Getting ->
            ( model, getHTML )

        Got (Ok x) ->
            ( x, Cmd.none )

        Got (Err _) ->
            ( "oh no", Cmd.none )



--------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ div [] (zipWith printWords (allTitles model) << map (parse << matches << .match) <| tables model)
        ]


possibleWords : String -> List PossibleWord
possibleWords =
    map (parse << matches << .match) tables


printWords : String -> List PossibleWord -> Html Msg
printWords title words =
    div []
        [ p [] [ text <| Debug.log "title" <| title ]
        , ul [] << map (or (text "") << Maybe.map printWord << toWord) <| words
        ]


printWord : Word -> Html Msg
printWord w =
    li [] [ span [] [ locSpan w.location, toSpan w.transliteration, toSpan w.translation, arabicSpan w.arabic ] ]


allTitles : String -> List String
allTitles model =
    map (concat << map (or "") << .submatches) (titles model)


arabicSpan : Arabic -> Html Msg
arabicSpan ( s1, s2, s3 ) =
    span [] [ toSpan s1, span [ style [ ( "color", "red" ) ] ] [ text s2 ], toSpan s3 ]


locSpan : Location -> Html Msg
locSpan ( a, b, c ) =
    span [] [ text <| String.join ":" [ a, b, c ] ]


toSpan : String -> Html Msg
toSpan x =
    span [] [ text x ]
