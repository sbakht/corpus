module Main exposing (..)

import Html exposing (text, div, p, span, Html, li, ul)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import List exposing (map, foldr)
import String exposing (concat)
import Regex exposing (..)
import Maybe exposing (withDefault)
import Parser exposing (Parser)
import Http exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Arabic =
    ( String, String, String )


type alias Word =
    { location : String
    , transliteration : String
    , translation : String
    , arabic : Arabic
    }


type alias PossibleWord =
    List (Maybe String)


wordRegex : Regex
wordRegex =
    regex <|
        concat <|
            [ "<td class=\"c1\"><span class=\"l\">(.+?)</span>.*?<i class=\"ab\">(.+?)</i></td>"
            , "<td class=\"c2\"><a .*?>(.+?)</a></td>"
            , "<td class=\"c3\">(.*?)<span class=\"auu\">(.+?)</span>(.*?)</td>"

            --, "<td class=\"c3\">(.*?)</td>"
            ]


req : Request String
req =
    Http.getString "arabic.html"



--Http.getString "http://corpus.quran.com/qurandictionary.jsp?"


ajax : Cmd Msg
ajax =
    Http.send Got req


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
    map .submatches


mkWord : List String -> Word
mkWord s =
    case s of
        [ loc, lit, tran, ar1, ar2, ar3 ] ->
            Word loc lit tran ( ar1, ar2, ar3 )

        otherwise ->
            Word "" "" "" ( "", "", "" )


toWord : List (Maybe String) -> Maybe Word
toWord =
    Maybe.map mkWord << seqM


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
        foldr go (Just []) xs


or : a -> Maybe a -> a
or =
    withDefault


empty : Html Msg
empty =
    text ""


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( data, ajax )


type Msg
    = Getting
    | Got (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Getting ->
            ( model, ajax )

        Got (Ok x) ->
            ( x, Cmd.none )

        Got (Err _) ->
            ( "oh no", Cmd.none )



--------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


toSpan : String -> Html Msg
toSpan x =
    span [] [ text x ]


arabicSpan : Arabic -> Html Msg
arabicSpan ( s1, s2, s3 ) =
    span [] [ toSpan s1, span [ style [ ( "color", "red" ) ] ] [ text s2 ], toSpan s3 ]


printWord : Word -> Html Msg
printWord w =
    li [] [ span [] [ toSpan w.location, toSpan w.transliteration, toSpan w.translation, arabicSpan w.arabic ] ]


printWords : String -> List PossibleWord -> Html Msg
printWords title words =
    div []
        [ p [] [ text <| Debug.log "title" <| title ]
        , ul [] << map (or empty << Maybe.map printWord << toWord) <| words
        ]


allTitles : String -> List String
allTitles model =
    map (concat << map (or "") << .submatches) (titles model)


view : Model -> Html Msg
view model =
    div []
        [ div [] (List.map2 printWords (allTitles model) << map (parse << matches << .match) <| tables model)
        ]


data : String
data =
    """
    <h4 class="dxe">Verb (form II) - to name</h4>
    <table class="taf" border="0" cellpadding="0" cellspacing="0"><tbody><tr><td class="c1"><span class="l">(3:36:16)</span> <i class="ab">sammaytuhā</i></td><td class="c2"><a name="(3:36:16)" href="wordmorphology.jsp?location=(3:36:16)">[I] (have) named her</a></td><td class="c3"> وَإِنِّي <span class="auu">سَمَّيْتُهَا</span> مَرْيَمَ</td></tr><tr><td class="c1"><span class="l">(7:71:12)</span> <i class="ab">sammaytumūhā</i></td><td class="c2"><a name="(7:71:12)" href="wordmorphology.jsp?location=(7:71:12)">you have named them </a></td><td class="c3"> أَتُجَادِلُونَنِي فِي أَسْمَاءٍ <span class="auu">سَمَّيْتُمُوهَا</span> أَنْتُمْ وَآبَاؤُكُمْ</td></tr><tr><td class="c1"><span class="l">(12:40:7)</span> <i class="ab">sammaytumūhā</i></td><td class="c2"><a name="(12:40:7)" href="wordmorphology.jsp?location=(12:40:7)">which you have named them</a></td><td class="c3"> مَا تَعْبُدُونَ مِنْ دُونِهِ إِلَّا أَسْمَاءً <span class="auu">سَمَّيْتُمُوهَا</span> أَنْتُمْ وَآبَاؤُكُمْ</td></tr><tr><td class="c1"><span class="l">(13:33:13)</span> <i class="ab">sammūhum</i></td><td class="c2"><a name="(13:33:13)" href="wordmorphology.jsp?location=(13:33:13)">Name them</a></td><td class="c3"> وَجَعَلُوا لِلَّهِ شُرَكَاءَ قُلْ <span class="auu">سَمُّوهُمْ</span></td></tr><tr><td class="c1"><span class="l">(22:78:19)</span> <i class="ab">sammākumu</i></td><td class="c2"><a name="(22:78:19)" href="wordmorphology.jsp?location=(22:78:19)">named you</a></td><td class="c3"> هُوَ <span class="auu">سَمَّاكُمُ</span> الْمُسْلِمِينَ مِنْ قَبْلُ وَفِي هَٰذَا</td></tr><tr><td class="c1"><span class="l">(53:23:5)</span> <i class="ab">sammaytumūhā</i></td><td class="c2"><a name="(53:23:5)" href="wordmorphology.jsp?location=(53:23:5)">you have named them</a></td><td class="c3"> إِنْ هِيَ إِلَّا أَسْمَاءٌ <span class="auu">سَمَّيْتُمُوهَا</span> أَنْتُمْ وَآبَاؤُكُمْ</td></tr><tr><td class="c1"><span class="l">(53:27:6)</span> <i class="ab">layusammūna</i></td><td class="c2"><a name="(53:27:6)" href="wordmorphology.jsp?location=(53:27:6)">surely they name</a></td><td class="c3"> إِنَّ الَّذِينَ لَا يُؤْمِنُونَ بِالْآخِرَةِ <span class="auu">لَيُسَمُّونَ</span> الْمَلَائِكَةَ تَسْمِيَةَ الْأُنْثَىٰ</td></tr><tr><td class="c1"><span class="l">(76:18:3)</span> <i class="ab">tusammā</i></td><td class="c2"><a name="(76:18:3)" href="wordmorphology.jsp?location=(76:18:3)">named</a></td><td class="c3"> عَيْنًا فِيهَا <span class="auu">تُسَمَّىٰ</span> سَلْسَبِيلًا</td></tr></tbody></table>
    <h4 class="dxe">Noun</h4>
    <table class="taf" border="0" cellpadding="0" cellspacing="0"><tbody><tr><td class="c1"><span class="l">(19:7:12)</span> <i class="ab">samiyyan</i></td><td class="c2"><a name="(19:7:12)" href="wordmorphology.jsp?location=(19:7:12)">(this) name</a></td><td class="c3"> يَا زَكَرِيَّا إِنَّا نُبَشِّرُكَ بِغُلَامٍ اسْمُهُ يَحْيَىٰ لَمْ نَجْعَلْ لَهُ مِنْ قَبْلُ <span class="auu">سَمِيًّا</span></td></tr><tr><td class="c1"><span class="l">(19:65:12)</span> <i class="ab">samiyyan</i></td><td class="c2"><a name="(19:65:12)" href="wordmorphology.jsp?location=(19:65:12)">any similarity</a></td><td class="c3"> فَاعْبُدْهُ وَاصْطَبِرْ لِعِبَادَتِهِ هَلْ تَعْلَمُ لَهُ <span class="auu">سَمِيًّا</span></td></tr></tbody></table>
    """
