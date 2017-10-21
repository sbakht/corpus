module Main exposing (..)

import Html exposing (text, div, p, span, Html, li, ul)
import List exposing (map)
import String exposing (concat)
import Regex exposing (..)
import Parser exposing (Parser)


type Msg
    = Msg


lis : Regex
lis =
    regex <|
        concat <|
            [ "<td class=\"c1\"><span class=\"l\">(.+?)</span>.*?<i class=\"ab\">(.+?)</i></td>"
            , "<td class=\"c2\"><a .*?>(.+?)</a></td>"
            ]



--<td class="c1"><span class="l">(7:57:14)</span> <i class="ab">suq'nāhu</i></td><


items : List Match
items =
    find All lis data


matchText : List Match -> List String
matchText m =
    map .match m


matchSubMatches : List Match -> List (List (Maybe String))
matchSubMatches m =
    map .submatches m


line : List (Maybe String) -> Html Msg
line xs =
    let
        go x =
            case x of
                Just x ->
                    span [] [ text x ]

                Nothing ->
                    span [] []
    in
        li [] (map go xs)


lines : List (List (Maybe String)) -> List (Html Msg)
lines xss =
    map line xss


main =
    div []
        [ p [] [ text <| toString <| matchText items ]
        , ul [] (lines (matchSubMatches items))
        ]


data : String
data =
    """
<table class="taf" border="0" cellpadding="0" cellspacing="0"><tbody><tr><td class="c1"><span class="l">(7:57:14)</span> <i class="ab">suq'nāhu</i></td><td class="c2"><a name="(7:57:14)" href="wordmorphology.jsp?location=(7:57:14)">We drive them</a></td><td class="c3"> حَتَّىٰ إِذَا أَقَلَّتْ سَحَابًا ثِقَالًا <span class="auu">سُقْنَاهُ</span> لِبَلَدٍ مَيِّتٍ فَأَنْزَلْنَا بِهِ الْمَاءَ</td></tr><tr><td class="c1"><span class="l">(8:6:7)</span> <i class="ab">yusāqūna</i></td><td class="c2"><a name="(8:6:7)" href="wordmorphology.jsp?location=(8:6:7)">they were driven</a></td><td class="c3"> كَأَنَّمَا <span class="auu">يُسَاقُونَ</span> إِلَى الْمَوْتِ وَهُمْ يَنْظُرُونَ</td></tr><tr><td class="c1"><span class="l">(19:86:1)</span> <i class="ab">wanasūqu</i></td><td class="c2"><a name="(19:86:1)" href="wordmorphology.jsp?location=(19:86:1)">And We will drive</a></td><td class="c3"> <span class="auu">وَنَسُوقُ</span> الْمُجْرِمِينَ إِلَىٰ جَهَنَّمَ وِرْدًا</td></tr><tr><td class="c1"><span class="l">(32:27:4)</span> <i class="ab">nasūqu</i></td><td class="c2"><a name="(32:27:4)" href="wordmorphology.jsp?location=(32:27:4)">drive</a></td><td class="c3"> أَوَلَمْ يَرَوْا أَنَّا <span class="auu">نَسُوقُ</span> الْمَاءَ إِلَى الْأَرْضِ الْجُرُزِ فَنُخْرِجُ بِهِ زَرْعًا</td></tr><tr><td class="c1"><span class="l">(35:9:7)</span> <i class="ab">fasuq'nāhu</i></td><td class="c2"><a name="(35:9:7)" href="wordmorphology.jsp?location=(35:9:7)">and We drive them</a></td><td class="c3"> وَاللَّهُ الَّذِي أَرْسَلَ الرِّيَاحَ فَتُثِيرُ سَحَابًا <span class="auu">فَسُقْنَاهُ</span> إِلَىٰ بَلَدٍ مَيِّتٍ</td></tr><tr><td class="c1"><span class="l">(39:71:1)</span> <i class="ab">wasīqa</i></td><td class="c2"><a name="(39:71:1)" href="wordmorphology.jsp?location=(39:71:1)">And (will) be driven</a></td><td class="c3"> <span class="auu">وَسِيقَ</span> الَّذِينَ كَفَرُوا إِلَىٰ جَهَنَّمَ زُمَرًا</td></tr><tr><td class="c1"><span class="l">(39:73:1)</span> <i class="ab">wasīqa</i></td><td class="c2"><a name="(39:73:1)" href="wordmorphology.jsp?location=(39:73:1)">And (will) be driven</a></td><td class="c3"> <span class="auu">وَسِيقَ</span> الَّذِينَ اتَّقَوْا رَبَّهُمْ إِلَى الْجَنَّةِ زُمَرًا</td></tr></tbody></table>
"""
