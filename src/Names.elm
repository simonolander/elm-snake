module Names exposing (..)

import Model
import Random
import Util
names =
    [ "adam"
    , "alena"
    , "amara"
    , "annetta"
    , "anny"
    , "ansley"
    , "arabelle"
    , "archibaldo"
    , "ardra"
    , "audre"
    , "aurelia"
    , "barb"
    , "barbi"
    , "barnabe"
    , "benny"
    , "benyamin"
    , "bernie"
    , "blair"
    , "blancha"
    , "bonita"
    , "caddric"
    , "camila"
    , "candace"
    , "carey"
    , "claudelle"
    , "clayson"
    , "corby"
    , "corrinne"
    , "danya"
    , "davita"
    , "denis"
    , "denys"
    , "donalt"
    , "donelle"
    , "donni"
    , "elladine"
    , "emerson"
    , "ermanno"
    , "ernest"
    , "eugenie"
    , "eugenio"
    , "felicle"
    , "forrester"
    , "franny"
    , "gennie"
    , "georgeanne"
    , "gerome"
    , "ginni"
    , "holly"
    , "ives"
    , "jemimah"
    , "jenn"
    , "jo-ann"
    , "joly"
    , "juanita"
    , "karlene"
    , "karyl"
    , "katine"
    , "klarrisa"
    , "larina"
    , "leonerd"
    , "lotti"
    , "marchelle"
    , "marget"
    , "mariellen"
    , "marwin"
    , "millie"
    , "morris"
    , "myra"
    , "neille"
    , "oby"
    , "paddy"
    , "rachelle"
    , "rafferty"
    , "rhonda"
    , "rici"
    , "rick"
    , "roma"
    , "roseanne"
    , "rouvin"
    , "rowland"
    , "sandor"
    , "shellysheldon"
    , "sheppard"
    , "sly"
    , "sondra"
    , "stanislaw"
    , "stearn"
    , "sven"
    , "thain"
    , "thekla"
    , "tisha"
    , "tresa"
    , "trescha"
    , "warren"
    , "wildon"
    , "xerxes"
    , "yves"
    ]


generateName : Cmd Model.Msg
generateName =
    Util.randomElement "anon" names |> Random.generate Model.NewName