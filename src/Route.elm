module Route exposing (HomeParams, Route(..), home, href, parse, pushUrl, pushUrl_, replaceUrl, replaceUrl_, toString)

import Browser.Navigation as Nav
import Html as H
import Html.Attributes as A
import Url exposing (Url)
import Url.Builder as B
import Url.Parser as P exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Q


type Route
    = Home HomeParams
    | Debug


type alias HomeParams =
    { difftype : Maybe String
    , beforeUrl : Maybe String
    , beforeText : Maybe String
    , afterUrl : Maybe String
    , afterText : Maybe String
    }


home : Route
home =
    Home <| HomeParams Nothing Nothing Nothing Nothing Nothing


parse : Url -> Maybe Route
parse =
    P.parse parser


parser : Parser (Route -> a) a
parser =
    P.oneOf
        [ P.map Home <|
            P.map HomeParams <|
                P.top
                    <?> Q.string "difftype"
                    <?> Q.string "before.url"
                    <?> Q.string "before.text"
                    <?> Q.string "after.url"
                    <?> Q.string "after.text"
        , P.map Debug <| P.s "debug"
        ]


toString : Route -> String
toString route =
    case route of
        Home p ->
            B.absolute []
                (List.filterMap (\( key, val ) -> Maybe.map (B.string key) val)
                    [ ( "difftype", p.difftype )
                    , ( "before.url", p.beforeUrl )
                    , ( "before.text", p.beforeText )
                    , ( "after.url", p.afterUrl )
                    , ( "after.text", p.afterText )
                    ]
                )

        Debug ->
            B.absolute [ "debug" ] []


href : Route -> H.Attribute msg
href =
    toString >> A.href


pushUrl_ : Nav.Key -> Route -> Cmd msg
pushUrl_ nav =
    toString >> Nav.pushUrl nav


pushUrl : Maybe Nav.Key -> Route -> Cmd msg
pushUrl mnav =
    case mnav of
        Nothing ->
            always Cmd.none

        Just nav ->
            pushUrl_ nav


replaceUrl_ : Nav.Key -> Route -> Cmd msg
replaceUrl_ nav =
    toString >> Nav.replaceUrl nav


replaceUrl : Maybe Nav.Key -> Route -> Cmd msg
replaceUrl mnav =
    case mnav of
        Nothing ->
            always Cmd.none

        Just nav ->
            replaceUrl_ nav
