module Pages.Home exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Ports
import Session exposing (Session)


type alias Model =
    { session : Session, value : Int }


type Msg
    = Submit


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, value = 0 }, Cmd.none )


toSession : Model -> Session
toSession m =
    m.session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            ( model, model |> diffRequest |> Ports.diffRequest )


diffRequest : Model -> JE.Value
diffRequest model =
    JE.null


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type InputLabel
    = Before
    | After


type DiffType
    = Char
    | Word
    | Sentence
    | Json
    | SortedJson


diffTypes =
    [ Char
    , Word
    , Sentence
    , Json
    , SortedJson
    ]


view : Model -> List (Html Msg)
view model =
    [ h1 [] [ text "Diff" ]
    , H.form [ class "container", onSubmit Submit ]
        [ viewInputBody Before model
        , viewInputBody After model
        , div [] (text "Diff type: " :: List.map (viewDiffType model) diffTypes)
        , input [ type_ "submit" ] [ text "Compare" ]
        ]
    , code [ class "diff" ] []
    ]


viewDiffType : Model -> DiffType -> Html msg
viewDiffType model t =
    label []
        [ input [ type_ "radio" ] []
        , text " "
        , text <| formatDiffType t
        ]


viewInputBody : InputLabel -> Model -> Html msg
viewInputBody label_ model =
    div [ class "input-body" ]
        [ h4 [] [ text <| formatLabel label_ ]
        , div []
            [ label [] [ input [ type_ "radio" ] [], text "URL " ]
            , input [ placeholder "http://.../blah.json" ] []
            ]
        , div []
            [ label [] [ input [ type_ "radio" ] [], text "File " ]
            , input [ type_ "file" ] []
            ]
        , div []
            [ label [] [ input [ type_ "radio" ] [], text "Text " ]
            , textarea [ rows 1 ] []
            ]
        ]


formatDiffType : DiffType -> String
formatDiffType t =
    case t of
        Char ->
            "Char"

        Word ->
            "Word"

        Sentence ->
            "Sentence"

        Json ->
            "JSON"

        SortedJson ->
            "Sorted JSON"


formatLabel : InputLabel -> String
formatLabel label_ =
    case label_ of
        Before ->
            "Before"

        After ->
            "After"
