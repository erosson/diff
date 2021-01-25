module Pages.Home exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Dict exposing (Dict)
import Html as H exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events as E exposing (..)
import Json.Decode as JD
import Json.Encode as JE
import Pages.NotFound
import Ports
import RemoteData exposing (RemoteData)
import Route exposing (Route)
import Session exposing (Session)


type alias Model =
    { session : Session
    , before : InputModel
    , after : InputModel
    , difftype : DiffType
    , diff : RemoteData String (List DiffEntry)
    }


type alias InputModel =
    { method : Method
    , text : String
    , url : String
    }


type Method
    = UrlMethod
      -- | FileMethod
    | TextMethod


type InputLabel
    = Before
    | After


type alias DiffEntry =
    { status : DiffEntryStatus, value : String }


type DiffEntryStatus
    = Added
    | Removed
    | Common


type DiffType
    = Char
    | Word
    | Sentence
    | Json
    | SortedJson


type alias DiffTypeData =
    { type_ : DiffType, label : String, id : String }


diffTypeDefault : DiffType
diffTypeDefault =
    Char


diffTypes : List DiffType
diffTypes =
    [ Char
    , Word
    , Sentence
    , Json
    , SortedJson
    ]


diffTypeData : DiffType -> DiffTypeData
diffTypeData t =
    case t of
        Char ->
            { type_ = t, label = "Char", id = "char" }

        Word ->
            { type_ = t, label = "Word", id = "word" }

        Sentence ->
            { type_ = t, label = "Sentence", id = "sentence" }

        Json ->
            { type_ = t, label = "JSON", id = "json" }

        SortedJson ->
            { type_ = t, label = "Sorted JSON", id = "sortjson" }


diffTypesById : Dict String DiffTypeData
diffTypesById =
    diffTypes |> List.map (diffTypeData >> (\t -> ( t.id, t ))) |> Dict.fromList


type Msg
    = Submit
    | DiffTypeClicked DiffType
    | InputMethodClicked InputLabel Method
    | InputText InputLabel String
    | InputUrl InputLabel String
    | DiffResponse JD.Value


init : Route.HomeParams -> Session -> ( Model, Cmd Msg )
init params session =
    let
        model =
            { session = session
            , before = inputInit params.beforeUrl params.beforeText
            , after = inputInit params.afterUrl params.afterText
            , difftype =
                params.difftype
                    |> Maybe.andThen (\t -> Dict.get t diffTypesById)
                    |> Maybe.map .type_
                    |> Maybe.withDefault diffTypeDefault
            , diff = RemoteData.Loading
            }
    in
    ( model, model |> encodeDiffRequest |> Ports.diffRequest )


inputInit : Maybe String -> Maybe String -> InputModel
inputInit murl mtext =
    case ( murl, mtext ) of
        ( Just url, _ ) ->
            { method = UrlMethod, text = "", url = url }

        ( Nothing, Just text ) ->
            { method = TextMethod, text = text, url = "" }

        _ ->
            { method = TextMethod, text = "", url = "" }


toSession : Model -> Session
toSession m =
    m.session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DiffTypeClicked type_ ->
            ( { model | difftype = type_ }, Cmd.none )

        InputMethodClicked label method ->
            ( model |> updateIModel label (\imodel -> { imodel | method = method }), Cmd.none )

        InputText label text ->
            ( model |> updateIModel label (\imodel -> { imodel | method = TextMethod, text = text }), Cmd.none )

        InputUrl label url ->
            ( model |> updateIModel label (\imodel -> { imodel | method = UrlMethod, url = url }), Cmd.none )

        Submit ->
            ( model, model |> toRoute |> Route.pushUrl model.session.nav )

        DiffResponse json ->
            ( { model
                | diff =
                    json
                        |> JD.decodeValue diffResponseDecoder
                        |> Result.mapError JD.errorToString
                        |> RemoteData.fromResult
              }
            , Cmd.none
            )


toRoute : Model -> Route
toRoute model =
    Route.Home
        { difftype = justIf (model.difftype /= diffTypeDefault) (diffTypeData model.difftype).id
        , beforeUrl = justIf (model.before.method == UrlMethod) model.before.url
        , beforeText = justIf (model.before.method == TextMethod) model.before.text
        , afterUrl = justIf (model.after.method == UrlMethod) model.after.url
        , afterText = justIf (model.after.method == TextMethod) model.after.text
        }


justIf : Bool -> a -> Maybe a
justIf pred val =
    if pred then
        Just val

    else
        Nothing


updateIModel : InputLabel -> (InputModel -> InputModel) -> Model -> Model
updateIModel label fn model =
    case label of
        Before ->
            { model | before = fn model.before }

        After ->
            { model | after = fn model.after }


diffRequest : Model -> JE.Value
diffRequest model =
    JE.null


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.diffResponse DiffResponse


view : Model -> List (Html Msg)
view model =
    [ h1 [] [ text "Diff" ]
    , H.form [ class "container", onSubmit Submit ]
        [ viewInputBody Before model
        , viewInputBody After model
        , div [] (text "Diff type: " :: List.map (viewDiffType model) diffTypes)
        , input [ type_ "submit" ] [ text "Compare" ]
        ]
    , div [] <|
        case model.diff of
            RemoteData.Success diffs ->
                let
                    numIns =
                        diffs |> List.filter (\d -> d.status == Added) |> List.length

                    numDel =
                        diffs |> List.filter (\d -> d.status == Removed) |> List.length

                    numCommon =
                        List.length diffs - numIns - numDel
                in
                [ p []
                    [ text <| String.fromInt numIns
                    , text " additions, "
                    , text <| String.fromInt numDel
                    , text " deletions, "
                    , text <| String.fromInt numCommon
                    , text " unchanged"
                    ]
                , pre [ class "diff" ] (diffs |> List.map viewDiffEntry)
                ]

            RemoteData.Failure err ->
                [ pre [ class "diff error" ] [ text err ] ]

            _ ->
                [ pre [ class "diff error" ] [ text "loading..." ] ]
    ]


viewDiffEntry : DiffEntry -> Html msg
viewDiffEntry entry =
    case entry.status of
        Added ->
            ins [] [ text entry.value ]

        Removed ->
            del [] [ text entry.value ]

        Common ->
            span [] [ text entry.value ]


viewDiffType : Model -> DiffType -> Html Msg
viewDiffType model t =
    label []
        [ input
            [ type_ "radio"
            , checked <| model.difftype == t
            , onCheck <| always <| DiffTypeClicked t
            ]
            []
        , text " "
        , text <| .label <| diffTypeData t
        ]


viewInputBody : InputLabel -> Model -> Html Msg
viewInputBody label_ model =
    let
        imodel =
            case label_ of
                Before ->
                    model.before

                After ->
                    model.after
    in
    div [ class "input-body" ]
        [ h4 [] [ text <| formatLabel label_ ]
        , div []
            [ label []
                [ input
                    [ type_ "radio"
                    , checked <| imodel.method == UrlMethod
                    , onCheck <| always <| InputMethodClicked label_ UrlMethod
                    ]
                    []
                , text "URL "
                ]
            , input
                [ placeholder "https://api.pathofexile.com/leagues"
                , onInput <| InputUrl label_
                , value imodel.url
                ]
                []
            , a [ href imodel.url, target "_blank" ] [ text imodel.url ]
            ]

        --, div []
        --    [ label []
        --        [ input
        --            [ type_ "radio"
        --            , checked <| imodel.method == FileMethod
        --            , onCheck <| always <| InputMethodClicked label_ FileMethod
        --            ]
        --            []
        --        , text "File "
        --        ]
        --    , input
        --        [ type_ "file"
        --        ]
        --        []
        --    ]
        , div []
            [ label []
                [ input
                    [ type_ "radio"
                    , checked <| imodel.method == TextMethod
                    , onCheck <| always <| InputMethodClicked label_ TextMethod
                    ]
                    []
                , text "Text "
                ]
            , textarea
                [ rows 1
                , onInput <| InputText label_
                ]
                [ text imodel.text ]
            ]
        ]


formatLabel : InputLabel -> String
formatLabel label_ =
    case label_ of
        Before ->
            "Before"

        After ->
            "After"


formatMethod : Method -> String
formatMethod m =
    case m of
        UrlMethod ->
            "url"

        TextMethod ->
            "text"


diffResponseDecoder : JD.Decoder (List DiffEntry)
diffResponseDecoder =
    JD.andThen
        (\status ->
            case status of
                "success" ->
                    JD.map2 DiffEntry
                        (JD.oneOf
                            [ JD.field "added" JD.bool |> JD.map (always Added)
                            , JD.field "removed" JD.bool |> JD.map (always Removed)
                            , JD.succeed Common
                            ]
                        )
                        (JD.field "value" JD.string)
                        |> JD.list
                        |> JD.field "data"

                _ ->
                    JD.fail <| "unknown status: " ++ status
        )
        (JD.field "status" JD.string)


encodeDiffRequest : Model -> JE.Value
encodeDiffRequest model =
    JE.object
        [ ( "difftype", model.difftype |> diffTypeData |> .id |> JE.string )
        , ( "before", encodeDiffRequestInput model.before )
        , ( "after", encodeDiffRequestInput model.after )
        ]


encodeDiffRequestInput : InputModel -> JE.Value
encodeDiffRequestInput imodel =
    JE.object
        [ ( "method", imodel.method |> formatMethod |> JE.string )
        , ( "text", imodel.text |> JE.string )
        , ( "url", imodel.url |> JE.string )
        ]
