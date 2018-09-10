module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation
import Html
import Html.Attributes
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Time
import Url



---- MODEL ----


type alias Model =
    { key : Browser.Navigation.Key -- Only used as a safeguard by utilities like Browser.Navigation.pushUrl
    , entries : Data
    , page : Page
    }


type Data
    = Requested
    | Received (List Entry)
    | Error Http.Error


type alias Entry =
    { id : Int
    , title : String
    , summary : String
    , content : String
    , flagged : Bool
    , seen : Bool
    , bookmark : Bool
    , image : String
    , link : String
    , updated : Time.Posix
    , sources : List Source
    }


type alias Source =
    { title : String
    , subtitle : String
    , link : String
    , active : Bool
    }


type Page
    = Home
    | NewFeed


init : flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key, entries = Requested, page = urlToPage url }, getEntries )


urlToPage : Url.Url -> Page
urlToPage { fragment } =
    case fragment of
        Just "new-feed" ->
            NewFeed

        _ ->
            Home



---- UPDATE ----


type Msg
    = NewEntries (Result Http.Error (List Entry))
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged url ->
            ( { model | page = urlToPage url }, Cmd.none )

        NewEntries (Err error) ->
            let
                _ =
                    Debug.log "error while retrieving entries" error
            in
            ( { model | entries = Error error }, Cmd.none )

        NewEntries (Ok entries) ->
            ( { model | entries = Received entries }, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.page of
                Home ->
                    case model.entries of
                        Requested ->
                            Html.text "Entries have been requested, please hold tight!"

                        Received entries ->
                            viewMain entries

                        Error error ->
                            Html.text "An error occured while requesting the entries"

                NewFeed ->
                    viewNewFeed
    in
    { title = "reRSS client"
    , body =
        [ Html.section [ Html.Attributes.class "main" ]
            [ viewHeader
            , viewAside
            , content
            , viewEntry
            ]
        ]
    }


viewHeader : Html.Html Msg
viewHeader =
    Html.header [ Html.Attributes.class "header" ]
        [ Html.progress [ Html.Attributes.style "display" "none" ] []
        , Html.section []
            [ Html.a
                [ Html.Attributes.href "#feeds"
                , Html.Attributes.class "button button-link"
                ]
                [ Html.text "Feeds" ]
            , Html.a
                [ Html.Attributes.href "http://0.0.0.0:1707/myfeed/atom"
                , Html.Attributes.class "button button-link"
                ]
                [ Html.i [ Html.Attributes.class "fa fa-rss" ] []
                , Html.text " My reRSS"
                ]
            ]
        , Html.section [ Html.Attributes.class "text-center" ]
            [ Html.a [ Html.Attributes.href "/" ] [ Html.text "uRSS" ]
            ]
        , Html.section [ Html.Attributes.class "text-right" ]
            [ Html.a
                [ Html.Attributes.href "#"
                , Html.Attributes.class "button"
                ]
                [ Html.i [ Html.Attributes.class "fa fa-refresh" ] [] ]
            , Html.text " "
            , Html.a
                [ Html.Attributes.href "#new-feed"
                , Html.Attributes.class "button"
                ]
                [ Html.i [ Html.Attributes.class "fa fa-plus" ] []
                , Html.text " New feed"
                ]
            ]
        ]


viewAside : Html.Html Msg
viewAside =
    Html.aside []
        [ viewFeeds
        ]


viewFeeds =
    Html.div [] []


viewMain : List Entry -> Html.Html Msg
viewMain entries =
    Html.section []
        [ viewEntries entries
        , viewNewFeed
        , viewEditfeed
        ]


viewEntries : List Entry -> Html.Html Msg
viewEntries entries =
    Html.div []
        [ viewTabs
        , Html.div [ Html.Attributes.class "cards" ]
            (List.map viewEntryItem entries)
        ]


viewTabs =
    Html.div [ Html.Attributes.class "tabs" ]
        [ Html.nav [ Html.Attributes.class "tabs-nav" ]
            [ Html.a [ Html.Attributes.class "{active: status == 'all'}", Html.Attributes.href "#" ]
                [ Html.text "All" ]
            , Html.a [ Html.Attributes.class "{active: status == 'unseen'}", Html.Attributes.href "#" ]
                [ Html.text "Unseen" ]
            , Html.a [ Html.Attributes.class "{active: status == 'bookmark'}", Html.Attributes.href "#" ]
                [ Html.text "Bookmarked" ]
            , Html.a [ Html.Attributes.class "{active: status == 'trending'}", Html.Attributes.href "#" ]
                [ Html.text "Trending" ]
            ]
        ]


viewEntryItem : Entry -> Html.Html Msg
viewEntryItem entry =
    let
        titleNode =
            Html.h4 []
                [ Html.text entry.title ]

        entryContent =
            if entry.image /= "" then
                [ titleNode
                , Html.div [ Html.Attributes.class "card-image" ]
                    [ Html.img [ Html.Attributes.class "img-responsive text-center", Html.Attributes.src entry.image ] [] ]
                ]

            else
                [ titleNode ]
    in
    Html.div [ Html.Attributes.class "card" ]
        [ Html.h5 []
            [ Html.span [ Html.Attributes.class "label" ]
                (entry.sources
                    |> List.map (\e -> e.title)
                    |> List.intersperse " ∙ "
                    |> List.map Html.text
                )
            ]
        , Html.h6 []
            [ Time.posixToMillis entry.updated
                |> String.fromInt
                |> Html.text
            ]
        , Html.a [ Html.Attributes.href "#" ]
            entryContent
        , Html.footer []
            [ Html.button [ Html.Attributes.title "Add to your ReRSS feed" ]
                [ Html.i [ Html.Attributes.class "fa fa-rss" ]
                    []
                ]
            , Html.button [ Html.Attributes.title "Remove from your ReRSS feed" ]
                [ Html.i [ Html.Attributes.class "fa fa-stop" ]
                    []
                ]
            , Html.button [ Html.Attributes.title "Read it later" ]
                [ Html.i [ Html.Attributes.class "fa fa-bookmark" ]
                    []
                ]
            , Html.button [ Html.Attributes.title "Mark as seen" ]
                [ Html.i [ Html.Attributes.class "fa fa-check" ]
                    []
                ]
            , Html.a [ Html.Attributes.class "button", Html.Attributes.href entry.link, Html.Attributes.title "Open" ]
                [ Html.i [ Html.Attributes.class "fa fa-link" ]
                    []
                ]
            ]
        ]


viewNewFeed : Html.Html Msg
viewNewFeed =
    Html.div []
        [ Html.form [ Html.Attributes.attribute "onsubmit" "{ add }" ]
            [ Html.fieldset []
                [ Html.legend []
                    [ Html.text "Add a new feed" ]
                , Html.div [ Html.Attributes.class "input-single" ]
                    [ Html.label [ Html.Attributes.for "link" ]
                        [ Html.text "URL" ]
                    , Html.input
                        [ Html.Attributes.class "input-big"
                        , Html.Attributes.id "link"
                        , Html.Attributes.placeholder "Feed URL"
                        , Html.Attributes.type_ "text"
                        ]
                        []
                    , Html.text "  "
                    ]
                , Html.div [ Html.Attributes.class "input-single" ]
                    [ Html.input [ Html.Attributes.class "button", Html.Attributes.type_ "submit", Html.Attributes.value "Create new feed" ]
                        []
                    , Html.a [ Html.Attributes.class "button button-link", Html.Attributes.href "/", Html.Attributes.type_ "reset" ]
                        [ Html.text "Cancel" ]
                    ]
                ]
            ]
        ]


viewEditfeed =
    Html.div [] []


viewEntry =
    Html.section []
        [ Html.div [] [] ]



---- DECODERS ----


entriesDecoder : Decode.Decoder (List Entry)
entriesDecoder =
    Decode.field "entries" (Decode.list entryDecoder)


entryDecoder : Decode.Decoder Entry
entryDecoder =
    Decode.succeed Entry
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "title" Decode.string
        |> Pipeline.required "summary" Decode.string
        |> Pipeline.optional "content" Decode.string ""
        |> Pipeline.required "flagged" Decode.bool
        |> Pipeline.required "seen" Decode.bool
        |> Pipeline.required "bookmark" Decode.bool
        |> Pipeline.optional "image" Decode.string ""
        |> Pipeline.required "link" Decode.string
        |> Pipeline.required "updated" updatedDecoder
        |> Pipeline.required "sources" (Decode.list sourceDecoder)


sourceDecoder : Decode.Decoder Source
sourceDecoder =
    Decode.succeed Source
        |> Pipeline.required "title" Decode.string
        |> Pipeline.required "subtitle" Decode.string
        |> Pipeline.required "link" Decode.string
        |> Pipeline.required "active" Decode.bool


updatedDecoder : Decode.Decoder Time.Posix
updatedDecoder =
    Decode.int
        |> Decode.map Time.millisToPosix


getEntries : Cmd Msg
getEntries =
    Http.get "http://0.0.0.0:1707/entry?seen=0&bookmark=0" entriesDecoder
        |> Http.send NewEntries



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , update = update
        , subscriptions = always Sub.none
        }
