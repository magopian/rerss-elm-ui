port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Navigation
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Task
import Time
import Url


server : String
server =
    "http://0.0.0.0:1707"



---- MODEL ----


type alias Model =
    { key : Browser.Navigation.Key -- Only used as a safeguard by utilities like Browser.Navigation.pushUrl
    , entries : RemoteData Entry
    , feeds : RemoteData Feed
    , page : Page
    , refreshing : Bool
    , filter : Filter
    , zone : Time.Zone
    , progress : Float
    }


type RemoteData a
    = Requested
    | Received (List a)
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
    , sources : List Feed
    }


type alias Feed =
    { title : String
    , subtitle : String
    , link : String
    , active : Bool
    }


type OriginalFeed
    = OriginalFeed Feed


type Page
    = HomePage
    | NewFeedPage String
    | EditFeedPage OriginalFeed Feed


type Filter
    = All
    | Unseen
    | Bookmarked
    | Trending


init : flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , entries = Requested
      , feeds = Requested
      , page = urlToPage url
      , refreshing = True
      , filter = All
      , zone = Time.utc
      , progress = 0
      }
    , Cmd.batch [ getEntries, getFeeds, Task.perform AdjustTimeZone Time.here ]
    )


urlToPage : Url.Url -> Page
urlToPage { fragment } =
    case fragment of
        Just "new-feed" ->
            NewFeedPage ""

        _ ->
            HomePage



---- UPDATE ----


type Msg
    = NewEntries (Result Http.Error (List Entry))
    | NewFeeds (Result Http.Error (List Feed))
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | AddingNewFeed String
    | AddNewFeed String
    | NewFeedAdded (Result Http.Error Feed)
    | EditingFeed OriginalFeed Feed
    | EditFeed OriginalFeed Feed
    | EditedFeed OriginalFeed (Result Http.Error Feed)
    | Sync
    | Flag Entry
    | Bookmark Entry
    | MarkSeen Entry
    | UpdatedEntry Entry (Result Http.Error Entry)
    | UpdateFilter Filter
    | AdjustTimeZone Time.Zone
    | UpdateProgress Float
    | ProgressDone Bool


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
            ( { model | entries = Error error, refreshing = False }, Cmd.none )

        NewEntries (Ok entries) ->
            ( { model | entries = Received entries, refreshing = False }, Cmd.none )

        NewFeeds (Err error) ->
            let
                _ =
                    Debug.log "error while retrieving feeds" error
            in
            ( { model | feeds = Error error }, Cmd.none )

        NewFeeds (Ok feeds) ->
            ( { model | feeds = Received feeds }, Cmd.none )

        AddingNewFeed feedUrl ->
            ( { model | page = NewFeedPage feedUrl }, Cmd.none )

        AddNewFeed feedUrl ->
            let
                jsonBody =
                    [ ( "link", Encode.string feedUrl ) ]
                        |> Encode.object
                        |> Http.jsonBody
            in
            ( model
            , Http.post (server ++ "/feed") jsonBody feedDecoder
                |> Http.send NewFeedAdded
            )

        NewFeedAdded (Err error) ->
            let
                _ =
                    Debug.log "error while adding new feed" error
            in
            ( model, Cmd.none )

        NewFeedAdded (Ok feed) ->
            let
                updatedFeeds =
                    case model.feeds of
                        Received feeds ->
                            if List.member feed feeds then
                                model.feeds

                            else
                                Received (feeds ++ [ feed ])

                        _ ->
                            Received [ feed ]
            in
            ( { model | feeds = updatedFeeds, refreshing = True }, Cmd.batch [ Browser.Navigation.pushUrl model.key "#", getEntries ] )

        EditingFeed originalFeed feed ->
            ( { model | page = EditFeedPage originalFeed feed }, Cmd.none )

        EditFeed (OriginalFeed originalFeed) feed ->
            let
                jsonBody =
                    [ ( "link", Encode.string feed.link )
                    , ( "title", Encode.string feed.title )
                    , ( "subtitle", Encode.string feed.subtitle )
                    , ( "active", Encode.bool feed.active )
                    ]
                        |> Encode.object
                        |> Http.jsonBody
            in
            ( model
            , Http.post (server ++ "/feed/" ++ originalFeed.link) jsonBody feedDecoder
                |> Http.send (EditedFeed (OriginalFeed originalFeed))
            )

        EditedFeed originalFeed (Err error) ->
            let
                _ =
                    Debug.log "error while editing feed" error
            in
            ( model, Cmd.none )

        EditedFeed (OriginalFeed originalFeed) (Ok feed) ->
            let
                updatedFeeds =
                    Received <|
                        case model.feeds of
                            Received feeds ->
                                listReplace originalFeed feed feeds

                            _ ->
                                [ feed ]

                updatedEntries =
                    case model.entries of
                        Received entries ->
                            -- Change all the entries' sources from the original feed to the newly updated one
                            entries
                                |> List.map
                                    (\entry ->
                                        { entry
                                            | sources =
                                                listReplace originalFeed feed entry.sources
                                        }
                                    )
                                |> Received

                        _ ->
                            model.entries
            in
            ( { model | feeds = updatedFeeds, entries = updatedEntries }, Browser.Navigation.pushUrl model.key "#" )

        Sync ->
            ( { model | refreshing = True }, sync () )

        Flag entry ->
            let
                jsonBody =
                    [ ( "flagged", Encode.bool (not entry.flagged) ) ]
                        |> Encode.object
                        |> Http.jsonBody
            in
            ( { model | refreshing = True }
            , Http.post (server ++ "/entry/" ++ entry.link) jsonBody entryDecoder
                |> Http.send (UpdatedEntry entry)
            )

        Bookmark entry ->
            let
                jsonBody =
                    [ ( "bookmark", Encode.bool (not entry.bookmark) ) ]
                        |> Encode.object
                        |> Http.jsonBody
            in
            ( { model | refreshing = True }
            , Http.post (server ++ "/entry/" ++ entry.link) jsonBody entryDecoder
                |> Http.send (UpdatedEntry entry)
            )

        MarkSeen entry ->
            let
                jsonBody =
                    [ ( "seen", Encode.bool (not entry.seen) ) ]
                        |> Encode.object
                        |> Http.jsonBody
            in
            ( { model | refreshing = True }
            , Http.post (server ++ "/entry/" ++ entry.link) jsonBody entryDecoder
                |> Http.send (UpdatedEntry entry)
            )

        UpdatedEntry originalEntry (Err error) ->
            let
                _ =
                    Debug.log "error while updating entry" error
            in
            ( { model | refreshing = False }, Cmd.none )

        UpdatedEntry originalEntry (Ok entry) ->
            let
                updatedEntries =
                    Received <|
                        case model.entries of
                            Received entries ->
                                listReplace originalEntry entry entries

                            _ ->
                                [ entry ]
            in
            ( { model | entries = updatedEntries, refreshing = False }, Cmd.none )

        UpdateFilter filter ->
            ( { model | filter = filter }, Cmd.none )

        AdjustTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )

        UpdateProgress amount ->
            ( { model | progress = amount }, Cmd.none )

        ProgressDone _ ->
            ( { model | progress = 0 }, Cmd.batch [ getEntries, getFeeds ] )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.page of
                HomePage ->
                    case model.entries of
                        Requested ->
                            Html.text "Entries have been requested, please hold tight!"

                        Received entries ->
                            viewEntries entries model.filter model.zone

                        Error error ->
                            Html.text "An error occured while requesting the entries"

                NewFeedPage newFeedUrl ->
                    viewNewFeed newFeedUrl

                EditFeedPage originalFeed feed ->
                    viewEditFeed originalFeed feed

        feedList =
            case model.feeds of
                Requested ->
                    Html.text "loading feeds"

                Received feeds ->
                    viewFeeds feeds

                Error error ->
                    Html.text "An error occured while requesting the feeds"
    in
    { title = "reRSS client"
    , body =
        [ Html.section [ Html.Attributes.class "main" ]
            [ viewHeader model.refreshing model.progress
            , feedList
            , content
            , viewEntry
            ]
        ]
    }


viewHeader : Bool -> Float -> Html.Html Msg
viewHeader refreshing currentProgress =
    Html.header [ Html.Attributes.class "header" ]
        [ Html.progress
            [ Html.Attributes.style "display"
                (if currentProgress /= 0 then
                    "block"

                 else
                    "none"
                )
            , Html.Attributes.id "progress"
            , Html.Attributes.value <| String.fromFloat currentProgress
            ]
            []
        , Html.section []
            [ Html.a
                [ Html.Attributes.href "#feeds"
                , Html.Attributes.class "button button-link"
                ]
                [ Html.text "Feeds" ]
            , Html.a
                [ Html.Attributes.href (server ++ "/myfeed/atom")
                , Html.Attributes.class "button button-link"
                ]
                [ Html.i [ Html.Attributes.class "fa fa-rss" ] []
                , Html.text " My reRSS"
                ]
            ]
        , Html.section [ Html.Attributes.class "text-center" ]
            [ Html.a [ Html.Attributes.href server ] [ Html.text "uRSS" ]
            ]
        , Html.section [ Html.Attributes.class "text-right" ]
            [ Html.a
                [ Html.Attributes.href "#"
                , Html.Attributes.title "Sync the RSS feeds"
                , Html.Attributes.class "button"
                , Html.Events.onClick Sync
                ]
                [ Html.i
                    [ Html.Attributes.class <|
                        "fa fa-sync-alt"
                            ++ (if refreshing then
                                    " fa-spin"

                                else
                                    ""
                               )
                    ]
                    []
                ]
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


viewFeeds : List Feed -> Html.Html Msg
viewFeeds feeds =
    Html.aside []
        [ Html.ul []
            (feeds
                |> List.map
                    (\feed ->
                        Html.li []
                            [ Html.a
                                [ Html.Attributes.class "btn"
                                , Html.Attributes.href "#"
                                , Html.Events.onClick (EditingFeed (OriginalFeed feed) feed)
                                ]
                                [ Html.i [ Html.Attributes.class "fa fa-pencil" ] [] ]
                            , Html.a
                                [ Html.Attributes.href "#" ]
                                [ Html.text (" " ++ feed.title) ]
                            ]
                    )
            )
        ]


viewEntries : List Entry -> Filter -> Time.Zone -> Html.Html Msg
viewEntries entries currentFilter zone =
    let
        filteredEntries =
            case currentFilter of
                All ->
                    entries

                Unseen ->
                    List.filter (\e -> not e.seen) entries

                Bookmarked ->
                    List.filter (\e -> e.bookmark) entries

                Trending ->
                    entries
    in
    Html.div []
        [ viewTabs currentFilter
        , Html.div [ Html.Attributes.class "cards" ]
            (List.map (viewEntryItem zone) filteredEntries)
        ]


viewTabs currentFilter =
    let
        filterItem label filter =
            Html.a
                [ Html.Attributes.class <|
                    if currentFilter == filter then
                        "active"

                    else
                        ""
                , Html.Attributes.href "#"
                , Html.Events.onClick <| UpdateFilter filter
                ]
                [ Html.text label ]
    in
    Html.div [ Html.Attributes.class "tabs" ]
        [ Html.nav [ Html.Attributes.class "tabs-nav" ]
            [ filterItem "All" All
            , filterItem "Unseen" Unseen
            , filterItem "Bookmarked" Bookmarked
            , filterItem "Trending" Trending
            ]
        ]


viewEntryItem : Time.Zone -> Entry -> Html.Html Msg
viewEntryItem zone entry =
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
            [ Html.text <| displayTime entry.updated zone
            ]
        , Html.a [ Html.Attributes.href "#" ]
            entryContent
        , Html.footer []
            [ Html.button
                [ Html.Attributes.title "Remove from your ReRSS feed"
                , Html.Events.onClick <| Flag entry
                ]
                (if entry.flagged then
                    [ Html.i [ Html.Attributes.class "fa fa-plus-square" ]
                        []
                    ]

                 else
                    [ Html.i [ Html.Attributes.class "far fa-plus-square" ]
                        []
                    ]
                )
            , Html.button
                [ Html.Attributes.title "Bookmark: Read it later"
                , Html.Events.onClick <| Bookmark entry
                ]
                [ Html.i
                    [ Html.Attributes.class <|
                        if entry.bookmark then
                            "fa fa-bookmark"

                        else
                            "far fa-bookmark"
                    ]
                    []
                ]
            , Html.button
                [ Html.Attributes.title "Mark as seen"
                , Html.Events.onClick <| MarkSeen entry
                ]
                [ Html.i
                    [ Html.Attributes.class <|
                        if entry.seen then
                            "far fa-envelope-open"

                        else
                            "far fa-envelope"
                    ]
                    []
                ]
            , Html.a [ Html.Attributes.class "button", Html.Attributes.href entry.link, Html.Attributes.title "Open" ]
                [ Html.i [ Html.Attributes.class "fa fa-link" ]
                    []
                ]
            ]
        ]


viewNewFeed : String -> Html.Html Msg
viewNewFeed newFeedUrl =
    Html.div []
        [ Html.form [ Html.Events.onSubmit (AddNewFeed newFeedUrl) ]
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
                        , Html.Events.onInput AddingNewFeed
                        , Html.Attributes.value newFeedUrl
                        ]
                        []
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


viewEditFeed : OriginalFeed -> Feed -> Html.Html Msg
viewEditFeed originalFeed feed =
    Html.form [ Html.Events.onSubmit (EditFeed originalFeed feed) ]
        [ Html.fieldset []
            [ Html.legend []
                [ Html.text "Edit feed" ]
            , Html.div [ Html.Attributes.class "input-single" ]
                [ Html.label [ Html.Attributes.for "title" ]
                    [ Html.text "Title" ]
                , Html.input
                    [ Html.Attributes.class "input-big"
                    , Html.Attributes.id "title"
                    , Html.Attributes.placeholder "Feed title"
                    , Html.Attributes.type_ "text"
                    , Html.Attributes.value feed.title
                    , Html.Events.onInput (\newTitle -> EditingFeed originalFeed { feed | title = newTitle })
                    ]
                    []
                ]
            , Html.div [ Html.Attributes.class "input-single" ]
                [ Html.label [ Html.Attributes.for "subtitle" ]
                    [ Html.text "Subtitle" ]
                , Html.input
                    [ Html.Attributes.class "input-big"
                    , Html.Attributes.id "subtitle"
                    , Html.Attributes.placeholder "Feed URL"
                    , Html.Attributes.type_ "text"
                    , Html.Attributes.value feed.subtitle
                    , Html.Events.onInput (\newSubtitle -> EditingFeed originalFeed { feed | subtitle = newSubtitle })
                    ]
                    []
                ]
            , Html.div [ Html.Attributes.class "input-single" ]
                [ Html.label [ Html.Attributes.for "link" ]
                    [ Html.text "URL" ]
                , Html.input
                    [ Html.Attributes.class "input-big"
                    , Html.Attributes.id "link"
                    , Html.Attributes.placeholder "Feed URL"
                    , Html.Attributes.type_ "text"
                    , Html.Attributes.value feed.link
                    , Html.Events.onInput (\newLink -> EditingFeed originalFeed { feed | link = newLink })
                    ]
                    []
                ]
            , Html.div [ Html.Attributes.class "input-single" ]
                [ Html.span [ Html.Attributes.class "switch" ]
                    [ Html.input
                        [ Html.Attributes.checked feed.active
                        , Html.Attributes.id "active"
                        , Html.Attributes.class "switch"
                        , Html.Attributes.name "active"
                        , Html.Attributes.type_ "checkbox"
                        , Html.Events.onCheck (\newActive -> EditingFeed originalFeed { feed | active = newActive })
                        ]
                        []
                    , Html.label [ Html.Attributes.for "active" ]
                        [ Html.text "Active" ]
                    ]
                ]
            , Html.div [ Html.Attributes.class "input-single" ]
                [ Html.input
                    [ Html.Attributes.class "button"
                    , Html.Attributes.type_ "submit"
                    , Html.Attributes.value "Save"
                    ]
                    []
                , Html.a
                    [ Html.Attributes.class "button button-link"
                    , Html.Attributes.href "/"
                    , Html.Attributes.type_ "reset"
                    ]
                    [ Html.text "Cancel" ]
                ]
            ]
        ]


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
        |> Pipeline.required "sources" (Decode.list feedDecoder)


feedsDecoder : Decode.Decoder (List Feed)
feedsDecoder =
    Decode.field "feeds" (Decode.list feedDecoder)


feedDecoder : Decode.Decoder Feed
feedDecoder =
    Decode.succeed Feed
        |> Pipeline.required "title" Decode.string
        |> Pipeline.required "subtitle" Decode.string
        |> Pipeline.required "link" Decode.string
        |> Pipeline.required "active" Decode.bool


updatedDecoder : Decode.Decoder Time.Posix
updatedDecoder =
    Decode.int
        |> Decode.map (\seconds -> seconds * 1000 |> Time.millisToPosix)


getEntries : Cmd Msg
getEntries =
    Http.get (server ++ "/entry") entriesDecoder
        |> Http.send NewEntries


getFeeds : Cmd Msg
getFeeds =
    Http.get (server ++ "/feed") feedsDecoder
        |> Http.send NewFeeds



---- UTILS ----


listReplace : a -> a -> List a -> List a
listReplace original updated list =
    list
        |> List.map
            (\element ->
                if element == original then
                    updated

                else
                    element
            )


displayTime : Time.Posix -> Time.Zone -> String
displayTime time zone =
    let
        year =
            String.fromInt (Time.toYear zone time)

        month =
            stringFromMonth (Time.toMonth zone time)

        day =
            String.fromInt (Time.toDay zone time)

        hour =
            String.fromInt (Time.toHour zone time)

        minute =
            String.fromInt (Time.toMinute zone time)

        second =
            String.fromInt (Time.toSecond zone time)
    in
    year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second


stringFromMonth : Time.Month -> String
stringFromMonth month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , update = update
        , subscriptions = subscriptions
        }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ syncProgress UpdateProgress
        , syncDone ProgressDone
        ]



---- PORTS ----


port sync : () -> Cmd msg


port syncProgress : (Float -> msg) -> Sub msg


port syncDone : (Bool -> msg) -> Sub msg
