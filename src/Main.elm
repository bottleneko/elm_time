import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)

main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- MODEL

type alias Model =
    { time : Time
    , paused : Bool
    }

init : (Model, Cmd Msg)
init =
    (Model 0 False, Cmd.none)

-- UPDATE

type Msg = Tick Time | Pause

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            if model.paused == True then
                (model, Cmd.none)
            else
                ({ model | time = newTime }, Cmd.none)

        Pause ->
            ({ model | paused = not model.paused }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.paused of
        True ->
            Sub.none

        False ->
            Time.every second Tick

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ svg [ viewBox "0 0 100 100", Svg.Attributes.width "300px" ]
            [ circle [ cx "50", cy "50", r "45", fill "#0B79CE"] []
            , line [ x1 "50", y1 "50", x2 (handX Time.inMinutes model), y2 (handY Time.inMinutes model), stroke "#023963" ] []
            , line [ x1 "50", y1 "50", x2 (handX Time.inHours model), y2 (handY Time.inHours model), stroke "#073FFF" ] []
            , line [ x1 "50", y1 "50", x2 (handX inDays model), y2 (handY inDays model), stroke "#0FF030" ] []
            ]
        , button [ onClick Pause ] [ Html.text "Pause" ]
        ]

inDays : Time -> Float
inDays t =
    t / (24*Time.hour)

angle : (Time -> Float) -> Model -> Float
angle inFormat model =
    turns (inFormat model.time)

handX : (Time -> Float) -> Model -> String
handX inFormat model =
    toString (50 + 40 * cos (angle inFormat model))

handY : (Time -> Float) -> Model -> String
handY inFormat model =
    toString (50 + 40 * sin (angle inFormat model))


