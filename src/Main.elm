module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Json.Decode as Decode exposing (Decoder, bool, int, list, map, string)
import MockData exposing (brewJSON)



---- MODEL ----


type Model
    = Failure
    | Success Brew


init : ( Model, Cmd Msg )
init =
    ( brew brewJSON, Cmd.none )


type alias Brew =
    { name : String }


brew : String -> Model
brew json =
    case Decode.decodeString brewDecoder brewJSON of
        Ok decodedBrew ->
            Success decodedBrew

        Err error ->
            Failure


brewDecoder : Decoder Brew
brewDecoder =
    map Brew
        (Decode.at [ "brew", "name" ] Decode.string)



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "Not good"

        Success brewModel ->
            div []
                [ img [ src "/logo.svg" ] []
                , h1 [] [ text brewModel.name ]
                ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
