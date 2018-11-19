module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, h3, img, li, p, text, ul)
import Html.Attributes exposing (class, src)
import Json.Decode as Decode exposing (Decoder, bool, int, list, map2, string)
import Json.Decode.Extra as Extra
import MockData exposing (brewJSON)
import Time exposing (Month(..))



---- MODEL ----


type Model
    = Failure Decode.Error
    | Success Brew


init : ( Model, Cmd Msg )
init =
    ( brew brewJSON, Cmd.none )


type alias Brew =
    { name : String
    , createdAt : Time.Posix
    , batchSizeLitres : Float
    , boilLossLitres : Float
    , fermentables : List Fermentable
    }


type alias Fermentable =
    { name : String
    , weightGrams : Float
    }


brew : String -> Model
brew json =
    case Decode.decodeString brewDecoder brewJSON of
        Ok decodedBrew ->
            Success decodedBrew

        Err error ->
            Failure error


brewDecoder : Decoder Brew
brewDecoder =
    Decode.map5
        (\brewName createdAt batchSizeLitres boilLossLitres fermentables ->
            { brewName = brewName
            , createdAt = createdAt
            , batchSizeLitres = batchSizeLitres
            , boilLossLitres = boilLossLitres
            , fermentables = fermentables
            }
        )
        (Decode.at [ "brew", "name" ] Decode.string)
        (Decode.at [ "brew", "created_at" ] Extra.datetime)
        (Decode.at [ "brew", "batch_size_litres" ] Extra.parseFloat)
        (Decode.at [ "brew", "boil_loss_litres" ] Extra.parseFloat)
        (Decode.field "fermentables" (Decode.list fermentableDecoder))
        |> Decode.andThen
            (\{ brewName, createdAt, batchSizeLitres, boilLossLitres, fermentables } ->
                let
                    fermentablesDict =
                        Dict.fromList fermentables
                in
                Decode.field "fermentable_additions" (Decode.list (fermentableAdditionDecoder fermentablesDict))
                    |> Decode.andThen
                        (\additions ->
                            Decode.succeed
                                { name = brewName
                                , createdAt = createdAt
                                , batchSizeLitres = batchSizeLitres
                                , boilLossLitres = boilLossLitres
                                , fermentables = additions
                                }
                        )
            )


fermentableDecoder : Decoder ( Int, String )
fermentableDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)


fermentableAdditionDecoder : Dict Int String -> Decoder Fermentable
fermentableAdditionDecoder fermentablesDict =
    Decode.map2 Tuple.pair
        (Decode.field "fermentable_id" Decode.int)
        (Decode.field "weight_grams" Extra.parseFloat)
        |> Decode.andThen
            (\( id, weightGrams ) ->
                case Dict.get id fermentablesDict of
                    Just name ->
                        Decode.succeed
                            { name = name
                            , weightGrams = weightGrams
                            }

                    Nothing ->
                        Decode.fail
                            ("Fermentable with ID "
                                ++ String.fromInt id
                                ++ " not found"
                            )
            )



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
        Failure err ->
            text ("Error:" ++ Decode.errorToString err)

        Success brewModel ->
            div [ class "mx-auto h-full justify-center items-center" ]
                [ h1 [ class "font-hairline mb-6 mt-6" ] [ text brewModel.name ]
                , h3 [ class "font-hairline mb-6 mt-6" ] [ text ("Created " ++ formatTime brewModel.createdAt) ]
                , listFermentables brewModel.fermentables
                , p [] [ text ("Total Weight: " ++ totalWeight brewModel.fermentables) ]
                , p [] [ text ("Batch Size Litres" ++ String.fromFloat brewModel.batchSizeLitres) ]
                , p [] [ text ("Boil Loss Litres" ++ String.fromFloat brewModel.boilLossLitres) ]
                ]


totalWeight : List Fermentable -> String
totalWeight fermentables =
    List.map (\n -> n.weightGrams) fermentables |> List.sum |> String.fromFloat


listFermentables : List Fermentable -> Html Msg
listFermentables fermentables =
    ul [ class "list-reset" ]
        (List.map
            fermentableItem
            fermentables
        )


fermentableItem : Fermentable -> Html Msg
fermentableItem fermentable =
    li [] [ text (String.fromFloat fermentable.weightGrams ++ " - " ++ fermentable.name) ]


formatTime : Time.Posix -> String
formatTime time =
    let
        month =
            case Time.toMonth Time.utc time of
                Jan ->
                    "January"

                Feb ->
                    "February"

                Mar ->
                    "March"

                Apr ->
                    "April"

                May ->
                    "May"

                Jun ->
                    "June"

                Jul ->
                    "July"

                Aug ->
                    "August"

                Sep ->
                    "September"

                Oct ->
                    "October"

                Nov ->
                    "November"

                Dec ->
                    "December"

        day =
            String.fromInt (Time.toDay Time.utc time)

        year =
            String.fromInt (Time.toYear Time.utc time)
    in
    month ++ " " ++ day ++ ", " ++ year



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
