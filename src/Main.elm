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
    , efficiency : Int
    , boilLossLitres : Float
    , fermentables : List Fermentable
    }


type alias Fermentable =
    { name : String
    , weightGrams : Float
    , totalYield : Int
    , fermentableType : String
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
    Decode.map6
        (\brewName createdAt batchSizeLitres boilLossLitres efficiency fermentables ->
            { brewName = brewName
            , createdAt = createdAt
            , batchSizeLitres = batchSizeLitres
            , boilLossLitres = boilLossLitres
            , efficiency = efficiency
            , fermentables = fermentables
            }
        )
        (Decode.at [ "brew", "name" ] Decode.string)
        (Decode.at [ "brew", "created_at" ] Extra.datetime)
        (Decode.at [ "brew", "batch_size_litres" ] Extra.parseFloat)
        (Decode.at [ "brew", "boil_loss_litres" ] Extra.parseFloat)
        (Decode.at [ "brew", "efficiency" ] Decode.int)
        (Decode.field "fermentables" (Decode.list fermentableDecoder))
        |> Decode.andThen
            (\{ brewName, createdAt, batchSizeLitres, boilLossLitres, efficiency, fermentables } ->
                let
                    rawFermentablesTupleList =
                        List.map (\n -> ( n.id, n )) fermentables

                    fermentablesDict =
                        Dict.fromList rawFermentablesTupleList
                in
                Decode.field "fermentable_additions" (Decode.list (fermentableAdditionDecoder fermentablesDict))
                    |> Decode.andThen
                        (\additions ->
                            Decode.succeed
                                { name = brewName
                                , createdAt = createdAt
                                , batchSizeLitres = batchSizeLitres
                                , boilLossLitres = boilLossLitres
                                , efficiency = efficiency
                                , fermentables = additions
                                }
                        )
            )


type alias RawFermentable =
    { id : Int, name : String, totalYield : Int, fermentableType : String }


fermentableDecoder : Decoder RawFermentable
fermentableDecoder =
    Decode.map4 RawFermentable
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "total_yield" Decode.int)
        (Decode.field "fermentable_type" Decode.string)


fermentableAdditionDecoder : Dict Int RawFermentable -> Decoder Fermentable
fermentableAdditionDecoder fermentablesDict =
    Decode.map2 Tuple.pair
        (Decode.field "fermentable_id" Decode.int)
        (Decode.field "weight_grams" Extra.parseFloat)
        |> Decode.andThen
            (\( id, weightGrams ) ->
                case Dict.get id fermentablesDict of
                    Just rawFermentable ->
                        Decode.succeed
                            { name = rawFermentable.name
                            , weightGrams = weightGrams
                            , totalYield = rawFermentable.totalYield
                            , fermentableType = rawFermentable.fermentableType
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
                , p [] [ text ("Original Gravity" ++ String.fromFloat (roundFloat (originalGravity brewModel) 3)) ]
                ]


roundFloat : Float -> Int -> Float
roundFloat value places =
    let
        multiplier =
            10 ^ places
    in
    toFloat (round (value * toFloat multiplier)) / toFloat multiplier


originalGravity : Brew -> Float
originalGravity brewModel =
    let
        mashedExtractUnits =
            getMashedExtractUnits brewModel.fermentables

        unmashedExtractUnits =
            getUnmashedExtractUnits brewModel.fermentables

        mashedGravity =
            (mashedExtractUnits * 0.3865 * (toFloat brewModel.efficiency / 100)) / brewModel.batchSizeLitres

        unmashedGravity =
            (unmashedExtractUnits * 0.3865) / brewModel.batchSizeLitres
    in
    1 + mashedGravity + unmashedGravity


getMashedExtractUnits : List Fermentable -> Float
getMashedExtractUnits fermentables =
    List.filter (\fermentable -> mashable fermentable) fermentables
        |> List.foldl (\fermentable total -> total + extractUnits fermentable) 0


getUnmashedExtractUnits : List Fermentable -> Float
getUnmashedExtractUnits fermentables =
    List.filter (\fermentable -> not (mashable fermentable)) fermentables
        |> List.foldl (\fermentable total -> total + extractUnits fermentable) 0


extractUnits : Fermentable -> Float
extractUnits fermentable =
    (fermentable.weightGrams / 1000) * (toFloat fermentable.totalYield / 100)


mashable : Fermentable -> Bool
mashable fermentable =
    List.member fermentable.fermentableType [ "Grain", "Adjunct" ]


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
    li [] [ text (formatGramsToKilos fermentable.weightGrams ++ " - " ++ fermentable.name ++ " total yield: " ++ String.fromInt fermentable.totalYield) ]


formatGramsToKilos : Float -> String
formatGramsToKilos grams =
    roundFloat (grams / toFloat 1000) 2
        |> String.fromFloat
        |> (\val -> val ++ " kg")


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
