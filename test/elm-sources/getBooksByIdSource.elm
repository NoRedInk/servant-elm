module GetBooksByIdSource exposing (..)

import String.Conversions as String
import Http
import SimulatedEffect.Http
import ProgramTest
import Url


getBooksById : (Result (Maybe (Http.Metadata, String), Http.Error) (Book) -> msg) -> Int -> Cmd msg
getBooksById toMsg capture_id =
    getBooksByIdTask capture_id |>
        Task.attempt toMsg


getBooksByIdTask : Int -> Task (Maybe (Http.Metadata, String), Http.Error) (Book)
getBooksByIdTask capture_id =
    Http.task
        { method =
            "GET"
        , headers =
            [ Http.header "X-Requested-With" "XMLHttpRequest"
            ]
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_id |> String.fromInt |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , resolver =
            Http.stringResolver
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString decodeBook body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_)))
                )
        , timeout =
            Nothing
        }


getBooksByIdSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (Book) -> msg) -> Int -> ProgramTest.SimulatedEffect msg
getBooksByIdSimulated toMsg capture_id =
    getBooksByIdSimulatedTask capture_id |>
        SimulatedEffect.Task.attempt toMsg


getBooksByIdSimulatedTask : Int -> ProgramTest.SimulatedTask (Maybe (Http.Metadata, String), Http.Error) (Book)
getBooksByIdSimulatedTask capture_id =
    SimulatedEffect.Http.task
        { method =
            "GET"
        , headers =
            [ SimulatedEffect.Http.header "X-Requested-With" "XMLHttpRequest"
            ]
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_id |> String.fromInt |> Url.percentEncode
                ]
        , body =
            SimulatedEffect.Http.emptyBody
        , resolver =
            SimulatedEffect.Http.stringResolver
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString decodeBook body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_)))
                )
        , timeout =
            Nothing
        }
