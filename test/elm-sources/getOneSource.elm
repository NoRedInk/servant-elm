module GetOneSource exposing (..)

import Http
import SimulatedEffect.Http
import ProgramTest
import String.Conversions as String
import Json.Decode exposing (..)


getOne : (Result (Maybe (Http.Metadata, String), Http.Error) (Int) -> msg) -> Cmd msg
getOne toMsg =
    getOneTask |>
        Task.attempt toMsg


getOneTask : Task (Maybe (Http.Metadata, String), Http.Error) (Int)
getOneTask =
    Http.task
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "one"
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
                            (decodeString int body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_)))
                )
        , timeout =
            Nothing
        }


getOneSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (Int) -> msg) -> ProgramTest.SimulatedEffect msg
getOneSimulated toMsg =
    getOneSimulatedTask |>
        SimulatedEffect.Task.attempt toMsg


getOneSimulatedTask : ProgramTest.SimulatedTask (Maybe (Http.Metadata, String), Http.Error) (Int)
getOneSimulatedTask =
    SimulatedEffect.Http.task
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "one"
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
                            (decodeString int body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_)))
                )
        , timeout =
            Nothing
        }
