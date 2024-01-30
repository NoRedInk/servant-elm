module GetWithAResponseHeaderSource exposing (..)

import Http
import SimulatedEffect.Http
import ProgramTest
import String.Conversions as String
import Json.Decode exposing (..)


getWitharesponseheader : (Result (Maybe (Http.Metadata, String), Http.Error) (String) -> msg) -> Cmd msg
getWitharesponseheader toMsg =
    getWitharesponseheaderTask |>
        Task.attempt toMsg


getWitharesponseheaderTask : Task (Maybe (Http.Metadata, String), Http.Error) (String)
getWitharesponseheaderTask =
    Http.task
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "with-a-response-header"
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
                            (decodeString string body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_)))
                )
        , timeout =
            Nothing
        }


getWitharesponseheaderSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (String) -> msg) -> ProgramTest.SimulatedEffect msg
getWitharesponseheaderSimulated toMsg =
    getWitharesponseheaderSimulatedTask |>
        SimulatedEffect.Task.attempt toMsg


getWitharesponseheaderSimulatedTask : ProgramTest.SimulatedTask (Maybe (Http.Metadata, String), Http.Error) (String)
getWitharesponseheaderSimulatedTask =
    SimulatedEffect.Http.task
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "with-a-response-header"
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
                            (decodeString string body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_)))
                )
        , timeout =
            Nothing
        }
