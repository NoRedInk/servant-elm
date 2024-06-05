module PostTwoSource exposing (..)

import Http
import SimulatedEffect.Http
import ProgramTest
import String.Conversions as String
import Json.Decode exposing (..)
import Json.Encode


postTwo : (Result (Maybe (Http.Metadata, String), Http.Error) (Maybe (Int)) -> msg) -> String -> Cmd msg
postTwo toMsg body =
    postTwoTask body |>
        Task.attempt toMsg


postTwoTask : String -> Task (Maybe (Http.Metadata, String), Http.Error) (Maybe (Int))
postTwoTask body =
    Http.task
        { method =
            "POST"
        , headers =
            [ Http.header "X-Requested-With" "XMLHttpRequest"
            ]
        , url =
            String.join "/"
                [ ""
                , "two"
                ]
        , body =
            Http.jsonBody (Json.Encode.string body)
        , resolver =
            Http.stringResolver
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString (nullable int) body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_)))
                )
        , timeout =
            Nothing
        }


postTwoSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (Maybe (Int)) -> msg) -> String -> ProgramTest.SimulatedEffect msg
postTwoSimulated toMsg body =
    postTwoSimulatedTask body |>
        SimulatedEffect.Task.attempt toMsg


postTwoSimulatedTask : String -> ProgramTest.SimulatedTask (Maybe (Http.Metadata, String), Http.Error) (Maybe (Int))
postTwoSimulatedTask body =
    SimulatedEffect.Http.task
        { method =
            "POST"
        , headers =
            [ SimulatedEffect.Http.header "X-Requested-With" "XMLHttpRequest"
            ]
        , url =
            String.join "/"
                [ ""
                , "two"
                ]
        , body =
            SimulatedEffect.Http.jsonBody (Json.Encode.string body)
        , resolver =
            SimulatedEffect.Http.stringResolver
                (\res ->
                    case res of
                        Http.BadUrl_ url -> Err (Nothing, Http.BadUrl url)
                        Http.Timeout_ -> Err (Nothing, Http.Timeout)
                        Http.NetworkError_ -> Err (Nothing, Http.NetworkError)
                        Http.BadStatus_ metadata body_ -> Err (Just (metadata, body_), Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString (nullable int) body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_)))
                )
        , timeout =
            Nothing
        }
