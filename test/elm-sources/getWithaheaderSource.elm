module GetWithAHeaderSource exposing (..)

import Http
import SimulatedEffect.Http
import ProgramTest
import String.Conversions as String
import Json.Decode exposing (..)


getWithaheader : (Result (Maybe (Http.Metadata, String), Http.Error) (String) -> msg) -> String -> Int -> Cmd msg
getWithaheader toMsg header_myStringHeader header_MyIntHeader =
    getWithaheaderTask header_myStringHeader header_MyIntHeader |>
        Task.attempt toMsg


getWithaheaderTask : String -> Int -> Task (Maybe (Http.Metadata, String), Http.Error) (String)
getWithaheaderTask header_myStringHeader header_MyIntHeader =
    Http.task
        { method =
            "GET"
        , headers =
            [ Http.header "X-Requested-With" "XMLHttpRequest"
            , Http.header "myStringHeader" (header_myStringHeader)
            , Http.header "MyIntHeader" (String.fromInt header_MyIntHeader)
            ]
        , url =
            String.join "/"
                [ ""
                , "with-a-header"
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


getWithaheaderSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (String) -> msg) -> String -> Int -> ProgramTest.SimulatedEffect msg
getWithaheaderSimulated toMsg header_myStringHeader header_MyIntHeader =
    getWithaheaderSimulatedTask header_myStringHeader header_MyIntHeader |>
        SimulatedEffect.Task.attempt toMsg


getWithaheaderSimulatedTask : String -> Int -> ProgramTest.SimulatedTask (Maybe (Http.Metadata, String), Http.Error) (String)
getWithaheaderSimulatedTask header_myStringHeader header_MyIntHeader =
    SimulatedEffect.Http.task
        { method =
            "GET"
        , headers =
            [ SimulatedEffect.Http.header "X-Requested-With" "XMLHttpRequest"
            , SimulatedEffect.Http.header "myStringHeader" (header_myStringHeader)
            , SimulatedEffect.Http.header "MyIntHeader" (String.fromInt header_MyIntHeader)
            ]
        , url =
            String.join "/"
                [ ""
                , "with-a-header"
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
