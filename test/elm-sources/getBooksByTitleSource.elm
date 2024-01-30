module GetBooksByTitleSource exposing (..)

import Http
import SimulatedEffect.Http
import ProgramTest
import String.Conversions as String
import Url


getBooksByTitle : (Result (Maybe (Http.Metadata, String), Http.Error) (Book) -> msg) -> String -> Cmd msg
getBooksByTitle toMsg capture_title =
    getBooksByTitleTask capture_title |>
        Task.attempt toMsg


getBooksByTitleTask : String -> Task (Maybe (Http.Metadata, String), Http.Error) (Book)
getBooksByTitleTask capture_title =
    Http.task
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_title |> Url.percentEncode
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


getBooksByTitleSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (Book) -> msg) -> String -> ProgramTest.SimulatedEffect msg
getBooksByTitleSimulated toMsg capture_title =
    getBooksByTitleSimulatedTask capture_title |>
        SimulatedEffect.Task.attempt toMsg


getBooksByTitleSimulatedTask : String -> ProgramTest.SimulatedTask (Maybe (Http.Metadata, String), Http.Error) (Book)
getBooksByTitleSimulatedTask capture_title =
    SimulatedEffect.Http.task
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_title |> Url.percentEncode
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
