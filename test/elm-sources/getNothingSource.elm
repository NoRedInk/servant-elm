module GetNothingSource exposing (..)

import String.Conversions as String
import Http
import SimulatedEffect.Http
import ProgramTest


getNothing : (Result (Maybe (Http.Metadata, String), Http.Error) (NoContent) -> msg) -> Cmd msg
getNothing toMsg =
    getNothingTask |>
        Task.attempt toMsg


getNothingTask : Task (Maybe (Http.Metadata, String), Http.Error) (NoContent)
getNothingTask =
    Http.task
        { method =
            "GET"
        , headers =
            [ Http.header "X-Requested-With" "XMLHttpRequest"
            ]
        , url =
            String.join "/"
                [ ""
                , "nothing"
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
                            if String.isEmpty body_ then
                                Ok (NoContent)
                            else
                                Err (Just (metadata, body_), Http.BadBody <| "Expected the response body to be empty, but it was '" ++ body_ ++ "'.")
                            
                )
        , timeout =
            Nothing
        }


getNothingSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (NoContent) -> msg) -> ProgramTest.SimulatedEffect msg
getNothingSimulated toMsg =
    getNothingSimulatedTask |>
        SimulatedEffect.Task.attempt toMsg


getNothingSimulatedTask : ProgramTest.SimulatedTask (Maybe (Http.Metadata, String), Http.Error) (NoContent)
getNothingSimulatedTask =
    SimulatedEffect.Http.task
        { method =
            "GET"
        , headers =
            [ SimulatedEffect.Http.header "X-Requested-With" "XMLHttpRequest"
            ]
        , url =
            String.join "/"
                [ ""
                , "nothing"
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
                            if String.isEmpty body_ then
                                Ok (NoContent)
                            else
                                Err (Just (metadata, body_), Http.BadBody <| "Expected the response body to be empty, but it was '" ++ body_ ++ "'.")
                            
                )
        , timeout =
            Nothing
        }
