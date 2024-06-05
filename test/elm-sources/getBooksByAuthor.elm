module GetBooksByAuthor exposing (..)

import Http
import SimulatedEffect.Http
import ProgramTest
import Url
import String.Conversions as String
import Json.Decode exposing (..)


getBooksbyauthorByAuthor : (Result (Maybe (Http.Metadata, String), Http.Error) (Book) -> msg) -> Id -> Author -> Cmd msg
getBooksbyauthorByAuthor toMsg header_Id capture_author =
    getBooksbyauthorByAuthorTask header_Id capture_author |>
        Task.attempt toMsg


getBooksbyauthorByAuthorTask : Id -> Author -> Task (Maybe (Http.Metadata, String), Http.Error) (Book)
getBooksbyauthorByAuthorTask header_Id capture_author =
    Http.task
        { method =
            "GET"
        , headers =
            [ Http.header "X-Requested-With" "XMLHttpRequest"
            , Http.header "Id" ((\(Id inner) -> String.fromInt inner) header_Id)
            ]
        , url =
            String.join "/"
                [ ""
                , "books-by-author"
                , capture_author |> (\(Author inner) -> identity inner) |> Url.percentEncode
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


getBooksbyauthorByAuthorSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (Book) -> msg) -> Id -> Author -> ProgramTest.SimulatedEffect msg
getBooksbyauthorByAuthorSimulated toMsg header_Id capture_author =
    getBooksbyauthorByAuthorSimulatedTask header_Id capture_author |>
        SimulatedEffect.Task.attempt toMsg


getBooksbyauthorByAuthorSimulatedTask : Id -> Author -> ProgramTest.SimulatedTask (Maybe (Http.Metadata, String), Http.Error) (Book)
getBooksbyauthorByAuthorSimulatedTask header_Id capture_author =
    SimulatedEffect.Http.task
        { method =
            "GET"
        , headers =
            [ SimulatedEffect.Http.header "X-Requested-With" "XMLHttpRequest"
            , SimulatedEffect.Http.header "Id" ((\(Id inner) -> String.fromInt inner) header_Id)
            ]
        , url =
            String.join "/"
                [ ""
                , "books-by-author"
                , capture_author |> (\(Author inner) -> identity inner) |> Url.percentEncode
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
