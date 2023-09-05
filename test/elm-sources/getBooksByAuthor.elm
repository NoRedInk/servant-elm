module GetBooksByAuthor exposing (..)

import Http
import SimulatedEffect.Http
import ProgramTest
import Url
import String.Conversions as String
import Json.Decode exposing (..)


getBooksbyauthorByAuthor : (Result (Maybe (Http.Metadata, String), Http.Error) (Book) -> msg) -> Id -> Author -> Cmd msg
getBooksbyauthorByAuthor toMsg header_Id capture_author =
    Http.request
        { method =
            "GET"
        , headers =
            [ Http.header "Id" ((\(Id inner) -> String.fromInt inner) header_Id)
            ]
        , url =
            String.join "/"
                [ ""
                , "books-by-author"
                , capture_author |> (\(Author inner) -> identity inner) |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse toMsg
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
        , tracker =
            Nothing
        }


getBooksbyauthorByAuthorSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (Book) -> msg) -> Id -> Author -> ProgramTest.SimulatedEffect msg
getBooksbyauthorByAuthorSimulated toMsg header_Id capture_author =
    SimulatedEffect.Http.request
        { method =
            "GET"
        , headers =
            [ SimulatedEffect.Http.header "Id" ((\(Id inner) -> String.fromInt inner) header_Id)
            ]
        , url =
            String.join "/"
                [ ""
                , "books-by-author"
                , capture_author |> (\(Author inner) -> identity inner) |> Url.percentEncode
                ]
        , body =
            SimulatedEffect.Http.emptyBody
        , expect =
            SimulatedEffect.Http.expectStringResponse toMsg
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
        , tracker =
            Nothing
        }
