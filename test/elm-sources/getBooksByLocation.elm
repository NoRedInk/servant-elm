module GetBooksByLocation exposing (..)

import Http
import ProgramTest

stringFromLocation : Location -> String
stringFromLocation value = case value of
    Home -> "home"
    School -> "school"
    Library -> "library"

getBooksByLocation : (Result (Maybe (Http.Metadata, String), Http.Error) (List (Book)) -> msg) -> Location -> Cmd msg
getBooksByLocation toMsg capture_location =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_location |> stringFromLocation |> Url.percentEncode -- stringFromLocation should be generated separately, by using Servant.Elm.renderUrlEncoder
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
                            (decodeString (list decodeBook) body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_)))
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getBooksByLocationSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (List (Book)) -> msg) -> Location -> ProgramTest.SimulatedEffect msg
getBooksByLocationSimulated toMsg capture_location =
    SimulatedEffect.Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "books"
                , capture_location |> stringFromLocation |> Url.percentEncode -- stringFromLocation should be generated separately, by using Servant.Elm.renderUrlEncoder
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
                            (decodeString (list decodeBook) body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_)))
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
