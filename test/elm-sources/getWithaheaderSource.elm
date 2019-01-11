module GetWithAHeaderSource exposing (..)

import Http
import String.Conversions as String
import Json.Decode exposing (..)


getWithaheader : (Result (Maybe (Http.Metadata, String), Http.Error) (String) -> msg) -> Maybe (String) -> Maybe (Int) -> String -> Int -> Cmd msg
getWithaheader toMsg header_myStringHeader header_MyIntHeader header_MyRequiredStringHeader header_MyRequiredIntHeader =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "myStringHeader") header_myStringHeader
                , Maybe.map (Http.header "MyIntHeader" << String.fromInt ) header_MyIntHeader
                , Maybe.map (Http.header "MyRequiredStringHeader") (Just header_MyRequiredStringHeader)
                , Maybe.map (Http.header "MyRequiredIntHeader" << String.fromInt ) (Just header_MyRequiredIntHeader)
                ]
        , url =
            String.join "/"
                [ ""
                , "with-a-header"
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
                            (decodeString string body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody
                                |> Result.mapError (Tuple.pair (Just (metadata, body_))))
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
