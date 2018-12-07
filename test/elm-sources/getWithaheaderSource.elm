module GetWithAHeaderSource exposing (..)

import Http
import String.Conversions as String
import Json.Decode exposing (..)


getWithaheader : (Result Http.Error (String) -> msg) -> Maybe (String) -> Maybe (Int) -> String -> Int -> Cmd msg
getWithaheader toMsg header_myStringHeader header_MyIntHeader header_MyRequiredStringHeader header_MyRequiredIntHeader =
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "myStringHeader") header_myStringHeader
                , Maybe.map (Http.header "MyIntHeader" << String.fromInt) header_MyIntHeader
                , Maybe.map (Http.header "MyRequiredStringHeader") (Just header_MyRequiredStringHeader)
                , Maybe.map (Http.header "MyRequiredIntHeader" << String.fromInt) (Just header_MyRequiredIntHeader)
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
                        Http.BadUrl_ url -> Err (Http.BadUrl url)
                        Http.Timeout_ -> Err Http.Timeout
                        Http.NetworkError_ -> Err Http.NetworkError
                        Http.BadStatus_ metadata _ -> Err (Http.BadStatus metadata.statusCode)
                        Http.GoodStatus_ metadata body_ ->
                            (decodeString string body_)
                                |> Result.mapError Json.Decode.errorToString
                                |> Result.mapError Http.BadBody)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
