module GetBooksSource exposing (..)

import Http
import String.Conversions as String
import Json.Decode exposing (..)


getBooks : Bool -> Maybe (String) -> Maybe (Int) -> String -> List (Maybe (Bool)) -> Http.Request (List (Book))
getBooks query_published query_sort query_year query_category query_filters =
    let
        params =
            List.filter (not << String.isEmpty)
                [ if query_published then
                    "published="
                  else
                    ""
                , query_sort
                    |> Maybe.map (Http.encodeUri >> (++) "sort=")
                    |> Maybe.withDefault ""
                , query_year
                    |> Maybe.map (toString >>Http.encodeUri >> (++) "year=")
                    |> Maybe.withDefault ""
                , Just query_category
                    |> Maybe.map (Http.encodeUri >> (++) "category=")
                    |> Maybe.withDefault ""
                , query_filters
                    |> List.map (\val -> "filters[]=" ++ (val |> Maybe.map (String.fromBool) |> Maybe.withDefault "" |> Http.encodeUri))
                    |> String.join "&"
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                String.join "/"
                    [ ""
                    , "books"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectStringResponse
                    (\response ->
                        Result.map
                            (\body -> { response | body = body })
                            (decodeString (list decodeBook) response.body))
            , timeout =
                Nothing
            , withCredentials =
                False
            }
