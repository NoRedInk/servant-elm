module GetBooksSource exposing (..)

import Http
import SimulatedEffect.Http
import ProgramTest
import String.Conversions as String
import Json.Decode exposing (..)
import Url


getBooks : (Result (Maybe (Http.Metadata, String), Http.Error) (List (Book)) -> msg) -> Bool -> Maybe (String) -> Maybe (Int) -> List (Maybe (Bool)) -> Cmd msg
getBooks toMsg query_published query_sort query_year query_filters =
    getBooksTask query_published query_sort query_year query_filters |>
        Task.attempt toMsg


getBooksTask : Bool -> Maybe (String) -> Maybe (Int) -> List (Maybe (Bool)) -> Task (Maybe (Http.Metadata, String), Http.Error) (List (Book))
getBooksTask query_published query_sort query_year query_filters =
    let
        params =
            List.filter (not << String.isEmpty)
                [ if query_published then
                    "query_published="
                  else
                    ""
                , query_sort
                    |> Maybe.map (Url.percentEncode >> (++) "sort=")
                    |> Maybe.withDefault ""
                , query_year
                    |> Maybe.map (String.fromInt >>Url.percentEncode >> (++) "year=")
                    |> Maybe.withDefault ""
                , query_filters
                    |> List.map (\val -> "query_filters[]=" ++ (val |> Maybe.map (String.fromBool) |> Maybe.withDefault "" |> Url.percentEncode))
                    |> String.join "&"
                ]
    in
        Http.task
            { method =
                "GET"
            , headers =
                [ Http.header "X-Requested-With" "XMLHttpRequest"
                ]
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
            , resolver =
                Http.stringResolver
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
            }


getBooksSimulated : (Result (Maybe (Http.Metadata, String), Http.Error) (List (Book)) -> msg) -> Bool -> Maybe (String) -> Maybe (Int) -> List (Maybe (Bool)) -> ProgramTest.SimulatedEffect msg
getBooksSimulated toMsg query_published query_sort query_year query_filters =
    getBooksSimulatedTask query_published query_sort query_year query_filters |>
        SimulatedEffect.Task.attempt toMsg


getBooksSimulatedTask : Bool -> Maybe (String) -> Maybe (Int) -> List (Maybe (Bool)) -> ProgramTest.SimulatedTask (Maybe (Http.Metadata, String), Http.Error) (List (Book))
getBooksSimulatedTask query_published query_sort query_year query_filters =
    let
        params =
            List.filter (not << String.isEmpty)
                [ if query_published then
                    "query_published="
                  else
                    ""
                , query_sort
                    |> Maybe.map (Url.percentEncode >> (++) "sort=")
                    |> Maybe.withDefault ""
                , query_year
                    |> Maybe.map (String.fromInt >>Url.percentEncode >> (++) "year=")
                    |> Maybe.withDefault ""
                , query_filters
                    |> List.map (\val -> "query_filters[]=" ++ (val |> Maybe.map (String.fromBool) |> Maybe.withDefault "" |> Url.percentEncode))
                    |> String.join "&"
                ]
    in
        SimulatedEffect.Http.task
            { method =
                "GET"
            , headers =
                [ SimulatedEffect.Http.header "X-Requested-With" "XMLHttpRequest"
                ]
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
                                (decodeString (list decodeBook) body_)
                                    |> Result.mapError Json.Decode.errorToString
                                    |> Result.mapError Http.BadBody
                                    |> Result.mapError (Tuple.pair (Just (metadata, body_)))
                    )
            , timeout =
                Nothing
            }
