# yesod-page-cursor

Cursor based pagination for `yesod` using index friendly keyset cursors.

Primer: [No Offset](https://use-the-index-luke.com/no-offset)

```hs
getSomeR :: Handler Value
getSomeR = do
  teacherId <- Param.required "teacherId"
  mCourseId <- Param.optional "courseId"
  page <- withPageAbsolute 100 entityKey $ \Cursor {..} -> do
    let (teacherId, mCourseId) = cursorParams
    fmap (sort cursorPosition) . runDB $ selectList
      (catMaybes
        [ Just $ SomeAssignmentTeacherId ==. teacherId
        , (SomeAssignmentCourseId ==.) <$> mCourseId
        , whereClause cursorPosition
        ]
      )
      [LimitTo $ fromMaybe 100 cursorLimit, orderBy cursorPosition]
  returnJson $ keyValueEntityToJSON <$> page
 where
  whereClause = \case
    First -> Nothing
    Previous p -> Just $ persistIdField <. p
    Next p -> Just $ persistIdField >. p
    Last -> Nothing
  orderBy = \case
    First -> Asc persistIdField
    Previous _ -> Desc persistIdField
    Next _ -> Asc persistIdField
    Last -> Desc persistIdField
  sort = \case
    First -> id
    Previous _ -> reverse
    Next _ -> id
    Last -> reverse
```

`cursorLastPosition` is configurable. A page sorted by `created_at` may look
like:

```hs
getSortedSomeR :: Handler Value
getSortedSomeR = do
  page <- withPageAbsolute 100 $ \Cursor {..} -> do
    fmap (sort cursorPosition) . runDB $ selectList
      (whereClause cursorPosition)
      [ LimitTo $ fromMaybe 100 cursorLimit
      , orderBy cursorPosition
      ]
  returnJson $ keyValueEntityToJSON <$> page
 where
  whereClause = \case
    First -> []
    Previous (pId, createdAt) ->
      [ SomeAssingmentCreatedAt <=. createdAt
      , persistIdField <. pId
      ]
    Next (pId, createdAt) ->
      [ SomeAssingmentCreatedAt >=. createdAt
      , persistIdField >. pId
      ]
    Last -> []
  orderBy = \case
    First -> Asc SomeAssignmentCreatedAt
    Previous _ -> Desc SomeAssignmentCreatedAt
    Next _ -> Asc SomeAssignmentCreatedAt
    Last -> Desc SomeAssignmentCreatedAt
  sort = \case
    First -> id
    Previous _ -> reverse
    Next _ -> id
    Last -> reverse
```

## Usage

Paginated requests return a single page and a link with a cursor token to
retrieve the next page.

```sh
$ curl 'some-rest.com/endpoint?limit=3'
{
  "first": : "https://some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "previous": null,
  "next": "https://some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "data": [...]
}
```

The link can be used to retrieve the next page.

```sh
$ curl 'some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ=='
{
  "first": : "https://some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "previous": "https://some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "next": "https://some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "data": [...]
}
```

If no pages remain then no link is returned

```sh
$ curl 'some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ=='
{
  "first": : "https://some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "previous": "https://some-rest.com/endpoint?next=eyJsYXN0UG9zaXRpb24iOjMsInBhcmFtcyI6WzEsbnVsbF0sImxpbWl0IjozfQ==",
  "next": null,
  "data": [...]
}
```
