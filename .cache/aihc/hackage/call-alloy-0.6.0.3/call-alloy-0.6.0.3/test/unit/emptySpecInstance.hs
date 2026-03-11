[ fromList
    [ ( Signature { scope = Nothing , sigName = "Int" }
      , Entry
          { annotation = Nothing
          , relation =
              fromList
                [ ( ""
                  , Single
                      (fromList
                         [ NumberObject { number = -8 }
                         , NumberObject { number = -7 }
                         , NumberObject { number = -6 }
                         , NumberObject { number = -5 }
                         , NumberObject { number = -4 }
                         , NumberObject { number = -3 }
                         , NumberObject { number = -2 }
                         , NumberObject { number = -1 }
                         , NumberObject { number = 0 }
                         , NumberObject { number = 1 }
                         , NumberObject { number = 2 }
                         , NumberObject { number = 3 }
                         , NumberObject { number = 4 }
                         , NumberObject { number = 5 }
                         , NumberObject { number = 6 }
                         , NumberObject { number = 7 }
                         ])
                  )
                ]
          }
      )
    , ( Signature { scope = Nothing , sigName = "String" }
      , Entry
          { annotation = Nothing
          , relation = fromList [ ( "" , EmptyRelation ) ]
          }
      )
    , ( Signature { scope = Nothing , sigName = "none" }
      , Entry
          { annotation = Nothing
          , relation = fromList [ ( "" , EmptyRelation ) ]
          }
      )
    , ( Signature { scope = Nothing , sigName = "univ" }
      , Entry
          { annotation = Nothing
          , relation =
              fromList
                [ ( ""
                  , Single
                      (fromList
                         [ NumberObject { number = -8 }
                         , NumberObject { number = -7 }
                         , NumberObject { number = -6 }
                         , NumberObject { number = -5 }
                         , NumberObject { number = -4 }
                         , NumberObject { number = -3 }
                         , NumberObject { number = -2 }
                         , NumberObject { number = -1 }
                         , NumberObject { number = 0 }
                         , NumberObject { number = 1 }
                         , NumberObject { number = 2 }
                         , NumberObject { number = 3 }
                         , NumberObject { number = 4 }
                         , NumberObject { number = 5 }
                         , NumberObject { number = 6 }
                         , NumberObject { number = 7 }
                         ])
                  )
                ]
          }
      )
    , ( Signature { scope = Just "seq" , sigName = "Int" }
      , Entry
          { annotation = Nothing
          , relation =
              fromList
                [ ( ""
                  , Single
                      (fromList
                         [ NumberObject { number = 0 }
                         , NumberObject { number = 1 }
                         , NumberObject { number = 2 }
                         , NumberObject { number = 3 }
                         ])
                  )
                ]
          }
      )
    ]
]
