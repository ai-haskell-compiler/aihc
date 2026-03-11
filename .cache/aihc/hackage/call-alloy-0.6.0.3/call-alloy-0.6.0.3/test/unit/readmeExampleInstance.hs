fromList [
  ( Signature {scope = Nothing, sigName = "$withFlow_x"},
    Entry {
      annotation = Just Skolem,
      relation = fromList [
        ( "",
          Single (fromList [
            Object {objSig = "Node", identifier = 1}
            ]))
        ]
      }),
  ( Signature {scope = Nothing, sigName = "$withFlow_y"},
    Entry {
      annotation = Just Skolem,
      relation = fromList [
        ( "",
          Single (fromList [
            Object {objSig = "Node", identifier = 0}
            ]))
        ]
      }),
  ( Signature {scope = Nothing, sigName = "Int"},
    Entry {
      annotation = Nothing,
      relation = fromList [
        ( "",
          Single (fromList [
            NumberObject {number = -4},
            NumberObject {number = -3},
            NumberObject {number = -2},
            NumberObject {number = -1},
            NumberObject {number = 0},
            NumberObject {number = 1},
            NumberObject {number = 2},
            NumberObject {number = 3}
            ]))
        ]
      }),
  ( Signature {scope = Nothing, sigName = "String"},
    Entry {
      annotation = Nothing,
      relation = fromList [("", EmptyRelation)]
      }),
  ( Signature {scope = Nothing, sigName = "none"},
    Entry {
      annotation = Nothing,
      relation = fromList [("", EmptyRelation)]
  }),
  ( Signature {scope = Nothing, sigName = "univ"},
    Entry {
      annotation = Nothing,
      relation = fromList [
        ( "",
          Single (fromList [
            Object {objSig = "Node", identifier = 0},
            Object {objSig = "Node", identifier = 1},
            NumberObject {number = -4},
            NumberObject {number = -3},
            NumberObject {number = -2},
            NumberObject {number = -1},
            NumberObject {number = 0},
            NumberObject {number = 1},
            NumberObject {number = 2},
            NumberObject {number = 3}
            ]))
        ]
      }),
  ( Signature {scope = Just "seq", sigName = "Int"},
    Entry {
      annotation = Nothing,
      relation = fromList [
        ( "",
          Single (fromList [
            NumberObject {number = 0},
            NumberObject {number = 1},
            NumberObject {number = 2}
            ]))
        ]
      }),
  ( Signature {scope = Just "this", sigName = "Node"},
    Entry {
      annotation = Nothing,
      relation = fromList [
        ( "",
          Single (fromList [
            Object {objSig = "Node", identifier = 0},
            Object {objSig = "Node", identifier = 1}
            ])),
        ( "flow",
          Triple (fromList [
            ( Object {objSig = "Node", identifier = 1},
              Object {objSig = "Node", identifier = 0},
              NumberObject {number = 3})
            ])),
        ( "stored",
          Double (fromList [
            ( Object {objSig = "Node", identifier = 0},
              NumberObject {number = 0}),
            ( Object {objSig = "Node", identifier = 1},
              NumberObject {number = 1})
            ]))
        ]
      })
  ]
