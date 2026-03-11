module ParserSpec
  ( spec )
  where

import qualified Text.ParserCombinators.Parsec as P

import Data.Aeson.JSONPath.Parser       (pQuery)
import Data.Either                      (isLeft)

import Data.Aeson.JSONPath.Types
import Test.Hspec

import Prelude

spec :: Spec
spec = do
  describe "Parse query string" $ do
    it "parses query: $" $
      P.parse pQuery "" "$"
      `shouldBe`
      Right (Query{queryType=Root,querySegments=[]})

    it "parses query: $.store" $
      P.parse pQuery "" "$.store"
      `shouldBe`
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Child,
          segment = Dotted "store"
        }]
      }

    it "parses query: $['store']" $
      P.parse pQuery "" "$['store']"
      `shouldBe`
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Child,
          segment = Bracketed [Name "store"]
        }]
      }

    it "parses query: $.store.books" $
      P.parse pQuery "" "$.store.books"
      `shouldBe`
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Child,
          segment = Dotted "store"
          }, QuerySegment {
          segmentType = Child,
          segment = Dotted "books"
        }]
      }

    it "parses query: $.store.books[0]" $
      P.parse pQuery "" "$.store.books[0]"
      `shouldBe`
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Child,
          segment = Dotted "store"
        }, QuerySegment {
          segmentType = Child,
          segment = Dotted "books"
        }, QuerySegment {
          segmentType = Child,
          segment = Bracketed [Index 0]
        }]
      }

    it "parses query: $.store.books[0,2]" $
      P.parse pQuery "" "$.store.books[0,2]"
      `shouldBe`
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Child,
          segment = Dotted "store"
        }, QuerySegment {
          segmentType = Child,
          segment = Dotted "books"
        }, QuerySegment {
          segmentType = Child,
          segment = Bracketed [Index 0, Index 2]
        }]
      }

    it "parses query: $.store.books[1:3]" $
      P.parse pQuery "" "$.store.books[1:3]"
      `shouldBe`
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Child,
          segment = Dotted "store"
        }, QuerySegment {
          segmentType = Child,
          segment = Dotted "books"
        }, QuerySegment {
          segmentType = Child,
          segment = Bracketed [ArraySlice (Just 1, Just 3, 1)]
        }]
      }

    it "parses query: $.store.books[1:4:2]" $
      P.parse pQuery "" "$.store.books[1:4:2]"
      `shouldBe`
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Child,
          segment = Dotted "store"
        }, QuerySegment {
          segmentType = Child,
          segment = Dotted "books"
        }, QuerySegment {
          segmentType = Child,
          segment = Bracketed [ArraySlice (Just 1, Just 4, 2)]
        }]
      }

    it "parses query: $.store.books[-1:-4:-2]" $
      P.parse pQuery "" "$.store.books[-1:-4:-2]"
      `shouldBe`
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Child,
          segment = Dotted "store"
        }, QuerySegment {
          segmentType = Child,
          segment = Dotted "books"
        }, QuerySegment {
          segmentType = Child,
          segment = Bracketed [ArraySlice (Just (-1), Just (-4), (-2))]
        }]
      }

    it "parses query: $.store.books[1:3, 0, 1]" $
      P.parse pQuery "" "$.store.books[1:3, 0, 1]"
      `shouldBe`
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Child,
          segment = Dotted "store"
        }, QuerySegment {
          segmentType = Child,
          segment = Dotted "books"
        }, QuerySegment {
          segmentType = Child,
          segment = Bracketed [ArraySlice (Just 1, Just 3, 1), Index 0, Index 1]
        }]
      }

    it "parses query: $.*" $
      P.parse pQuery "" "$.*" 
      `shouldBe` 
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Child,
          segment = WildcardSegment
        }]
      }

    it "parses query: $[*]" $
      P.parse pQuery "" "$[*]"
      `shouldBe`
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Child,
          segment = Bracketed [WildcardSelector]
        }]
      }

    it "parses query: $..*" $
      P.parse pQuery "" "$..*"
      `shouldBe`
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Descendant,
          segment = WildcardSegment
        }]
      }

    it "parses query: $..[*]" $
      P.parse pQuery "" "$..[*]"
      `shouldBe` 
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Descendant,
          segment = Bracketed [WildcardSelector]
        }]
      }

    it "fails with $.1startsWithNum" $
      P.parse pQuery "" "$.1startsWithNum"
      `shouldSatisfy` isLeft

    it "parses query $.©®±×÷Ωπ•€→∀∃∈≠≤≥✓λ" $
      P.parse pQuery "" "$.©®±×÷Ωπ•€→∀∃∈≠≤≥✓λ"
      `shouldBe`
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Child,
          segment = Dotted "©®±×÷Ωπ•€→∀∃∈≠≤≥✓λ"
        }]
      }

    it "parses query: $._" $
      P.parse pQuery "" "$._"
      `shouldBe`
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Child,
          segment = Dotted "_"
        }]
      }

    it "parses query: $.underscore_key" $
      P.parse pQuery "" "$.underscore_key"
      `shouldBe`
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Child,
          segment = Dotted "underscore_key"
        }]
      }

    it "parses query: $[0,TAB/LINEFEED/RETURN/SPACE1]" $
      P.parse pQuery "" "$[0,\n\t\r 1]"
      `shouldBe`
      Right Query {
        queryType = Root,
        querySegments = [QuerySegment {
          segmentType = Child,
          segment = Bracketed [Index 0, Index 1]
        }]
      }

    describe "parses JSPFilter Query" $ do
      it "$[?@.category == 'reference']" $
        P.parse pQuery "" "$[?@.category == 'reference']"
        `shouldBe`
        Right Query {
          queryType = Root,
          querySegments = [QuerySegment {
            segmentType = Child,
            segment = Bracketed [Filter (LogicalOr [LogicalAnd [Comparison (Comp (CompSQ SingularQuery {
              singularQueryType = CurrentSQ,
              singularQuerySegments = [NameSQSeg "category"]
            }) Equal (CompLit (LitString "reference")))]])]
          }]
        }

      it "test expr: $..books[?@.price].title" $
        P.parse pQuery "" "$..books[?@.price].title"
        `shouldBe`
        Right Query {
          queryType = Root,
          querySegments = [QuerySegment {
            segmentType = Descendant,
            segment = Dotted "books"
          }, QuerySegment {
            segmentType = Child,
            segment = Bracketed [Filter (LogicalOr [LogicalAnd [Test (FilterQuery Query {
              queryType = Current,
              querySegments = [QuerySegment {
                segmentType = Child,
                segment = Dotted "price"
              }]
            })]])]
          }, QuerySegment {
            segmentType = Child,
            segment = Dotted "title"
          }]
        }

      it "and expr: $[?@.price < 20 && @.price > 10]" $
        P.parse pQuery "" "$[?@.price < 20 && @.price > 10]" 
        `shouldBe`
        Right Query {
          queryType = Root,
          querySegments = [QuerySegment {
            segmentType = Child,
            segment = Bracketed [Filter (LogicalOr [LogicalAnd [Comparison (Comp (CompSQ SingularQuery {
              singularQueryType = CurrentSQ,
              singularQuerySegments = [NameSQSeg "price"]
            }) Less (CompLit (LitNum 20.0))), Comparison (Comp (CompSQ SingularQuery {
              singularQueryType = CurrentSQ,
              singularQuerySegments = [NameSQSeg "price"]
            }) Greater (CompLit (LitNum 10.0)))]])]
          }]
        }

      it "or expr: $[?@.price < 20 || @.price > 10]" $
        P.parse pQuery "" "$[?@.price < 20 || @.price > 10]"
        `shouldBe`
        Right Query {
          queryType = Root,
          querySegments = [QuerySegment {
            segmentType = Child,
            segment = Bracketed [Filter (LogicalOr [LogicalAnd [Comparison (Comp (CompSQ SingularQuery {
              singularQueryType = CurrentSQ,
              singularQuerySegments = [NameSQSeg "price"]
            }) Less (CompLit (LitNum 20.0)))], LogicalAnd [Comparison (Comp (CompSQ SingularQuery {
              singularQueryType = CurrentSQ,
              singularQuerySegments = [NameSQSeg "price"]
            }) Greater (CompLit (LitNum 10.0)))]])]
          }]
        }

      it "root filter: $..books[?$.price].title" $
        P.parse pQuery "" "$..books[?$.price].title"
        `shouldBe`
        Right Query {
          queryType = Root,
          querySegments = [QuerySegment {
            segmentType = Descendant,
            segment = Dotted "books"
          }, QuerySegment {
            segmentType = Child,
            segment = Bracketed [Filter (LogicalOr [LogicalAnd [Test (FilterQuery Query {
              queryType = Root,
              querySegments = [QuerySegment {
                segmentType = Child,
                segment = Dotted "price"
              }]
            })]])]
          }, QuerySegment {
            segmentType = Child,
            segment = Dotted "title"
          }]
        }

      it "not expr: $[?!(@.price < 20 && @.price > 10)]" $
        P.parse pQuery "" "$[?!(@.price < 20 && @.price > 10)]" 
        `shouldBe`
        Right Query {
          queryType = Root,
          querySegments = [QuerySegment {
            segmentType = Child,
            segment = Bracketed [Filter (LogicalOr [LogicalAnd [NotParen (LogicalOr [LogicalAnd [Comparison (Comp (CompSQ SingularQuery {
              singularQueryType = CurrentSQ,
              singularQuerySegments = [NameSQSeg "price"]
            })  Less (CompLit (LitNum 20.0))), Comparison (Comp (CompSQ SingularQuery {
              singularQueryType = CurrentSQ,
              singularQuerySegments = [NameSQSeg "price"]
            }) Greater (CompLit (LitNum 10.0)))]])]])]
          }]
        }

      it "scientific: $.store.books[?@.price < -1e20]" $
        P.parse pQuery "" "$.store.books[?@['price'] < -1e20]"
        `shouldBe`
        Right Query {
          queryType = Root,
          querySegments = [QuerySegment {
            segmentType = Child,
            segment = Dotted "store"
          }, QuerySegment {
            segmentType = Child,
            segment = Dotted "books"
          }, QuerySegment {
            segmentType = Child,
            segment = Bracketed [Filter (LogicalOr [LogicalAnd [Comparison (Comp (CompSQ SingularQuery {
              singularQueryType = CurrentSQ,
              singularQuerySegments = [NameSQSeg "price"]
            }) Less (CompLit (LitNum (-1.0e20))))]])]
          }]
        }

      it "double: $.store.books[?@.price < 0.01]" $
        P.parse pQuery "" "$.store.books[?@['price'] < 0.01]"
        `shouldBe`
        Right Query {
          queryType = Root,
          querySegments = [QuerySegment {
            segmentType = Child,
            segment = Dotted "store"
          }, QuerySegment {
            segmentType = Child,
            segment = Dotted "books"
          }, QuerySegment {
            segmentType = Child,
            segment = Bracketed [Filter (LogicalOr [LogicalAnd [Comparison (Comp (CompSQ SingularQuery {
              singularQueryType = CurrentSQ,
              singularQuerySegments = [NameSQSeg "price"]
            }) Less (CompLit (LitNum (1.0e-2))))]])]
          }]
        }

      it "bool: $.store.books[?@.is_available == true]" $
        P.parse pQuery "" "$.store.books[?@.is_available == true]"
        `shouldBe`
        Right Query {
          queryType = Root,
          querySegments = [QuerySegment {
            segmentType = Child,
            segment = Dotted "store"
          }, QuerySegment {
            segmentType = Child,
            segment = Dotted "books"
          }, QuerySegment {
            segmentType = Child,
            segment = Bracketed [Filter (LogicalOr [LogicalAnd [Comparison (Comp (CompSQ SingularQuery {
              singularQueryType = CurrentSQ,
              singularQuerySegments = [NameSQSeg "is_available"]
            }) Equal (CompLit (LitBool True)))]])]
          }]
        }

      it "null: $.store.books[?@.is_available == null]" $
        P.parse pQuery "" "$.store.books[?@.is_available == null]"
        `shouldBe`
        Right Query {
          queryType = Root,
          querySegments = [QuerySegment {
            segmentType = Child,
            segment = Dotted "store"
          }, QuerySegment {
            segmentType = Child,
            segment = Dotted "books"
          }, QuerySegment {
            segmentType = Child,
            segment = Bracketed [Filter (LogicalOr [LogicalAnd [Comparison (Comp (CompSQ SingularQuery {
              singularQueryType = CurrentSQ,
              singularQuerySegments = [NameSQSeg "is_available"]
            }) Equal (CompLit LitNull))]])]
          }]
        }
