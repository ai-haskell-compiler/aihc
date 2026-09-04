{-
Hand-written test exception.

Essential property: the hand-written annotation walk in "Aihc.Resolve.Traverse"
visits every annotation of a checked module, in the same order as a generic
"Data.Data" walk. A missed annotation would keep a pending type annotation
in the output.

No fixture can test this property. A fixture checks the rendered output of
one program, but a missed annotation only shows for the syntax constructor
that the walk skips, and the generic walk is the only complete reference.
This test uses that generic walk as the oracle over every type checker
fixture. The user approved this exception.
-}
module Test.Tc.Traverse (tcTraverseTests) where

import Aihc.Parser.Syntax (Annotation, Module)
import Aihc.Resolve.Generic (everything)
import Aihc.Resolve.Traverse (annotationList)
import Data.Maybe (maybeToList)
import Data.Typeable (cast)
import TcAnnotatedGolden qualified as TAG
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

-- | Check the hand-written annotation walk against the generic walk.
tcTraverseTests :: IO TestTree
tcTraverseTests =
  testGroup "annotation-traversal" . map mkTraverseTest <$> TAG.loadTcAnnotatedCases

mkTraverseTest :: TAG.TcAnnotatedCase -> TestTree
mkTraverseTest tcase = testCase (TAG.caseId tcase) $
  case TAG.checkTcAnnotatedCase tcase of
    -- A case that does not type-check has its own golden test.
    Left _ -> pure ()
    Right modules -> mapM_ checkModule modules
  where
    checkModule :: Module -> IO ()
    checkModule modu =
      assertEqual "annotations in source order" (genericAnnotationList modu) (annotationList modu)

genericAnnotationList :: Module -> [Annotation]
genericAnnotationList = everything (maybeToList . cast)
