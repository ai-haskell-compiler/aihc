{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

import Language.Haskell.TH (runIO)

import Data.Functor.ProductIsomorphic.TH
  (reifyRecordType, defineProductConstructor)


newtype NNormal = NNormal Int
newtype NRec = NRec { _nrec :: Int }
data DNormal = DNormal Int
data DRec = DRec { _drec :: Int }

-- expect compilation error on test failure
$(mapM_ (\(tn, dn) -> do
           runIO . putStrLn $ "testing " ++ show (tn, dn)
           --- (((TypeQ, [Name]), ExpQ), (Maybe [Name], [TypeQ]))
           (((tq, vns), eq), (fns, ftqs)) <- reifyRecordType tn
           ty   <- tq
           expr <- eq
           fts  <- sequence ftqs
           runIO $ print (((ty, vns), expr), (fns, fts))
        )
  [(''NNormal, 'NNormal), (''NRec, 'NRec),
   (''DNormal, 'DNormal), (''DRec, 'DRec)]
  >> return [])

$(defineProductConstructor ''NNormal)
$(defineProductConstructor ''NRec)
$(defineProductConstructor ''DNormal)
$(defineProductConstructor ''DRec)

main :: IO ()
main = return ()
