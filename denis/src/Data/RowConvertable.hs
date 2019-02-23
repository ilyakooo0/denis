{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators #-}


module Data.RowConvertable (
    -- RowConvertable(..)
 ) where 

import Data.Int (Int64)
import Squeal.PostgreSQL
import Squeal.PostgreSQL.Schema 
import GHC.TypeLits (Symbol)
import Squeal.PostgreSQL.Render
import qualified Squeal.PostgreSQL.PQ as P
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Text (Text)
    

-- class RowConvertable t r where
--     convertToRow :: a -> RowPG r
--     fromRow :: TuplePG r -> a

