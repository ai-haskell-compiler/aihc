{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Swish.RDF.Vocabulary.XSD
--  Copyright   :  (c) 2011 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings
--
--  This module defines vocabulary terms from the XSD document.
--
--------------------------------------------------------------------------------

module Swish.RDF.Vocabulary.XSD
    ( 
      namespaceXSD
      
    -- * XSD data types
    --  
    -- | See the XSD Schema Part 2 documentation at <http://www.w3.org/TR/xmlschema-2/>;
    -- the version used is \"W3C Recommendation 28 October 2004\",  
    -- <http://www.w3.org/TR/2004/REC-xmlschema-2-20041028/>.  
    , xsdType 
    
    -- ** Primitive datatypes  
    --  
    -- | See the section \"Primitive datatypes\" at  
    -- <http://www.w3.org/TR/xmlschema-2/#built-in-primitive-datatypes>.  
    , xsdString
    , xsdBoolean
    , xsdDecimal
    , xsdFloat
    , xsdDouble
    , xsdDateTime
    , xsdTime
    , xsdDate
    , xsdAnyURI  
      
    -- ** Derived datatypes  
    --  
    -- | See the section \"Derived datatypes\" at  
    -- <http://www.w3.org/TR/xmlschema-2/#built-in-derived>.  
    , xsdInteger
    , xsdNonPosInteger
    , xsdNegInteger
    , xsdLong
    , xsdInt
    , xsdShort
    , xsdByte
    , xsdNonNegInteger
    , xsdUnsignedLong
    , xsdUnsignedInt
    , xsdUnsignedShort
    , xsdUnsignedByte
    , xsdPosInteger

    )
where

import Swish.Namespace (Namespace, ScopedName, makeNamespace, makeNSScopedName)
import Swish.QName (LName)

import Data.Maybe (fromMaybe)
import Network.URI (URI, parseURI)

------------------------------------------------------------
--  Namespace
------------------------------------------------------------

xsdURI :: URI
xsdURI = fromMaybe (error "Internal error processing XSD URI") $ parseURI "http://www.w3.org/2001/XMLSchema#"

-- | Maps @xsd@ to <http://www.w3.org/2001/XMLSchema#>.
namespaceXSD :: Namespace
namespaceXSD = makeNamespace (Just "xsd") xsdURI

------------------------------------------------------------
--  Terms
------------------------------------------------------------

-- | Create a scoped name for an XSD datatype with the given name.
xsdType :: LName -> ScopedName
xsdType = makeNSScopedName namespaceXSD

-- | @xsd:string@ from <http://www.w3.org/TR/xmlschema-2/#string>.
xsdString           :: ScopedName
xsdString           = xsdType "string"

-- | @xsd:boolean@ from <http://www.w3.org/TR/xmlschema-2/#boolean>.
xsdBoolean          :: ScopedName
xsdBoolean          = xsdType "boolean"

-- | @xsd:decimal@ from <http://www.w3.org/TR/xmlschema-2/#decimal>.
xsdDecimal          :: ScopedName
xsdDecimal          = xsdType "decimal"

-- | @xsd:integer@ from <http://www.w3.org/TR/xmlschema-2/#integer>.
xsdInteger          :: ScopedName
xsdInteger          = xsdType "integer"

-- | @xsd:nonNegativeInteger@ from <http://www.w3.org/TR/xmlschema-2/#nonNegativeInteger>.
xsdNonNegInteger   :: ScopedName
xsdNonNegInteger   = xsdType "nonNegativeInteger"

-- | @xsd:nonPositiveInteger@ from <http://www.w3.org/TR/xmlschema-2/#nonPositiveInteger>.
xsdNonPosInteger   :: ScopedName
xsdNonPosInteger   = xsdType "nonPositiveInteger"

-- | @xsd:positiveInteger@ from <http://www.w3.org/TR/xmlschema-2/#positiveInteger>.
xsdPosInteger      :: ScopedName
xsdPosInteger      = xsdType "positiveInteger"

-- | @xsd:negativeInteger@ from <http://www.w3.org/TR/xmlschema-2/#negativeInteger>.
xsdNegInteger      :: ScopedName
xsdNegInteger      = xsdType "negativeInteger"

-- | @xsd:float@ from <http://www.w3.org/TR/xmlschema-2/#float>.
xsdFloat            :: ScopedName
xsdFloat            = xsdType "float"

-- | @xsd:double@ from <http://www.w3.org/TR/xmlschema-2/#double>.
xsdDouble           :: ScopedName
xsdDouble           = xsdType "double"

-- | @xsd:long@ from <http://www.w3.org/TR/xmlschema-2/#long>.
xsdLong :: ScopedName
xsdLong = xsdType "long"

-- | @xsd:int@ from <http://www.w3.org/TR/xmlschema-2/#int>.
xsdInt :: ScopedName
xsdInt = xsdType "int"

-- | @xsd:short@ from <http://www.w3.org/TR/xmlschema-2/#short>.
xsdShort :: ScopedName
xsdShort = xsdType "short"

-- | @xsd:byte@ from <http://www.w3.org/TR/xmlschema-2/#byte>.
xsdByte :: ScopedName
xsdByte = xsdType "byte"

-- | @xsd:unsignedLong@ from <http://www.w3.org/TR/xmlschema-2/#unsignedLong>.
xsdUnsignedLong :: ScopedName
xsdUnsignedLong = xsdType "unsignedLong"

-- | @xsd:unsignedInt@ from <http://www.w3.org/TR/xmlschema-2/#unsignedInt>.
xsdUnsignedInt :: ScopedName
xsdUnsignedInt = xsdType "unsignedInt"

-- | @xsd:unsignedShort@ from <http://www.w3.org/TR/xmlschema-2/#unsignedShort>.
xsdUnsignedShort :: ScopedName
xsdUnsignedShort = xsdType "unsignedShort"

-- | @xsd:unsignedByte@ from <http://www.w3.org/TR/xmlschema-2/#unsignedByte>.
xsdUnsignedByte :: ScopedName
xsdUnsignedByte = xsdType "unsignedByte"

-- | @xsd:date@ from <http://www.w3.org/TR/xmlschema-2/#date>.
xsdDate :: ScopedName
xsdDate = xsdType "date"

-- | @xsd:dateTime@ from <http://www.w3.org/TR/xmlschema-2/#dateTime>.
xsdDateTime :: ScopedName
xsdDateTime = xsdType "dateTime"

-- | @xsd:time@ from <http://www.w3.org/TR/xmlschema-2/#time>.
xsdTime :: ScopedName
xsdTime = xsdType "time"

-- | @xsd:anyURI@ from <http://www.w3.org/TR/xmlschema-2/#anyURI>.
xsdAnyURI :: ScopedName
xsdAnyURI = xsdType "anyURI"

--------------------------------------------------------------------------------
--
--  Copyright (c) 2011 Douglas Burke
--  All rights reserved.
--
--  This file is part of Swish.
--
--  Swish is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  Swish is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Swish; if not, write to:
--    The Free Software Foundation, Inc.,
--    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
--------------------------------------------------------------------------------
