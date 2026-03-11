{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}
--------------------------------------------------------------------------------

#include <Yoga.h>
#include <bindings.dsl.h>

--------------------------------------------------------------------------------

module Bindings.Yoga where

import Data.Data        (Data)
import Data.Typeable    (Typeable)
import Foreign.C.Types  (CFloat(..), CInt(..), CUInt(..), CBool(..), CDouble(..))
import Foreign.Ptr      (FunPtr, Ptr, plusPtr)
import Foreign.Storable (Storable(..))

import Prelude (Eq, IO, Show)
import Prelude (($), return)

import Bindings.Yoga.Enums
--------------------------------------------------------------------------------

#starttype struct YGSize
#field width       , CFloat
#field height      , CFloat
#stoptype
deriving instance Typeable C'YGSize

#opaque_t YGConfig
deriving instance Typeable C'YGConfig
deriving instance Data     C'YGConfig

#opaque_t YGNode
deriving instance Typeable C'YGNode
deriving instance Data     C'YGNode

#starttype struct YGValue
#field value , CFloat
#field unit  , <YGUnit>
#stoptype
deriving instance Typeable C'YGValue

#globalvar YGValueAuto      , <YGValue>
#globalvar YGValueUndefined , <YGValue>
#globalvar YGValueZero      , <YGValue>

#callback_t YGMeasureFunc, Ptr <YGNode> -> CFloat -> <YGMeasureMode> -> CFloat -> <YGMeasureMode> -> IO (Ptr <YGSize>)
#callback_t YGBaselineFunc, Ptr <YGNode> -> CFloat -> CFloat -> IO CFloat
#callback_t YGDirtiedFunc, Ptr <YGNode> -> IO ()
-- #callback_t YGPrintFunc, Ptr <YGNode> -> IO ()
-- #callback_t YGLogger, Ptr <YGConfig> -> Ptr <YGNode> -> <YGLogLevel> -> CString -> <va_list> -> IO CInt
#callback_t YGCloneNodeFunc, Ptr <YGNode> -> Ptr <YGNode> -> CInt -> IO (Ptr <YGNode>)

#ccall YGNodeNew, IO (Ptr <YGNode>)
#ccall YGNodeNewWithConfig, Ptr <YGConfig> -> IO (Ptr <YGNode>)
#ccall YGNodeClone, Ptr <YGNode> -> IO (Ptr <YGNode>)
#ccall YGNodeFree, Ptr <YGNode> -> IO ()
#ccall YGNodeFreeRecursive, Ptr <YGNode> -> IO ()
#ccall YGNodeFinalize, Ptr <YGNode> -> IO ()
#ccall YGNodeReset, Ptr <YGNode> -> IO ()

#ccall YGNodeGetHasNewLayout, Ptr <YGNode> -> IO CBool
#ccall YGNodeSetHasNewLayout, Ptr <YGNode> -> CBool -> IO ()

-- Mark a node as dirty. Only valid for nodes with a custom measure function
-- set.
-- YG knows when to mark all other nodes as dirty but because nodes with
-- measure functions
-- depends on information not known to YG they must perform this dirty
-- marking manually.
#ccall YGNodeMarkDirty, Ptr <YGNode> -> IO ()
#ccall YGNodeIsDirty, Ptr <YGNode> -> IO CBool
#ccall YGNodeGetDirtiedFunc, Ptr <YGNode> -> IO <YGDirtiedFunc>
#ccall YGNodeSetDirtiedFunc, Ptr <YGNode> -> <YGDirtiedFunc> -> IO ()

#ccall YGNodeInsertChild, Ptr <YGNode> -> Ptr <YGNode> -> CUInt -> IO ()
#ccall YGNodeSwapChild, Ptr <YGNode> -> Ptr <YGNode> -> CUInt -> IO ()

#ccall YGNodeRemoveChild, Ptr <YGNode> -> Ptr <YGNode> -> IO ()
#ccall YGNodeRemoveAllChildren, Ptr <YGNode> -> IO ()
#ccall YGNodeGetChild, Ptr <YGNode> -> CUInt -> IO (Ptr <YGNode>)
#ccall YGNodeGetOwner, Ptr <YGNode> -> IO (Ptr <YGNode>)
#ccall YGNodeGetParent, Ptr <YGNode> -> IO (Ptr <YGNode>)
#ccall YGNodeGetChildCount, Ptr <YGNode> -> IO CUInt
#ccall YGNodeSetChildren, Ptr <YGNode> -> Ptr (Ptr <YGNode>) -> CUInt -> IO ()

#ccall YGNodeSetConfig, Ptr <YGNode> -> Ptr <YGConfig> -> IO ()
#ccall YGNodeGetConfig, Ptr <YGNode> -> IO (Ptr <YGConfig>)

#ccall YGNodeSetContext, Ptr <YGNode> -> Ptr () -> IO ()
#ccall YGNodeGetContext, Ptr <YGNode> -> IO (Ptr ())

#ccall YGNodeHasMeasureFunc, Ptr <YGNode> -> IO CBool
#ccall YGNodeSetMeasureFunc, Ptr <YGNode> -> Ptr <YGMeasureFunc> -> IO ()

#ccall YGNodeHasBaselineFunc, Ptr <YGNode> -> IO CBool
#ccall YGNodeSetBaselineFunc, Ptr <YGNode> -> <YGBaselineFunc> -> IO ()
#ccall YGNodeSetIsReferenceBaseline, Ptr <YGNode> -> CBool -> IO ()
#ccall YGNodeIsReferenceBaseline, Ptr <YGNode> -> IO CBool

#ccall YGNodeGetNodeType, Ptr <YGNode> -> IO <YGNodeType>
#ccall YGNodeSetNodeType, Ptr <YGNode> -> <YGNodeType> -> IO ()

-- #indef NDEBUG
-- #ccall YGNodeSetPrintFunc, Ptr <YGNode> -> Ptr <YGPrintFunc> -> IO ()
-- #ccall YGNodePrint, Ptr <YGNode> -> <YGPrintOptions> -> IO ()

#ccall YGNodeSetAlwaysFormsContainingBlock, Ptr <YGNode> -> CBool -> IO ()

#ccall YGNodeCalculateLayout, Ptr <YGNode> -> CFloat -> CFloat -> <YGDirection> -> IO ()

#ccall YGFloatIsUndefined, CFloat -> IO CBool

#ccall YGNodeCopyStyle, Ptr <YGNode> -> Ptr <YGNode> -> IO ()

#ccall YGNodeStyleSetDirection, Ptr <YGNode> -> <YGDirection> -> IO ()
#ccall YGNodeStyleGetDirection, Ptr <YGNode> -> IO <YGDirection>

#ccall YGNodeStyleSetFlexDirection, Ptr <YGNode> -> <YGFlexDirection> -> IO ()
#ccall YGNodeStyleGetFlexDirection, Ptr <YGNode> -> IO <YGFlexDirection>

#ccall YGNodeStyleSetJustifyContent, Ptr <YGNode> -> <YGJustify> -> IO ()
#ccall YGNodeStyleGetJustifyContent, Ptr <YGNode> -> IO <YGJustify>

#ccall YGNodeStyleSetAlignContent, Ptr <YGNode> -> <YGAlign> -> IO ()
#ccall YGNodeStyleGetAlignContent, Ptr <YGNode> -> IO <YGAlign>

#ccall YGNodeStyleSetAlignItems, Ptr <YGNode> -> <YGAlign> -> IO ()
#ccall YGNodeStyleGetAlignItems, Ptr <YGNode> -> IO <YGAlign>

#ccall YGNodeStyleSetAlignSelf, Ptr <YGNode> -> <YGAlign> -> IO ()
#ccall YGNodeStyleGetAlignSelf, Ptr <YGNode> -> IO <YGAlign>

#ccall YGNodeStyleSetPositionType, Ptr <YGNode> -> <YGPositionType> -> IO ()
#ccall YGNodeStyleGetPositionType, Ptr <YGNode> -> IO <YGPositionType>

#ccall YGNodeStyleSetFlexWrap, Ptr <YGNode> -> <YGWrap> -> IO ()
#ccall YGNodeStyleGetFlexWrap, Ptr <YGNode> -> IO <YGWrap>

#ccall YGNodeStyleSetOverflow, Ptr <YGNode> -> <YGOverflow> -> IO ()
#ccall YGNodeStyleGetOverflow, Ptr <YGNode> -> IO <YGOverflow>

#ccall YGNodeStyleSetDisplay, Ptr <YGNode> -> <YGDisplay> -> IO ()
#ccall YGNodeStyleGetDisplay, Ptr <YGNode> -> IO <YGDisplay>

#ccall YGNodeStyleSetFlex, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleGetFlex, Ptr <YGNode> -> IO CFloat

#ccall YGNodeStyleSetFlexGrow, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleGetFlexGrow, Ptr <YGNode> -> IO CFloat

#ccall YGNodeStyleSetFlexShrink, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleGetFlexShrink, Ptr <YGNode> -> IO CFloat

#ccall YGNodeStyleSetFlexBasis, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleSetFlexBasisPercent, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleSetFlexBasisAuto, Ptr <YGNode> -> IO ()
-- #ccall YGNodeStyleGetFlexBasis, Ptr <YGNode> -> IO <YGValue>
#ccall YGNodeStyleGetFlexBasisWrapper, Ptr <YGNode> -> IO (Ptr <YGValue>)

#ccall YGNodeStyleSetPosition, Ptr <YGNode> -> <YGEdge> -> CFloat -> IO ()
#ccall YGNodeStyleSetPositionPercent, Ptr <YGNode> -> <YGEdge> -> CFloat -> IO ()
-- #ccall YGNodeStyleGetPosition, Ptr <YGNode> -> <YGEdge> -> IO <YGValue>
#ccall YGNodeStyleGetPositionWrapper, Ptr <YGNode> -> <YGEdge> -> IO (Ptr <YGValue>)

#ccall YGNodeStyleSetMargin, Ptr <YGNode> -> <YGEdge> -> CFloat -> IO ()
#ccall YGNodeStyleSetMarginPercent, Ptr <YGNode> -> <YGEdge> -> CFloat -> IO ()
#ccall YGNodeStyleSetMarginAuto, Ptr <YGNode> -> <YGEdge> -> IO ()
-- #ccall YGNodeStyleGetMargin, Ptr <YGNode> -> <YGEdge> -> IO <YGValue>
#ccall YGNodeStyleGetMarginWrapper, Ptr <YGNode> -> <YGEdge> -> IO (Ptr <YGValue>)

#ccall YGNodeStyleSetPadding, Ptr <YGNode> -> <YGEdge> -> CFloat -> IO ()
#ccall YGNodeStyleSetPaddingPercent, Ptr <YGNode> -> <YGEdge> -> CFloat -> IO ()
-- #ccall YGNodeStyleGetPadding, Ptr <YGNode> -> <YGEdge> -> IO <YGValue>
#ccall YGNodeStyleGetPaddingWrapper, Ptr <YGNode> -> <YGEdge> -> IO (Ptr <YGValue>)

#ccall YGNodeStyleSetBorder, Ptr <YGNode> -> <YGEdge> -> CFloat -> IO ()
#ccall YGNodeStyleGetBorder, Ptr <YGNode> -> <YGEdge> -> IO CFloat

#ccall YGNodeStyleSetGap, Ptr <YGNode> -> <YGGutter> -> CFloat -> IO ()
#ccall YGNodeStyleGetGap, Ptr <YGNode> -> <YGGutter> -> IO CFloat

#ccall YGNodeStyleSetWidth, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleSetWidthPercent, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleSetWidthAuto, Ptr <YGNode> -> IO ()
-- #ccall YGNodeStyleGetWidth, Ptr <YGNode> -> IO <YGValue>
#ccall YGNodeStyleGetWidthWrapper, Ptr <YGNode> -> IO (Ptr <YGValue>)

#ccall YGNodeStyleSetHeight, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleSetHeightPercent, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleSetHeightAuto, Ptr <YGNode> -> IO ()
-- #ccall YGNodeStyleGetHeight, Ptr <YGNode> -> IO <YGValue>
#ccall YGNodeStyleGetHeightWrapper, Ptr <YGNode> -> IO (Ptr <YGValue>)

#ccall YGNodeStyleSetMinWidth, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleSetMinWidthPercent, Ptr <YGNode> -> CFloat -> IO ()
-- #ccall YGNodeStyleGetMinWidth, Ptr <YGNode> -> IO <YGValue>
#ccall YGNodeStyleGetMinWidthWrapper, Ptr <YGNode> -> IO (Ptr <YGValue>)

#ccall YGNodeStyleSetMinHeight, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleSetMinHeightPercent, Ptr <YGNode> -> CFloat -> IO ()
-- #ccall YGNodeStyleGetMinHeight, Ptr <YGNode> -> IO <YGValue>
#ccall YGNodeStyleGetMinHeightWrapper, Ptr <YGNode> -> IO (Ptr <YGValue>)

#ccall YGNodeStyleSetMaxWidth, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleSetMaxWidthPercent, Ptr <YGNode> -> CFloat -> IO ()
-- #ccall YGNodeStyleGetMaxWidth, Ptr <YGNode> -> IO <YGValue>
#ccall YGNodeStyleGetMaxWidthWrapper, Ptr <YGNode> -> IO (Ptr <YGValue>)

#ccall YGNodeStyleSetMaxHeight, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleSetMaxHeightPercent, Ptr <YGNode> -> CFloat -> IO ()
-- #ccall YGNodeStyleGetMaxHeight, Ptr <YGNode> -> IO <YGValue>
#ccall YGNodeStyleGetMaxHeightWrapper, Ptr <YGNode> -> IO (Ptr <YGValue>)

-- YGValue wrapper
#ccall YGValueFree, Ptr <YGValue> -> IO ()

-- Yoga specific properties, not compatible with flexbox specification Aspect
-- ratio control the size of the undefined dimension of a node. Aspect ratio is
-- encoded as a floating point value width/height. e.g. A value of 2 leads to a
-- node with a width twice the size of its height while a value of 0.5 gives the
-- opposite effect.
--
-- - On a node with a set width/height aspect ratio control the size of the
--   unset dimension
-- - On a node with a set flex basis aspect ratio controls the size of the node
--   in the cross axis if unset
-- - On a node with a measure function aspect ratio works as though the measure
--   function measures the flex basis
-- - On a node with flex grow/shrink aspect ratio controls the size of the node
--   in the cross axis if unset
-- - Aspect ratio takes min/max dimensions into account
#ccall YGNodeStyleSetAspectRatio, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleGetAspectRatio, Ptr <YGNode> -> IO CFloat

#ccall YGNodeLayoutGetLeft, Ptr <YGNode> -> IO CFloat
#ccall YGNodeLayoutGetTop, Ptr <YGNode> -> IO CFloat
#ccall YGNodeLayoutGetRight, Ptr <YGNode> -> IO CFloat
#ccall YGNodeLayoutGetBottom, Ptr <YGNode> -> IO CFloat
#ccall YGNodeLayoutGetWidth, Ptr <YGNode> -> IO CFloat
#ccall YGNodeLayoutGetHeight, Ptr <YGNode> -> IO CFloat
#ccall YGNodeLayoutGetDirection, Ptr <YGNode> -> IO <YGDirection>
#ccall YGNodeLayoutGetHadOverflow, Ptr <YGNode> -> IO CBool

-- Get the computed values for these nodes after performing layout. If they were
-- set using point values then the returned value will be the same as
-- YGNodeStyleGetXXX. However if they were set using a percentage value then the
-- returned value is the computed value used during layout.
#ccall YGNodeLayoutGetMargin, Ptr <YGNode> -> <YGEdge> -> IO CFloat
#ccall YGNodeLayoutGetBorder, Ptr <YGNode> -> <YGEdge> -> IO CFloat
#ccall YGNodeLayoutGetPadding, Ptr <YGNode> -> <YGEdge> -> IO CFloat

-- YGConfig
#ccall YGConfigNew, IO (Ptr <YGConfig>)
#ccall YGConfigFree, Ptr <YGConfig> -> IO ()
#ccall YGConfigGetDefault, IO (Ptr <YGConfig>)

#ccall YGConfigSetExperimentalFeatureEnabled, Ptr <YGConfig> -> <YGExperimentalFeature> -> CBool -> IO ()
#ccall YGConfigIsExperimentalFeatureEnabled, Ptr <YGConfig> -> <YGExperimentalFeature> -> IO CBool

-- Using the web defaults is the preferred configuration for new projects. Usage
-- of non web defaults should be considered as legacy.
#ccall YGConfigSetUseWebDefaults, Ptr <YGConfig> -> CBool -> IO ()
#ccall YGConfigGetUseWebDefaults, Ptr <YGConfig> -> IO CBool

-- Set this to number of pixels in 1 point to round calculation results If you
-- want to avoid rounding - set PointScaleFactor to 0
#ccall YGConfigSetPointScaleFactor, Ptr <YGConfig> -> CFloat -> IO ()
#ccall YGConfigGetPointScaleFactor, Ptr <YGConfig> -> IO CFloat

#ccall YGConfigSetErrata, Ptr <YGConfig> -> <YGErrata> -> IO ()
#ccall YGConfigGetErrata, Ptr <YGConfig> -> IO <YGErrata>

#ccall YGConfigSetCloneNodeFunc, Ptr <YGConfig> -> <YGCloneNodeFunc> -> IO ()
#ccall YGConfigSetPrintTreeFlag, Ptr <YGConfig> -> CBool -> IO ()

#ccall YGConfigSetContext, Ptr <YGConfig> -> Ptr () -> IO ()
#ccall YGConfigGetContext, Ptr <YGConfig> -> IO (Ptr ())

#ccall YGRoundValueToPixelGrid, CDouble -> CDouble -> CBool -> CBool -> IO CFloat
