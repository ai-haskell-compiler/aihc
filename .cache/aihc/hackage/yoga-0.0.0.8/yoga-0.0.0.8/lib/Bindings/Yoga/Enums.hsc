{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}

--------------------------------------------------------------------------------

#include <Yoga.h>
#include <bindings.dsl.h>

--------------------------------------------------------------------------------

module Bindings.Yoga.Enums where

import Prelude (Num, IO)
import Foreign.C.Types  (CUInt(..))
import Foreign.C.String (CString)
import Foreign.Ptr      (FunPtr)

#integral_t enum YGAlign
#num YGAlignAuto
#num YGAlignFlexStart
#num YGAlignCenter
#num YGAlignFlexEnd
#num YGAlignStretch
#num YGAlignBaseline
#num YGAlignSpaceBetween
#num YGAlignSpaceAround
#ccall YGAlignToString, <YGAlign> -> IO CString

#integral_t enum YGDimension
#num YGDimensionWidth
#num YGDimensionHeight
#ccall YGDimensionToString, <YGAlign> -> IO CString

#integral_t enum YGDirection
#num YGDirectionInherit
#num YGDirectionLTR
#num YGDirectionRTL
#ccall YGDirectionToString, <YGAlign> -> IO CString

#integral_t enum YGDisplay
#num YGDisplayFlex
#num YGDisplayNone
#ccall YGDisplayToString, <YGAlign> -> IO CString

#integral_t enum YGEdge
#num YGEdgeLeft
#num YGEdgeTop
#num YGEdgeRight
#num YGEdgeBottom
#num YGEdgeStart
#num YGEdgeEnd
#num YGEdgeHorizontal
#num YGEdgeVertical
#num YGEdgeAll
#ccall YGEdgeToString, <YGAlign> -> IO CString

#integral_t enum YGErrata
#num YGErrataNone
#num YGErrataStretchFlexBasis
#num YGErrataStartingEndingEdgeFromFlexDirection
#num YGErrataPositionStaticBehavesLikeRelative
#num YGErrataAbsolutePositioning
#num YGErrataAll
#num YGErrataClassic
#ccall YGErrataToString, <YGAlign> -> IO CString

#integral_t enum YGExperimentalFeature
#num YGExperimentalFeatureWebFlexBasis
#num YGExperimentalFeatureAbsolutePercentageAgainstPaddingEdge
#ccall YGExperimentalFeatureToString, <YGAlign> -> IO CString

#integral_t enum YGFlexDirection
#num YGFlexDirectionColumn
#num YGFlexDirectionColumnReverse
#num YGFlexDirectionRow
#num YGFlexDirectionRowReverse
#ccall YGFlexDirectionToString, <YGAlign> -> IO CString

#integral_t enum YGGutter
#num YGGutterColumn
#num YGGutterRow
#num YGGutterAll
#ccall YGGutterToString, <YGAlign> -> IO CString

#integral_t enum YGJustify
#num YGJustifyFlexStart
#num YGJustifyCenter
#num YGJustifyFlexEnd
#num YGJustifySpaceBetween
#num YGJustifySpaceAround
#num YGJustifySpaceEvenly
#ccall YGJustifyToString, <YGAlign> -> IO CString

#integral_t enum YGLogLevel
#num YGLogLevelError
#num YGLogLevelWarn
#num YGLogLevelInfo
#num YGLogLevelDebug
#num YGLogLevelVerbose
#num YGLogLevelFatal
#ccall YGLogLevelToString, <YGAlign> -> IO CString

#integral_t enum YGMeasureMode
#num YGMeasureModeUndefined
#num YGMeasureModeExactly
#num YGMeasureModeAtMost
#ccall YGMeasureModeToString, <YGAlign> -> IO CString

#integral_t enum YGNodeType
#num YGNodeTypeDefault
#num YGNodeTypeText
#ccall YGNodeTypeToString, <YGAlign> -> IO CString

#integral_t enum YGOverflow
#num YGOverflowVisible
#num YGOverflowHidden
#num YGOverflowScroll
#ccall YGOverflowToString, <YGAlign> -> IO CString

#integral_t enum YGPositionType
#num YGPositionTypeStatic
#num YGPositionTypeRelative
#num YGPositionTypeAbsolute
#ccall YGPositionTypeToString, <YGAlign> -> IO CString

#integral_t enum YGPrintOptions
#num YGPrintOptionsLayout
#num YGPrintOptionsStyle
#num YGPrintOptionsChildren
#ccall YGPrintOptionsToString, <YGAlign> -> IO CString

#integral_t enum YGUnit
#num YGUnitUndefined
#num YGUnitPoint
#num YGUnitPercent
#num YGUnitAuto
#ccall YGUnitToString, <YGAlign> -> IO CString

#integral_t enum YGWrap
#num YGWrapNoWrap
#num YGWrapWrap
#num YGWrapWrapReverse
#ccall YGWrapToString, <YGAlign> -> IO CString
