{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Cairo.Drawing.Paths.CairoPathT (
	Path(..), CairoPathT, pattern CairoPathT, withCairoPathT, mkCairoPathT,
	MoveTo(..), LineCurveTo(..), CloseTo(..),
	CairoPatchPathT, pattern CairoPatchPathT,
	pattern CairoPathTPatch, mkCairoPatchPathT ) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Data.Bool
import Data.Word

import Graphics.Cairo.Exception

import System.IO.Unsafe

#include <cairo.h>

newtype CairoPathT = CairoPathT_ (ForeignPtr CairoPathT) deriving Show

pattern CairoPathT :: [Path] -> CairoPathT
pattern CairoPathT ps <- (unsafePerformIO . cairoPathTPathList -> ps) where
	CairoPathT = unsafePerformIO . pathListToCairoPathT

withCairoPathT :: CairoPathT -> (Ptr CairoPathT -> IO a) -> IO a
withCairoPathT (CairoPathT_ fpth) = withForeignPtr fpth

mkCairoPathT :: Ptr CairoPathT -> IO CairoPathT
mkCairoPathT p =
	CairoPathT_ <$> newForeignPtr p (c_cairo_path_destroy p)
		<* (cairoStatusToThrowError =<< cairoPathTStatus p)

foreign import ccall "cairo_path_destroy" c_cairo_path_destroy ::
	Ptr CairoPathT -> IO ()

cairoPathTStatus :: Ptr CairoPathT -> IO #{type cairo_status_t}
cairoPathTStatus = #{peek cairo_path_t, status}

cairoPathTData ::Ptr CairoPathT -> IO (Ptr CairoPathDataT)
cairoPathTData = #{peek cairo_path_t, data}

cairoPathTNumData :: Ptr CairoPathT -> IO CInt
cairoPathTNumData = #{peek cairo_path_t, num_data}

newtype CairoPathDataT = CairoPathDataT (Ptr CairoPathDataT) deriving Show

nextPtr :: Ptr a -> Int -> Int -> Ptr a
nextPtr p sz al = alignPtr (plusPtr p sz) al

nextCairoPathDataT :: Ptr CairoPathDataT -> Ptr CairoPathDataT
nextCairoPathDataT p = nextPtr p #{size cairo_path_data_t} #{alignment cairo_path_data_t}

nextByLength :: Ptr CairoPathDataT -> CInt -> Ptr CairoPathDataT
nextByLength p n | n < 1 = p
nextByLength p n = nextCairoPathDataT $ nextByLength p (n - 1)

cairoPathDataTHeaderType :: Ptr CairoPathDataT -> IO #{type cairo_path_data_type_t}
cairoPathDataTHeaderType = #{peek cairo_path_data_t, header.type}

cairoPathDataTHeaderLength :: Ptr CairoPathDataT -> IO CInt
cairoPathDataTHeaderLength = #{peek cairo_path_data_t, header.length}

cairoPathDataTPointX, cairoPathDataTPointY :: Ptr CairoPathDataT -> IO CDouble
cairoPathDataTPointX = #{peek cairo_path_data_t, point.x}
cairoPathDataTPointY = #{peek cairo_path_data_t, point.y}

data Path
	= PathMoveTo CDouble CDouble
	| PathLineTo CDouble CDouble
	| PathCurveTo CDouble CDouble CDouble CDouble CDouble CDouble
	| PathClosePath
	deriving Show

cairoPathTPathList :: CairoPathT -> IO [Path]
cairoPathTPathList (CairoPathT_ fp) = withForeignPtr fp \p -> do
	d <- cairoPathTData p
	n <- cairoPathTNumData p
	cairoPathDataTPathList d n

cairoPathDataTPathList :: Ptr CairoPathDataT -> CInt -> IO [Path]
cairoPathDataTPathList _ n | n < 1 = pure []
cairoPathDataTPathList p n = unsafeInterleaveIO do
	pth <- unsafeInterleaveIO $ cairoPathDataTHeaderType p >>= \case
		#{const CAIRO_PATH_MOVE_TO} -> PathMoveTo
			<$> cairoPathDataTPointX p1 <*> cairoPathDataTPointY p1
		#{const CAIRO_PATH_LINE_TO} -> PathLineTo
			<$> cairoPathDataTPointX p1 <*> cairoPathDataTPointY p1
		#{const CAIRO_PATH_CURVE_TO} -> PathCurveTo
			<$> cairoPathDataTPointX p1 <*> cairoPathDataTPointY p1
			<*> cairoPathDataTPointX p2 <*> cairoPathDataTPointY p2
			<*> cairoPathDataTPointX p3 <*> cairoPathDataTPointY p3
		#{const CAIRO_PATH_CLOSE_PATH} -> pure PathClosePath
		_ -> error "no such path"
	ln <- cairoPathDataTHeaderLength p
	(pth :) <$> cairoPathDataTPathList (nextByLength p ln) (n - ln)
	where
	p1 = nextByLength p 1
	p2 = nextByLength p 2
	p3 = nextByLength p 3

pathToNumData :: Path -> Int
pathToNumData = \case
	PathMoveTo _ _ -> 2; PathLineTo _ _ -> 2; PathCurveTo _ _ _ _ _ _ -> 4; PathClosePath -> 1

pathToCairoPathData :: Ptr CairoPathDataT -> Path -> IO ()
pathToCairoPathData p = \case
	PathMoveTo x y -> do
		#{poke cairo_path_data_t, header.type} p (#{const CAIRO_PATH_MOVE_TO} :: #{type cairo_path_data_type_t})
		#{poke cairo_path_data_t, header.length} p (2 :: CInt)
		#{poke cairo_path_data_t, point.x} p1 x
		#{poke cairo_path_data_t, point.y} p1 y
	PathLineTo x y -> do
		#{poke cairo_path_data_t, header.type} p (#{const CAIRO_PATH_LINE_TO} :: #{type cairo_path_data_type_t})
		#{poke cairo_path_data_t, header.length} p (2 :: CInt)
		#{poke cairo_path_data_t, point.x} p1 x
		#{poke cairo_path_data_t, point.y} p1 y
	PathCurveTo x1 y1 x2 y2 x3 y3 -> do
		#{poke cairo_path_data_t, header.type} p (#{const CAIRO_PATH_CURVE_TO} :: #{type cairo_path_data_type_t})
		#{poke cairo_path_data_t, header.length} p (4 :: CInt)
		#{poke cairo_path_data_t, point.x} p1 x1
		#{poke cairo_path_data_t, point.y} p1 y1
		#{poke cairo_path_data_t, point.x} p2 x2
		#{poke cairo_path_data_t, point.y} p2 y2
		#{poke cairo_path_data_t, point.x} p3 x3
		#{poke cairo_path_data_t, point.y} p3 y3
	PathClosePath -> do
		#{poke cairo_path_data_t, header.type} p (#{const CAIRO_PATH_CLOSE_PATH} :: #{type cairo_path_data_type_t})
		#{poke cairo_path_data_t, header.length} p (1 :: CInt)
	where
	p1 = nextByLength p 1
	p2 = nextByLength p 2
	p3 = nextByLength p 3

calcAlignedSize :: Int -> Int -> Int
calcAlignedSize sz al = (sz `div` al + signum (sz `mod` al)) * al

cairoPathDataTSize :: Int
cairoPathDataTSize = calcAlignedSize #{size cairo_path_data_t} #{alignment cairo_path_data_t}

pathListToCairoPathT :: [Path] -> IO CairoPathT
pathListToCairoPathT pths = CairoPathT_ <$> do
	pd <- mallocBytes $ sum (pathToNumData <$> pths) * cairoPathDataTSize
	pathListToCairoPathDataT pd pths
	p <- mallocBytes #{size cairo_path_t}
	#{poke cairo_path_t, status} p (#{const CAIRO_STATUS_SUCCESS} :: #{type cairo_status_t})
	#{poke cairo_path_t, data} p pd
	#{poke cairo_path_t, num_data} p $ sum (pathToNumData <$> pths)
	newForeignPtr p $ free pd >> free p

pathListToCairoPathDataT :: Ptr CairoPathDataT -> [Path] -> IO ()
pathListToCairoPathDataT pd = \case
	[] -> pure ()
	pth : pths -> pathToCairoPathData pd pth >> pathListToCairoPathDataT (pd `plusPtr` (cairoPathDataTSize * pathToNumData pth)) pths

cairoPathTCheckPaths :: CairoPathT -> [#{type cairo_path_data_type_t} -> Bool] -> IO Bool
cairoPathTCheckPaths (CairoPathT_ fpth) ts = withForeignPtr fpth \ppth -> do
	d <- cairoPathTData ppth
	n <- cairoPathTNumData ppth
	cairoPathDataCheckPaths d ts n

cairoPathDataCheckPaths :: Ptr CairoPathDataT -> [#{type cairo_path_data_type_t} -> Bool] -> CInt -> IO Bool
cairoPathDataCheckPaths _ [] 0 = pure True
cairoPathDataCheckPaths _ [] _ = pure False
cairoPathDataCheckPaths _ _ 0 = pure False
cairoPathDataCheckPaths p (t : ts) n = do
	b <- t <$> cairoPathDataTHeaderType p
	ln <- cairoPathDataTHeaderLength p
	(b &&) <$> unsafeInterleaveIO (cairoPathDataCheckPaths (nextByLength p ln) ts (n - ln))

cairoPathTGetGoal :: CairoPathT -> Int -> IO (Maybe (CDouble, CDouble))
cairoPathTGetGoal (CairoPathT_ fpth) i = withForeignPtr fpth \ppth -> do
	d <- cairoPathTData ppth
	n <- cairoPathTNumData ppth
	cairoPathDataTGetGoal d i n

cairoPathDataTGetGoal :: Ptr CairoPathDataT -> Int -> CInt -> IO (Maybe (CDouble, CDouble))
cairoPathDataTGetGoal _ _ n | n < 1 = pure Nothing
cairoPathDataTGetGoal p 0 _ = unsafeInterleaveIO do
	cairoPathDataTHeaderType p >>= \case
		#{const CAIRO_PATH_MOVE_TO} -> (\x y -> Just (x, y))
			<$> cairoPathDataTPointX p1 <*> cairoPathDataTPointY p1
		#{const CAIRO_PATH_LINE_TO} -> (\x y -> Just (x, y))
			<$> cairoPathDataTPointX p1 <*> cairoPathDataTPointY p1
		#{const CAIRO_PATH_CURVE_TO} -> (\x y -> Just (x, y))
			<$> cairoPathDataTPointX p3 <*> cairoPathDataTPointY p3
		#{const CAIRO_PATH_CLOSE_PATH} -> pure Nothing
		_ -> error "no such path"
	where p1 = nextByLength p 1; p3 = nextByLength p 3
cairoPathDataTGetGoal p i n = do
	ln <- cairoPathDataTHeaderLength p
	cairoPathDataTGetGoal (nextByLength p ln) (i - 1) (n - ln)

lineOrCurve :: #{type cairo_path_data_type_t} -> Bool
lineOrCurve = (||) <$> (== #{const CAIRO_PATH_LINE_TO}) <*> (== #{const CAIRO_PATH_CURVE_TO})

isCairoPatchPath :: CairoPathT -> IO Bool
isCairoPatchPath pth = do
	b1 <- cairoPathTCheckPaths pth [
		(== #{const CAIRO_PATH_MOVE_TO}), lineOrCurve, lineOrCurve, lineOrCurve,
		(== #{const CAIRO_PATH_CLOSE_PATH}) ]
	b2 <- cairoPathTCheckPaths pth [
		(== #{const CAIRO_PATH_MOVE_TO}), lineOrCurve, lineOrCurve, lineOrCurve, lineOrCurve ]
	b3 <- cairoPathTGetGoal pth 0 >>= \case
		Nothing -> pure False
		Just (x0, y0) -> cairoPathTGetGoal pth 4 >>= \case
			Nothing -> pure False
			Just (xc, yc) -> pure $ xc == x0 && yc == y0
	pure $ b1 || (b2 && b3)

newtype CairoPatchPathT = CairoPatchPathT_ (ForeignPtr CairoPathT) deriving Show

mkCairoPatchPathT :: Ptr CairoPathT -> IO CairoPatchPathT
mkCairoPatchPathT p =
	CairoPatchPathT_ <$> newForeignPtr p (c_cairo_path_destroy p)
		<* (cairoStatusToThrowError =<< cairoPathTStatus p)

pattern CairoPathTPatch :: CairoPatchPathT -> CairoPathT
pattern CairoPathTPatch ppth <- (unsafePerformIO . cairoPathTPatch -> Just ppth) where
	CairoPathTPatch (CairoPatchPathT_ fpth) = CairoPathT_ fpth

cairoPathTPatch :: CairoPathT -> IO (Maybe CairoPatchPathT)
cairoPathTPatch pth@(CairoPathT_ fpth) =
	bool Nothing (Just $ CairoPatchPathT_ fpth) <$> isCairoPatchPath pth

data MoveTo = MoveTo CDouble CDouble deriving Show

data LineCurveTo
	= LineTo CDouble CDouble
	| CurveTo CDouble CDouble CDouble CDouble CDouble CDouble
	deriving Show

data CloseTo
	= CloseLineTo
	| CloseCurveTo CDouble CDouble CDouble CDouble
	deriving Show

pattern CairoPatchPathT :: MoveTo -> LineCurveTo -> LineCurveTo -> LineCurveTo -> CloseTo -> CairoPatchPathT
pattern CairoPatchPathT mt lct1 lct2 lct3 cls <- (unsafePerformIO . drawFromCairoPatchPathT -> (mt, lct1, lct2, lct3, cls)) where
	CairoPatchPathT mt lct1 lct2 lct3 cls = unsafePerformIO $ patchPathsToCairoPatchPathT mt lct1 lct2 lct3 cls

drawFromCairoPatchPathT :: CairoPatchPathT -> IO (MoveTo, LineCurveTo, LineCurveTo, LineCurveTo, CloseTo)
drawFromCairoPatchPathT (CairoPatchPathT_ fpth) = withForeignPtr fpth \ppth -> do
	d <- cairoPathTData ppth
	n <- cairoPathTNumData ppth
	drawFromCairoPathDataT d n >>= \case
		(Just r, 0) -> pure r
		_ -> error "badbadbad"
		
drawFromCairoPathDataT :: Ptr CairoPathDataT -> CInt ->
	IO (Maybe (MoveTo, LineCurveTo, LineCurveTo, LineCurveTo, CloseTo), CInt)
drawFromCairoPathDataT p0 n0 = do
	(mmt, pn1) <- drawMoveToFromCairoPathDataT p0 n0
	(mlct1, pn2) <- uncurry drawLineCurveToFromCairoPathDataT pn1
	(mlct2, pn3) <- uncurry drawLineCurveToFromCairoPathDataT pn2
	(mlct3, pn4) <- uncurry drawLineCurveToFromCairoPathDataT pn3
	(mcls_, (_p5, n5)) <- uncurry drawCloseToFromCairoPathDataT pn4
	let	mcls = case mmt of
			Nothing -> error "bad"
			Just (MoveTo x0 y0) -> case mcls_ of
				Nothing -> error "bad"
				Just (m, Nothing) -> Just m
				Just (m, Just (x1, y1))
					| x0 == x1 && y0 == y1 -> Just m
					| otherwise -> error "bad"
	if n5 /= 0 then error "bad" else pure 
		. (, n5) $ (,,,,) <$> mmt <*> mlct1 <*> mlct2 <*> mlct3 <*> mcls

drawMoveToFromCairoPathDataT :: Ptr CairoPathDataT -> CInt -> IO (Maybe MoveTo, (Ptr CairoPathDataT, CInt))
drawMoveToFromCairoPathDataT p n = do
	mt <- cairoPathDataTHeaderType p >>= \case
		#{const CAIRO_PATH_MOVE_TO} -> Just
			<$> (MoveTo <$> cairoPathDataTPointX p1 <*> cairoPathDataTPointY p1)
		_ -> pure Nothing
	ln <- cairoPathDataTHeaderLength p
	if n - ln < 0 then error "bad" else pure (mt, (nextByLength p ln, n - ln))
	where p1 = nextByLength p 1

drawLineCurveToFromCairoPathDataT :: Ptr CairoPathDataT -> CInt -> IO (Maybe LineCurveTo, (Ptr CairoPathDataT, CInt))
drawLineCurveToFromCairoPathDataT p n = do
	lct <- cairoPathDataTHeaderType p >>= \case
		#{const CAIRO_PATH_LINE_TO} -> Just
			<$> (LineTo <$> cairoPathDataTPointX p1 <*> cairoPathDataTPointY p1)
		#{const CAIRO_PATH_CURVE_TO} -> (Just <$>) $ CurveTo
			<$> cairoPathDataTPointX p1 <*> cairoPathDataTPointY p1
			<*> cairoPathDataTPointX p2 <*> cairoPathDataTPointY p2
			<*> cairoPathDataTPointX p3 <*> cairoPathDataTPointY p3
		_ -> pure Nothing
	ln <- cairoPathDataTHeaderLength p
	if n - ln < 0 then error "bad" else pure (lct, (nextByLength p ln, n - ln))
	where p1 = nextByLength p 1; p2 = nextByLength p 2; p3 = nextByLength p 3

drawCloseToFromCairoPathDataT :: Ptr CairoPathDataT -> CInt -> IO (Maybe (CloseTo, Maybe (CDouble, CDouble)), (Ptr CairoPathDataT, CInt))
drawCloseToFromCairoPathDataT p n = do
	ctgl <- cairoPathDataTHeaderType p >>= \case
		#{const CAIRO_PATH_LINE_TO} -> (Just <$>) $ ((CloseLineTo ,) . Just <$>) $
			(,) <$> cairoPathDataTPointX p1 <*> cairoPathDataTPointY p1
		#{const CAIRO_PATH_CURVE_TO} -> (Just <$>) $ (,)
			<$> (CloseCurveTo
				<$> cairoPathDataTPointX p1 <*> cairoPathDataTPointY p1
				<*> cairoPathDataTPointX p2 <*> cairoPathDataTPointY p2)
			<*> (Just <$> ((,) <$> cairoPathDataTPointX p3 <*> cairoPathDataTPointY p3))
		#{const CAIRO_PATH_CLOSE_PATH} -> pure $ Just (CloseLineTo, Nothing)
		_ -> pure Nothing
	ln <- cairoPathDataTHeaderLength p
	if n - ln < 0 then error "bad" else pure (ctgl, (nextByLength p ln, n - ln))
	where [p1, p2, p3] = nextByLength p <$> [1, 2, 3]

patchPathsToCairoPatchPathT ::
	MoveTo -> LineCurveTo -> LineCurveTo -> LineCurveTo -> CloseTo -> IO CairoPatchPathT
patchPathsToCairoPatchPathT mt lct1 lct2 lct3 cls = CairoPatchPathT_ <$> do
	pd <- mallocBytes $ patchPathSize mt lct1 lct2 lct3 cls * cairoPathDataTSize
	patchPathsToCairoPathDataT pd mt lct1 lct2 lct3 cls
	p <- mallocBytes #{size cairo_path_t}
	#{poke cairo_path_t, status} p (#{const CAIRO_STATUS_SUCCESS} :: #{type cairo_status_t})
	#{poke cairo_path_t, data} p pd
	#{poke cairo_path_t, num_data} p $ patchPathSize mt lct1 lct2 lct3 cls
	newForeignPtr p $ free pd >> free p

patchPathSize :: MoveTo -> LineCurveTo -> LineCurveTo -> LineCurveTo -> CloseTo -> Int
patchPathSize _mt lct1 lct2 lct3 cls =
	2 + sum (lineCurveToSize <$> [lct1, lct2, lct3]) + closeToSize cls

lineCurveToSize :: LineCurveTo -> Int
lineCurveToSize (LineTo _ _) = 2
lineCurveToSize (CurveTo _ _ _ _ _ _) = 4

closeToSize :: CloseTo -> Int
closeToSize CloseLineTo = 1
closeToSize (CloseCurveTo _ _ _ _) = 4

patchPathsToCairoPathDataT :: Ptr CairoPathDataT ->
	MoveTo -> LineCurveTo -> LineCurveTo -> LineCurveTo -> CloseTo -> IO ()
patchPathsToCairoPathDataT pd mt lct1 lct2 lct3 cls = do
	pd1 <- moveToToCairoPathDataT pd mt
	pd2 <- lineCurveToToCairoPathDataT pd1 lct1
	pd3 <- lineCurveToToCairoPathDataT pd2 lct2
	pd4 <- lineCurveToToCairoPathDataT pd3 lct3
	_ <- closeToToCairoPathDataT pd4 mt cls
	pure ()

moveToToCairoPathDataT :: Ptr CairoPathDataT -> MoveTo -> IO (Ptr CairoPathDataT)
moveToToCairoPathDataT p (MoveTo x y) = do
	#{poke cairo_path_data_t, header.type} p (#{const CAIRO_PATH_MOVE_TO} :: #{type cairo_path_data_type_t})
	#{poke cairo_path_data_t, header.length} p (2 :: CInt)
	#{poke cairo_path_data_t, point.x} p1 x
	#{poke cairo_path_data_t, point.y} p1 y
	pure $ p `plusPtr` (cairoPathDataTSize * 2)
	where p1 = nextByLength p 1

lineCurveToToCairoPathDataT :: Ptr CairoPathDataT -> LineCurveTo -> IO (Ptr CairoPathDataT)
lineCurveToToCairoPathDataT p = \case
	LineTo x y -> do
		#{poke cairo_path_data_t, header.type} p (#{const CAIRO_PATH_LINE_TO} :: #{type cairo_path_data_type_t})
		#{poke cairo_path_data_t, header.length} p (2 :: CInt)
		#{poke cairo_path_data_t, point.x} p1 x
		#{poke cairo_path_data_t, point.y} p1 y
		pure $ p `plusPtr` (cairoPathDataTSize * 2)
	CurveTo x1 y1 x2 y2 x3 y3 -> do
		#{poke cairo_path_data_t, header.type} p (#{const CAIRO_PATH_CURVE_TO} :: #{type cairo_path_data_type_t})
		#{poke cairo_path_data_t, header.length} p (4 :: CInt)
		#{poke cairo_path_data_t, point.x} p1 x1
		#{poke cairo_path_data_t, point.y} p1 y1
		#{poke cairo_path_data_t, point.x} p2 x2
		#{poke cairo_path_data_t, point.y} p2 y2
		#{poke cairo_path_data_t, point.x} p3 x3
		#{poke cairo_path_data_t, point.y} p3 y3
		pure $ p `plusPtr` (cairoPathDataTSize * 4)
	where p1 = nextByLength p 1; p2 = nextByLength p 2; p3 = nextByLength p 3

closeToToCairoPathDataT :: Ptr CairoPathDataT -> MoveTo -> CloseTo -> IO (Ptr CairoPathDataT)
closeToToCairoPathDataT p (MoveTo x0 y0) = \case
	CloseLineTo -> do
		#{poke cairo_path_data_t, header.type} p (#{const CAIRO_PATH_CLOSE_PATH} :: #{type cairo_path_data_type_t})
		#{poke cairo_path_data_t, header.length} p (1 :: CInt)
		pure $ p `plusPtr` cairoPathDataTSize
	CloseCurveTo x1 y1 x2 y2 -> do
		#{poke cairo_path_data_t, header.type} p (#{const CAIRO_PATH_CURVE_TO} :: #{type cairo_path_data_type_t})
		#{poke cairo_path_data_t, header.length} p (4 :: CInt)
		#{poke cairo_path_data_t, point.x} p1 x1
		#{poke cairo_path_data_t, point.y} p1 y1
		#{poke cairo_path_data_t, point.x} p2 x2
		#{poke cairo_path_data_t, point.y} p2 y2
		#{poke cairo_path_data_t, point.x} p3 x0
		#{poke cairo_path_data_t, point.y} p3 y0
		pure $ p `plusPtr` (cairoPathDataTSize * 4)
	where p1 = nextByLength p 1; p2 = nextByLength p 2; p3 = nextByLength p 3
