{-# LANGUAGE FlexibleContexts #-}

import Graphics.X11		(
	Display, openDisplay, closeDisplay, defaultScreen, rootWindow,
	whitePixel, blackPixel,
	Window, createSimpleWindow, destroyWindow, mapWindow,
	GC, createGC, freeGC, supportsLocale, setLocaleModifiers,
	selectInput, exposureMask, keyPressMask, focusChangeMask,
	Atom, internAtom, setWMProtocols, XEventPtr, nextEvent, allocaXEvent	)
import Graphics.X11.Xlib.Extras	(
	Event(ClientMessageEvent, KeyEvent), getEvent, ev_data,
	FontSet, createFontSet
									)
import Graphics.X11.Types	( xK_Return				)
import Graphics.X11.Xim		(
	XIM, openIM, closeIM, XIC, createIC, destroyIC, getICValue,
	filterEvent, XNInputStyle(..),
	utf8LookupString, utf8DrawString				)
import Data.IORef		( IORef, newIORef, readIORef, writeIORef,
					modifyIORef			)
import Data.Maybe		( fromMaybe				)
import Data.Bits		( (.|.)					)
import Data.Convertible		( Convertible, convert			)
import Control.Monad		( unless, when				)
import Control.Monad.Tools	( doUntil_ 				)
import System.Exit		( exitFailure				)
import System.Locale.SetLocale	( setLocale, Category(..)		)
import Foreign.C.Types		( CInt					)

setLocaleAndCheck :: IO ()
setLocaleAndCheck = do
	ret <- setLocale LC_CTYPE Nothing
	case ret of
		Nothing -> putStrLn "Can't set locale." >> exitFailure
		_	-> return ()

supportsLocaleAndCheck :: IO ()
supportsLocaleAndCheck = do
	sl <- supportsLocale
	unless sl $ putStrLn "Current locale is not supported" >> exitFailure

keyEventAction :: Display -> Window -> FontSet -> GC -> XIC -> IORef Int ->
			IORef Int -> XEventPtr -> IO Bool
keyEventAction dpy win fs gc ic posx posy e = do
	( mstr, mks ) <- utf8LookupString ic e
	x <- readIORef posx
	y <- readIORef posy
	if ( mks == Just xK_Return )
		then do
			putStrLn ""
			writeIORef posx 5
			modifyIORef posy (+ 13)
		else case ( mstr, mks ) of
			( Just str, _ ) -> do
				modifyIORef posx (+ length str * 13)
				putStr str
				utf8DrawString dpy win fs gc x y str
			_ -> return ()
	return False

runWithXIM ::
	( Display -> Window -> GC -> XIM -> XIC -> Atom -> FontSet -> XEventPtr
			-> IO () )
			-> IO ()
runWithXIM act = do
	setLocaleAndCheck
	supportsLocaleAndCheck
	_ <- setLocaleModifiers ""
	dpy	<-openDisplay ""
	let	scr	=  defaultScreen dpy
		black	=  blackPixel dpy scr
		white	=  whitePixel dpy scr
	rootWin		<- rootWindow dpy scr
	win		<- createSimpleWindow dpy rootWin 0 0 100 100 1
				black white
	gc		<- createGC dpy win
	im		<- openIM dpy Nothing Nothing Nothing
	ic		<- createIC im [ XIMPreeditNothing, XIMStatusNothing ] win
	fevent		<- getICValue ic "filterEvents"
	selectInput dpy win $ exposureMask .|. keyPressMask .|.
			focusChangeMask .|. fevent
	delWin		<- internAtom dpy "WM_DELETE_WINDOW" True
	setWMProtocols dpy win [ delWin ]
	mapWindow dpy win
	( _, _, fs )	<- createFontSet dpy
				"-*-fixed-medium-r-normal--14-*-*-*"
	allocaXEvent $ \e -> act dpy win gc im ic delWin fs e
	destroyIC ic
	closeIM im
	freeGC dpy gc
	destroyWindow dpy win
	closeDisplay dpy

nextNotFilteredEvent :: Display -> XEventPtr -> IO ()
nextNotFilteredEvent dpy e = do
	nextEvent dpy e
	filtOut <- filterEvent e 0
	if filtOut
		then nextNotFilteredEvent dpy e
		else return ()

main :: IO ()
main = runWithXIM $ \dpy win gc im ic delWin fs e -> do
	setICFocus ic
	posx	<- newIORef 5
	posy	<- newIORef 20
	doUntil_ $ do
		nextNotFilteredEvent dpy e
		ev <- getEvent e
		case ev of
			KeyEvent {} -> keyEventAction dpy win fs gc ic posx posy e
			ClientMessageEvent {} ->
				return $ getClientMessageAtom ev == delWin
			_                     -> return False

getClientMessageAtom :: Convertible CInt a => Event -> a
getClientMessageAtom = convert . head . ev_data
