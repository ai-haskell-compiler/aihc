


#include <X11/Xlib.h>
#include <X11/keysym.h>

module Graphics.X11.XimTypes (

  XNInputStyle(..)
, getCXNInputStyle

, XLookupStatus(..)
, fromCXLookupStatus

) where

import Data.Bits	( (.|.)		)
import Foreign		( Int32, Int64	)

data XNInputStyle =
	XIMPreeditArea	|
	XIMPreeditCallbacks	|
	XIMPreeditPosition	|
	XIMPreeditNothing	|
	XIMPreeditNone	|
	XIMStatusArea	|
	XIMStatusCallbacks	|
	XIMStatusNothing	|
	XIMStatusNone

getCXNInputStyle1 :: XNInputStyle -> #type long

getCXNInputStyle1 XIMPreeditArea	= #const XIMPreeditArea
getCXNInputStyle1 XIMPreeditCallbacks	= #const XIMPreeditCallbacks
getCXNInputStyle1 XIMPreeditPosition	= #const XIMPreeditPosition
getCXNInputStyle1 XIMPreeditNothing	= #const XIMPreeditNothing
getCXNInputStyle1 XIMPreeditNone	= #const XIMPreeditNone
getCXNInputStyle1 XIMStatusArea	= #const XIMStatusArea
getCXNInputStyle1 XIMStatusCallbacks	= #const XIMStatusCallbacks
getCXNInputStyle1 XIMStatusNothing	= #const XIMStatusNothing
getCXNInputStyle1 XIMStatusNone	= #const XIMStatusNone


getCXNInputStyle :: [ XNInputStyle ] -> #{ type long }
getCXNInputStyle = foldl1 (.|.) . map getCXNInputStyle1

data XLookupStatus =
	XBufferOverflow	|
	XLookupNone	|
	XLookupChars	|
	XLookupKeySym	|
	XLookupBoth
fromCXLookupStatus :: #{ type Status } -> XLookupStatus

fromCXLookupStatus ( #const XBufferOverflow	) = XBufferOverflow
fromCXLookupStatus ( #const XLookupNone	) = XLookupNone
fromCXLookupStatus ( #const XLookupChars	) = XLookupChars
fromCXLookupStatus ( #const XLookupKeySym	) = XLookupKeySym
fromCXLookupStatus ( #const XLookupBoth	) = XLookupBoth

fromCXLookupStatus _  = error "bad Status"
