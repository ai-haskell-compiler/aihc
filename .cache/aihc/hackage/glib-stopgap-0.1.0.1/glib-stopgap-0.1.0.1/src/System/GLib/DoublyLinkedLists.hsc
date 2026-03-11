{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.GLib.DoublyLinkedLists (
	-- * TYPE
	GList,

	-- * LIST
	g_list_to_list, g_list_to_prev_next_lists,

	-- * POINTER
	g_list_prev_data_next, g_list_data, g_list_prev, g_list_next,

	-- * FREE
	c_g_list_free ) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.Storable

#include <glib.h>

data GList a

g_list_to_list :: Ptr (GList a) -> IO (Maybe [Ptr a])
g_list_to_list = ((uncurry appendReverse <$>) <$>) . g_list_to_prev_next_lists

appendReverse :: [a] -> [a] -> [a]
[] `appendReverse` ys = ys
(x : xs) `appendReverse` ys = xs `appendReverse` (x : ys)

g_list_to_prev_next_lists :: Ptr (GList a) -> IO (Maybe ([Ptr a], [Ptr a]))
g_list_to_prev_next_lists = \case
	NullPtr -> pure Nothing
	p -> Just <$> (
		(,)	<$> (g_list_to_prev_list =<< #{peek GList, prev} p)
			<*> g_list_to_next_list p )

g_list_to_prev_list :: Ptr (GList a) -> IO [Ptr a]
g_list_to_prev_list = \case
	NullPtr -> pure []
	p -> (:)
		<$> #{peek GList, data} p
		<*> (g_list_to_prev_list =<< #{peek GList, prev} p)

g_list_to_next_list :: Ptr (GList a) -> IO [Ptr a]
g_list_to_next_list = \case
	NullPtr -> pure []
	p -> (:)
		<$> #{peek GList, data} p
		<*> (g_list_to_next_list =<< #{peek GList, next} p)

g_list_prev_data_next ::
	Ptr (GList a) -> IO (Maybe (Ptr (GList a), Ptr a, Ptr (GList a)))
g_list_prev_data_next = \case
	NullPtr -> pure Nothing
	p -> (Just <$>) $ (,,)
		<$> #{peek GList, prev} p
		<*> #{peek GList, data} p
		<*> #{peek GList, next} p

g_list_data :: Ptr (GList a) -> IO (Maybe (Ptr a))
g_list_data = \case NullPtr -> pure Nothing; p -> Just <$> #{peek GList, data} p

g_list_prev, g_list_next :: Ptr (GList a) -> IO (Maybe (Ptr (GList a)))
g_list_prev = \case NullPtr -> pure Nothing; p -> Just <$> #{peek GList, prev} p
g_list_next = \case NullPtr -> pure Nothing; p -> Just <$> #{peek GList, next} p

foreign import ccall "g_list_free" c_g_list_free :: Ptr (GList a) -> IO ()
