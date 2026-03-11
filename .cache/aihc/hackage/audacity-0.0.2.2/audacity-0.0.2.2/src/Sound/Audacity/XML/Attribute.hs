module Sound.Audacity.XML.Attribute where

import qualified Sound.Audacity.XML as XML

import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name.MixedCase as Name


string :: String -> (a -> String) -> Attr.T Name.T (a -> String)
string  =  XML.attr

bool :: String -> (a -> Bool) -> Attr.T Name.T (a -> String)
bool = enum

enum :: (Enum b) => String -> (a -> b) -> Attr.T Name.T (a -> String)
enum name value  =  XML.attr name (show . fromEnum . value)

int :: String -> (a -> Int) -> Attr.T Name.T (a -> String)
int name value  =  XML.attr name (show . value)

float :: String -> (a -> Float) -> Attr.T Name.T (a -> String)
float name value  =  XML.attr name (show . value)

double :: String -> (a -> Double) -> Attr.T Name.T (a -> String)
double name value  =  XML.attr name (show . value)
