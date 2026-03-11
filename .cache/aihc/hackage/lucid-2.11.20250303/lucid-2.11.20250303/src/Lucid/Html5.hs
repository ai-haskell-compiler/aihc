{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-type-defaults #-}

-- | Html5 terms.

module Lucid.Html5 where

import           Lucid.Base

import           Data.Monoid
import           Data.Text (Text, unwords)

-------------------------------------------------------------------------------
-- Elements

-- | @DOCTYPE@ element
--
-- This is implemented as "raw output", because the doctype doesn't
-- accept attributes, such as those inserted via 'with'.
--
doctype_ :: Applicative m => HtmlT m ()
doctype_ = HtmlT (pure (const "<!DOCTYPE HTML>", ()))

-- | @DOCTYPE@ element + @html@ element
doctypehtml_ :: Applicative m => HtmlT m a -> HtmlT m a
doctypehtml_ m = doctype_ *> html_ m

-- | @a@ element
a_ :: Term arg result => arg -> result
a_ = term "a"

-- | @abbr@ element
abbr_ :: Term arg result => arg -> result
abbr_ = term "abbr"

-- | @address@ element
address_ :: Term arg result => arg -> result
address_ = term "address"

-- | @area@ element
area_ :: Applicative m => [Attribute] -> HtmlT m ()
area_ = with (makeElementNoEnd "area")

-- | @article@ element
article_ :: Term arg result => arg -> result
article_ = term "article"

-- | @aside@ element
aside_ :: Term arg result => arg -> result
aside_ = term "aside"

-- | @audio@ element
audio_ :: Term arg result => arg -> result
audio_ = term "audio"

-- | @b@ element
b_ :: Term arg result => arg -> result
b_ = term "b"

-- | @base@ element
base_ :: Applicative m => [Attribute] -> HtmlT m ()
base_ = with (makeElementNoEnd "base")

-- | @bdo@ element
bdo_ :: Term arg result => arg -> result
bdo_ = term "bdo"

-- | @blockquote@ element
blockquote_ :: Term arg result => arg -> result
blockquote_ = term "blockquote"

-- | @body@ element
body_ :: Term arg result => arg -> result
body_ = term "body"

-- | @br@ element
br_ :: Applicative m => [Attribute] -> HtmlT m ()
br_ = with (makeElementNoEnd "br")

-- | @button@ element
button_ :: Term arg result => arg -> result
button_ = term "button"

-- | @canvas@ element
canvas_ :: Term arg result => arg -> result
canvas_ = term "canvas"

-- | @caption@ element
caption_ :: Term arg result => arg -> result
caption_ = term "caption"

-- | @cite@ element or @cite@ attribute.
cite_ :: Term arg result => arg -> result
cite_ = term "cite"

-- | @code@ element
code_ :: Term arg result => arg -> result
code_ = term "code"

-- | @col@ element
col_ :: Applicative m => [Attribute] -> HtmlT m ()
col_ = with (makeElementNoEnd "col")

-- | @colgroup@ element
colgroup_ :: Term arg result => arg -> result
colgroup_ = term "colgroup"

-- | @command@ element
command_ :: Term arg result => arg -> result
command_ = term "command"

-- | @datalist@ element
datalist_ :: Term arg result => arg -> result
datalist_ = term "datalist"

-- | @dd@ element
dd_ :: Term arg result => arg -> result
dd_ = term "dd"

-- | @del@ element
del_ :: Term arg result => arg -> result
del_ = term "del"

-- | @details@ element
details_ :: Term arg result => arg -> result
details_ = term "details"

-- | @dfn@ element
dfn_ :: Term arg result => arg -> result
dfn_ = term "dfn"

-- | @div@ element
div_ :: Term arg result => arg -> result
div_ = term "div"

-- | @dl@ element
dl_ :: Term arg result => arg -> result
dl_ = term "dl"

-- | @dt@ element
dt_ :: Term arg result => arg -> result
dt_ = term "dt"

-- | @em@ element
em_ :: Term arg result => arg -> result
em_ = term "em"

-- | @embed@ element
embed_ :: Applicative m => [Attribute] -> HtmlT m ()
embed_ = with (makeElementNoEnd "embed")

-- | @fieldset@ element
fieldset_ :: Term arg result => arg -> result
fieldset_ = term "fieldset"

-- | @figcaption@ element
figcaption_ :: Term arg result => arg -> result
figcaption_ = term "figcaption"

-- | @figure@ element
figure_ :: Term arg result => arg -> result
figure_ = term "figure"

-- | @footer@ element
footer_ :: Term arg result => arg -> result
footer_ = term "footer"

-- | @form@ element or @form@ attribute
form_ :: Term arg result => arg -> result
form_ = term "form"

-- | @h1@ element
h1_ :: Term arg result => arg -> result
h1_ = term "h1"

-- | @h2@ element
h2_ :: Term arg result => arg -> result
h2_ = term "h2"

-- | @h3@ element
h3_ :: Term arg result => arg -> result
h3_ = term "h3"

-- | @h4@ element
h4_ :: Term arg result => arg -> result
h4_ = term "h4"

-- | @h5@ element
h5_ :: Term arg result => arg -> result
h5_ = term "h5"

-- | @h6@ element
h6_ :: Term arg result => arg -> result
h6_ = term "h6"

-- | @head@ element
head_ :: Term arg result => arg -> result
head_ = term "head"

-- | @header@ element
header_ :: Term arg result => arg -> result
header_ = term "header"

-- | @hgroup@ element
hgroup_ :: Term arg result => arg -> result
hgroup_ = term "hgroup"

-- | @hr@ element
hr_ :: Applicative m => [Attribute] -> HtmlT m ()
hr_ = with (makeElementNoEnd "hr")

-- | @html@ element
html_ :: Term arg result => arg -> result
html_ = term "html"

-- | @i@ element
i_ :: Term arg result => arg -> result
i_ = term "i"

-- | @iframe@ element
iframe_ :: Term arg result => arg -> result
iframe_ = term "iframe"

-- | @img@ element
img_ :: Applicative m => [Attribute] -> HtmlT m ()
img_ = with (makeElementNoEnd "img")

-- | @input@ element
input_ :: Applicative m => [Attribute] -> HtmlT m ()
input_ = with (makeElementNoEnd "input")

-- | @ins@ element
ins_ :: Term arg result => arg -> result
ins_ = term "ins"

-- | @kbd@ element
kbd_ :: Term arg result => arg -> result
kbd_ = term "kbd"

-- | @keygen@ element
keygen_ :: Applicative m => [Attribute] -> HtmlT m ()
keygen_ = with (makeElementNoEnd "keygen")

-- | @label@ element or @label@ attribute
label_ :: Term arg result => arg -> result
label_ = term "label"

-- | @legend@ element
legend_ :: Term arg result => arg -> result
legend_ = term "legend"

-- | @li@ element
li_ :: Term arg result => arg -> result
li_ = term "li"

-- | @link@ element
link_ :: Applicative m => [Attribute] -> HtmlT m ()
link_ = with (makeElementNoEnd "link")

-- | @map@ element
map_ :: Term arg result => arg -> result
map_ = term "map"

-- | @main@ element
main_ :: Term arg result => arg -> result
main_ = term "main"

-- | @mark@ element
mark_ :: Term arg result => arg -> result
mark_ = term "mark"

-- | @menu@ element
menu_ :: Term arg result => arg -> result
menu_ = term "menu"

-- | @menuitem@ element
menuitem_ :: Applicative m => [Attribute] -> HtmlT m ()
menuitem_ = with (makeElementNoEnd "menuitem")

-- | @meta@ element
meta_ :: Applicative m => [Attribute] -> HtmlT m ()
meta_ = with (makeElementNoEnd "meta")

-- | @meter@ element
meter_ :: Term arg result => arg -> result
meter_ = term "meter"

-- | @nav@ element
nav_ :: Term arg result => arg -> result
nav_ = term "nav"

-- | @noscript@ element
noscript_ :: Term arg result => arg -> result
noscript_ = term "noscript"

-- | @object@ element
object_ :: Term arg result => arg -> result
object_ = term "object"

-- | @ol@ element
ol_ :: Term arg result => arg -> result
ol_ = term "ol"

-- | @optgroup@ element
optgroup_ :: Term arg result => arg -> result
optgroup_ = term "optgroup"

-- | @option@ element
option_ :: Term arg result => arg -> result
option_ = term "option"

-- | @output@ element
output_ :: Term arg result => arg -> result
output_ = term "output"

-- | @p@ element
p_ :: Term arg result => arg -> result
p_ = term "p"

-- | @param@ element
param_ :: Applicative m => [Attribute] -> HtmlT m ()
param_ = with (makeElementNoEnd "param")

-- | The @svg@ attribute.
svg_ :: Term arg result => arg -> result
svg_ = term "svg"

-- | @pre@ element
pre_ :: Term arg result => arg -> result
pre_ = term "pre"

-- | @progress@ element
progress_ :: Term arg result => arg -> result
progress_ = term "progress"

-- | @q@ element
q_ :: Term arg result => arg -> result
q_ = term "q"

-- | @rp@ element
rp_ :: Term arg result => arg -> result
rp_ = term "rp"

-- | @rt@ element
rt_ :: Term arg result => arg -> result
rt_ = term "rt"

-- | @ruby@ element
ruby_ :: Term arg result => arg -> result
ruby_ = term "ruby"

-- | @samp@ element
samp_ :: Term arg result => arg -> result
samp_ = term "samp"

-- | @script@ element
script_ :: TermRaw arg result => arg -> result
script_ = termRaw "script"

-- | @section@ element
section_ :: Term arg result => arg -> result
section_ = term "section"

-- | @select@ element
select_ :: Term arg result => arg -> result
select_ = term "select"

-- | @small@ element
small_ :: Term arg result => arg -> result
small_ = term "small"

-- | @source@ element
source_ :: Applicative m => [Attribute] -> HtmlT m ()
source_ = with (makeElementNoEnd "source")

-- | @span@ element or @span@ attribute
span_ :: Term arg result => arg -> result
span_ = term "span"

-- | @strong@ element
strong_ :: Term arg result => arg -> result
strong_ = term "strong"

-- | @style@ element or @style@ attribute
style_ :: TermRaw arg result => arg -> result
style_ = termRaw "style"

-- | @sub@ element
sub_ :: Term arg result => arg -> result
sub_ = term "sub"

-- | @summary@ element or @summary@ attribute
summary_ :: Term arg result => arg -> result
summary_ = term "summary"

-- | @sup@ element
sup_ :: Term arg result => arg -> result
sup_ = term "sup"

-- | @table@ element
table_ :: Term arg result => arg -> result
table_ = term "table"

-- | @tbody@ element
tbody_ :: Term arg result => arg -> result
tbody_ = term "tbody"

-- | @td@ element
td_ :: Term arg result => arg -> result
td_ = term "td"

-- | @textarea@ element
textarea_ :: Term arg result => arg -> result
textarea_ = term "textarea"

-- | @tfoot@ element
tfoot_ :: Term arg result => arg -> result
tfoot_ = term "tfoot"

-- | @th@ element
th_ :: Term arg result => arg -> result
th_ = term "th"

-- | @template@ element
template_ :: Term arg result => arg -> result
template_ = term "template"

-- | @thead@ element
thead_ :: Term arg result => arg -> result
thead_ = term "thead"

-- | @time@ element
time_ :: Term arg result => arg -> result
time_ = term "time"

-- | @title@ element or @title@ attribute
title_ :: Term arg result => arg -> result
title_ = term "title"

-- | @tr@ element
tr_ :: Term arg result => arg -> result
tr_ = term "tr"

-- | @track@ element
track_ :: Applicative m => [Attribute] -> HtmlT m ()
track_ = with (makeElementNoEnd "track")

-- | @ul@ element
ul_ :: Term arg result => arg -> result
ul_ = term "ul"

-- | @var@ element
var_ :: Term arg result => arg -> result
var_ = term "var"

-- | @video@ element
video_ :: Term arg result => arg -> result
video_ = term "video"

-- | @wbr@ element
wbr_ :: Applicative m => [Attribute] -> HtmlT m ()
wbr_ = with (makeElementNoEnd "wbr")

-------------------------------------------------------------------------------
-- Attributes

-- | The @accept@ attribute.
accept_ :: Text -> Attribute
accept_ = makeAttribute "accept"

-- | The @acceptCharset@ attribute.
acceptCharset_ :: Text -> Attribute
acceptCharset_ = makeAttribute "accept-charset"

-- | The @accesskey@ attribute.
accesskey_ :: Text -> Attribute
accesskey_ = makeAttribute "accesskey"

-- | The @action@ attribute.
action_ :: Text -> Attribute
action_ = makeAttribute "action"

-- | The @alt@ attribute.
alt_ :: Text -> Attribute
alt_ = makeAttribute "alt"

-- | The @async@ attribute.
async_ :: Text -> Attribute
async_ = makeAttribute "async"

-- | The @autocomplete@ attribute.
autocomplete_ :: Text -> Attribute
autocomplete_ = makeAttribute "autocomplete"

-- | The @autofocus@ attribute.
autofocus_ :: Attribute
autofocus_ = makeAttribute "autofocus" mempty

-- | The @autoplay@ attribute.
autoplay_ :: Text -> Attribute
autoplay_ = makeAttribute "autoplay"

-- | The @challenge@ attribute.
challenge_ :: Text -> Attribute
challenge_ = makeAttribute "challenge"

-- | The @charset@ attribute.
charset_ :: Text -> Attribute
charset_ = makeAttribute "charset"

-- | The @checked@ attribute.
checked_ :: Attribute
checked_ = makeAttribute "checked" mempty

-- | The @class@ attribute.
class_ :: Text -> Attribute
class_ = makeAttribute "class"

-- | Smart constructor for @class@ attribute.
--
-- @since 2.9.8
classes_ :: [Text] -> Attribute
classes_ = makeAttribute "class" . Data.Text.unwords

-- | The @cols@ attribute.
cols_ :: Text -> Attribute
cols_ = makeAttribute "cols"

-- | The @colspan@ attribute.
colspan_ :: Text -> Attribute
colspan_ = makeAttribute "colspan"

-- | The @content@ attribute.
content_ :: Text -> Attribute
content_ = makeAttribute "content"

-- | The @contenteditable@ attribute.
contenteditable_ :: Text -> Attribute
contenteditable_ = makeAttribute "contenteditable"

-- | The @contextmenu@ attribute.
contextmenu_ :: Text -> Attribute
contextmenu_ = makeAttribute "contextmenu"

-- | The @controls@ attribute.
controls_ :: Text -> Attribute
controls_ = makeAttribute "controls"

-- | The @coords@ attribute.
coords_ :: Text -> Attribute
coords_ = makeAttribute "coords"

-- | The @crossorigin@ attribute.
--
-- @since 2.9.8
crossorigin_ :: Text -> Attribute
crossorigin_ = makeAttribute "crossorigin"

-- | The @data@ attribute.
data_ :: Text -> Text -> Attribute
data_ name = makeAttribute ("data-" <> name)

-- | The @datetime@ attribute.
datetime_ :: Text -> Attribute
datetime_ = makeAttribute "datetime"

-- | The @defer@ attribute.
defer_ :: Text -> Attribute
defer_ = makeAttribute "defer"

-- | The @dir@ attribute.
dir_ :: Text -> Attribute
dir_ = makeAttribute "dir"

-- | The @disabled@ attribute.
disabled_ :: Text -> Attribute
disabled_ = makeAttribute "disabled"

-- | The @download@ attribute.
download_ :: Text -> Attribute
download_ = makeAttribute "download"

-- | The @draggable@ attribute.
draggable_ :: Text -> Attribute
draggable_ = makeAttribute "draggable"

-- | The @enctype@ attribute.
enctype_ :: Text -> Attribute
enctype_ = makeAttribute "enctype"

-- | The @for@ attribute.
for_ :: Text -> Attribute
for_ = makeAttribute "for"

-- | The @formaction@ attribute.
formaction_ :: Text -> Attribute
formaction_ = makeAttribute "formaction"

-- | The @formenctype@ attribute.
formenctype_ :: Text -> Attribute
formenctype_ = makeAttribute "formenctype"

-- | The @formmethod@ attribute.
formmethod_ :: Text -> Attribute
formmethod_ = makeAttribute "formmethod"

-- | The @formnovalidate@ attribute.
formnovalidate_ :: Text -> Attribute
formnovalidate_ = makeAttribute "formnovalidate"

-- | The @formtarget@ attribute.
formtarget_ :: Text -> Attribute
formtarget_ = makeAttribute "formtarget"

-- | The @headers@ attribute.
headers_ :: Text -> Attribute
headers_ = makeAttribute "headers"

-- | The @height@ attribute.
height_ :: Text -> Attribute
height_ = makeAttribute "height"

-- | The @hidden@ attribute.
hidden_ :: Text -> Attribute
hidden_ = makeAttribute "hidden"

-- | The @high@ attribute.
high_ :: Text -> Attribute
high_ = makeAttribute "high"

-- | The @href@ attribute.
href_ :: Text -> Attribute
href_ = makeAttribute "href"

-- | The @hreflang@ attribute.
hreflang_ :: Text -> Attribute
hreflang_ = makeAttribute "hreflang"

-- | The @httpEquiv@ attribute.
httpEquiv_ :: Text -> Attribute
httpEquiv_ = makeAttribute "http-equiv"

-- | The @icon@ attribute.
icon_ :: Text -> Attribute
icon_ = makeAttribute "icon"

-- | The @id@ attribute.
id_ :: Text -> Attribute
id_ = makeAttribute "id"

-- | The @integrity@ attribute.
--
-- @since 2.9.8
integrity_ :: Text -> Attribute
integrity_ = makeAttribute "integrity"

-- | The @ismap@ attribute.
ismap_ :: Text -> Attribute
ismap_ = makeAttribute "ismap"

-- | The @item@ attribute.
item_ :: Text -> Attribute
item_ = makeAttribute "item"

-- | The @itemprop@ attribute.
itemprop_ :: Text -> Attribute
itemprop_ = makeAttribute "itemprop"

-- | The @keytype@ attribute.
keytype_ :: Text -> Attribute
keytype_ = makeAttribute "keytype"

-- | The @lang@ attribute.
lang_ :: Text -> Attribute
lang_ = makeAttribute "lang"

-- | The @list@ attribute.
list_ :: Text -> Attribute
list_ = makeAttribute "list"

-- | The @loading@ attribute.
loading_ :: Text -> Attribute
loading_ = makeAttribute "loading"

-- | The @loop@ attribute.
loop_ :: Text -> Attribute
loop_ = makeAttribute "loop"

-- | The @low@ attribute.
low_ :: Text -> Attribute
low_ = makeAttribute "low"

-- | The @manifest@ attribute.
manifest_ :: Text -> Attribute
manifest_ = makeAttribute "manifest"

-- | The @max@ attribute.
max_ :: Text -> Attribute
max_ = makeAttribute "max"

-- | The @maxlength@ attribute.
maxlength_ :: Text -> Attribute
maxlength_ = makeAttribute "maxlength"

-- | The @media@ attribute.
media_ :: Text -> Attribute
media_ = makeAttribute "media"

-- | The @method@ attribute.
method_ :: Text -> Attribute
method_ = makeAttribute "method"

-- | The @min@ attribute.
min_ :: Text -> Attribute
min_ = makeAttribute "min"

-- | The @minlength@ attribute.
minlength_ :: Text -> Attribute
minlength_ = makeAttribute "minlength"

-- | The @multiple@ attribute.
multiple_ :: Text -> Attribute
multiple_ = makeAttribute "multiple"

-- | The @name@ attribute.
name_ :: Text -> Attribute
name_ = makeAttribute "name"

-- | The @novalidate@ attribute.
novalidate_ :: Text -> Attribute
novalidate_ = makeAttribute "novalidate"

-- | The @onbeforeonload@ attribute.
onbeforeonload_ :: Text -> Attribute
onbeforeonload_ = makeAttribute "onbeforeonload"

-- | The @onbeforeprint@ attribute.
onbeforeprint_ :: Text -> Attribute
onbeforeprint_ = makeAttribute "onbeforeprint"

-- | The @onblur@ attribute.
onblur_ :: Text -> Attribute
onblur_ = makeAttribute "onblur"

-- | The @oncanplay@ attribute.
oncanplay_ :: Text -> Attribute
oncanplay_ = makeAttribute "oncanplay"

-- | The @oncanplaythrough@ attribute.
oncanplaythrough_ :: Text -> Attribute
oncanplaythrough_ = makeAttribute "oncanplaythrough"

-- | The @onchange@ attribute.
onchange_ :: Text -> Attribute
onchange_ = makeAttribute "onchange"

-- | The @onclick@ attribute.
onclick_ :: Text -> Attribute
onclick_ = makeAttribute "onclick"

-- | The @oncontextmenu@ attribute.
oncontextmenu_ :: Text -> Attribute
oncontextmenu_ = makeAttribute "oncontextmenu"

-- | The @ondblclick@ attribute.
ondblclick_ :: Text -> Attribute
ondblclick_ = makeAttribute "ondblclick"

-- | The @ondrag@ attribute.
ondrag_ :: Text -> Attribute
ondrag_ = makeAttribute "ondrag"

-- | The @ondragend@ attribute.
ondragend_ :: Text -> Attribute
ondragend_ = makeAttribute "ondragend"

-- | The @ondragenter@ attribute.
ondragenter_ :: Text -> Attribute
ondragenter_ = makeAttribute "ondragenter"

-- | The @ondragleave@ attribute.
ondragleave_ :: Text -> Attribute
ondragleave_ = makeAttribute "ondragleave"

-- | The @ondragover@ attribute.
ondragover_ :: Text -> Attribute
ondragover_ = makeAttribute "ondragover"

-- | The @ondragstart@ attribute.
ondragstart_ :: Text -> Attribute
ondragstart_ = makeAttribute "ondragstart"

-- | The @ondrop@ attribute.
ondrop_ :: Text -> Attribute
ondrop_ = makeAttribute "ondrop"

-- | The @ondurationchange@ attribute.
ondurationchange_ :: Text -> Attribute
ondurationchange_ = makeAttribute "ondurationchange"

-- | The @onemptied@ attribute.
onemptied_ :: Text -> Attribute
onemptied_ = makeAttribute "onemptied"

-- | The @onended@ attribute.
onended_ :: Text -> Attribute
onended_ = makeAttribute "onended"

-- | The @onerror@ attribute.
onerror_ :: Text -> Attribute
onerror_ = makeAttribute "onerror"

-- | The @onfocus@ attribute.
onfocus_ :: Text -> Attribute
onfocus_ = makeAttribute "onfocus"

-- | The @onformchange@ attribute.
onformchange_ :: Text -> Attribute
onformchange_ = makeAttribute "onformchange"

-- | The @onforminput@ attribute.
onforminput_ :: Text -> Attribute
onforminput_ = makeAttribute "onforminput"

-- | The @onhaschange@ attribute.
onhaschange_ :: Text -> Attribute
onhaschange_ = makeAttribute "onhaschange"

-- | The @oninput@ attribute.
oninput_ :: Text -> Attribute
oninput_ = makeAttribute "oninput"

-- | The @oninvalid@ attribute.
oninvalid_ :: Text -> Attribute
oninvalid_ = makeAttribute "oninvalid"

-- | The @onkeydown@ attribute.
onkeydown_ :: Text -> Attribute
onkeydown_ = makeAttribute "onkeydown"

-- | The @onkeyup@ attribute.
onkeyup_ :: Text -> Attribute
onkeyup_ = makeAttribute "onkeyup"

-- | The @onload@ attribute.
onload_ :: Text -> Attribute
onload_ = makeAttribute "onload"

-- | The @onloadeddata@ attribute.
onloadeddata_ :: Text -> Attribute
onloadeddata_ = makeAttribute "onloadeddata"

-- | The @onloadedmetadata@ attribute.
onloadedmetadata_ :: Text -> Attribute
onloadedmetadata_ = makeAttribute "onloadedmetadata"

-- | The @onloadstart@ attribute.
onloadstart_ :: Text -> Attribute
onloadstart_ = makeAttribute "onloadstart"

-- | The @onmessage@ attribute.
onmessage_ :: Text -> Attribute
onmessage_ = makeAttribute "onmessage"

-- | The @onmousedown@ attribute.
onmousedown_ :: Text -> Attribute
onmousedown_ = makeAttribute "onmousedown"

-- | The @onmousemove@ attribute.
onmousemove_ :: Text -> Attribute
onmousemove_ = makeAttribute "onmousemove"

-- | The @onmouseout@ attribute.
onmouseout_ :: Text -> Attribute
onmouseout_ = makeAttribute "onmouseout"

-- | The @onmouseover@ attribute.
onmouseover_ :: Text -> Attribute
onmouseover_ = makeAttribute "onmouseover"

-- | The @onmouseup@ attribute.
onmouseup_ :: Text -> Attribute
onmouseup_ = makeAttribute "onmouseup"

-- | The @onmousewheel@ attribute.
onmousewheel_ :: Text -> Attribute
onmousewheel_ = makeAttribute "onmousewheel"

-- | The @ononline@ attribute.
ononline_ :: Text -> Attribute
ononline_ = makeAttribute "ononline"

-- | The @onpagehide@ attribute.
onpagehide_ :: Text -> Attribute
onpagehide_ = makeAttribute "onpagehide"

-- | The @onpageshow@ attribute.
onpageshow_ :: Text -> Attribute
onpageshow_ = makeAttribute "onpageshow"

-- | The @onpause@ attribute.
onpause_ :: Text -> Attribute
onpause_ = makeAttribute "onpause"

-- | The @onplay@ attribute.
onplay_ :: Text -> Attribute
onplay_ = makeAttribute "onplay"

-- | The @onplaying@ attribute.
onplaying_ :: Text -> Attribute
onplaying_ = makeAttribute "onplaying"

-- | The @onprogress@ attribute.
onprogress_ :: Text -> Attribute
onprogress_ = makeAttribute "onprogress"

-- | The @onpropstate@ attribute.
onpropstate_ :: Text -> Attribute
onpropstate_ = makeAttribute "onpropstate"

-- | The @onratechange@ attribute.
onratechange_ :: Text -> Attribute
onratechange_ = makeAttribute "onratechange"

-- | The @onreadystatechange@ attribute.
onreadystatechange_ :: Text -> Attribute
onreadystatechange_ = makeAttribute "onreadystatechange"

-- | The @onredo@ attribute.
onredo_ :: Text -> Attribute
onredo_ = makeAttribute "onredo"

-- | The @onresize@ attribute.
onresize_ :: Text -> Attribute
onresize_ = makeAttribute "onresize"

-- | The @onscroll@ attribute.
onscroll_ :: Text -> Attribute
onscroll_ = makeAttribute "onscroll"

-- | The @onseeked@ attribute.
onseeked_ :: Text -> Attribute
onseeked_ = makeAttribute "onseeked"

-- | The @onseeking@ attribute.
onseeking_ :: Text -> Attribute
onseeking_ = makeAttribute "onseeking"

-- | The @onselect@ attribute.
onselect_ :: Text -> Attribute
onselect_ = makeAttribute "onselect"

-- | The @onstalled@ attribute.
onstalled_ :: Text -> Attribute
onstalled_ = makeAttribute "onstalled"

-- | The @onstorage@ attribute.
onstorage_ :: Text -> Attribute
onstorage_ = makeAttribute "onstorage"

-- | The @onsubmit@ attribute.
onsubmit_ :: Text -> Attribute
onsubmit_ = makeAttribute "onsubmit"

-- | The @onsuspend@ attribute.
onsuspend_ :: Text -> Attribute
onsuspend_ = makeAttribute "onsuspend"

-- | The @ontimeupdate@ attribute.
ontimeupdate_ :: Text -> Attribute
ontimeupdate_ = makeAttribute "ontimeupdate"

-- | The @onundo@ attribute.
onundo_ :: Text -> Attribute
onundo_ = makeAttribute "onundo"

-- | The @onunload@ attribute.
onunload_ :: Text -> Attribute
onunload_ = makeAttribute "onunload"

-- | The @onvolumechange@ attribute.
onvolumechange_ :: Text -> Attribute
onvolumechange_ = makeAttribute "onvolumechange"

-- | The @onwaiting@ attribute.
onwaiting_ :: Text -> Attribute
onwaiting_ = makeAttribute "onwaiting"

-- | The @open@ attribute.
open_ :: Text -> Attribute
open_ = makeAttribute "open"

-- | The @optimum@ attribute.
optimum_ :: Text -> Attribute
optimum_ = makeAttribute "optimum"

-- | The @pattern@ attribute.
pattern_ :: Text -> Attribute
pattern_ = makeAttribute "pattern"

-- | The @ping@ attribute.
ping_ :: Text -> Attribute
ping_ = makeAttribute "ping"

-- | The @placeholder@ attribute.
placeholder_ :: Text -> Attribute
placeholder_ = makeAttribute "placeholder"

-- | The @poster@ attribute.
poster_ :: Text -> Attribute
poster_ = makeAttribute "poster"

-- | The @preload@ attribute.
preload_ :: Text -> Attribute
preload_ = makeAttribute "preload"

-- | The @pubdate@ attribute.
pubdate_ :: Text -> Attribute
pubdate_ = makeAttribute "pubdate"

-- | The @radiogroup@ attribute.
radiogroup_ :: Text -> Attribute
radiogroup_ = makeAttribute "radiogroup"

-- | The @readonly@ attribute.
readonly_ :: Text -> Attribute
readonly_ = makeAttribute "readonly"

-- | The @rel@ attribute.
rel_ :: Text -> Attribute
rel_ = makeAttribute "rel"

-- | The @required@ attribute.
required_ :: Text -> Attribute
required_ = makeAttribute "required"

-- | The @reversed@ attribute.
reversed_ :: Text -> Attribute
reversed_ = makeAttribute "reversed"

-- | The @role@ attribute.
role_ :: Text -> Attribute
role_ = makeAttribute "role"

-- | The @rows@ attribute.
rows_ :: Text -> Attribute
rows_ = makeAttribute "rows"

-- | The @rowspan@ attribute.
rowspan_ :: Text -> Attribute
rowspan_ = makeAttribute "rowspan"

-- | The @sandbox@ attribute.
sandbox_ :: Text -> Attribute
sandbox_ = makeAttribute "sandbox"

-- | The @scope@ attribute.
scope_ :: Text -> Attribute
scope_ = makeAttribute "scope"

-- | The @scoped@ attribute.
scoped_ :: Text -> Attribute
scoped_ = makeAttribute "scoped"

-- | The @seamless@ attribute.
seamless_ :: Text -> Attribute
seamless_ = makeAttribute "seamless"

-- | The @selected@ attribute.
selected_ :: Text -> Attribute
selected_ = makeAttribute "selected"

-- | The @shape@ attribute.
shape_ :: Text -> Attribute
shape_ = makeAttribute "shape"

-- | The @size@ attribute.
size_ :: Text -> Attribute
size_ = makeAttribute "size"

-- | The @sizes@ attribute.
sizes_ :: Text -> Attribute
sizes_ = makeAttribute "sizes"

-- | The @spellcheck@ attribute.
spellcheck_ :: Text -> Attribute
spellcheck_ = makeAttribute "spellcheck"

-- | The @src@ attribute.
src_ :: Text -> Attribute
src_ = makeAttribute "src"

-- | The @srcdoc@ attribute.
srcdoc_ :: Text -> Attribute
srcdoc_ = makeAttribute "srcdoc"

-- | The @start@ attribute.
start_ :: Text -> Attribute
start_ = makeAttribute "start"

-- | The @step@ attribute.
step_ :: Text -> Attribute
step_ = makeAttribute "step"

-- | The @subject@ attribute.
subject_ :: Text -> Attribute
subject_ = makeAttribute "subject"

-- | The @tabindex@ attribute.
tabindex_ :: Text -> Attribute
tabindex_ = makeAttribute "tabindex"

-- | The @target@ attribute.
target_ :: Text -> Attribute
target_ = makeAttribute "target"

-- | The @type@ attribute.
type_ :: Text -> Attribute
type_ = makeAttribute "type"

-- | The @usemap@ attribute.
usemap_ :: Text -> Attribute
usemap_ = makeAttribute "usemap"

-- | The @value@ attribute.
value_ :: Text -> Attribute
value_ = makeAttribute "value"

-- | The @width@ attribute.
width_ :: Text -> Attribute
width_ = makeAttribute "width"

-- | The @wrap@ attribute.
wrap_ :: Text -> Attribute
wrap_ = makeAttribute "wrap"

-- | The @xmlns@ attribute.
xmlns_ :: Text -> Attribute
xmlns_ = makeAttribute "xmlns"
