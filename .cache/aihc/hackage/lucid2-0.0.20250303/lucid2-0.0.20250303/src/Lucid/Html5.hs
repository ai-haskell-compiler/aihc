{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-type-defaults #-}

-- | Html5 terms.

module Lucid.Html5 where

import Lucid.Base

import Data.ByteString
import Data.Text (Text)

-------------------------------------------------------------------------------
-- Elements

-- | @DOCTYPE@ element
--
-- This is implemented as "raw output", because the doctype doesn't
-- accept attributes.
--
doctype_ :: Monad m => HtmlT m ()
doctype_ = toHtmlRaw ("<!DOCTYPE HTML>" :: ByteString)

-- | @DOCTYPE@ element + @html@ element
doctypehtml_ :: Monad m => HtmlT m a -> HtmlT m a
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
area_ :: Monad m => [Attributes] -> HtmlT m ()
area_ = makeElementNoEnd "area"

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
base_ :: Monad m => [Attributes] -> HtmlT m ()
base_ = makeElementNoEnd "base"

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
br_ :: Monad m => [Attributes] -> HtmlT m ()
br_ = makeElementNoEnd "br"

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
col_ :: Monad m => [Attributes] -> HtmlT m ()
col_ = makeElementNoEnd "col"

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

-- | @dialog@ element
dialog_ :: Term arg result => arg -> result
dialog_ = term "dialog"

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
embed_ :: Monad m => [Attributes] -> HtmlT m ()
embed_ = makeElementNoEnd "embed"

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
hr_ :: Monad m => [Attributes] -> HtmlT m ()
hr_ = makeElementNoEnd "hr"

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
img_ :: Monad m => [Attributes] -> HtmlT m ()
img_ = makeElementNoEnd "img"

-- | @input@ element
input_ :: Monad m => [Attributes] -> HtmlT m ()
input_ = makeElementNoEnd "input"

-- | @ins@ element
ins_ :: Term arg result => arg -> result
ins_ = term "ins"

-- | @kbd@ element
kbd_ :: Term arg result => arg -> result
kbd_ = term "kbd"

-- | @keygen@ element
keygen_ :: Monad m => [Attributes] -> HtmlT m ()
keygen_ = makeElementNoEnd "keygen"

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
link_ :: Monad m => [Attributes] -> HtmlT m ()
link_ = makeElementNoEnd "link"

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
menuitem_ :: Monad m => [Attributes] -> HtmlT m ()
menuitem_ = makeElementNoEnd "menuitem"

-- | @meta@ element
meta_ :: Monad m => [Attributes] -> HtmlT m ()
meta_ = makeElementNoEnd "meta"

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
param_ :: Monad m => [Attributes] -> HtmlT m ()
param_ = makeElementNoEnd "param"

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
source_ :: Monad m => [Attributes] -> HtmlT m ()
source_ = makeElementNoEnd "source"

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
track_ :: Monad m => [Attributes] -> HtmlT m ()
track_ = makeElementNoEnd "track"

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
wbr_ :: Monad m => [Attributes] -> HtmlT m ()
wbr_ = makeElementNoEnd "wbr"

-------------------------------------------------------------------------------
-- Attributes

-- | The @accept@ attribute.
accept_ :: Text -> Attributes
accept_ = makeAttributes "accept"

-- | The @acceptCharset@ attribute.
acceptCharset_ :: Text -> Attributes
acceptCharset_ = makeAttributes "accept-charset"

-- | The @accesskey@ attribute.
accesskey_ :: Text -> Attributes
accesskey_ = makeAttributes "accesskey"

-- | The @action@ attribute.
action_ :: Text -> Attributes
action_ = makeAttributes "action"

-- | The @alt@ attribute.
alt_ :: Text -> Attributes
alt_ = makeAttributes "alt"

-- | The @async@ attribute.
async_ :: Text -> Attributes
async_ = makeAttributes "async"

-- | The @autocomplete@ attribute.
autocomplete_ :: Text -> Attributes
autocomplete_ = makeAttributes "autocomplete"

-- | The @autofocus@ attribute.
autofocus_ :: Attributes
autofocus_ = makeAttributes "autofocus" mempty

-- | The @autoplay@ attribute.
autoplay_ :: Text -> Attributes
autoplay_ = makeAttributes "autoplay"

-- | The @challenge@ attribute.
challenge_ :: Text -> Attributes
challenge_ = makeAttributes "challenge"

-- | The @charset@ attribute.
charset_ :: Text -> Attributes
charset_ = makeAttributes "charset"

-- | The @checked@ attribute.
checked_ :: Attributes
checked_ = makeAttributes "checked" mempty

-- | The @class@ attribute.
class_ :: Text -> Attributes
class_ = makeAttributes "class"

-- | The @cols@ attribute.
cols_ :: Text -> Attributes
cols_ = makeAttributes "cols"

-- | The @colspan@ attribute.
colspan_ :: Text -> Attributes
colspan_ = makeAttributes "colspan"

-- | The @content@ attribute.
content_ :: Text -> Attributes
content_ = makeAttributes "content"

-- | The @contenteditable@ attribute.
contenteditable_ :: Text -> Attributes
contenteditable_ = makeAttributes "contenteditable"

-- | The @contextmenu@ attribute.
contextmenu_ :: Text -> Attributes
contextmenu_ = makeAttributes "contextmenu"

-- | The @controls@ attribute.
controls_ :: Text -> Attributes
controls_ = makeAttributes "controls"

-- | The @coords@ attribute.
coords_ :: Text -> Attributes
coords_ = makeAttributes "coords"

-- | The @crossorigin@ attribute.
--
-- @since 2.9.8
crossorigin_ :: Text -> Attributes
crossorigin_ = makeAttributes "crossorigin"

-- | The @data@ attribute.
data_ :: Text -> Text -> Attributes
data_ name = makeAttributes ("data-" <> name)

-- | The @datetime@ attribute.
datetime_ :: Text -> Attributes
datetime_ = makeAttributes "datetime"

-- | The @defer@ attribute.
defer_ :: Text -> Attributes
defer_ = makeAttributes "defer"

-- | The @dir@ attribute.
dir_ :: Text -> Attributes
dir_ = makeAttributes "dir"

-- | The @disabled@ attribute.
disabled_ :: Text -> Attributes
disabled_ = makeAttributes "disabled"

-- | The @download@ attribute.
download_ :: Text -> Attributes
download_ = makeAttributes "download"

-- | The @draggable@ attribute.
draggable_ :: Text -> Attributes
draggable_ = makeAttributes "draggable"

-- | The @enctype@ attribute.
enctype_ :: Text -> Attributes
enctype_ = makeAttributes "enctype"

-- | The @for@ attribute.
for_ :: Text -> Attributes
for_ = makeAttributes "for"

-- | The @formaction@ attribute.
formaction_ :: Text -> Attributes
formaction_ = makeAttributes "formaction"

-- | The @formenctype@ attribute.
formenctype_ :: Text -> Attributes
formenctype_ = makeAttributes "formenctype"

-- | The @formmethod@ attribute.
formmethod_ :: Text -> Attributes
formmethod_ = makeAttributes "formmethod"

-- | The @formnovalidate@ attribute.
formnovalidate_ :: Text -> Attributes
formnovalidate_ = makeAttributes "formnovalidate"

-- | The @formtarget@ attribute.
formtarget_ :: Text -> Attributes
formtarget_ = makeAttributes "formtarget"

-- | The @headers@ attribute.
headers_ :: Text -> Attributes
headers_ = makeAttributes "headers"

-- | The @height@ attribute.
height_ :: Text -> Attributes
height_ = makeAttributes "height"

-- | The @hidden@ attribute.
hidden_ :: Text -> Attributes
hidden_ = makeAttributes "hidden"

-- | The @high@ attribute.
high_ :: Text -> Attributes
high_ = makeAttributes "high"

-- | The @href@ attribute.
href_ :: Text -> Attributes
href_ = makeAttributes "href"

-- | The @hreflang@ attribute.
hreflang_ :: Text -> Attributes
hreflang_ = makeAttributes "hreflang"

-- | The @httpEquiv@ attribute.
httpEquiv_ :: Text -> Attributes
httpEquiv_ = makeAttributes "http-equiv"

-- | The @icon@ attribute.
icon_ :: Text -> Attributes
icon_ = makeAttributes "icon"

-- | The @id@ attribute.
id_ :: Text -> Attributes
id_ = makeAttributes "id"

-- | The @integrity@ attribute.
--
-- @since 2.9.8
integrity_ :: Text -> Attributes
integrity_ = makeAttributes "integrity"

-- | The @ismap@ attribute.
ismap_ :: Text -> Attributes
ismap_ = makeAttributes "ismap"

-- | The @item@ attribute.
item_ :: Text -> Attributes
item_ = makeAttributes "item"

-- | The @itemprop@ attribute.
itemprop_ :: Text -> Attributes
itemprop_ = makeAttributes "itemprop"

-- | The @keytype@ attribute.
keytype_ :: Text -> Attributes
keytype_ = makeAttributes "keytype"

-- | The @lang@ attribute.
lang_ :: Text -> Attributes
lang_ = makeAttributes "lang"

-- | The @list@ attribute.
list_ :: Text -> Attributes
list_ = makeAttributes "list"

-- | The @loading@ attribute.
loading_ :: Text -> Attributes
loading_ = makeAttributes "loading"

-- | The @loop@ attribute.
loop_ :: Text -> Attributes
loop_ = makeAttributes "loop"

-- | The @low@ attribute.
low_ :: Text -> Attributes
low_ = makeAttributes "low"

-- | The @manifest@ attribute.
manifest_ :: Text -> Attributes
manifest_ = makeAttributes "manifest"

-- | The @max@ attribute.
max_ :: Text -> Attributes
max_ = makeAttributes "max"

-- | The @maxlength@ attribute.
maxlength_ :: Text -> Attributes
maxlength_ = makeAttributes "maxlength"

-- | The @media@ attribute.
media_ :: Text -> Attributes
media_ = makeAttributes "media"

-- | The @method@ attribute.
method_ :: Text -> Attributes
method_ = makeAttributes "method"

-- | The @min@ attribute.
min_ :: Text -> Attributes
min_ = makeAttributes "min"

-- | The @minlength@ attribute.
minlength_ :: Text -> Attributes
minlength_ = makeAttributes "minlength"

-- | The @multiple@ attribute.
multiple_ :: Text -> Attributes
multiple_ = makeAttributes "multiple"

-- | The @name@ attribute.
name_ :: Text -> Attributes
name_ = makeAttributes "name"

-- | The @novalidate@ attribute.
novalidate_ :: Text -> Attributes
novalidate_ = makeAttributes "novalidate"

-- | The @onbeforeonload@ attribute.
onbeforeonload_ :: Text -> Attributes
onbeforeonload_ = makeAttributesRaw "onbeforeonload"

-- | The @onbeforeprint@ attribute.
onbeforeprint_ :: Text -> Attributes
onbeforeprint_ = makeAttributesRaw "onbeforeprint"

-- | The @onblur@ attribute.
onblur_ :: Text -> Attributes
onblur_ = makeAttributesRaw "onblur"

-- | The @oncanplay@ attribute.
oncanplay_ :: Text -> Attributes
oncanplay_ = makeAttributesRaw "oncanplay"

-- | The @oncanplaythrough@ attribute.
oncanplaythrough_ :: Text -> Attributes
oncanplaythrough_ = makeAttributesRaw "oncanplaythrough"

-- | The @onchange@ attribute.
onchange_ :: Text -> Attributes
onchange_ = makeAttributesRaw "onchange"

-- | The @onclick@ attribute.
onclick_ :: Text -> Attributes
onclick_ = makeAttributesRaw "onclick"

-- | The @oncontextmenu@ attribute.
oncontextmenu_ :: Text -> Attributes
oncontextmenu_ = makeAttributesRaw "oncontextmenu"

-- | The @ondblclick@ attribute.
ondblclick_ :: Text -> Attributes
ondblclick_ = makeAttributesRaw "ondblclick"

-- | The @ondrag@ attribute.
ondrag_ :: Text -> Attributes
ondrag_ = makeAttributesRaw "ondrag"

-- | The @ondragend@ attribute.
ondragend_ :: Text -> Attributes
ondragend_ = makeAttributesRaw "ondragend"

-- | The @ondragenter@ attribute.
ondragenter_ :: Text -> Attributes
ondragenter_ = makeAttributesRaw "ondragenter"

-- | The @ondragleave@ attribute.
ondragleave_ :: Text -> Attributes
ondragleave_ = makeAttributesRaw "ondragleave"

-- | The @ondragover@ attribute.
ondragover_ :: Text -> Attributes
ondragover_ = makeAttributesRaw "ondragover"

-- | The @ondragstart@ attribute.
ondragstart_ :: Text -> Attributes
ondragstart_ = makeAttributesRaw "ondragstart"

-- | The @ondrop@ attribute.
ondrop_ :: Text -> Attributes
ondrop_ = makeAttributesRaw "ondrop"

-- | The @ondurationchange@ attribute.
ondurationchange_ :: Text -> Attributes
ondurationchange_ = makeAttributesRaw "ondurationchange"

-- | The @onemptied@ attribute.
onemptied_ :: Text -> Attributes
onemptied_ = makeAttributesRaw "onemptied"

-- | The @onended@ attribute.
onended_ :: Text -> Attributes
onended_ = makeAttributesRaw "onended"

-- | The @onerror@ attribute.
onerror_ :: Text -> Attributes
onerror_ = makeAttributesRaw "onerror"

-- | The @onfocus@ attribute.
onfocus_ :: Text -> Attributes
onfocus_ = makeAttributesRaw "onfocus"

-- | The @onformchange@ attribute.
onformchange_ :: Text -> Attributes
onformchange_ = makeAttributesRaw "onformchange"

-- | The @onforminput@ attribute.
onforminput_ :: Text -> Attributes
onforminput_ = makeAttributesRaw "onforminput"

-- | The @onhaschange@ attribute.
onhaschange_ :: Text -> Attributes
onhaschange_ = makeAttributesRaw "onhaschange"

-- | The @oninput@ attribute.
oninput_ :: Text -> Attributes
oninput_ = makeAttributesRaw "oninput"

-- | The @oninvalid@ attribute.
oninvalid_ :: Text -> Attributes
oninvalid_ = makeAttributesRaw "oninvalid"

-- | The @onkeydown@ attribute.
onkeydown_ :: Text -> Attributes
onkeydown_ = makeAttributesRaw "onkeydown"

-- | The @onkeyup@ attribute.
onkeyup_ :: Text -> Attributes
onkeyup_ = makeAttributesRaw "onkeyup"

-- | The @onload@ attribute.
onload_ :: Text -> Attributes
onload_ = makeAttributesRaw "onload"

-- | The @onloadeddata@ attribute.
onloadeddata_ :: Text -> Attributes
onloadeddata_ = makeAttributesRaw "onloadeddata"

-- | The @onloadedmetadata@ attribute.
onloadedmetadata_ :: Text -> Attributes
onloadedmetadata_ = makeAttributesRaw "onloadedmetadata"

-- | The @onloadstart@ attribute.
onloadstart_ :: Text -> Attributes
onloadstart_ = makeAttributesRaw "onloadstart"

-- | The @onmessage@ attribute.
onmessage_ :: Text -> Attributes
onmessage_ = makeAttributesRaw "onmessage"

-- | The @onmousedown@ attribute.
onmousedown_ :: Text -> Attributes
onmousedown_ = makeAttributesRaw "onmousedown"

-- | The @onmousemove@ attribute.
onmousemove_ :: Text -> Attributes
onmousemove_ = makeAttributesRaw "onmousemove"

-- | The @onmouseout@ attribute.
onmouseout_ :: Text -> Attributes
onmouseout_ = makeAttributesRaw "onmouseout"

-- | The @onmouseover@ attribute.
onmouseover_ :: Text -> Attributes
onmouseover_ = makeAttributesRaw "onmouseover"

-- | The @onmouseup@ attribute.
onmouseup_ :: Text -> Attributes
onmouseup_ = makeAttributesRaw "onmouseup"

-- | The @onmousewheel@ attribute.
onmousewheel_ :: Text -> Attributes
onmousewheel_ = makeAttributesRaw "onmousewheel"

-- | The @ononline@ attribute.
ononline_ :: Text -> Attributes
ononline_ = makeAttributesRaw "ononline"

-- | The @onpagehide@ attribute.
onpagehide_ :: Text -> Attributes
onpagehide_ = makeAttributesRaw "onpagehide"

-- | The @onpageshow@ attribute.
onpageshow_ :: Text -> Attributes
onpageshow_ = makeAttributesRaw "onpageshow"

-- | The @onpause@ attribute.
onpause_ :: Text -> Attributes
onpause_ = makeAttributesRaw "onpause"

-- | The @onplay@ attribute.
onplay_ :: Text -> Attributes
onplay_ = makeAttributesRaw "onplay"

-- | The @onplaying@ attribute.
onplaying_ :: Text -> Attributes
onplaying_ = makeAttributesRaw "onplaying"

-- | The @onprogress@ attribute.
onprogress_ :: Text -> Attributes
onprogress_ = makeAttributesRaw "onprogress"

-- | The @onpropstate@ attribute.
onpropstate_ :: Text -> Attributes
onpropstate_ = makeAttributesRaw "onpropstate"

-- | The @onratechange@ attribute.
onratechange_ :: Text -> Attributes
onratechange_ = makeAttributesRaw "onratechange"

-- | The @onreadystatechange@ attribute.
onreadystatechange_ :: Text -> Attributes
onreadystatechange_ = makeAttributesRaw "onreadystatechange"

-- | The @onredo@ attribute.
onredo_ :: Text -> Attributes
onredo_ = makeAttributesRaw "onredo"

-- | The @onresize@ attribute.
onresize_ :: Text -> Attributes
onresize_ = makeAttributesRaw "onresize"

-- | The @onscroll@ attribute.
onscroll_ :: Text -> Attributes
onscroll_ = makeAttributesRaw "onscroll"

-- | The @onseeked@ attribute.
onseeked_ :: Text -> Attributes
onseeked_ = makeAttributesRaw "onseeked"

-- | The @onseeking@ attribute.
onseeking_ :: Text -> Attributes
onseeking_ = makeAttributesRaw "onseeking"

-- | The @onselect@ attribute.
onselect_ :: Text -> Attributes
onselect_ = makeAttributesRaw "onselect"

-- | The @onstalled@ attribute.
onstalled_ :: Text -> Attributes
onstalled_ = makeAttributesRaw "onstalled"

-- | The @onstorage@ attribute.
onstorage_ :: Text -> Attributes
onstorage_ = makeAttributesRaw "onstorage"

-- | The @onsubmit@ attribute.
onsubmit_ :: Text -> Attributes
onsubmit_ = makeAttributesRaw "onsubmit"

-- | The @onsuspend@ attribute.
onsuspend_ :: Text -> Attributes
onsuspend_ = makeAttributesRaw "onsuspend"

-- | The @ontimeupdate@ attribute.
ontimeupdate_ :: Text -> Attributes
ontimeupdate_ = makeAttributesRaw "ontimeupdate"

-- | The @onundo@ attribute.
onundo_ :: Text -> Attributes
onundo_ = makeAttributesRaw "onundo"

-- | The @onunload@ attribute.
onunload_ :: Text -> Attributes
onunload_ = makeAttributesRaw "onunload"

-- | The @onvolumechange@ attribute.
onvolumechange_ :: Text -> Attributes
onvolumechange_ = makeAttributesRaw "onvolumechange"

-- | The @onwaiting@ attribute.
onwaiting_ :: Text -> Attributes
onwaiting_ = makeAttributesRaw "onwaiting"

-- | The @open@ attribute.
open_ :: Text -> Attributes
open_ = makeAttributes "open"

-- | The @optimum@ attribute.
optimum_ :: Text -> Attributes
optimum_ = makeAttributes "optimum"

-- | The @pattern@ attribute.
pattern_ :: Text -> Attributes
pattern_ = makeAttributes "pattern"

-- | The @ping@ attribute.
ping_ :: Text -> Attributes
ping_ = makeAttributes "ping"

-- | The @placeholder@ attribute.
placeholder_ :: Text -> Attributes
placeholder_ = makeAttributes "placeholder"

-- | The @poster@ attribute.
poster_ :: Text -> Attributes
poster_ = makeAttributes "poster"

-- | The @preload@ attribute.
preload_ :: Text -> Attributes
preload_ = makeAttributes "preload"

-- | The @pubdate@ attribute.
pubdate_ :: Text -> Attributes
pubdate_ = makeAttributes "pubdate"

-- | The @radiogroup@ attribute.
radiogroup_ :: Text -> Attributes
radiogroup_ = makeAttributes "radiogroup"

-- | The @readonly@ attribute.
readonly_ :: Text -> Attributes
readonly_ = makeAttributes "readonly"

-- | The @rel@ attribute.
rel_ :: Text -> Attributes
rel_ = makeAttributes "rel"

-- | The @required@ attribute.
required_ :: Text -> Attributes
required_ = makeAttributes "required"

-- | The @reversed@ attribute.
reversed_ :: Text -> Attributes
reversed_ = makeAttributes "reversed"

-- | The @role@ attribute.
role_ :: Text -> Attributes
role_ = makeAttributes "role"

-- | The @rows@ attribute.
rows_ :: Text -> Attributes
rows_ = makeAttributes "rows"

-- | The @rowspan@ attribute.
rowspan_ :: Text -> Attributes
rowspan_ = makeAttributes "rowspan"

-- | The @sandbox@ attribute.
sandbox_ :: Text -> Attributes
sandbox_ = makeAttributes "sandbox"

-- | The @scope@ attribute.
scope_ :: Text -> Attributes
scope_ = makeAttributes "scope"

-- | The @scoped@ attribute.
scoped_ :: Text -> Attributes
scoped_ = makeAttributes "scoped"

-- | The @seamless@ attribute.
seamless_ :: Text -> Attributes
seamless_ = makeAttributes "seamless"

-- | The @selected@ attribute.
selected_ :: Text -> Attributes
selected_ = makeAttributes "selected"

-- | The @shape@ attribute.
shape_ :: Text -> Attributes
shape_ = makeAttributes "shape"

-- | The @size@ attribute.
size_ :: Text -> Attributes
size_ = makeAttributes "size"

-- | The @sizes@ attribute.
sizes_ :: Text -> Attributes
sizes_ = makeAttributes "sizes"

-- | The @spellcheck@ attribute.
spellcheck_ :: Text -> Attributes
spellcheck_ = makeAttributes "spellcheck"

-- | The @src@ attribute.
src_ :: Text -> Attributes
src_ = makeAttributes "src"

-- | The @srcdoc@ attribute.
srcdoc_ :: Text -> Attributes
srcdoc_ = makeAttributes "srcdoc"

-- | The @start@ attribute.
start_ :: Text -> Attributes
start_ = makeAttributes "start"

-- | The @step@ attribute.
step_ :: Text -> Attributes
step_ = makeAttributes "step"

-- | The @subject@ attribute.
subject_ :: Text -> Attributes
subject_ = makeAttributes "subject"

-- | The @tabindex@ attribute.
tabindex_ :: Text -> Attributes
tabindex_ = makeAttributes "tabindex"

-- | The @target@ attribute.
target_ :: Text -> Attributes
target_ = makeAttributes "target"

-- | The @type@ attribute.
type_ :: Text -> Attributes
type_ = makeAttributes "type"

-- | The @usemap@ attribute.
usemap_ :: Text -> Attributes
usemap_ = makeAttributes "usemap"

-- | The @value@ attribute.
value_ :: Text -> Attributes
value_ = makeAttributes "value"

-- | The @width@ attribute.
width_ :: Text -> Attributes
width_ = makeAttributes "width"

-- | The @wrap@ attribute.
wrap_ :: Text -> Attributes
wrap_ = makeAttributes "wrap"

-- | The @xmlns@ attribute.
xmlns_ :: Text -> Attributes
xmlns_ = makeAttributes "xmlns"
