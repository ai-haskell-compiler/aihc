%% 
%% FMP Tutorial (German)
%% Copyright (C) 1998 Joachim Korittky
%% 
%% This file is part of Functional MetaPost.
%% 
%% Functional MetaPost is free software; you can redistribute it
%% and/or modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version 2 of
%% the License, or (at your option) any later version.
%% 
%% Functional MetaPost is distributed in the hope that it will be
%% useful, but WITHOUT ANY WARRANTY; without even the implied warranty
%% of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with Functional MetaPost; if not, write to the
%% Free Software Foundation, Inc., 59 Temple Place, Suite 330,
%% Boston, MA 02111-1307 USA
%% 

\documentclass[fleqn]{article}


%let grafiken = True


\usepackage{a4wide}
\usepackage[german,english]{babel}
\usepackage{latexsym}
%\usepackage{moreverb}
%\usepackage{boxedminipage}
\usepackage{mflogo}
\usepackage{color}
\usepackage{float}
\usepackage{wrapfig}
\usepackage{floatflt}
\makeatletter
\usepackage{amstext}
\usepackage{amssymb}
\usepackage{epsfig}
\usepackage{ulem}\normalem
\usepackage{fancyheadings}

\pagestyle{fancyplain}
%\renewcommand{\sectionmark}[1]{\markright{#1}{\thesection\ #1}}
\lhead[\fancyplain{}{\bfseries\thepage}]
      {\fancyplain{}{\itshape\rightmark}}
\rhead[\fancyplain{}{\itshape\leftmark}]
      {\fancyplain{}{\bfseries\thepage}}
\cfoot{}
\setlength{\headheight}{13pt}

\DeclareFontFamily{OT1}{cmtex}{}
\DeclareFontShape{OT1}{cmtex}{m}{n}
  {<5><6><7><8>cmtex8
   <9>cmtex9
   <10><10.95><12><14.4><17.28><20.74><24.88>cmtex10}{}
\DeclareFontShape{OT1}{cmtex}{m}{it}
  {<-> ssub * cmtt/m/it}{}
\newcommand{\texfamily}{\fontfamily{cmtex}\selectfont}
\DeclareFontShape{OT1}{cmtt}{bx}{n}
  {<5><6><7><8>cmtt8
   <9>cmbtt9
   <10><10.95><12><14.4><17.28><20.74><24.88>cmbtt10}{}
\DeclareFontShape{OT1}{cmtex}{bx}{n}
  {<-> ssub * cmtt/bx/n}{}
\newcommand{\tex}[1]{\text{\texfamily#1}}       % NEU

\newcommand{\Sp}{\hskip.33334em\relax}
\usepackage{times}\renewcommand{\ttdefault}{cmtt}
\SetMathAlphabet{\mathrm}{normal}{OT1}{ptm}{m}{n}

\SetMathAlphabet{\mathbf}{normal}{OT1}{ptm}{bx}{n}
\SetMathAlphabet{\mathit}{normal}{OT1}{ptm}{m}{it}

\newlength{\lwidht}\setlength{\lwidht}{4.5cm}
\newlength{\cwidth}\setlength{\cwidth}{8mm} % 3mm

\newcommand{\Conid}[1]{\mathit{#1}}
\newcommand{\Varid}[1]{\mathit{#1}}

\newcommand{\anonymous}{\kern0.06em \vbox{\hrule\@@width.5em}}
\newcommand{\plus}{\mathbin{+\!\!\!+}}
\newcommand{\bind}{\mathbin{>\!\!\!>\mkern-6.7mu=}}
\newcommand{\sequ}{\mathbin{>\!\!\!>}}
\renewcommand{\leq}{\leqslant}
\renewcommand{\geq}{\geqslant}
\newcommand{\NB}{\textbf{NB~}}
\newcommand{\Todo}[1]{$\langle$\textbf{To do:}~#1$\rangle$}
\newcommand{\Key}[1]{\uline{#1}}

\newcommand{\FMP}{\textrm{\textit{functional}} \MP }
\newcommand{\Registered}{\Pisymbol{psy}{210}}
\newcommand{\beside}[2]{\parbox[c]{#1\linewidth}{#2}}
%\newcommand{\beside}[2]{\parbox[c]{#1\linewidth}{\begin{center}#2\end{center}}}
\newcommand{\HRule}{\rule{\linewidth}{0.5pt}\vspace{10pt}}
\newcommand{\VRule}[1]{\hspace{10pt}\beside{0.01}{\rule{0.5pt}{#1}}}

\definecolor{lightGrey}{gray}{.9}

\newfloat{tabelle}{tbhp}{lop}[subsection]
\floatname{tabelle}{Tabelle}
\floatplacement{tabelle}{H}
\newcommand{\Tabelle}[2]{\begin{tabelle}\caption{#1}\begin{center}
\label{#1}
\colorbox{lightGrey}{#2}\end{center}\end{tabelle}}

%\floatstyle{boxed}
\newfloat{abbildung}{tbhp}{lop}[subsection]
\floatname{abbildung}{Abbildung}
\floatplacement{abbildung}{H}
\newcommand{\Abbildung}[2]{\begin{abbildung}\caption{#1}\begin{center}
%\label{#1}
\setlength{\fboxsep}{16pt}
\fbox{#2}\end{center}\end{abbildung}}


\newfloat{beispiel}{tbhp}{lop}[subsection]
\floatname{beispiel}{Beispiel}
\floatplacement{beispiel}{H}
\newcommand{\Beispiel}[2]{\begin{beispiel}\caption{#1}\begin{center}
%\label{#1}
\setlength{\fboxsep}{16pt}
\fbox{\small #2}\end{center}\end{beispiel}}

\newcommand{\BeispielWrap}[2]{%
\begin{floatingfigure}[r]{#1}
    \setlength{\fboxsep}{0pt}
    \fbox{\small #2}
\end{floatingfigure}}

\newfont{\Pretty}{pretty10}


%\newcommand{\addtoprog}[2]{\newcommand{#1}{\protect #2}\addcontentsline{prg}{program}{\protect#1}}
%\newcommand{\l@@program}[2]{\par\noindent#1 {\itshape #2}}
%\newcommand{\listofprog}{\@@starttoc{prg}}

\parindent0cm

\begin{document}

\makeatother
\title{\texttt{\FMP} \\
       Eine Beschreibungssprache f"ur Grafiken\\
       Tutorial}
\author{Joachim Korittky}


\selectlanguage{german}

\begin{hide}

> module Tutorial where
> import FMPTurtle
> import FMPTree
> import FMPCanvas
> import FMPMatrix
> import FMPFrames

\end{hide}

\maketitle


\tableofcontents
\newpage
\enlargethispage{3cm}

> dbs				=  draw [arrow ("Information"<+C) ("Query"<+C),
>					arrow ("Spec"<+C) ("Query"<+C),
>					arrow ("Proc"<+C) ("Querying"<+C),
>					arrow ("Output"<+C) ("Querying"<+C),
>					arrow ("Querying"<+C) ("Bib"<+C),
>					arrow ("Query"<+C) (xy (ref("Servers"<+C))(ref("Query"<+C)))
>						#setEndCut"Servers",
>					line (xy (ref ("Center"<+W)-vec(10,0)) (ref ("Information" <+N)))
>						(xy (ref ("Center"<+W)-vec(10,0)) (ref ("Warehouse"<+S)))
>						# setPattern dotted,
>					line (xy (ref ("Center"<+E)+vec(10,0)) (ref ("Information" <+N)))
>						(xy (ref ("Center"<+E)+vec(10,0)) (ref ("Warehouse"<+S)))
>						# setPattern dotted
>					]
>				  (cdrop (0,0) frontEnd	& cdrop (200,0) backEnd
>				&  cdrop (440,30) servers)
>	where
>	frontEnd		=  (oval (texCol "specification\\\\Component") # setName "Spec")
>				|=|(oval (texCol "Data Processing\\\\Component") # setName "Proc")
>				|=|(oval (texCol "Output\\\\Component") # setName "Output")
>	backEnd			=   setTrueBoundingBox(drum (texCol "Information\\\\Servers\\\\Descriptions") # setName "Information")
>				|=|(box ((box "\\parbox{150pt}{\\begin{itemize}\\item Query Generation\\item Information Server Selection\\item Query Translation\\item$\\ldots$\\end{itemize}}"  #setName "Query")
>					|=| (box "\\parbox{95pt}{\\begin{itemize}\\item Querying\\item Browsing\\item Reorganization\\item Ranking\\item$\\ldots$\\end{itemize}}" # setName "Querying"))#setName"Center" # setDX 16 # setDY 16)
>				|=|(setTrueBoundingBox(drum (texCol "Bibliographic\\\\Data\\\\Records") #setName"Bib")
>					|=|"Warehouse" #setBGColor 0.9#setName"Warehouse")
>	servers			=  fuzzy 1 2 (tex "Library Catalogues"	|=| vspace 30
>					|=| tex "Electronic Journals"	|=| vspace 30
>					|=| tex "FTP Servers") # setName "Servers"
>	texCol c		=  tex ("\\begin{tabular}{c}"++c++"\\end{tabular}")

\begin{center}
%if grafiken
\perform{ generate "DataBases" 1 dbs }
%else
\epsfig{file=DataBases.1,width=12cm}
%endif
\end{center}


\newpage


\subsection{Einf"uhrung}


> conc259			=  draw [arrow ("1" <+ C) (dk!!0,0),
>					arrow ("2" <+ C) (dk!!1,-5),
>					arrow ("n" <+ C) (dk!!8,-40)]
>				   (cards
>				&  cdraw (darrow (100+dk!!1,-20) (0,-20) "d_2")
>				&  cdraw (darrow (100+dk!!2,-30) (0,-30) "d_3")
>				&  cdraw (darrow (100-100*d 11,-55) (0,-55) "d_{n+1}")
>				&  cdraw ((-330,-45).-.(100-100*d 11,-45)
>						.-.(100-100*d 11,-70))
>				&  cdrop (-230,-60) "table"
>				&  cdrop (-120,20) (tex "card $1$" # setName "1")
>				&  cdrop (-175,15) (tex "card $2$" # setName "2")
>				&  cdrop (-310,-20) (tex "card $n$"# setName "n"))
>	where
>	cards			=  foldl (&) relax [cdraw ((x,y).-.(x+100,y)
>					.-.(x+100,y-5).-.(x,y-5).-.cycle')
>						|(x,y)<-zip dk [0,-5..]]
>	dk			= [-100*d i|i<-[2..10]]
>	d 1			= 0
>	d n			= (n-1+sum [d i|i<-[1..(n-1)]])/(n-1)
>	darrow a b s		= a .-. b # setArrowHead (arrowHeadSize 4 180)
>					  # setStartArrowHead (arrowHeadSize 4 180)
>					  # setLabel 0.5 C (math s # setBGColor white)

\begin{center}
%if grafiken
\perform{ generate "Concrete" 259 conc259 }
%else
\epsfig{file=Concrete.259}
%endif
\end{center}

\newpage

> conc66			=  box (0,0) (50,100) (\i->(0,200-i*1.5))
>							(\i->(i*1.5,200))
>				&  box(50,0) (100,33.3) (\i->(0,i)) (\i->(i,-50))
>				&  box(75,33.3) (100,66.6) (\i->(0,100-i)) (\i->(i,100))
>				&  box(50,33.3) (75,53.3) (\i->(0,i)) (\i->(i,0))
>				&  cdraw (vec(50,100).-.vec(100,100).-.vec(100,50))
>				&  frac (-10,50) 1
>				&  frac (25,-12) 2
>				&  frac (75,-12) 2
>				&  frac (110,17) 3
>				&  frac (110,50) 3
>				&  frac (62,65) 4
>				&  frac (88,78) 4
>				&  frac (40,42) 5
>				&  cdrop (65,90) "etc."
>	where
>	frac p n		=  cdrop p (math ("\\frac{\\textstyle 1}{\\textstyle "
>						++show n ++"}")	# setBGColor white)
>	box (ax,ay)(bx,by)fa fb =  cdrop (0.5*(ax+bx),0.5*(ay+by)) 
>					(cdraws [fa i.-.fb i|i<-[-50,-40..200]]
>					& cclip p
>					& cdraw p)
>		where
>		p		=  vec(ax,ay) .-.vec(bx,ay) .-.vec(bx,by)
>				   .-.vec(ax,by) .-. cycle'

\begin{center}
%if grafiken
\perform{ generate "Concrete" 66 conc66 }
%else
\epsfig{file=Concrete.66}
%endif
\end{center}

\newpage

> hamilton			=  arrows (matrix [[empty,empty,k 1,empty,k 4],
>						   [i ||| hspace 20,empty,empty,empty,
>						    empty,empty,hspace 20|||ip1],
>						   [empty,k 2,empty,k 5,empty,
>						    k 6 # setName "6"]])
>	where
>	i			=  circle (math "i") # setName "i"
>	ip1			=  oval (math "i+1") # setName "ip1"
>	k s			=  boxArrows s (box (math ("K_"++show s))
>			  	   # setDX 8
>			 	   # setDY 8)
>	arrows			=  draw [("i"<+C) ... (2<*1<*C)  # setEndAngle 0,
>					("i"<+C) ... (1<*2<*C)  # setEndAngle 0,
>					curve (1<*5<*C) 0 0 (4<*3<*C),
>					curve (2<*4<*C) 0 0 (5<*3<*C),
>					curve (5<*6<*C) 0 0 (6<*2<*C),
>					arrow (4<*6<*C) ("ip1"<+C) # setStartAngle 0,
>					arrow (6<*5<*C) ("ip1"<+C) # setStartAngle 0]
>	curve x a1 a2 y		=  x ... y # setStartAngle a1 # setEndAngle a2			
>	boxArrows 		:: Int -> Frame -> Picture
>	boxArrows s		=  setName s . draw [	out 4 0.2, out 5 0.5, out 6 0.8,
>							to 1 0.2, to 2 0.5, to 3 0.8]
>		where
>		out m n		=  arrow (med n (ref NE) (ref SE))
>					 (med n (ref NE) (ref SE)+vec(10,0))
>				   # setLabel 1 C (empty # setName (m::Int))
>		to m n		=  arrow (med n (ref NW) (ref SW)-vec(10,0))
>					 (med n (ref NW) (ref SW))
>				   # setLabel 0 C (empty # setName (m::Int))

\begin{center}
%if grafiken
\perform{ generate "Schoening" 165 hamilton }
%else
\epsfig{file=Schoening.165}
%endif
\end{center}

\newpage

> torte	= cake [(30,blue),(15,red),(10,green),(10,yellow)]
>	where
>	cake as = drawBack (toAngles as) 
>		& drawCake (toAngles as)
>	toAngles as = ad (map (\(a,c)-> (a/(sum (map fst as)/360),c)) as)
>	ad []		= [(0,black)]
>	ad ((a,c):as)	= (a+fst (head (ad as)),c):ad as
>	drawBack []	= relax
>	drawBack [_]	= relax
>	drawBack ((a,c):(b,c2):as)
>			= cfill (toArea (carc' (vec(0,-10)) 100 60 a b .-. carc' (vec(0,0)) 100 40 b a .-. cycle')
>				#setColor (c-grey 0.3)) & drawBack ((b,c2):as)
>	drawCake []	= relax
>	drawCake [_]	= relax
>	drawCake ((a,c):(b,c2):as)
>			=  cfill (toArea (vec (0,0) .-. carc' (vec(0,0)) 100 60 a b .-. cycle')
>				#setColor c) & drawCake ((b,c2):as)


> carc'		:: Point -> Numeric -> Numeric -> Numeric -> Numeric -> Path
> carc' p rx ry w1 w2		=  (p + vec (rx*cos a1, - ry*sin a1)
>				... p + vec (rx*cos a2, - ry*sin a2)
>				... p + vec (rx*cos a2b, - ry*sin a2b)
>				... p + vec (rx*cos a3, - ry*sin a3) 
>				... p + vec (rx*cos a4b, - ry*sin a4b)
>				... p + vec (rx*cos a4, - ry*sin a4)
>				... p + vec (rx*cos a5, - ry*sin a5))
>		where
>		a1		=  w1
>		a2		=  w1 + 0.05*(w2 - w1)
>		a2b		=  w1 + 0.25*(w2 - w1)
>		a3		=  w1 + 0.5 *(w2 - w1)
>		a4b		=  w1 + 0.75*(w2 - w1)
>		a4		=  w1 + 0.95*(w2 - w1)
>		a5		=  w2


\begin{center}
%if grafiken
\perform{ generate "Torte" 1 torte }
%else
\epsfig{file=Torte.1}
%endif
\end{center}

\newpage

\subsection{Haskell}

eingebettete Sprache
Klassen, Pragmatics/Default etc.
generelle Idee

\subsection{Atomare Bilder}

Der Grundtyp auf dem die Sprache \FMP\ aufbaut ist
|Picture| bzw. k"urzer |Pic|. Mit Ausdr"ucken dieses Typs
kann man Bilder beschreiben, die sich dann in ein
\MP--Programm kompilieren oder zu neuen, komplizierteren
Bildern kombinieren lassen.

Mit atomaren Bildern sind hier solche gemeint, die nicht aus
Kombination oder Ver"anderung anderer hervorgehen.

Zwei solche atomare Funktionen sind die Einbettung von beliebigen 
\LaTeX--Ausdr"ucken 
\footnote{Zu beachten ist der Vorteil der Verwendung von \LaTeX\ in den Bildern, wenn
der umgebende Text im Dokument auch mit \LaTeX\ gesetzt ist. Da die
gleichen Zeichens"atzt und Formatierungen zur Anwendung kommen, pa"st
sich die Erscheinungs des Bildes sehr gut in den Text ein. Die volle
M"achtigkeit von \LaTeX\ steht zur Verf"ugung.}
und das Erzeugen eines rechteckigen, leeren Bildes
bestimmter Gr"o"se.

< tex				:: String -> Picture

< space				:: Numeric -> Numeric -> Picture

Der Zweck der Funktion |space| ist vielleicht nicht sofort klar,
da nichts sichtbares erzeugt wird. Vielmehr liefert die Funktion einen
Platzhalter, denn jedes Bild hat eine es umgebende bounding box
\footnote{Die bounding box mu"s nicht wirklich rechteckig, sondern
kann auch rund oder dreieckig sein.}. Anhand der bounding boxen
lassen sich Bilder ohne "Uberlappungen zu komplizierteren Bildern
kombinieren.

Leicht ergeben sich aus den beiden atomaren Funktionen |tex| und |space|
weitere n"utzliche Funktionen. Z.B. zum automatischen Satz im Mathematikmodus
von \LaTeX

< math 				:: String -> Picture
< math p			=  tex ("$" ++ p ++ "$")

oder in Anlehnung an die entsprechenden Befehle in \LaTeX\ zum
Erzeugen horizontaler und vertikaler leerr"aume

< hspace, vspace		:: Numeric -> Picture
< hspace n			=  space n 0
< vspace n			=  space 0 n

Eine Funktion, die wie sich noch zeigen wird auch n"utzlich sein kann,
erzeugt das leere Bild.

< empty				:: Picture
< empty				=  space 0 0

\subsection{Farben}

Jedes sichtbare Objekt in \FMP\ kann eine Farbe erhalten. Die
Voreinstellung ist dabei |black|. Als Farbraum kommt das
RGB-Modell zur Anwendung, bei dem eine Farbe durch additives Mischen
der Grundfarben rot gr"un und blau beschrieben wird.

< color				:: Double -> Double -> Double -> Color

Es stehen schon Funktionen f"ur h"aufig ben"otigte Farben zur Verf"ugung.

< white 			=  color 1 1 1
< black				=  color 0 0 0
< red				=  color 1 0 0
< green				=  color 0 1 0
< blue				=  color 0 0 1
< yellow			=  color 1 1 0
< cyan				=  color 0 1 1
< magenta			=  color 1 0 1
< grey n			=  color n n n

Ferner ist der Typ |Color| eine Instanz von |Num| und |Fractional|, was bedeutet,
da"s sich Farben auch addieren, subtrahieren sowie multiplizieren lassen.
Diese Operationen erfolgen komponentenweise auf den roten, gr"unen bzw.
blauen Anteilen der Farben.
Der Ausdruck |red + green| bezeichnet also die Farbe |yellow|
und |cyan - blue| bezeichnet die Farbe |green|.
Weil der Typ |Color| auch die Funktion |fromRational| implemmentiert,
steht |0.5 * magenta| f"ur |grey 0.5 * magenta|, d.h. |color 0.5 0 0.5|.

Um in einem Bild den Farbwert zu "andern, existiert die Funktion

< setColor			:: Color -> Picture -> Picture

Eigenschaften von Bildern wie Farben kann man als Attribute betrachten,
die einen bestimmten voreingestellten Wert haben. Wir wollen die
Attributmenge von Objekten als Attributierung und die Funktionen
diese zu ver"andern als Attributierungsfunktionen bezeichnen.
Eine Attributierungsfunktion zum Setzen der Hintergrundfarbe ist

< setBGColor			:: Color -> Picture -> Picture

Attributierungen lassen sich besser lesen, wenn man die postfix-Notation
verwendet. Dazu dient der |#|-Operator.

< (#)				:: a -> (a -> b) -> b
< a # f				=  f a

\Abbildung{Attributierung zur Farbwahl}{
\beside{0.75}{

> color1			= tex "colors"
>				  # setColor green
>				  # setBGColor 0.2

}
\VRule{30pt}

\beside{0.16}{
\begin{center}
%if grafiken
\perform{ generate "TutColor" 1 color1 }
%else
\epsfig{file=TutColor.1}
%endif
\end{center}
}}


\subsection{Frames}

< box, circle, oval		:: Picture -> Picture

%if grafiken
\perform{ generate "TutDrum" 1 newshapes }
%else
\epsfig{file=TutDrum.1}
%endif

> newshapes			= -- shape drum 
>				  -- |||| shape (fuzzy 345 67)
>				  -- |||| shape (fuzzy 346 67)
>				    shape (setDX 10.triangle)
>				    |||| shape (triangle)
>					|||| cloud 345 67 (tex "joe")

> shape				:: (IsPicture a, HasBGColor a)
>				=> (Picture -> a) -> Picture
> shape	f			=  setTrueBoundingBox
>					(draw 	[ ref C .-. ref C + 60 .* dir a
>						| a <- [0,  10.. 360] ] 
>						(f (tex "joe") # setBGColor 0.97))

erw"ahne automatische Gr"o"senanpassung
Attributierung; dx
Boxen fester Gr"o"se

\subsection{Kombination von Bildern}

Bisher haben wir nur die M"oglichkeit kennengelert, atomare
Bilder zu erzeugen und diese mit einem Rahmen zu versehen.
Nun wollen wir zwei Bilder zu einem neuen kombinieren.
F"ur die h"aufigsten Kombinationen, n"amlich die Anordnung
nebeneinander und "ubereinander gibt es zwei Operatoren.

< (|||)				:: Picture -> Picture -> Picture

< (|-|)				:: Picture -> Picture -> Picture

Meistens ist aber zwischen den Bildern ein kleiner Abstand gew"unscht.
F"ur diesen Fall k"onnen wir jetzt schon mit unseren bisherigen
Sprachmitteln die entsprechenden Operatoren ableiten.

< (||||)			:: Picture -> Picture -> Picture
< a |||| b			=  a ||| hspace 8 ||| b

< (|=|)				:: Picture -> Picture -> Picture
< a |=| b			=  a |-| vspace 8 |-| b

Es wurde bisher noch nicht erw"ahnt, in welchen Einheiten Werte
angegeben werden. \MP\ verwendet als Grundeinheit PostScript Punkte,
d.h. $1/72$ Inch. Dies ist auch in \FMP\ so. |hspace 8| definiert
also einen horizontalen Abstand von $1/9$ Inch was ungef"ahr $2.82$
mm entspricht. Um Abst"ande in anderen Einheiten angeben zu k"onnen,
gibt es vordefinierte Konstanten mit denen die Werte entsprechend zu
multiplizieren sind. 

< mm, pt, bp, cm, pc, cc, inch	:: Numeric
< mm				=  2.84528
< pt				=  1
< bp				=  1.00375
< cm				=  28.45276
< pc				=  12
< cc				=  12.84010
< inch				=  72.27

|hspace (2*cm)| gibt also einen Abstand von zwei Zentimetern an.


\Abbildung{Plazierungen}{
\beside{0.9}{

> bsp30				=    tree1
>				|||| hspace 10
>				|||| ("$\\Longrightarrow$" |=| "right rotation")
>				|||| hspace 10
>				|||| tree2

\begin{hide}

> tree1 = node (circle "y" # label E "-1") [enode (circle "x" # label E "-1") [
>			enode tri1 [],
>			enode tri2 []],
>		enode tri3 []] # setDistH 16
> tree2 = node (circle "x" # label E "0") [enode tri1 [],edge (node (circle "y" # label E "0") [
>			edge (node tri2 [] # setDistV (distCenter 30)),
>			edge (node tri3 [] # setDistV (distCenter 30))] # setDistH 10)] # setDistH 16
> tri1 = triangle "1" # setDY 1 # setHeight 60 # setWidth 20 # label S (math "h")
> tri2 = triangle "2" # setDY 1 # setHeight 30 # setWidth 10 # label S (math "h-1")
> tri3 = triangle "3" # setDY 1 # setHeight 30 # setWidth 10 # label S (math "h-1")

\end{hide}

\HRule

\begin{center}
%if grafiken
\perform{ generate "TutArrange" 30 bsp30 }
%else
\epsfig{file=TutArrange.30}
%endif
\end{center}
}}

\Abbildung{Kombination aus Rahmen und Plazierungen}{
\beside{0.9}{

> bsp40 			= box2 (tex "set of all languages"
>				|=| box2 (tex"rec. enum. languages or typ 0"
>				|=| box2 (tex "decideable languages"
>				|=| box2 (tex "contextsensitive or typ 1"
>				|=| box2 (tex "contextfree or typ 2"
>				|=| box2 "regular or typ 3--languages")))))
>		where
>		box2 a		= rbox 20 a
>				# setDX 8
>				# setDY 8

\HRule

\begin{center}
%if grafiken
\perform{ generate "TutArrange" 40 bsp40 }
%else
\epsfig{file=TutArrange.40}
%endif
\end{center}
}}

F"ur mehr als zwei Bilder gibt es Funktionen, die eine
Anordnung in einer Reihe bzw. Spalte bewirken.

< row				:: [Picture] -> Picture

< column			:: [Picture] -> Picture

Abbildung \ref{fig:traffic} zeigt noch eine Variante, bei der
zwischen den Bildern ein zus"atzlicher Abstand angegeben werden kann.

< rowSepBy			:: Numeric -> [Picture] -> Picture

< columnSepBy			:: Numeric -> [Picture] -> Picture

\Abbildung{\label{fig:traffic}Das Bild einer Ampel (Siehe auch \cite[FinneJones95])}{
\beside{0.75}{

> bsp60				= box ( columnSepBy 10
>				        [circle "R"
>					 # setDX 10
>					 # setBGColor red,
>					 circle "O"
>					 # setDX 10
>					 # setBGColor yellow,
>					 circle "G"
>					 # setDX 10
>					 # setBGColor green ] )
>				  # setBGColor black
>				  # setDX 10
>				  # setDY 10

}\VRule{150pt}
\beside{0.16}{
\begin{center}
%if grafiken
\perform{ generate "TutArrange" 60 bsp60 }
%else
\epsfig{file=TutArrange.60}
%endif
\end{center}
}}

In manchen F"allen reichen die vorgestellten Operatoren zur
Anordnung von Bildern nicht aus. Abbildung \ref{fig:matrix1}
zeigt eine Situation, in der die Buchstabenreihen a und b
bzw. c und d so untereinander angeordnet sind, da"s der Buchstabe
c unter b steht. Zur L"osung des Problems bedarf es einer
Ber"ucksichtigung der horizontalen und gleichzeitig der vertikalen Dimensionen.
Dies ber"ucksichtigt die Funktion

< matrix			:: [[Picture]] -> Picture

Die Funktion Matrix teilt die Ebene in Spalten und Reihen ein.
Jede Spalte ist so brei, wie das breiteste Bild in ihr. Analog
dazu ist jede Zeile so hoch, wie das h"ochste Bild in ihr.
In den damit definierten rechteckigen Zellen wird jedes Bild
zentriert plaziert.

Wie auch bei den Funktionen |row| und |column| gibt es eine
Variante, bei der sich der ein zus"atzlicher Abstand zwischen
den Zellen horizontal wie vertikal angeben l"a"st.

< matrixSepBy			:: Numeric -> Numeric -> [[Picture]] -> Picture

\Abbildung{Ausrichtung mit der Funktion |matrix|\label{fig:matrix1}}{
\beside{0.75}{

> matrix1			= matrix [	[tex "a", tex "b" ],
>				   	  	[empty,   tex "c", tex "d"]]

}\VRule{40pt}
\beside{0.16}{
\begin{center}
%if grafiken
\perform{ generate "TutMatrix" 1 matrix1 }
%else
\epsfig{file=TutMatrix.1}
%endif
\end{center}
}}


\subsection{Pfade}

type Point, 

< vec 				:: (Numeric, Numeric) -> Point

< med				:: Numeric -> Point -> Point -> Point

type Numeric

< xpart 			:: Point -> Numeric

< ypart 			:: Point -> Numeric

< angle 			:: Point -> Numeric

< dist	 			:: Point -> Point -> Numeric

< med				:: Numeric -> Numeric -> Numeric -> Numeric

hergeleitet:

< dir				:: Numeric -> Point
< dir a				=  vec (cos a, sin a)

< xy				:: Point -> Point -> Point
< xy p1 p2			=  vec (xpart p1, ypart p2)

< xdist 			:: Point -> Point -> Numeric
< xdist p1 p2			=  xpart p1 - xpart p2

< ydist 			:: Point -> Point -> Numeric
< ydist p1 p2			=  ypart p1 - ypart p2

Pfadsyntax

< (.-.)				:: (IsPath a, IsPath b) => a -> b -> Path

< (...)				:: (IsPath a, IsPath b) => a -> b -> Path

< (.--.)			:: (IsPath a, IsPath b) => a -> b -> Path

< (....)			:: (IsPath a, IsPath b) => a -> b -> Path

< (.&.)				:: (IsPath a, IsPath b) => a -> b -> Path

< cycle'			:: Path

\Abbildung{Ein einfacher Linienzug}{
\beside{0.9}{

> path1				=  vec (20,20) .-. vec (0,0) .-. vec (0,30)
>			        .-.vec (30,0) .-. vec (0,0)

\begin{hide}

> dot'		= toPicture bullet

> path1'	= path1
>		# setLabel 0 C dot'
>		# setLabel 0.25 C dot'
>		# setLabel 0.5 C dot'
>		# setLabel 0.75 C dot'
>		# setLabel 1 C dot'

\end{hide}

\HRule

\begin{center}
%if grafiken
\perform{ generate "TutPath" 1 path1' }
%else
\epsfig{file=TutPath.1}
%endif
\end{center}
}}


\Abbildung{Die Wirkung verschiedener Pfadverbindunngen (Siehe figure 3 und 4 in \cite{Hobby92})}{
\parbox[t]{0.26\linewidth}{

\begin{center}
|z0 ... z1 ... z2 ... z3 ... z4|\phantom{|e'|}
\end{center}

\begin{hide}

> mark5				:: (IsPicture a) => a -> Picture
> mark5 p			= overlay' [
>					ref ((0::Int) <+ C) .= ref (global "z0"),
>					ref ((1::Int) <+ C) .= ref (global "z1"),
>					ref ((2::Int) <+ C) .= ref (global "z2"),
>					ref ((3::Int) <+ C) .= ref (global "z3"),
>					ref ((4::Int) <+ C) .= ref (global "z4")]
>					(Just 1) [dot', dot', dot', dot',
>							dot', toPicture p]

> fig4a				=  define [ 	ref "z0" .= vec (0,0),
>						ref "z1" .= vec (60,40),
>						ref "z2" .= vec (40,90),
>						ref "z3" .= vec (10,70),
>						ref "z4" .= vec (30,50)]
>				   (column [
>				setTrueBoundingBox (mark5 ("z0" ... "z1" ... "z2" ... "z3" ... "z4"))
>				])

> fig4b				=  define [ 	ref "z0" .= vec (0,0),
>						ref "z1" .= vec (60,40),
>						ref "z2" .= vec (40,90),
>						ref "z3" .= vec (10,70),
>						ref "z4" .= vec (30,50)]
>				   (column [
>				setTrueBoundingBox (mark5 ("z0" ... "z1" ... "z2" ... "z3" ... "z4" ... cycle'))
>				])

> fig4c				=  define [ 	ref "z0" .= vec (0,0),
>						ref "z1" .= vec (60,40),
>						ref "z2" .= vec (40,90),
>						ref "z3" .= vec (10,70),
>						ref "z4" .= vec (30,50)]
>				   (column [
>				setTrueBoundingBox (mark5 ("z0" ... "z1" ... "z2" ... "z3" .-. "z4" .-. cycle'))
>				])


\end{hide}

\HRule

\begin{center}
%if grafiken
\perform{ generate "TutFigureA" 4 fig4a }
%else
\epsfig{file=TutFigureA.4}
%endif
\end{center}
}
\parbox[t]{0.32\linewidth}{

\begin{center}
|z0 ... z1 ... z2 ... z3 ... z4 ... cycle'|
\end{center}

\HRule

\begin{center}
%if grafiken
\perform{ generate "TutFigureB" 4 fig4b }
%else
\epsfig{file=TutFigureB.4}
%endif
\end{center}
}
\parbox[t]{0.32\linewidth}{

\begin{center}
|z0 ... z1 ... z2 ... z3 .-. z4 .-. cycle'|
\end{center}

\HRule

\begin{center}
%if grafiken
\perform{ generate "TutFigureC" 4 fig4c }
%else
\epsfig{file=TutFigureC.4}
%endif
\end{center}
}
}



\Abbildung{Start-- und Endwinkel von Pfadsegmenten k"onnen vorgegeben werden (Siehe figure 7 und 8 in \cite{Hobby92})}{
\beside{0.90}{
\beside{0.5}{

> fig7				= [ vec (0, 0) ... vec (3*cm, 0) 
>				     # setStartAngle 45
>				     # setEndAngle (-10*a) 
>				  | a <- [0..9] ]

}\VRule{60pt}
\beside{0.10}{
%if grafiken
\perform{ generate "TutPath" 7 fig7 }
%else
\epsfig{file=TutPath.7}
%endif
}\\
\beside{0.5}{

> fig8				= [ vec (0, 0) ... vec (3*cm, 0) 
>				    # setStartAngle 45
>				    # setEndAngle (10*a) 
>				  | a <- [0..7] ]

}\VRule{60pt}
\beside{0.10}{
%if grafiken
\perform{ generate "TutPath" 8 fig8 }
%else
\epsfig{file=TutPath.8}
%endif
}}}

\Abbildung{Der Unterschied zwischen den Pfadverbindungen |(...)| und |(....)| (Siehe figure 9 in \cite{Hobby92}).}{
\beside{0.9}{

> fig9				= rowSepBy 20 [	(z0...z1
>						# setStartVector up
>						# setEndVector right)...z2
>						# setEndVector down,
>						(z0....z1
>						# setStartVector up
>						# setEndVector right)....z2
>						# setEndVector down ]

\begin{hide}

>	where 
>	z0 = vec(-50,0)
>	z1 = vec(  0,8)
>	z2 = vec( 50,0)

> fig9'				= rowSepBy 20 [	mark3 ((z0...z1
>						# setStartVector up
>						# setEndVector right)...z2
>						# setEndVector down),
>						mark3 ((z0....z1
>						# setStartVector up
>						# setEndVector right)....z2
>						# setEndVector down) ]
>	where 
> 	mark3			:: (IsPicture a) => a -> Picture
> 	mark3 p			= setTrueBoundingBox (overlay' [
>					ref ((0::Int) <+ C) .= z0,
>					ref ((1::Int) <+ C) .= z1,
>					ref ((2::Int) <+ C) .= z2]
>					(Just 1) [dot', dot',
>							dot', toPicture p])
>	z0 = vec(-50,0)
>	z1 = vec(  0,8)
>	z2 = vec( 50,0)

\end{hide}

\HRule

\begin{center}
%if grafiken
\perform{ generate "TutPath" 9 fig9' }
%else
\epsfig{file=TutPath.9}
%endif
\end{center}
}}

\Abbildung{Der Parameter tension gibt den "'Zug"' an, der auf ein Pfadsegment wirkt (Siehe figure 10 in \cite{Hobby92}).}{
\beside{0.9}{

> fig10				= rowSepBy 20 [	
>					z0...(	z1...z2 
>	  					# setJoin (joinTension (tension 1)) )
>					...z3,
>					z0...(	z1...z2 
>						# setJoin (joinTension (tension 2.3)) )
>					...z3,
>					z0...(	z1...z2 
>						# setJoin (joinTensions (tension 2.5) 
>									(tension 0.8)) )
>					...z3 ]

\begin{hide}

>	where 
>	z0 = vec(  0,0)
>	z1 = vec( 15,15)
>	z2 = vec( 75,15)
>	z3 = vec( 90,0)

> fig10'				= rowSepBy 20 [	
>					mark4 (z0...(z1...z2 
>	  					# setJoin (joinTension (tension 1)))
>					...z3),
>					mark4 (z0...(	z1...z2 
>						# setJoin (joinTension (tension 2.3)))
>					...z3),
>					mark4 (z0...(	z1...z2 
>						# setJoin (joinTensions (tension 2.5) 
>									(tension 0.8)))
>					...z3) ]
>	where 
> 	mark4			:: (IsPicture a) => a -> Picture
> 	mark4 p			= setTrueBoundingBox (overlay' [
>					ref ((0::Int) <+ C) .= z0,
>					ref ((1::Int) <+ C) .= z1,
>					ref ((2::Int) <+ C) .= z2,
>					ref ((3::Int) <+ C) .= z3]
>					(Just 1) [dot', dot', dot',
>							dot', toPicture p])
>	z0 = vec(  0,0)
>	z1 = vec( 15,15)
>	z2 = vec( 75,15)
>	z3 = vec( 90,0)


\end{hide}

\HRule

\begin{center}
%if grafiken
\perform{ generate "TutPath" 10 fig10' }
%else
\epsfig{file=TutPath.10}
%endif
\end{center}
}}

< draw				:: [Path] -> Picture -> Picture

\subsection{Strichmuster}

< dashPattern			:: [Double] -> Pattern

abgeleitet:

< dashed 			:: Pattern
< dashed			=  dashPattern [3, 3]

< dotted 			:: Pattern
< dotted			=  dashPattern [-1, 2.5, 0, 2.5]


\subsection{Stifte}

< penSquare			:: (Numeric, Numeric) -> Numeric -> Pen

< penCircle			:: (Numeric, Numeric) -> Numeric -> Pen


\Abbildung{Kalligraphische Effekte}{
\beside{0.9}{

> calligraphic			= (transformPath (scaled 40) fullcircle
>				        # setPen (penCircle (0.1, 5) 20))
>				|||| (transformPath (scaled 40) halfcircle
>				        # setPen (penSquare (5, 5) 45))
>				|||| (transformPath (scaled 40) quartercircle
>				        # setPen 5)

\HRule

\begin{center}
%if grafiken
\perform{ generate "TutPen" 1 calligraphic }
%else
\epsfig{file=TutPen.1}
%endif
\end{center}
}}


\Abbildung{Eine horizontale Klammer}{
\beside{0.9}{

> hBrack			= hBracket (vec(0,0)) (vec(100,0))
>		where
>		hBracket pl pr	=	pl
>				   .... pl + vec(5, 5)
>				   .--. med 0.5 pl pr + vec(-5, 5)
>				   .... med 0.5 pl pr + vec( 0, 10)
>				   .&.  med 0.5 pl pr + vec( 0, 10)
>				   .... med 0.5 pl pr + vec( 5, 5)
>				   .--. pr + vec(-5, 5)
>				   .... pr
>				        # setPen (penCircle (0.001, 1) 0)

\HRule

\begin{center}
%if grafiken
\perform{ generate "TutPen" 2 hBrack }
%else
\epsfig{file=TutPen.2}
%endif
\end{center}
}}

\subsection{Pfeile}

< defaultArrowHead		:: PathArrowHead
< arrowHeadSize			:: Double -> Double -> PathArrowHead

abgeleitet:

< arrowHeadBig 			:: PathArrowHead
< arrowHeadBig			=  pathArrowHeadSize 8 4
< default'			=  defaultArrowHead

verschiedene Pfeilarten

< ahFilled, ahLine		:: ArrowHeadStyle
< ahFilled			=  AHFilled
< ahLine			=  AHLine

< setArrowHeadStyle		:: ArrowHeadStyle -> ArrowHead -> ArrowHead
< getArrowHeadStyle		:: ArrowHead -> ArrowHeadStyle

\Abbildung{Darstellung einer verketteten Feldstruktur}{
\beside{0.9}{

> bsp50				= pointerChain 25 ["12", "2", "2", "103"]
>	where
>	pointerChain dx ps 	= draw (backarrow :chainarrows)
>				  (rowSepBy dx [ b # setName (i::Int)
>					       | (b, i) <- zip (map recBox ps) [0..]])
>		where
>		n		= length ps
>		backarrow	= arrow (ref (n-1 <* "bullet" <+ C))
>					(ref (n-1 <* "bullet" <+ C) + vec(0,20))
>				     .--. arrow (ref (0 <* W) + vec(0,20))
>						(ref (0 <* W))
>		chainarrows	= [ arrow (ref (i <* "bullet" <+ C))
>					  (ref (i+1 <* W))
>				  | i <- [0..n-2] ]
>		recBox a	=   (box a # setHeight (16*pt))
>				||| (box (toPicture bullet # setName "bullet") 
>		     		     # setHeight (16*pt) # setWidth (16*pt))

\HRule

\begin{center}
%if grafiken
\perform{ generate "TutArrow" 50 bsp50 }
%else
\epsfig{file=TutArrow.50}
%endif
\end{center}
}}

%-------------------------------

\subsection{Namen}

< data Dir			=  C | N | NE | E | SE | S | SW | W | NW

Bezugspunkte: C,S,N,W,E.., ref

< setName			:: Name -> Picture -> Picture

< (<+)				:: Name -> Name -> Name
< (<*)				:: Int -> Name -> Name

type Name + Operationen


%----------------------

\subsection{Fl"achen}

type Area

< fill				:: [Area] -> Picture -> Picture

\Abbildung{Beliebige zyklische Pfade lassen sich ausf"ullen (Siehe figure 21 in \cite{Hobby92}).}{

\beside{0.6}{

> bsp4 				= let p	= vec(-1*cm, 0) 
>					... vec(0, -1*cm)
>					... vec(1*cm, 0)
>					# setStartVector down
>					# setEndVector up
>				  in
>				  fill [ (p ...vec(0, 0) 
>					  # setEndVector (vec(-1, -2)))
>				       ...cycle' 
>					  # setEndVector up ]
>				  (p ... vec(0, 1*cm) ... cycle')

}\VRule{140pt}
\beside{0.115}{
%if grafiken
\perform{generate "TutArea" 4 bsp4}
%else
\epsfig{file=TutArea.4}
%endif
}}

\Abbildung{Ich komme ins RefManual (Siehe figure 22 in \cite{Hobby92})}{

\beside{0.9}{

> bsp5				= box (	math "U"
>				    ||| ooalign
>					[ toPicture [	toArea a  # setColor 0.7,
>							toArea b  # setColor 0.7,
>							toArea ab # setColor 0.4 ],
>					  aOverB ] )
>		where
>		aOverB		= column [ math "B" # setBGColor white,
>					   vspace 50,
>					   math "A" # setBGColor white ]
>		a		= transformPath (scaled 30) fullcircle
>		aa		= transformPath (scaled 30) halfcircle
>		b		= transformPath (scaled 30 & shifted (0,-30))
>						fullcircle
>		ab		= buildCycle aa b

\HRule

\begin{center}
%if grafiken
\perform{generate "TutArea" 5 bsp5}
%else
\epsfig{file=TutArea.5}
%endif
\end{center}
}}


\subsection{Clipping}

< clip				:: Path -> Picture -> Picture

\subsection{Symbolische Gleichungen}

< define			:: [Equations] -> Picture -> Picture
< define			:: [Equations] -> Path -> Path
< define			:: [Equations] -> Area -> Area

type Equation

<	(.=)			:: a -> a -> Equation
<	equal			:: [a] -> Equation

<	(.==)			:: a -> a -> Boolean
<	(./=)			:: a -> a -> Boolean
<	(.<)			:: a -> a -> Boolean
<	(.<=)			:: a -> a -> Boolean

< whatever			:: Point

< whatever			:: Numeric

\Abbildung{Unbekannte Werte werden hergelietet (Siehe figure 13 in \cite{Hobby92}).}{
\beside{0.65}{\small

> bsp3				=  define [
>		equal [ref "z1", -ref "z2", vec(0.2*inch,0)],
>		equal [ xpart (ref "z3"), - xpart (ref "z6"), 0.3*inch ],
>		equal [ xpart (ref "z3")+ypart (ref "z3"), 
>			xpart (ref "z6")+ypart (ref "z6"), 1.1*inch],
>		ref "z4" 	.= med (1/3) (ref "z3") (ref "z6"),
>		ref "z5"	.= med (2/3) (ref "z3") (ref "z6"),
>		equal [ ref "z20", 
>			med whatever (ref "z1") (ref "z3"),
>			med whatever (ref "z2") (ref "z4")],
>		equal [ ref "z30",
>			med whatever (ref "z1") (ref "z4"),
>			med whatever (ref "z2") (ref "z5")],
>		equal [ ref "z40",
>			med whatever (ref "z1") (ref "z5"),
>			med whatever (ref "z2") (ref "z6")]]
>				   (toPicture ["z1" .-. "z20"
>					   .-. "z2" .-. "z30"
>					   .-. "z1" .-. "z40"
>				           .-. "z2",
>					       "z1" .-. "z2"
>					       # setPen 1,
>					       "z3" .-. "z6"
>					       # setPen 1])

}
\VRule{280pt}

\beside{0.12}{
%if grafiken
\perform{ generate "TutDefine" 1 bsp3 }
%else
\epsfig{file=TutDefine.1}
%endif

}}

\Beispiel{Klammern mit define}{
\beside{0.9}{

> brack 			=  [ bracket ((5+x/14) .* dir x, (10+x/8) .* dir x)
>				   | x <- [0, 23 .. 720] ]

> bracket 			:: (Point, Point) -> Path
> bracket (pl, pr)		=  define [
>		ref "start"	.= pl,
>		ref "end"	.= pr,
>		var "ang"	.= angle (ref "start"-ref "end"),
>		var "d"		.= cond (dist (ref "start") (ref "end") .< 20)
>					(dist (ref "start") (ref "end")/4)
>					5,
>		ref "vecl"	.= var "d" .* dir (var "ang"-135),
>		ref "vecr"	.= var "d" .* dir (var "ang"-45),
>		ref "start2"	.= ref "start" + ref "vecl",
>		ref "end2"	.= ref "end" + ref "vecr",
>		ref "mid"	.= med 0.5 (ref "start") (ref "end")
>					+ (1.41 * var "d")
>					.* dir (var "ang"-90),
>		ref "midl"	.= ref "mid" - ref "vecl",
>		ref "midr"	.= ref "mid" - ref "vecr" ]
>			  (pl .... ref "start2" .--. ref "midl" .... ref "mid"
>		       .&. ref "mid" .... ref "midr" .--. ref "end2" .... pr
>			   # setPen (penCircle (0.001, var "d"/5) (var "ang")))

\HRule

\begin{center}
%if grafiken
\perform{ generate "TutDefine" 2 brack }
%else
\epsfig{file=TutDefine.2}
%endif
\end{center}
}}

< overlay			:: [Equations] -> [Picture] -> Picture

\Beispiel{Klammern mit "'intelligentem"' Label}{
\beside{0.9}{

> brackLabel 			=  [ bracket' x	
>					((5+x/14) .* dir x, (10+x/8) .* dir x)
>				   | x <- [0, 67 .. 720] ]

> bracket'			:: IsPicture a => a -> (Point, Point) -> Path
> bracket' l (pl, pr)		=  bracket (pl, pr)
>		   		   # setLabel 0.5 C label
>	where 
>	label			=  overlay' [
>		var "ang"	.= angle (pl-pr),
>		ref (0 <* C)	.= cond (var "ang" .< (-175.5) 
>						 + 175.5 .< var "ang")
>							(ref (1 <* S))
>	 		(cond (var "ang" .< (-112.5))	(ref (1 <* SE))
>			  (cond (var "ang" .< (-67.5))	(ref (1 <* E))
>			   (cond (var "ang" .< (-22.5))	(ref (1 <* NE))
>			    (cond (var "ang" .< 22.5)	(ref (1 <* N))
>			     (cond (var "ang" .< 67.5)	(ref (1 <* NW))
>			      (cond (var "ang" .< 112.5)(ref (1 <* W))
>				 			(ref (1 <* SW))
>			 	))))))]	(Just 0)
>					[empty, toPicture l]

\HRule

\begin{center}
%if grafiken
\perform{ generate "TutDefine" 3 brackLabel }
%else
\epsfig{file=TutDefine.3}
%endif
\end{center}
}}



%-------


\subsection{Transformationen}

< scale 			:: Double -> Picture -> Picture
< rotate 			:: Double -> Picture -> Picture

\Abbildung{Eine rekursive Grafik (Siehe figure 28 in \cite{Hobby92}).}{
\beside{0.9}{

> fig28				= rek 4 empty
>		where
>		rek 0 pic	= pic
>		rek n pic	= ooalign [draw [p] [ toArea a # setColor 0.6,
>						      toArea p # setColor white ],
>					rotate 90 (scale (1/3) (rek (n-1) pic)) ]
>		p		= transformPath (scaled 30) fullcircle
>		a		= (vec(90, 0) ... vec(0, 30) ... vec(-90, 0) 
>				   # setEndCurl 1)
>				   ... vec(0, -30) ... cycle'
>				       # setEndCurl 1

\HRule

\begin{center}
%if grafiken
\perform{ generate "TutTransform" 1 fig28 }
%else
\epsfig{file=TutTransform.1}
%endif
\end{center}
}}

\Abbildung{Eine rekursive Grafik (Siehe figure 28 in \cite{Hobby92}).}{
\beside{0.9}{

> inverse [] = []
> inverse (b:bs) = inv b:inverse bs
>	where 
>	inv '0'	= 'f'
>	inv '1'	= 'e'
>	inv '2'	= 'd'
>	inv '3'	= 'c'
>	inv '4'	= 'b'
>	inv '5'	= 'a'
>	inv '6'	= '9'
>	inv '7'	= '8'
>	inv '8'	= '7'
>	inv '9'	= '6'
>	inv 'a'	= '5'
>	inv 'b'	= '4'
>	inv 'c'	= '3'
>	inv 'd'	= '2'
>	inv 'e'	= '1'
>	inv 'f'	= '0'
>	inv _	= '0'

\begin{hide}

> figBit = image Depth24 colorBit
> figBit0 = image Depth1 (map inverse figBit')
> figBit2 = image Depth1 (map inverse figBit'')
> figBit'' = [
>	"ffffffffffffffffffffffffffffffffffffffff", "ffffffffffffffffffffffffffffffffffffffff",
>	"ffffffffffffffffffffffffffffffffffffffff", "ffffffffffffffffffffffffffffffffffffffff",
>	"ffffffffffffffffffffffffffffffffffffffff", "ffffffffffffffffffffffffffffffffffffffff",
>	"ffffffffffffffffffffffffffffffffffffffff", "ffffffffffffffffffffffffffffffffffffffff",
>	"ffffffffffffffffffffffffffffffffffffffff", "ffffffffffffffffffffffffffffffffffffffff",
>	"ffffffffffffffffffffffffffffffffffffffff", "ffffffffffffffffffffffffffffffffffffffff",
>	"ffffffffffffffffffffffffffffffffffffffff", "ffffffffff9fffffffffffffffffffffffffffff",
>	"ffffffffffbfffffffffffffffffffffffffffff", "ffffffffffbfffffffffffffffffffffffffffff",
>	"ffffffffffbfffffffffffffffffffffffffffff", "ffffffffff0fffffffffffffffffffffffffffff",
>	"ffffffffffafffffffffffffffffffffffffffff", "ffffffffff0fffffffffffffffffffffffffffff",
>	"ffffffffff2fffffffffffffffffffffffffffff", "fffffffffb0fffffffffffffffffffffffffffff",
>	"fffffffffa17ffffffffffffffffffffffffffff", "fffffffffecfffffffffffffffffffffffffffff",
>	"fffffffff907ffffffffffffffffffffffffffff", "fffffffffd13ffffffffffffffffffffffffffff",
>	"fffffffffd11ffffffffffffffffffffffffffff", "fffffffff843ffffffffffffffffffffffffffff",
>	"fffffffffc10ffffffffffffffffffffffffffff", "fffffffff8c2ffffffffffffffffffffffffffff",
>	"fffffffff880ffffffffffffffffffffffffffff", "fffffffffa883fffffffffffffffffffffffffff",
>	"fffffffff8463fffffffffffffffffffffffffff", "fffffffff9053fffffffffffffffffffffffffff",
>	"fffffffff88407ffffffffffffffffffffffffff", "fffffffffb246fffffffffffffffffffffffffff",
>	"fffffffff00807ffffffffffffffffffffffffff", "fffffffff00927ffffffffffffffffffffffffff",
>	"ffffffffd10057ffffffffffffffffffffffffff", "ffffffffe44811ffffffffffffffffffffffffff",
>	"ffffffffe820500fffffffffffffffffffffffff", "fffffffff21000801e0c3fffffffffffffffffff",
>	"ffffffffd4c2009480fe50f3ffffffffffffffff", "fffffffff5d400c009f4ff8007ffffffffffffff",
>	"ffffffffdbeb49e010c0421fa3ffffffffffffff", "ffffffffff0383f08130120087ffffffffffffff",
>	"fffffffffef913fe0005e57609ffffffffffffff", "fffffffffc0647bd043d9fd7faffffffffffffff",
>	"fffffffffc00cbfe40f68723ad7fffffffffffff", "fffffffff8017f9f847303c0e05fffffffffffff",
>	"fffffffff8007f7ca80c1806019fffffffffffff", "fffffffff800757c0001f9fec74fffffffffffff",
>	"fffffffff1e07da0204361743033ffffffffffff", "fffffffff3f861101001a1a006d9ffffffffffff",
>	"fffffffff7fc7fb00000620146e6ffffffffffff", "fffffffff7fe6bf0040008d853137fffffffffff",
>	"fffffffff7fe1e800000101e194ebfffffffffff", "fffffffff64f1f2002000a02ace47fffffffffff",
>	"fffffffff637c730000003a1e309dfffffffffff", "fffffffff607fe3c10000178118ecfffffffffff",
>	"fffffffffe07fe790000001e9c664bffffffffff", "fffffffffa07ff7c0400003786a199ffffffffff",
>	"fffffffffc07fffe8000000c529ccdffffffffff", "fffffffffe0e3ffe00000007c8666bffffffffff",
>	"fffffffffffd9bff40000003793218ffffffffff", "fffffffffff38fffe0000000de44c27fffffffff",
>	"ffffffffffea47fff00000007da66b3fffffffff", "ffffffffffd983fff00000081ed199afffffffff",
>	"ffffffffff0b2bfff800002283ba4667ffffffff", "fffffffffe9503fffc00000052f15333ffffffff",
>	"ffffffffffaf2bfffe00004aa9de1aadffffffff", "ffffffffff1503ffff0001015e77c277ffffffff",
>	"ffffffffff8b23ffff800015492e2931ffffffff", "ffffffffff9e4bffffc002426f9dc084ffffffff",
>	"ffffffffff3503ffffe0100adac7f8aeffffffff", "ffffffffffab2bfffff004a57781ee11ffffffff",
>	"ffffffffff1d47fffff00012db60f884ffffffff", "ffffffffff9683fffff82106dec07f127fffffff",
>	"ffffffffffaa97fffffc042b57a03ff23fffffff", "fffffffffff687fffffe014a81508f50ffffffff",
>	"ffffffffffad97ffffff282be0e045ebffffffff", "ffffffffffea87bfffff80885000631cffffffff",
>	"ffffffffffdb47ffffffd260780848f7ffffffff", "ffffffffffb697ffffffe0021a00567fffffffff",
>	"ffffffffffd5477ffffff4000680171f7fffffff", "fffffffffff4c77ffffff80000002c0dffffffff",
>	"ffffffffffe94fffffffff8000001e87bfffffff", "ffffffffffd557fffffffffc00001da0ffffffff",
>	"ffffffffffed47fffffffffe00000580ffffffff", "ffffffffffdaceffffffffff008003a07fffffff",
>	"fffffffffff587ffffffffff80fc03f83bffffff", "ffffffffffd6afffffefffffc0ffe25a1effffff",
>	"fffffffffffa87ffffefffffe0fff9f5047fffff", "ffffffffffeaaffffffffff0803ffcb3421fffff",
>	"fffffffffff687ffffffffd4248ffcfb204fffff", "ffffffffffed97fffffffe28401ffe3a9001ffff",
>	"fffffffffffa87fffffff092954fff3ee8007fff", "fffffffffff697ffffffd2442bd7ff8b6c000fff",
>	"fffffffffffaaffffffe20bffff7ffc6fb0003bf", "fffffffffff525f7fffebfffffffffeafd1000ff",
>	"ffffffffffff95effffffffffffffff9760001ff", "fffffffffffa45fffffffffffffffffc3844003f",
>	"ffffeffffff597efffffffffffffffff1e08400f", "fffffffffffb57ffffffffffffffffffef81c1ff",
>	"fffffffffffecdffffffffffffffffffffe07fff", "fffffffffffd43fffff7fffffffffffffff83fff",
>	"fdfffffffffdd7fbefefffffffffffffffffdfff", "fffffffffffb4793ffdfffffff7fffffffffffff",
>	"ffffffffdffed7ffffbfffffffdfffffffffffff", "ffffbffffefaa6ffffffffffffffffffffffffff",
>	"ffffdfffefdfc52ff7ffffffffffffffffffffff", "ffffffffeff556fffffffffff7ffffffffffffff",
>	"ffffffffffeee7ffdffdffffffffffffffffffff", "ffffffffbfdf93ff77ffffffffffffffffffffff",
>	"fffffffffbfed7ff37ffffffffffffffffffffff", "fdfffffff5fdd7fff5fdffffffffffffffffffff",
>	"ff7ffefffdfeafff71fefffffeffffffffffffff", "ffdfffffffffeffffd9ffffff7ffffffffffffff",
>	"fffffffffffd4fffffe79fffbfffffffffffffff", "fffbffffffffefffffeffffdffffffffffffffff",
>	"fffffff67ffeafffe777ffefffffffffffffffff", "ffffbfffffdfefffffefde7fffffffffffffffff",
>	"fffffffdfeeeaffffffe5fffffffffffffffffff", "fffffffdffffefffeb9effffffffffffffffffff",
>	"fffffdddffff5fffcbffffffffffffffffffffff", "ffffffffdfffefffd2b7ffffffffffffffffffff",
>	"ffffffffff3f6fdfdfbfbfbfffffffffffffffff", "ffffffffffffef8f6aebffffffffffffffffffff",
>	"fffffff7ffffaeef51f7cff7ffffffffffffffff", "ffffbffbfe7dec7d1d1cffffffffffffffffffff",
>	"fffffffffffb6fff9f1beffdffffffffffffffff", "fffbff7bfff5c98d1fffefffffffffffffffffff",
>	"fffffffffe7ae989deffffffffffffffffffffff", "fffffffbfff3cfffefff7ff7ffffffffffffffff",
>	"fffbff5bffffcfdfffffffffffffffffffffffff", "fffffffbfffbcf8fc69fefffffffffffffffffff",
>	"fffffcfcffefcbaebffbffefffffffffffffffff", "ffebffffbfe9cbf767f5fffdffffffffffffffff",
>	"ffffff5ffff1e8de7dffffffffffffffffffffff", "ffffffdfffffcb977edfffffffffffffffffffff",
>	"fffffedffff5cfbeffb7ff7fffffffffffffffff", "fffffdf7ffd96b9ff3cf79fbffffffffffffffff",
>	"fffffffffffe8737ff577ffeffffffffffffffff", "fffffffbfeffcf2efff1dffffdffffffffffffff",
>	"fffffff7fff7dff7fffddf7fffffffffffffffff", "fffffff9bdffffffffff7f3fffffffffffffffff",
>	"fffffffffeffefffdfffffffffffffffffffffff", "fffffffd6fdffffdffeddfffffff5fffffffffff",
>	"ffffffdecffffefff76bfffffffffdffffffffff", "fffffffbd7fffcfffbe77fffffffffffffffffff",
>	"ffffffbff7ffffffbfebffffffffffffffffffff", "ffffff7ff7fff7efff7d7fffefffffffffffffff",
>	"fffffefcbffff7f5ffcdfffffbfffbffffffffff", "ffffffffffeff6fbefcffffff7ffffffffffffff",
>	"fffffffffefef3fbfbfbffffffffffffffffffff", "ffff7feffffffffffdfffefdffffffffffffffff",
>	"ffffffffffbfffffffffffffffffffffffffffff", "fffffefffbfffffffffffffeffffffffffffffff",
>	"ffdffdfff7ffffffffbfffffffffffffffffffff", "ffeffffffdfe7b7fffffffffddefffffffffffff",
>	"ffffffffdbffffbfffffffffffffffffffffffff", "ffefdfffffebffbfffffffffffffffffffffffff",
>	"ffffffffffffffffbff9fffdfdffffffffffffff", "fffffffefffffffffffbffffffffffffffffffff",
>	"ffff7fffffffbcfdfffcffffffffffffffffffff", "fffffdf7fffe2fffdfffffffffffffffffffffff",
>	"fbfffbffffffffffffffffffffffffffffffffff", "ffffffffffffffffffffffffffffffffffffffff",
>	"ff7dfffffffffdf7dffffffffdffffffffffffff", "ffffffeffffffdf4fff7ffffffffffffffffffff",
>	"ffffffffffffffffffffffffffffffffffffffff", "fffffffffffffffffffbffffffffffffffffffff",
>	"ffffffbffffffbffffffffffffffffffffffffff", "fffffffff7fffffffffdffffffffffffffffffff",
>	"ffffffffffffffffffffffffffffffffffffffff", "fffffffffbfff7fffffdffffffffffffffffffff",
>	"ffffffffffffffffffffffffffffffffffffffff", "fffff7ffffffffffffff7fffffffffffffffffff",
>	"fffffffffffdffffffffffffffffffffffffffff", "ffffffffffffffffffffffffffffffffffffffff",
>	"ffffffffffffffffffffffffffffffffffffffff", "ffffffffffffffffffffffffffffffffffffffff",
>	"ffffffffffffffffffffffffffffffffffffffff", "ffffffffffffdfffffffffffffffffffffffffff",
>	"ffffffffffffffff7fffffffffffffffffffffff", "ffffffffffffffff7fffffffffffffffffffffff",
>	"ffffffffffffffffffffffffffffffffffffffff", "ffffffffffffffff7fffffffffffffffffffffff",
>	"ffffffffffffbfff9fffffffffffffffffffffff", "ffffffffffffff7fbfffffffffffffffffffffff",
>	"ffffffffffffffffffffffffffffffffffffffff", "ffffffffffffbfffffffffffffffffffffffffff",
>	"ffffffffffffbfffdfffffffffffffffffffffff", "ffffffffffffffffdfffffffffffffffffffffff",
>	"ffffffffffffffffdfffffffffffffffffffffff", "ffffffffffffffffffffffffffffffffffffffff",
>	"ffffffffffffffffffffffffffffffffffffffff", "ffffffffffffffffffffffffffffffffffffffff",
>	"ffffffffffffffffefffffffffffffffffffffff", "ffffffffffffffffefffffffffffffffffffffff",
>	"ffffffffffffffffefffffffffffffffffffffff", "ffffffffffffffffefffffffffffffffffffffff",
>	"ffffffffffffffffffffffffffffffffffffffff"
>		]

> figBit' = [
>	"00000000000000000000000000000000000000000000000000000000000000",
>	"00000000000000000000000000000000000000000000000000000000000000",
>	"01020000002000000000000000000080000800000000000000000000000000",
>	"00000000000800000001020000002010000000000000000000000000000000",
>	"00008102200020802100000008040000000100800000000000000000000000",
>	"00000000080000040000000400000000440010000000000000000100001000",
>	"00000000000000000000000000000000000000040080002000000000100400",
>	"20000000000000000020008000000008000000001020100104000000000000",
>	"02080800000011000004200001020800000000000004000000008000000000",
>	"00000020000000000000000040008000002002000000000000000008000000",
>	"00000000000000002000000000000000020800800000000000400000000000",
>	"00020000000000000000000000000002000200220000000000000000440000",
>	"00000000020400100000001004000100000000000000000000000800000400",
>	"00002008000000000000000001000000000000000000010000080000008000",
>	"00000000400020040820420080008000000000000000200000020000000000",
>	"22200200008000000000000000200000400000004000000000000000000000",
>	"00080000000000000000000000000808000000000200000200000000000000",
>	"00000000100000000008000000000200000000000088000000000080000000",
>	"00000040000401000000000000000000100000000000000000000800000000",
>	"00010000000020081000004080000000000440000000080010000008000400",
>	"00000000010000020001100020000004000010400001000004000000400000",
>	"20002000000000000000000000800000844000100000000000000000020000",
>	"04000000000000000000000000211040001000008000008000010000000000",
>	"00800008000000000400020000000010000400000000000000000000000000",
>	"00200401000000000100000000000400000100000000000000000000000000",
>	"00000000082100000040000000000000200048000100001000000000000400",
>	"00020000000008000008004204112284828000080001000202202220020000",
>	"00000000000000110000000001000820080400008000400000000002000000",
>	"00000400000000002000000000000100212000000000000000000000000100",
>	"0000000000000000000108000012aaad4a4db5400800000000000000000000",
>	"00000000400004000000000008800092aa2948004000100000000000800000",
>	"1000000010100000000000088256ff555aaa55540040000000000000000000",
>	"00000220000040000000020024a480556b5552200100000100040000040000",
>	"000000000000000000000000014abfad5aaa94a40000010000008000010000",
>	"0021000000000000000000012d2aa9554a5555555000000002000000000000",
>	"00000000020010080100004892556b555292d56a0044000000000400000000",
>	"04000000000001000010400555adb4aaaaaa2492d400800000000000000800",
>	"000000000010000080001211557b56aaaaa949555000000000000080000000",
>	"00000202000000100000008555d5755552aa522aa400104100400020000000",
>	"008000800000000000000012d52b4aaaad55ada4aa40000020000000010000",
>	"00044000200000000000092ab6daaad5ab556aaaa910000000000004000000",
>	"000000000200000008040255ad556db6dadb5aaa5540080000080001000000",
>	"0000000000110004010000ad6ad5ab6db6b6f6a4a550020000020000000400",
>	"00000008000040010022255555ad5aab6ad555aa9540008000000000000000",
>	"000000000000100000004d5ab6abab7db7afef7aa955000008004000200000",
>	"040400000000000020010bb56d5556a6ecdab5ed2490000000001000008000",
>	"008040000000000000946a6f52adb5fbbb77df5bea2a000000000000000000",
>	"00001004400002000021555955556eaef7dd75f6a892110800000000000000",
>	"0000000000000000010aad56aad555db5ab7eedff648800000000000000100",
>	"00000200008000000054ab555155beb5fded5beaba82000088000000000000",
>	"40000000000000410122aaaaaaa96def575ffebfee54840002000020000000",
>	"0040000010000000002d6ddd5556ab5afaeaabeabba1214000000000220000",
>	"00000000000000104152db6b2d55d6ebafbffeb7deca000000400000002000",
>	"00000000000040000a96a55552aaaddf7ad6abfd77fa920800004402000000",
>	"0400002000100000012aad52a92dabaad7fdff57deea008000000000000000",
>	"000080020200000424aaab552a4adb77bf57aafd7bba240000000000000000",
>	"000020000000080005555ad2d5bbb6fad5fd7fd7d7ef804420100000000400",
>	"002000000000000089a96aa552aad5af7f57d57f7d7b480008000040000000",
>	"00000000000000002aabb5555555bf7ddbfeffddefddd52000020800010000",
>	"1000020100000004aaad555554bb55ab7edb6ef7bd6f540082008000200000",
>	"04000000002042000aab7a52a956fbdfebfffbbef7fba88800000000000000",
>	"00000000200000417aaac2aad655577abf56dfffbeaef72210400004004200",
>	"0000408004000000a556ba4aaaadedd7edfff6dbebfbac8084000000080000",
>	"00000000000000095ab5a5535555bb7f7f75bfffff6efbd400080000000000",
>	"0208000000011042555554aaaaaad7f5dbdffb6f7efbdd5021011000000000",
>	"0000000000200400d555555555b7bd5f7ffd6ffbd7af77a500000000000800",
>	"000010000000010ab5aaaaab556d77fdd6b7fedffdfddd5a48000020000040",
>	"0000000410000055555aaaaad55baed77ffdbbfb7f6beaaa80200004000000",
>	"000000000400000ad6aaaaaaab6d7bffdb6feedff7ffbfd550001100448900",
>	"004000000000025ab55556aaaabbd6b57ffb7ffddddafab501090042000010",
>	"1010008000108015aad5556ad6d6bdffd55fedb77f7fadd550400000001040",
>	"00002000000002aaad55b4aab5bdd7adfffb7ffffbf5ff55a4045524aa4508",
>	"000000000004002ab5555555a7577d7fb76fdaddeedfd5d569510080000850",
>	"0004000000004aaaaaa9b52a5abdabeafdfeffffbfed7f6aaa00aa2aa55288",
>	"000000040000025556a56a6d56d7debfefbbb776fdbfdbadd4450040080000",
>	"000000000400012b55a954aaadbd6bf6bbeffdffeef5f6eaaa902a8aa2aaa8",
>	"000008008000054ab557536b6b6bff5fff7b6fddbfdfbbbb6a454024440040",
>	"000002400010289aaaacaaaad5deaaf6d5dffb77f7f6eed55a921249295528",
>	"09124012494546b55553555d5f7b7ddffff6dffedebbb5ead554a4a482a210",
>	"0000094000002aab6aaad55555add6badf7ffb6ffbeef6b6ba92a908544948",
>	"524aa41555554555555d15555bdbbbeff5edbffadf7fddfaaaa492a1051210",
>	"0410114092082b6aaab2ab6ab7776d5dbfbbf6dff7f57756aaaaa40a524948",
>	"528aa40a24a54ad555ad4ad56eeefff7faff7ff77edffafdb55152a1022220",
>	"04221151410892aeaaaaaaaad55daabb6fb5edbfdbf6afaaad4a244a548894",
>	"a9444a0a0a552559aaaad55abfb77feffeffbffafebff57d55515290884200",
>	"42129152d492ad56b5552aa5eaedeabd5bb6f6afb7f6afd6aaaa8402021048",
>	"28a44aa4892552ddaaaaddab5b5f5df7ffffdffbfd5ffabd5aaa12a1040400",
>	"420892155254aab55aa92a92f5b5f75bbb6d7dbf6ff6adeb55514908212048",
>	"5aeb2ad55555555556aad5552f6f5efefffff7f7fdbfdb5d56aa2041080200",
>	"4a9aaaaaaaaaadaab5552aaadad5edd7b6aededdb7f6eef6954a8508004820",
>	"512411100204455aaaa9525aadbf777dfffffffffedfbb5f6dd4a842220200",
>	"94914a4aa9529955555544aafb6afdd76dfb5bdb6ffaeefaaaa92500800000",
>	"2a4a9122420826955552aaaaaef7af7fffdffefffb6fbbaf576a4850244820",
>	"409022489442a96aaad4aaaadddefdeaadf6f7b5bffdeefaaad28100000000",
>	"2a2549252290aa9556aaa9555bb5dbbfff7fdfeffb6f7baeaad49400000000",
>	"888810000804556ab5552aab757fb6f6dbd6f6ff7fffdffb45b52140000000",
>	"2202a22aa0912aa9552a54adaeed6fdef6ffffdbdb6efb7f55721000000000",
>	"9448088004202ca52a4aa52adb5ff57ddf6dbb7fffffdfed25a94288000000",
>	"010240089009555556aa8ad5bdf55febb5fb6fdeeeadbb7f52da8800000000",
>	"20401220048055556d52512b56afeabf5b57daf7fbffefdd82b25280000000",
>	"0412000200000a8a9554825afff53deadaecb7debed55abb54ad2008000000",
>	"40000440420056a955552856aabef6b7aab5adb5efbf6ded01ba4a20008400",
>	"1244801000205592aaa54155dff5af6d5d5b5befbaeadb5f54aa9084000000",
>	"0000000004002ea52aaa92552a2d7c922124aabaff5556aaa2bb5000100000",
>	"0000220200004aaaa55550155aab55a48a55555daaaa955b54b6a420000000",
>	"0008000000005d525492055403dafa11540009237b7aaaaba2aa9080000000",
>	"0000000001006a54a555505406b5548a2092424bd6d54955692e8100000000",
>	"0800000000092aa514a942010007a8554a0088117aaeda15495d9220000000",
>	"0000000000002d2a52929054402d541550001102eabbb4d552aa8084044000",
>	"0000000000016a54a52aa020000ba92aa524a403dd6ea52aa4aea100000000",
>	"00000000000016d50954924c0000006a4a0000a00132005402ba9200000000",
>	"0000000000016aa56aa52010000a8014904015000054815501568080000000",
>	"0000044000015aaa102a896f000001692a04809120803415682d5420000000",
>	"04080010004072a945492014200a00224a0022020415484a80168100000000",
>	"4000000020015552292a906d0004815498800950a140121aa92da800000000",
>	"0000000000056a94842a42aafa20026aaa088205400964a5080b0080000000",
>	"00000000000952a5114a952aa00801292a402a812041290a522aa020000000",
>	"0000000000055aaaa15420ab5fd202d6aa855482a82e8269040a0000000000",
>	"000000000049554904918a5ad450016d5528ad01410900aa15254800000000",
>	"000000000002b552a92a5156bb54015aaa453492a0168254a00a0000000000",
>	"000200000015524900aaa2aad6b6837555a95403e8aa8054040aa000000000",
>	"00000000000aaa9242928a55bdad00ab6d54a90be816802a00120020000000",
>	"00000000000014a1155552bb776e8356db69741bf2b5525400048802248800",
>	"100000000005654a42555556aedb80ab6ed55496e8aaa895002a0010000248",
>	"000040000010492114a5516dfdbe936adb51740be93742540004a000880000",
>	"00000000000492402350aebaabee84aa9555a83ff82d5c9500120a8a22a948",
>	"000001000001549296a1596f56bf82aaaaaab25beaaaa2b40004a010944000",
>	"000000000001292456c476b4ddf6a36aaaadc15ef82dda95a009054a412aa8",
>	"00000000000452513691dd6eabaf89d55aaba877fa2ab5550024a801144408",
>	"000000000001a484ada06ab55d7e836ad556915dd88dd52aa00905542092a8",
>	"00000008010849281b15b7eaf7edd2b7beda8376fd32b56d402aa892952a48",
>	"00000000004112aab64add5d56df91ecd5b682dbbaaaad5520120524488aa8",
>	"049008915414a4242d55eaeaef75a45aaaa02f6ef655755a00aaaa4aaa5250",
>	"00044402014212a8b6aab7bdb5dfd056ab5489bbaeaaad55043552912a8a48",
>	"295102a8aa14a44a2fd5fd6b6f7d5d0010017eeefaadb55a014a8924501080",
>	"82022902104952909555b6deddd7f64a824affb5df55eeb50015244a8aa510",
>	"115440a8a52249142fab7d6b6bfeac90290156ef754b556c015252912888a0",
>	"a44aaa4508955249157eedbedf5bfb57d57ffdbdfed0dfd504a48824411208",
>	"4a9104925109549437d5bd6bb5fead552aaeab77db456abc0109054114a150",
>	"1026695556ea9241057f6b76ef6bfffffffbddaafff05aa104505014000a00",
>	"5554922aaa95549457d6f6ddf5ffaaafffdf77efb752556c408282a092a490",
>	"49255549256aa920157f5d76afaafffdb6f5aeb5fed02b5005542904000a00",
>	"555aaaaad54a920207d5f55dfdffb55fffd57deffbdfc00a10008010448000",
>	"aaaaaaaaaab5544095ffb576abdafffddf55f77abeb51020824a2042102090",
>	"14a489491140229003556d5fdf77aab7754aadafef6ffd5e08000410010000",
>	"a94a5212444a950205fbf57575deffffff55bfddfad56d5502912004804400",
>	"229484a4929048a0136d5b5fdff5dd5b6d456b7bbfbbfa9d00000840240000",
>	"945129492001250200feeaf576bf77fff55af406eaeabfea40440000000020",
>	"088a22200524484805bb9b56dbdbddb6dd55581bffd5ed5d10008000000000",
>	"a220880aa881010100beeaddbeeef7ffeabba002d12abff404000000000000",
>	"0884208002489450016fb6b7777bbb556ab69083ed55d6ed00200200000000",
>	"521102125400010200bb7d5dadeeefffdb5aa022f955bfb400040000000000",
>	"0040488000495010415ed3f7ffbbb57db5fe8b4aa0556dfd00000080000800",
>	"09080024810004020837ad5aaaefffeed7b690aaf5557f5600000000080000",
>	"2002120010020040400aab6fdfbd555fbd7dab5a82b54bfc00000000000000",
>	"80a08080004000001206aafd6af7fffb6bd75554a96abef500000004000000",
>	"2400201240108101402aaaabbfaf55addf7da96d52d56bee00040000000040",
>	"000800000400000012008bfeeafdfeff5aeb6b5555b55eb400000000000000",
>	"010004000000000000052aabffababbab7deaadb556d6bec40000000000000",
>	"000000000000004004a14bf6aafefeefdd55f6aad6dbb77400808000020000",
>	"0000000000000000200856adffabb7bb75f6ad6d556d6dec04002000000000",
>	"002000000000001002828bdb6afefeeddeadd5dbabb6db7500000200800000",
>	"000000000000000004296ab6dfababbeab6b76aa56edb6e800000080002000",
>	"2000000000002000114156dbbafdfeeb5faadd6aedbb5bb400000000000000",
>	"0000000000000000252a5db777afdffef95752aaab76f6f400400000000000",
>	"00000000200800000a556aaaeefb75ab56b5b6b555ad5bd404000000000000",
>	"000000100000000025495b7f5b6fff7bb555494aadfbb6a000000000000800",
>	"0010040000000000015556aafdfd6fab6ad554a56bad6bf400100042080000",
>	"00010000000000002aaab57dd757fafb5555492aaaf6df4000000000000000",
>	"0000000000000011012aaad6bdfdbfcaea020441255daad400020000000000",
>	"0000000000004200006ad5af6bafef6ab414a82a956b6fa020000000810400",
>	"0000000040000000015aad59df7b7db680054a880156dd8000002000000000",
>	"000000000080000000ab6ab77aefefd5200a9425015daba004000000000000",
>	"0000010800000000015adb6ad7befb75a002aa8802ab7f4000100000000000",
>	"0000000000000000955552d7bedbbed74008005520356a8000040000000000",
>	"00000000100400002aaab5bad5feff9a90020014002abda004010000000000",
>	"00040000000000000aaad56defb7f56b6a5dd4000009d68080000200000000",
>	"04000800000004006ab5bab6bd7dbfd5a80f50000092bd8000000000000000",
>	"000000000000010095556adb6beff6ab52abd40000156b2000000000000000",
>	"0000000000020076aaf55b6edebedd4aed2dbbffa0aade8000000000000000",
>	"000001040000004455bb75755bebf6dadaaff69f00457c8020000000000000",
>	"0000000108002d29edfaaeaef6bf5d2bb6a95aeba5596d0204008100400000",
>	"0004000001004a92b5b5595adf75f5557d5577bf40d37e8000200000010800",
>	"00000000000015456dfeb6b575dfad56eb55ad6b2154d50000000000000000",
>	"010000000005ea1ba9baad6daf76faad5edb6b5d56d57c0100000000000000",
>	"00000000000974a2d576aed55adfb6abfbb55aaba551768000000000000000",
>	"0000810001f74aafabff555ef7baedaaaeeff5545b55dc0000000000000000",
>	"00000000015a5559557aadb55aefbaab7bb5555556a57c8000000008000000",
>	"0000000008b75556d5feab6befb6f6d5aefefaaaadaaf50110000000000000",
>	"00800000f7ad52bbb5bf6d5eb57b5bbeebabadad5b52bc0000080000000000",
>	"000020002f6b556ead7edaeadbadf7eb5edf7f55b6d5748001021000000000",
>	"0000005ff9aaaad575bfd6ad6d7ebd5fd36aeaff5daada0000000000000000",
>	"0000001b56d55dbfaabeb55bbbabfffab6bebfaaf755748000000000000000",
>	"00002776ddb56b695d6fabf6d776ad576de9557faed2ddb500000200000000",
>	"000009adb6aaaed6eabfdaadb6ed7ffdd556b7eb5b6aaaa400000004000000",
>	"00000b7b6b555ab559bfebbb6b5bad6b75f56d5ef6d576a920000000200000",
>	"002ff6d6def6f56fb57ff555def577deeeaeab52adf5d55555000000020000",
>	"000adbedb5adafb56d5ff56fbb56defbb5daae955bbabc9548400000008000",
>	"17ff6ebb6aaab56edbfffe9d76ddbbd6feaaa96ad576d155555a9000002000",
>	"25aadd6ddb556eddaabffc5bddb6efbbdb755694aff5b6aaaaa40000000000",
>	"0777ebb776b6db6b6dfff5ad776dbef77dad6aa55aad545555514000000000",
>	"fdaebedaadadb6ddb53fffaaeedb6bedef6aad5ab7f6d5aaaaad5f64000000",
>	"eeddeb6dfb556b6bdaf7fed3b5b6deb77bd6abadaffd8a5555555040000800",
>	"5b6b5ef6aebbdeed7b3ffff4ded56ffdff7ddd777df6b5aaaaab6dbda80000",
>	"6dddb5ddfdd6b5dbd6bfffe4b5b55d57ddeb6bdd57ff45555aad5a55200000",
>	"b6b76f6baaad6f6ababfffeadb7577fd7f5edebbdf755adad55aaab5480000",
>	"efedfadeff7bdadeeaaefffe5edbaeb7f7f5b56d7fdd56b5b6b76febb7d200",
>	"dabb5775a9d6b76b5abffffcabbd6dfdddffebdbdbf255aeaaaad55d5d5000",
>	"b7d6fdeefebdeddeedafbfff5577b6afff6d5ebeffedb555b6adbeebb6ff80",
>	"ed7db75babab5bb5b56fffff56dddb7bb7ffebf7f7b5556d6d6b535d6dae00",
>	"dbebfdf6fd76ed7edb5fefffd5af6eeefdef5d5dbdeb555bab5aedb6dbdf80",
>	"b75f575b57adb7d5beebffffeb75fb5fef7ef7ffffaab5555ad75ad5b6b578",
>	"6dfaedfdfd76dd6f755ffefffadedefbbfdbdaab6ed5ab76b5bad7eadaefd0",
>	"bbafb75757ddb7badbadfffffd25b7aef7feb7fffbd56aab6ed6ba3f775d6c",
>	"eefdfdfdfebb6d6fb6afeffffeaeaddbdff7fb6fff555ad5a9bd6aeadaebb4",
>	"bdd7db5755f6dfdaed6ffffffa4adf77777d57fbb5d5ab5b5776dd5bb75f6c",
>	"f77d7efbfead6abdb6adfbffffdb6bdefddffd7fed5576b6eccaaab6daeaf4",
>	"bfefebd76bbbbdd6ddafff7fffd6db75dff7abfff555556db5b76edb77bb5c",
>	"7abebebdded6eb7bb755ffdffff8b6df777ddfadb556adaad6aaadb6ed6ee8",
>	"effbf7ebbbbdddaf6eeffffffff55d5aefefaaff6ab5755b6d6f5b5b5db5dc",
>	"bdaf5dbf76d6b77adb55ffffffe976efbafedfadeaaaaad6db5aadb6eadeb4",
>	"f7fdf7eaedbbedd7b6d5fff7fffe4b94afd5f7e0156edeb4b6d7776ddfb5ec",
>	"5d6fbd5fdf6d5b7eefabfffffffeabeb5ebb5be95ad575aaaaaadadeb56eb4",
>	"f7db77f56af7efab5aeafffdffffaa5ad2afffe0175baf6dad56b56b6ab5dc",
>	"db7eed5fdf5ab5def755ffffffffd5b6b4ab57e0ad555adb5b6dd55ddf6f74",
>	"ffdbbff575efff6b5df5dfffdffffc00013dfee80aafeb55555abb776ab5dc",
>	"d5feeadfefbaaaf6eb55fffffffff6aa802b5be016d55eafb757f56edddb74",
>	"ffbbbff55af7ff5d5fb57fff7ffffc84215df7e89dadf5daaaaaaaddb6b6dc",
>	"5aeef55ff7aed5ebeaebfffff7fffd9144b77faa135b2eb55777756b6d6fb4",
>	"f7fffffb6efb7fbeb75dffffffffff45293adff40eb6d5ab6adadb5edadd6c",
>	"fed55b6fb5afd575eed37dfffdfffc9401df7d8b25edab6aedb6b6b5b7abb4",
>	"dbfff7feef7dbfef5deddffffffffd290577efba8b2aaed55b5b6d6edd76dc",
>	"ffaadedbb5ebd55aeb5bfffffdfffc5201bebfd50aefaaaaeaf6b55b76bb74",
>	"56fffbbefabf7ff7bdd4ffffffbff501206fee9757556d7db6addbb6ededac",
>	"ffeaaeebadf5eadad771fbfffefffc0200bbffad0aaedbab6aaaad6db6aba8",
>	"aadfffbef75edfbf7fad3fffffefd090402fad4ba5e9b576dab6b6db6f6eb4",
>	"f7faaafddafbbaf5d5eaffffffffd204002eff9b5556aaad6d5b6badb5daa8",
>	"dad7ffd7bfadf75f6fb57fbfffffb040800fae9725b5addbb5aead7edb56d4",
>	"fffd757d7577adf5baeaafffffde8292280dfd9ba55576b56f69dbab6eef58",
>	"aab7ffefdfdedbdfedda7fffffff8120900bbe97a955adeb5a5b56ddb5d5b4",
>	"fffd557abb75ef7ab76aaffffffd52540601fd1b46f6db5edbaab5b6ef5ad4",
>	"f7b7ffdf6ddfbaeffdb57ffffffe90a81405fc9751adb5b5ad56ab6bbab5b4",
>	"dd7daaf6df75efdeabdaafeffff7a5641509fd17a6d5576b76aaaddd6dead4",
>	"bfefff7dabdfbd75ff6b7ffffff45ab48a017c9fa576dadead55db6bdb5bbc",
>	"f6daadef7ff577bfbadaaffffffd75d41281fe2b65daabb5db6d56deb6aad4",
>	"6fbfff7db6bbeeed6fb66ffbfff05fa0a4017c9fa9b57edb56d56db5edddb4",
>	"faf5fbf7dfeebb5bf57a5fffffd07fa50a00fd17e2d7556d55555ad6ab6b6c",
>	"dfafb6dd757deeff5baa6ffffff02b885201f45f556cdbb6abd56bbb76b574",
>	"76f5fff7fff75baafef69bffffe17fa2bec07d5fe5dbb56d6ab6dd6dadd6ac",
>	"efbf6d7f5b5ff6ffd5fa2fff7fd06ec8de80fd5feb6aaedbd56dabd6b6baa8",
>	"dd75ffd5fdfadfd6bebe4efffec5552afb417abfb55f6b6ab6db7d6ddb65dc",
>	"bbefb5ef6f6fbabdebb62fffffaab54aeec1765fe5555db6d5b6abdab6dab4",
>	"f75edfb5fbfaf7f7bf7e8bbffeaa4d55b9107d7f55b5ab6dbaabb6adadab6c",
>	"5af5effbdfbfaead6ad62ffffb552aab6e417e5ff56b56d5555eedbb5576d8",
>	"ef5fbd5776f6fd7bbfbe8ffffe925592dd447577a5b6ed5b6aeb5b6d5dad54",
>	"bdf577ddffdfabeeeaf60adffa6e855502b57e7ff56d9bb55aadd6db5b5b6c",
>	"eb5fdd7bad75fd5bbfbe8fbffe9a5aaa553574bf65ab756b55aab556d6d558",
>	"bff6f7eeffff6ff6d6f703fffab6854285feed7feadaabdd6eb7dbf55ab6b4",
>	"ed5fbebbb5effb5fbdde8fdffd2ead9525b7745feb75b6b7596ab6ab57ad6c",
>	"fbf5edeeffbbaef5777602fffa5a8b5542fefd7ff5ad6aedabaded6d5aaaa8",
>	"aeafbfbbaaeefbdfaddf8bdbedaf555b15efd57f6aeb5fb56d5b5bdb556dac",
>	"fb7aeadfffbbdeb6ff7e03fffabf4d57477ffa7ff5dd6aaedb76ed56aed6a8",
>	"add77daad5ef77dfaaef83b776adaadd21dfb2dfe56bd6d5b5adb6b575bb54",
>	"ff7ddf57ef7add75dfbb02fbeeaf96ab87f7f57ff5deadbb6f7b6daed6d6d4",
>	"55d776dbddeff7df757e83ef6aaea55721ffa97fe5b5d556dad6db5555b554",
>	"ff7ddfa57bbabd75efef817fbf5bdadd0fdfeb7ff96ebaddb5b5b6b6db6ea8",
>	"add776c9eeffebdf5abe837dfb6e96aa57ffb2fff5db56b76f6d6dadb6d5b4",
>	"ff7dffb2bfb6beedf7efd07fdedfa5f696efebdfeab6eddad55b5b55555aa8",
>	"d5d7dbe4eddffbfeddbd817fffefaaad57bfd37ffadddb6dbdb6b6b6abb554",
>	"7f7d7fd2bffaaeab77efd17f7ebb4bb517efb5ffeab6b6b6ab6d6dad6d5768",
>	"dbd7eb7d376ffbffeeb5b07fffff956aaffbab7ffdefaddafedbea55ab7554",
>	"7efd5df4affb57ad5befd25fffefabaa5bdfd3dfeabadbb75bb6b55556ad54",
>	"abb7f7af55aefd7bfebdf07ffffe8af48effd7fffd776d6af76dd4b6aadae8",
>	"fdfebefb4bfbb7ef6bf7785fffffabaa5fefabfff5ddbed6daf6b55555b754",
>	"576bebd797777d7dbeafd03fefbe857517dfd6fbf56b6bbdb75b756ad56ad8",
>	"fdbebd7f69ded6d7edfdf45ffbff03805fffafffeabedd6b6dddc556955ab4",
>	"d7ebf7db45fbffbf77abf81ffff602b40eefabfffaab6b56df77595aaab2ac",
>	"7d7f5d7ff92f5afddd77745bffdd49015ffeeffffaf6defd69dd556d55ad68",
>	"d7daf7eb4adff7577fddf41ffff680441fff9bfffdbdb5abb76b555b5576d4",
>	"feffdebfe8adbdffd6ebfc1ffffd2a9157dfaffffabb6b5d6dfd5576aaadb4",
>	"55d57deb7ebff75b7fbd5c17fec154009fffefbffab7b6ebfaababaaa95554",
>	"ff6ff7bfdb2b5dffeaf7fc9fffd455245ffeaffffabedabd576e555aaa2db4",
>	"55faaeeafefff76adfad7d13ff6154009fef5bfffaab6d56fab5abaaaadb54",
>	"ffaffdbfdbaabfbff6fedd17fec8a9505fffafddfebdb6df5f6eab6aaa96b4",
>	"d57d6bf6beffeaf5adab7e8bfb3614001ffb6ffffd6b6d6aead92add556d54",
>	"bfd7febff7ab7feffbfddd93fea948485fff5ffffabedadbbdb6ad554aab54",
>	"f57d55ed5dfdd6bad7577f0bfa5612009ffed7ffff55b76d6b752b5a952ad4",
>	"6fd7ff7fb76f7deffdfd6f83fa14845017feaffffabedabbddaed6d6a95558",
>	"bd7d55d5eddbd75eabb7bda3fa2a08005ff75fffff57756d76da95b4aa4ab4",
>	"ebefff7fb77f7df5fefedf82fc1602a01ffeefeffebdafb7edf57b6aaa9554",
>	"bf5db6d6efead7bfb5dbffa3f8260a885ffabbfffed76add5b5b56d554aad4",
>	"eaf77dbfbadfbeeadf7ead83fa0a00801ff75fffffaed6befdead56aa92ab4",
>	"bfadd7eaefb5ebbff5d5ffa2e80a842457fa7ffbff75bdebabb52ed5555554",
>	"eafbfabf5eff7ef76ffeafa1f00100489ffaaffffeafd336ded56daa554ad4",
>	"bfaf5f7bebd7dbadfab77fe2e40441245ffa7ffffffabeedb5badb56a92aa8",
>	"d57af5af5f7efef6dffd6fa3e88190489ff57fffffafeadb6bd56dbaaa5554",
>	"7bafbf7df5ebdbddf577dfc9620001105ffa5ffffefab7b6dd6b5ed5549554",
>	"befef5d7af5f7f775fdebfe2594950041ff0ffffff57ed6df6daab6eaaaaa8",
>	"fbdbef7efbf5d5fdf575d7a0a48050095ff57ffffeecbadb5b6aabdaa924b4",
>	"af6f5dd5aedf7f57bfdf7fe85a00040017f27fffffdb776eefb56e6daa5528",
>	"fafdeb7ffbf5ebfaf576dfe0940200105fd97feffeb6dadbbaedd5db6a9554",
>	"6fd75dd557dfbededfdf7ff45d0048049ff57fffffdb77b6d7dabeb6d4aab4",
>	"dabfb77ffd7aebbbbd75afe00000000017f17ffffff6dd7b7ab5a5adb54a6c",
>	"77eaeeead7d7ffeeebdf7ff4aa0000001ffa7ffffedb75adaf76debb692d58",
>	"dd5f5bb77dfeaabdff756fe0280000489ff17ffffffeef76f5d5b56d5a4b54"
>	]

> colorBit =[
>  "edcfb1ffe2bbf7e2b1ffe2bbffedcfe2c59effe2bbffe2bbffedcfedcfb1ffe2bbedcfa7ffe2bbf7d9b1edcfa7e2c59ee2c59eedcfb1a79e80bba7899e897693806b9e8976a79e80271c00584e3030300030300008080027270093806b9e8980806b62806b626b6b586b6b5862625858584e58584e626258",
>  "f7d9b1edcfb1edcfa7edcfb1edcfb1edcfb1edcfa7ffe2bbf7e2b1c5bb899ebb4effff80c5d94e89a7439ebb4eedcfa7c5b193edcfb1bba7899e898093806b89766276766293806bcfa7804e3000624e00080800302708584e309e89809e8980898076806b626b6b5858584e6b6b5858584e58584e58584e",
>  "cfb193edcfb1ffe2bbedcfb1edcfb1edcfb1edcfb1ffff89a7c562c5d94e93b1629ebb4effff8093b16293b16289a7439ebb4ec5b19393806b93806b93806b806b62767662bb9e43898030806b00080800080800433a1c624e006b6b2793806b6b6b58766b62766b62766b6258584e58584e58584e626258",
>  "edcfb1edcfb1ffedcfedcfb1ffe2bbffe2bb93b1626b8927ffff80b1c56bb1cf4ed9e24effff80edf79ee2f780ffff80d9e24e9eb14393806b767662806b62806b6289803089a743a780276b6212896b126b3000272700623a126b62126b6b58767662766b626b6b5858584e58584e58584e4e4e4358584e",
>  "edcfb1ffedcfffedcfcfb193edcfb1ffff8076933af7f74eb1c56bffffa7ffffa7ffffa7ffffa7ffff89d9ed9389a74376933a6b6b58584e30806b62806b6293806b898030d9d96b6b6212896b00a780270808004e3000c589271c1200271c006b6b586b6b586b6b586b6b586b6b586262584e584e4e584e",
>  "edcfb1ffedcfffe2bbedcfb1e2cf9e76933af7f74e6b892789a743ffff80a7bb08f7ff6b9ebb4ed9ed80a79e8076933a4e763a89a743899e126b80128076587676629eb1436b6212bb9e2776933ad9e24ea776086b6212896b27433a003a4300807658806b626b6b58766b6258584e6b6b5858584e4e4e43",
>  "cfb193ffe2bbedcfb1edcfb1e2f7809ebb4e4e62004e763affff80ffff8076933a89a74376933a76933a6b8927ffffa7ffff80c5d94e6b89274e6b12806b627676629ebb4e303000a78043624e00896b126b62124e6b12fff727624e12cfb16b6b6b586b6b586b6b586b6b586b6b5858584e58584e4e4e43",
>  "c5b193edcfb1edcfb1f7d9b1cfe26bd9d96b4e6b1c4e6208e2ed6b809e274e6b1c6b89279ebb4eedf76bb1cf4e93b13089a7433a4e1c4e763a4e763a3030088076586b6212433a004e6b1c9eb143272700e29e126b6b27a78027272700121200271c0058584e766b6262625858584e58584e58584e626258",
>  "c5b193edcfb1f7d9b19e8062d9cf43c5d94ed9e24e6b89274e6b1cb1cf4e4e763ae2ed6b9eb1434e6b1c3a4e08ffff8993b1309ebb4e4e763a4e6b123a4e0827301c4e3000c58927624e121c1200624e00a78027a77608896b274e3000303000303008766b62766b62766b626b6b5862625858584e58584e",
>  "bba789d9bb93edcfb1898030b1cf4e4e6b126b89274e763a6b6b27c5d94ec5d94ed9e24e4e6b1cedf76b93b130cfe26b4e763a4e763a4e763a9ebb4e3a4e08271c00271c00bb9e43624e00a78043f793001c12001c1200eda727624e12433a00896b27ffffcfffffc5ffffcfffffcfd9bb9362625858584e",
>  "d9bb93cfb193e2c59e6b6b27b1cf4eb1c5274e6b1289a743cfe26bb1cf4e4e6b1c4e6b124e6b1c4e6b1c6b6b273a4e1c4e763aedf79e4e763a8980303a4e080000000000006b6b27e28027303008303008ffbb08624e00896b00ffffe2ffffcfffffcfffffc5ffffcfffffc5ffffc5ffffa7ffffa7fff79e",
>  "bba789cfb193f7d9b14e6b1c809e274e62004e6b126b8927b1c56b93b1624e6b1c3a4e1c4e6b1c3a4e0876933a6b892776933a4e763a3a4e1c3a43003a43003a3a1c080800624e00271c00896b004e6208894e006b6212ffffe2ffffe2ffff80ffffcfffffc5ffffc5ffffcffff780d99e27fff79effffa7",
>  "cfb189cfb193edcfb19ebb4e6b892789a743809e276b6b274e763a8076584e6b1c3a43003a4e084e6b1c3a43006b80126b89273030083a43003a4300272700767662000000624e00ffc527d99e27896b27303000ffffe2ffffc5ffffe2ffffc5f7c54effd962f7c54effd962ffffa7fff79ebb9e27fff79e",
>  "e2c59ed9bb93edcfa7ffffc59eb143b1c527809e273a43004e6b1c6b89273a43004e6b123a4e08271c00271c00434e00433a004e6b123a4300272700271c00766b62080800ffff80e29e12272700433a00c58927ffffe2ffffe2ffffcfffff80fff76bffffa7ffd962fff780f7c54efff79effffa7a78043",
>  "ffffcfffffcfffffe2ffffcf9eb1436b80126b89274e62084e62004e62083a43003a4e084e62086b6b273a4300433a004e62083a4300434e00121200303000ffedcfbbcf301c1200080800a78027121200ffffcfffffe2fff79effcf43ffffa7ffffa7ffffa7ffffa7ffff89ffffa7ffffc5623a12a79376",
>  "ffffcfffffe2ffffe2ffffe2ffd980d9e24ef7f74e809e27b1c527b1c5276b89274e6b126b8012303000623a122727001212002727000000001c1200ffff4ed9d96b1212002727003a4300303000272700ffffcffff780ffffc5ffffc5fff76bfff76bffffc5ffa743edc56bfff79ebba789433a00b19e76",
>  "ffedcfffedcfffedcfffffcfffedcfffff80fff76ba7bb08e2e2276b80004e6b123a4e086b3000ff5827b12700cf4308cf3a12272700ffed12ffff80623a12272700ffffe2ffffcf898030ffffc5ffffa7fff79effffc5ffff80ffff80cf8008ffd962896b12bb9e43edc56b934e1c8980306b6b27626258",
>  "ffffcff7d9a7f7d9b1ffe2bbffff4effff4eeded27eded27fff727586200272700a74e12cf1c00ff5827f71c00ffffcfcf1c00b11c006b6b00898000624e00ffffcfffff80ffffa7ffff80ffffa7ffffa7272700ffffa7fff780cf8008e29e12ffd962c58927584e30584e30806b6227270827301c3a3a1c",
>  "ffe2bbedcfb1ffe2bbcfc508ffff27ffff80ffffa7ffffa7ffffa7ffff27271c00cf3a00b11c00cf3a12b11c00b11c00891200b12700624e00433a00fff76beded27ffff00ffff27d9d900fff76bffff00fff780ffa743a74e12edc56bffd962a77608623a123a3a1c3a3a1c43431c271c002727123a3a1c",
>  "a78043d98062f7d9a7a7bb08ffff4efff727c5cf08e2e227b1c527a7bb006b8012a73a08e25830b127008912006212006212004308004e3000ffed00ffff12ffffa7ffff80939300ffffc5806b00ff8962ff8962ffa743ffff4effff27ff9e27623a12433a00433a1c584e30584e30272708272712271c00",
>  "bb9e27bb9e27e2c59e896b00ffcf43c55830434e006b80126b80124e6200434e006212008912008912006212006212004e30006b1c00896b12899e00ffffcfffffe2e2e227586200ffff80ff9e27ff763aff763aff9e27ffffa7fff727ff9e27cf4e123027083030083a3a1c27301c121208121208271c00",
>  "cf6b4343080030300093431cc5623a6b1c002727002727003a43003030000808003a00006212003a0000430800430800d9cf43fff76bffff4effffc5ffff80eded27ffffc5ffff803a4300ffed12ff5827ff5827ff761cfff727ffc527f77600893008303008303008271c00272712080800080800080800",
>  "8927006b1c00430800893008621200d980621c12002727001c1200080800121200ff4308ff5827ff43083a4300ffffa7f77600f77600a77608bb9e43ffffa7434e00bb9e27898000ffa743ffbb12ff761cff6b08ff6b08ff9308cf7600894e00303000271c00271c003a3a1c271c00080800000000121208",
>  "a78043d98062272700430800a780439358306b30003a4300271c00623a12ffb189cf1c00ff5827f71c00fff76bfff727ff761cff761cff3000cf8008cfc508d9cf43303000080800a78027ff8012ff9312ff761cff6b08cf6b00cf6b006b3000272700121200121200272708080800121200000000121208",
>  "c55830623a12d9cf43303000271c00ff89620808000808000000006b1c00a74e12621200cf1c008912006b8012a77608fff79eff3000cf27006b1c006b6b006b6b00b1a7891212003a3a1cff763aff761cff6b08ff6b08ed8008cf4e12624e001c1200080800271c00271c00080800080800000000121208",
>  "3a000027270058584e1c1200623a128927006b3000121200f79e766b1c00584e303a0000621200ff3000ff5827430800f71c00ff3000a73a083030003a3a1c27301c0000004e4e43271c00a74e12f74308ff6b08ff8012e28027894e00433a00000000000000080800121200080800000000000000000000",
>  "934e276b3000cf6b43935830000000430800623a12935830430800430800272712272700f74308ff761cff4308934e27ed4312b11c0030300008080027301c76767627301c121208272712121200623a12894e08cf4308a74e126b3000000000271c00080800080800121208080800000000000000000000",
>  "893008621200b14e27893008433a1c271c00584e3000000000000058584e3a3a1c3a3a1cf71c00ff4308cf1c00623a12584e30edcfb10808004e584e584e304e584e4e584e0000009393934e584e0000003a0000271c00433a1c433a1c3a3a1c584e30584e3027301c080800080800000000000000000000",
>  "584e306b6b586b6b58433a1c000000c5b1930000001212000000000000000808000808004308006b1c00121200271c006b6b58807658807658806b6293939327301c93939327271276767627301c121200807658584e30584e3043431c4e4e438976623a3a1c3a3a1c3a3a1c27301c272712080800121208",
>  "584e30584e306b6b273a3a1c0000003a3a1c0808003a3a1c0808000000006262580000008076589e8976f7d9b1cfa780f7d9b1d9bb93ffe2bbffedcf93806b8076588076584e4e434e584e121208e2c59ee2c59eedcfa7f7d9b1d9bb93cfb193f7e2b1e2c59e807658767662806b623a3a1c2727123a3a1c"]



\end{hide}

\HRule

%\epsfig{file=Wirth.ps}

\begin{center}
%if grafiken
\perform{ generate "TutBit" 1 (scale 5 figBit ||| box "test mit Text") }
%else
\epsfig{file=TutBit.1}
%endif
\end{center}
}}


\end{document}

