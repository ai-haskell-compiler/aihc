%% 
%% FMP Manual (German)
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

% overlay erzeugt Var
%514.12user 61.66system 10:11.88elapsed 94%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (31347major+170044minor)pagefaults 0swaps

% overlay erzeugt keine Var
%503.50user 61.41system 9:56.18elapsed 94%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (31064major+169775minor)pagefaults 0swaps

% Pfad neu
%493.23user 57.88system 9:44.19elapsed 94%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (31063major+171916minor)pagefaults 0swaps

% bullet und dot mit Shape
%533.49user 70.38system 10:39.54elapsed 94%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (34940major+171807minor)pagefaults 0swaps

%Pfade und Terme mit PP
%738.34user 72.55system 14:21.70elapsed 94%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (33910major+168330minor)pagefaults 0swaps

%eigener PP mit langen Zeilen
%391.46user 72.10system 8:21.12elapsed 92%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (35208major+168812minor)pagefaults 0swaps

%eigener PP mit gekuerzten Zeilen
%466.33user 72.63system 9:32.75elapsed 94%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (34759major+170025minor)pagefaults 0swaps

%eigener PP mit global gekuerzten Zeilen
%527.17user 73.71system 10:34.65elapsed 94%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (34719major+170140minor)pagefaults 0swaps

%eigener PP mit global gekuerzten Zeilen in showDoc
%502.46user 73.08system 10:09.13elapsed 94%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (34868major+170122minor)pagefaults 0swaps

%ohne Laden von Hugs
%401.24user 7.76system 7:05.07elapsed 96%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (17232major+68621minor)pagefaults 0swaps

%ohne Laden von Hugs und Aufruf von MP (d.h. reine Rechenzeit)
%336.27user 3.60system 5:54.28elapsed 95%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (195major+2751minor)pagefaults 0swaps

%0.67 Rechenzeit
%0.20 Laden v hugs
%0.13 MP

%418.99user 7.60system 7:35.94elapsed 93%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (17242major+71177minor)pagefaults 0swaps


%438.82user 7.83system 7:48.91elapsed 95%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (17210major+66805minor)pagefaults 0swaps

%506.92user 71.87system 10:27.47elapsed 92%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (35282major+170593minor)pagefaults 0swaps

%ohne derived Show,Read,Eq
%339.60user 7.21system 6:08.80elapsed 94%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (17231major+59718minor)pagefaults 0swaps
%424.51user 71.14system 8:46.61elapsed 94%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (34709major+169500minor)pagefaults 0swaps

%318.80user 7.44system 5:34.26elapsed 97%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (17319major+61171minor)pagefaults 0swaps


%269.00user 8.83system 4:54.45elapsed 94%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (17557major+107377minor)pagefaults 0swaps

%204.13user 3.54system 3:35.41elapsed 96%CPU (0avgtext+0avgdata 0maxresident)k
%0inputs+0outputs (148major+8392minor)pagefaults 0swaps


\usepackage{a4wide}
\usepackage[german,english]{babel}
\usepackage{latexsym}
\usepackage{mflogo}
\usepackage{color}
\usepackage{float}
\usepackage{wrapfig}
\usepackage{mflogo}
\usepackage{floatflt}
\makeatletter
\usepackage{amstext}
\usepackage{amssymb}
\usepackage{epsfig}
\usepackage{ulem}\normalem
\usepackage{fancyheadings}

\pagestyle{fancyplain}
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
\newfont{\Pretty}{pretty10}

\newcommand{\Sp}{\hskip.33334em\relax}
\usepackage{times}\renewcommand{\ttdefault}{cmtt}
\SetMathAlphabet{\mathrm}{normal}{OT1}{ptm}{m}{n}

\SetMathAlphabet{\mathbf}{normal}{OT1}{ptm}{bx}{n}
\SetMathAlphabet{\mathit}{normal}{OT1}{ptm}{m}{it}

\newlength{\lwidht}\setlength{\lwidht}{4.5cm}
\newlength{\cwidth}\setlength{\cwidth}{8mm} % 3mm

\newcommand{\Conid}[1]{\mathit{#1}}
\newcommand{\Varid}[1]{\mathit{#1}}

%\newcommand{\anonymous}{\kern0.06em \vbox{\hrule\@width.5em}}
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
\newcommand{\beside}[2]{\parbox[c]{#1\linewidth}{\begin{center}#2\end{center}}}
\newcommand{\HRule}{\rule{\linewidth}{0.5pt}\vspace{10pt}}
\newcommand{\VRule}[1]{\hspace{10pt}\beside{0.01}{\rule{0.5pt}{#1}}}

\definecolor{lightGrey}{gray}{.9}

\newfloat{tabelle}{tbhp}{lop}[subsection]
\floatname{tabelle}{Tabelle}
\floatplacement{tabelle}{H}
\newcommand{\Tabelle}[2]{\begin{tabelle}\caption{#1}\begin{center}
\label{#1}
\colorbox{lightGrey}{#2}\end{center}\end{tabelle}}

\newfloat{abbildung}{tbhp}{lop}[subsection]
\floatname{abbildung}{Abbildung}
\floatplacement{abbildung}{H}
\newcommand{\Abbildung}[2]{\begin{abbildung}\caption{#1}\begin{center}
\label{#1}
\setlength{\fboxsep}{16pt}
\fbox{#2}\end{center}\end{abbildung}}


\newfloat{beispiel}{tbhp}{lop}[subsection]
\floatname{beispiel}{Beispiel}
\floatplacement{beispiel}{H}
\newcommand{\Beispiel}[2]{\begin{beispiel}\caption{#1}\begin{center}
\label{#1}
\setlength{\fboxsep}{16pt}
\fbox{\small #2}\end{center}\end{beispiel}}

\newcommand{\BeispielWrap}[2]{%
\begin{floatingfigure}[r]{#1}
    \setlength{\fboxsep}{0pt}
    \fbox{\small #2}
\end{floatingfigure}}



%\newcommand{\addtoprog}[2]{\newcommand{#1}{\protect #2}\addcontentsline{prg}{program}{\protect#1}}
%\newcommand{\l@@program}[2]{\par\noindent#1 {\itshape #2}}
%\newcommand{\listofprog}{\@@starttoc{prg}}


\begin{document}

\makeatother
\title{\texttt{\FMP} \\
       Eine Beschreibungssprache f"ur Grafiken\\
       User's manual}
\author{Joachim Korittky}


\selectlanguage{german}

%if False

> module Main where
> import FMPTurtle
> import FMPTree
> import FMPCanvas
> import FMPMatrix
> import FMPFrames

> import FMPMpsyntax
> import FMPFile
> import System

%endif

\maketitle

\tableofcontents
\newpage

%format \	= "$\setminus$"

\section{Einf"uhrung}



\section{Kernfunktionen}

\subsection{Text}

Text ist eins der Grundelemente von \FMP und wird mit der Funktion
|text :: String -> Picture| erzeugt. Dies definiert ein Bild mit einer Boundingbox
und neun zugeh"origen Punkten |C,| |N,| |NE,| |E,| |SE,| |S,| |SW,| |W,| |NW| wie 
in Abbildung \ref{Boundingbox eines Textes und Bezugspunkte} zu sehen.


\Abbildung{Boundingbox eines Textes und Bezugspunkte}{
%if grafiken
\perform{ generate "bound" 1 textBounding1 }
%else
\epsfig{file=bound.1}
%endif
}


|text :: String -> Picture| eignet sich aber nur f"ur einzelne Zahlen 
oder W"orter, den Funktionsumfang von \TeX\ erh"alt man mit dem Befehl 
|tex :: String -> Picture|.
\footnote{Da \MP jedoch f"ur jeden |tex|-Befehl \TeX\ startet,
um die Gr"o"se der Textbox zu ermitteln, ist |text| etwas effizienter.}
Da \TeX\ jedoch vom "`Backslash"' |\| regen Gebrauch macht, und
dieser in Zeichenketten Sonderzeichen einleitet, ist |tex "\large A"|
als |tex "\\large A"| zu notieren.
Gleiches gilt auch f"ur den Befehl |math :: String -> Picture|, der 
die |$|-Zeichen am Anfang und Ende des Strings einspart, die              %$
\LaTeX\ in die Mathe-Umgebung umschalten.


Ebenfalls n"utzlich, wenn dies auch vielleicht erst sp"ater deutlich wird,
ist ein Punkt |bullet|, der "ubrigens wie alle Bilder die Bezugspunkte 
|C,N ..| ebenfalls besitzt. Was es mit den Bezugspunkten genau auf sich 
hat, dazu mehr in den folgenden Kapiteln.

Als minimales Bild existiert |nullPic|, das erst in Kombination mit 
einem umgebenden Rahmen sinnvoll wird, weil es keine Ausdehnung hat und 
unsichtbar ist.

\Tabelle{Zeichenprimitive}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Befehl}&erzeugt\\
\hline\hline
|text|		&|:: String -> Picture|&Einzelne W"orter\\
|tex|		&|:: String -> Picture|&Echtes \TeX\\
|math|		&|:: String -> Picture|&\TeX-Matheumgebung\\
|dot,bullet|	&|:: Picture          |&Einen fetter Punkt\\
|nullPic|	&|:: Picture          |&Das leere Bild\\
|trueBox|	&|:: Picture -> Picture|	&Liefert das Bild mit minimaler\\
		&				&Boundingbox, die alles wirklich\\
		&				&umschlie"st.\\
\hline
\end{tabular}
}

Dies sind die Grundelemmente, die sich zusammen mit Linien und Pfaden,
sowie Fl"achenf"ullungen zu komplexen Bildern zusammensetzen lassen.

\begin{hide}

> primitives = matrix [
>	[tex "text \"text\"", tex "yields", text "text"],
>	[tex "text \"text with spaces\"", tex "yields", text "text with spaces"],
>	[tex "tex \"text with spaces\"", tex "yields", tex "text with spaces"],
>	[tex "math \"$\\backslash\\backslash$frac\\{1\\}\\{$\\backslash\\backslash$sqrt\\{x$^\\land$2-1\\}\\}\"", tex "yields", math "\\frac{1}{\\sqrt {x^2-1}}"],
>	[tex "dot", tex "yields", toPicture dot]
>	]

\end{hide}


\Abbildung{noname}{
%if grafiken
\perform{ generate "primitives" 1 primitives }
%else
\epsfig{file=primitives.1}
%endif
}

\subsection{Attributierung}

Die Attributierungen erm"oglichen eine Einflu"snahme auf Attribute,
die eine Plazierung oder das Aussehen von Objekten steuern. Sie sind
die Mittel f"ur das F"arben von Objekten und das Setzen von Abst"anden.

Weil es verschiedene Objekte gibt, die aber teilweise in gleicher Weise
zu beeinflussen sind, wurden die Funktionen zur Steuerung der Attribute
in sog. Typklassen organisiert.



\Tabelle{"Ubersicht der m"oglichen Attributierungen}{
{\small
\begin{tabular}{||l||l||l||l||l||l||l||||l||l||l||}
\hline
Attribut	&Klasse		&Picture	&Frame	&Path	&PathElemDescr	&Area	&Tree	&Edge&Turtle\\
\hline\hline
setName		&HasName	&X		&	&	&		&	&	&	&\\
getNames	&HasName	&X		&	&	&		&	&	&	&\\
\hline
setDX		&HasDXY		&		&X	&	&		&	&	&	&\\
getDX		&HasDXY		&		&X	&	&		&	&	&	&\\
setDY		&HasDXY		&		&X	&	&		&	&	&	&\\
getDY		&HasDXY		&		&X	&	&		&	&	&	&\\
\hline
setWidth	&HasExtent	&		&X	&	&		&	&	&	&\\
removeWidth	&HasExtent	&		&X	&	&		&	&	&	&\\
getWidth	&HasExtent	&		&X	&	&		&	&	&	&\\
setHeight	&HasExtent	&		&X	&	&		&	&	&	&\\
removeHeight	&HasExtent	&		&X	&	&		&	&	&	&\\
getHeight	&HasExtent	&		&X	&	&		&	&	&	&\\
\hline
setColor	&HasColor	&X		&X	&X	&X		&X	&	&X	&X\\
getColor	&HasColor	&X		&X	&X	&X		&X	&	&X	&X\\
\hline
setBGColor	&HasBGColor	&X		&X	&	&		&	&	&	&\\
getBGColor	&HasBGColor	&X		&X	&	&		&	&	&	&\\
\hline
setLabel	&HasLabel	&		&	&X	&X		&	&	&X	&\\
removeLabel	&HasLabel	&		&	&X	&X		&	&	&X	&\\
\hline
setPattern	&HasPattern	&		&X	&X	&X		&	&	&X	&\\
removePattern	&HasPattern	&		&X	&X	&X		&	&	&X	&\\
getPattern	&HasPattern	&		&X	&X	&X		&	&	&X	&\\
\hline
setPen		&HasPen		&		&X	&X	&X		&X	&	&X	&X\\
setDefaultPen	&HasPen		&		&X	&X	&X		&X	&	&X	&X\\
getPen		&HasPen		&		&X	&X	&X		&X	&	&X	&X\\
\hline
setArrowHead	&HasArrowHead	&		&	&X	&X		&	&	&X	&\\
removeArrowHead	&HasArrowHead	&		&	&X	&X		&	&	&X	&\\
getArrowHead	&HasArrowHead	&		&	&X	&X		&	&	&X	&\\
setStartArrowHead&HasArrowHead	&		&	&X	&X		&	&	&X	&\\
removeStartArrowHead&HasArrowHead&		&	&X	&X		&	&	&X	&\\
getStartArrowHead&HasArrowHead	&		&	&X	&X		&	&	&X	&\\
\hline
setStartCut	&HasStartEndCut	&		&	&X	&X		&	&	&X	&\\
removeStartCut	&HasStartEndCut	&		&	&X	&X		&	&	&X	&\\
setEndCut	&HasStartEndCut	&		&	&X	&X		&	&	&X	&\\
removeEndCut	&HasStartEndCut	&		&	&X	&X		&	&	&X	&\\
\hline
setStartAngle	&HasStartEndDir&		&	&X	&X		&	&	&X	&\\
setStartCurl	&HasStartEndDir&		&	&X	&X		&	&	&X	&\\
setStartVector	&HasStartEndDir&		&	&X	&X		&	&	&X	&\\
removeStartDir	&HasStartEndDir&		&	&X	&X		&	&	&X	&\\
setEndAngle	&HasStartEndDir&		&	&X	&X		&	&	&X	&\\
setEndCurl	&HasStartEndDir&		&	&X	&X		&	&	&X	&\\
setEndVector	&HasStartEndDir&		&	&X	&X		&	&	&X	&\\
removeEndDir	&HasStartEndDir&		&	&X	&X		&	&	&X	&\\
\hline
setJoin		&HasJoin	&		&	&X	&X		&	&	&	&\\
getJoin		&HasJoin	&		&	&X	&X		&	&	&	&\\
\hline
setShadow	&HasShadow	&		&X	&	&		&	&	&	&\\
clearShadow	&HasShadow	&		&X	&	&		&	&	&	&\\
\hline
setBack		&HasLayer	&		&	&	&		&X	&	&	&\\
setFront	&HasLayer	&		&	&	&		&X	&	&	&\\
\hline
setAlign	&HasAlign	&		&	&	&		&	&X	&	&\\
\hline
hide		&IsHideable	&		&X	&X	&		&	&	&X	&X\\
\hline
\end{tabular}
}}


\subsection{Rahmen}

Wendet man die Funktion |box| auf ein Bild an, so erh"alt man dieses
Bild mit einem Rahmen versehen zur"uck.

\begin{hide}

> bsp1 = textBox box
> bsp2 = circBox circle
> bsp3 = triBox triangle
> bsp4 = textBox oval
> bsp0 = t bsp1 |||| t bsp4 |=| t bsp3 |||| t bsp2
> 	where t = TrueBox

\end{hide}

%if grafiken

\Abbildung{dx}{
\perform{ generate "text" 1 bsp0 }
}
%else
\Abbildung{dx}{
\epsfig{file=text.1}
}
%endif



\Tabelle{Rahmentypen}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Befehl}&erzeugt\\
\hline\hline
|box|		&|:: IsPicture a => a -> Frame|		&|box p|\\
		&					&| = frame (box' Nothing|\\
		&					&|               Nothing p)|\\
|circle|	&|:: IsPicture a => a -> Picture|	&|circle p|\\
		&					&| = frame (circle' Nothing p)|\\
|oval|		&|:: IsPicture a => a -> Picture|	&|oval p|\\
		&					&| = frame (oval' Nothing|\\
		&					&|                Nothing p)|\\
|triangle|	&|:: IsPicture a => a -> Picture|	&|triangle p|\\
		&					&| = frame (triangle' Nothing|\\
		&					&|            Nothing Nothing p)|\\
|frame,|&&\\
|toPicture|&|:: Frame -> Picture|			&\\
\hline
|(##)|		&|:: Picture -> (Frame -> Frame)|	&|(Frame a) ## f = frame (f a)|\\
		&| -> Picture|				&|a ## _         = a|\\
|dot,bullet|	&|:: Picture|	&|dot = circle nullPic|\\
		&		&|      ## setBGColor black|\\
		&		&|      ## setDX 1|\\
\hline
\end{tabular}
}



\subsubsection{Schatten}

\Tabelle{Schatten}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Befehl}&erzeugt\\
\hline\hline
|setShadow|	&|:: (Double, Double) -> Picture -> Picture|&Schatten\\
\hline
\end{tabular}
}

\subsection{Farben}

Objekten wie Bildern, Pfaden, Pfadst"ucken, F"ullfl"achen und Turtlegrafiken
kann man eine Farbe zuweisen. Bilder besitzen noch zus"atzlich eine Hintergrundfarbe,
die aber nur bei Boxen gezeichnet wird. Die Wirkung der Vordergrundfarbe ist
in kanonischer weise gegeben; au"ser, da"s sie sich bei Boxen auf die
Umrandung bezieht.

Als Farbmodell kommt das RGB-Modell zum Einsatz, d.h. |color 1 0 0| entspricht der
Farbe rot, |color 0 1 0| gr"un und |color 0 0 1| blau.\\
Einige Farben sind schon vordefiniert:

\Tabelle{Vordefinierte Farben}{
\begin{tabular}{||l||l||}
\hline
Konstante&Wert\\
\hline\hline
 white		& color 1 1 1\\
 black		& color 0 0 0\\
 red		& color 1 0 0\\
 green		& color 0 1 0\\
 blue		& color 0 0 1\\
 yellow		& color 1 1 0\\
 cyan		& color 0 1 1\\
 magenta	& color 1 0 1\\
\hline
\end{tabular}
}

Weiterhin kann man Farben addieren, subtrahieren und sogar multiplizieren
und dividieren (n"utzlich um eine Farbe etwas abzudunkeln oder aufzuhellen). 
Konstanten vom Typ |Int| oder |Integer| weden automatisch in eine Graustufe 
umgewandelt, so da"s |0.5*green| ein mittleres Gr"un ergibt.

Ein weiteres "`Schmankerl"' sind Farbverl"aufe, die in drei Qualit"atsstufen
(Low, Med, High) zur Verf"ugung stehen \footnote{Die Funktion
|graduate :: Color -> Color -> Angle -> Quality -> Color| ist noch flexibler.}.
|graduateMed red blue 10| definiert einen Farbverlauf mit $64$ Abstufungen von 
rot nach blau mit einer um $10$ Grad gedrehten Achse.
\footnote{Bekommt die Funktion f"ur einen Farbverlauf |graduate..| als Parameter 
wiederum einen Farbverlauf, also z.B. |graduateMed red 
(graduateMed blue black 0) 10|, so bestimmt sich der Farbwert des Parameters 
aus seiner ersten Farbe, also im Beispiel ein Farbverlauf von |red| nach |blue|.}


\begin{hide}

\Beispiel{Farbverl"aufe}{
\beside{0.75}{

> bild3	= box (tex "Farbverlauf" 
>		# setColor (graduateLow red blue 10)
>		)
>		# setDX 10
>		# setDY 10
>		# setBGColor (graduateLow white 0.4   10)
>		# setColor   (graduateMed 0.9   black 90)
>		# setPen 3

}
%if grafiken
\beside{0.24}{
\perform{generate "text" 3 bild3}
}
%else
\beside{0.24}{
\epsfig{file=text.3}
}
%endif
}

> colors = matrix [
>	[tex "\\parbox{250pt}{tex \"text\"\\\\ \\# setColor green}",
>	tex "yields", tex "text" # setColor green],
>	[tex "\\parbox{250pt}{tex \"text\"\\\\ \\# setColor white\\\\ \\# setBGColor black}",
>	tex "yields", tex "text" # setColor white # setBGColor black],
>	[tex "\\parbox{250pt}{tex \"text\"\\\\ \\# setColor (graduateLow white black 0)\\\\ \\# setBGColor (graduateLow white black 90)}",
>	tex "yields", tex "text" # setColor (graduateLow white black 0) # setBGColor (graduateLow white black 90)],
>	[tex "\\parbox{250pt}{scale 2 (dot \\# setColor (graduateLow white black (-30)))}",
>	tex "yields", scale 2 (dot # setColor (graduateLow white black (-30)))],
>	[tex "\\parbox{250pt}{box \"text\"\\\\ \\# setColor (graduateLow white black (-30))}",
>	tex "yields", toPicture (box "text" # setColor (graduateLow white black (-30)))]
>	]

\end{hide}

\Abbildung{Farbattributierungen und Farbverl"aufe}{
%if grafiken
\perform{ generate "colors" 1 colors }
%else
\epsfig{file=colors.1}
%endif
}



\BeispielWrap{0.49\textwidth}{
\beside{0.42}{

> from0 = line (ref W - vec (50,0)) (ref C)
>	# setArrowHead arrowHeadBig
>	# setPen 2
>	# setColor (graduateMed white black 0)

}
}
Ein Farbverlauf erstreckt sich automatisch "uber hintereinanderliegende 
Pfadsegmente, wenn diese die gleiche Farbe haben.
Aber noch eine Warnung: Farbverl"aufe sollten immer ganz sp"arlich eingesetzt 
werden, denn erstens lenkt zuviel Schnickschnack vom Wesentlichen ab, und
zweitens vergr"o"sert ein Farbverlauf die PS-datei, verlangsamt
das Drucken sowie nicht zuletzt den Bildaufbau beim Bl"attern am Bildschirm.
Abstand nehmen sollten Sie vor allen Dingen von gestrichelten Pfaden mit
Farbverlauf, die bei extensivem Gebrauch schnell zu einem Speicher"uberlauf 
in \MP\ f"uhren.

Trotzdem sind Effekte wie das Auftauchen eines
%if grafiken
\perform{ generate "text" 4 from0 }%
%else
\epsfig{file=text.4}%
%endif
Pfeiles aus dem Nichts ganz h"ubsch.

\subsection{Punkte und Numerics}

\Beispiel{Punktdefinitionen}{
\beside{0.65}{

> punkte= markPoint p6 "p6" 
>	(markPoint p2 "p2" 
>	(markPoint p3 "p3"
>	(markPoint p4 "p4" 
>	(markPoint p5 "p5"
>	(markPoint p1 "p1"
>		(toPicture (box (tex "box"
>			# setColor 0.7 
>		)) # setName "box"))))))
>
>	where 
>	p1	= ref ("box" <+ C)
>	p2	= ref ("box" <+ NW)
>	p3	= ref ("box" <+ NW) + vec(-width "box", 0)
>	p4	= ref ("box" <+ SE) + dist p1 p2 .* dir (-45)
>	p5	= xy p3 p4
>	p6	= med 0.333 p5 p4
>	script l= tex ("\\scriptsize "++l)
>	markPoint p l
>		= constraint p C
>			(dot # label N (script l))
>	constraint p d l p'
>		= Overlay [ p .= ref (0 <* d) ] (Just 1) [l, p']

}
\VRule{250pt}
%if grafiken
\beside{0.24}{
\perform{ generate "punkte" 1 punkte }%
}
%else
\beside{0.24}{
\epsfig{file=punkte.1}%
}
%endif
}



\Tabelle{Namensbildung f"ur Objekte}{
\begin{tabular}{||l||l||}
\hline
Ausdruck&benennt\\
\hline\hline
|ref C|			&Zentrum des letzten Bildes\\
|ref ("a" <+ N)|		&Punkt |N| des Bildes |a|\\
|ref ("a" <+ (1::Int) <+ N)|	&Punkt |N| des Bildes 1\\
				&innerhalb eines Bildes |a|\\
				&(Die Zahlen werden automatisch durch\\
				&|overlay| Operationen vergeben.)\\
|var C|				&numerische Variable mit Namen |C|\\
|var ((1::Int) <+ (0::Int))|	&numerische Variable |0| im Bild |1|\\
\hline
\end{tabular}
}


\Tabelle{Definition von Numerics}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Ausdruck}&Bedeutung\\
\hline\hline
|var|		&|:: IsName a => a -> Numeric|	&Numerische Variable\\
|xpart|		&|:: Point -> Numeric|		&$x$-Anteil des Punktes\\
|ypart|		&|:: Point -> Numeric|		&$y$-Anteil des Punktes\\
|width|		&|:: IsName a => a -> Numeric|	&|width s|\\
		&				&| = xpart (ref (toName s <+ E))|\\
		&				&| - xpart (ref (toName s <+ W))|\\
|height|	&|:: IsName a => a -> Numeric|	&|height s|\\
		&				&| = ypart (ref (toName s <+ N))|\\
		&				&| - ypart (ref (toName s <+ S))|\\
|dist|		&|:: Point -> Point -> Numeric|	&Abstand zwischen den Punkten\\
|xdist|		&|:: Point -> Point -> Numeric|	&Differenz der $x$-Anteile der Punkte\\
|ydist|		&|:: Point -> Point -> Numeric|	&Differenz der $y$-Anteile der Punkte\\
|med|		&|:: Numeric -> Numeric|	&\\
		&| -> Numeric -> Numeric|&\\
|maximum'|	&|:: [Numeric] -> Numeric|&\\
|minimum'|	&|:: [Numeric] -> Numeric|&\\
|(+), (-), (*),|&&\\
|(/),(**)|	&|:: Numeric -> Numeric -> Numeric|&Wie gewohnt\\
\multicolumn{2}{||l||}{|negate, abs, signum,|}	&\\
\multicolumn{2}{||l||}{|recip, exp, log, sqrt,|}&\\
|sin,cos,tan|	&|:: Numeric -> Numeric|	&\\
|fromInteger|	&|:: Integer -> Numeric|	&\\
|fromInt|	&|:: Int -> Numeric|		&\\
|fromRational|	&|:: Rational -> Numeric|	&\\
|pi|		&|:: Numeric|			&\\
\hline
\end{tabular}
}


\newpage

\Tabelle{Definition von Punkten}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Ausdruck}&definiert Punkt\\
\hline\hline
|ref|		&|:: IsName a => a -> Point|	&Punktvariable\\
|dir|		&|:: Numeric -> Point|		&|dir a|\\
		&				&| = vec (cos a, sin a)|\\
|vec|		&|:: (Numeric, Numeric) -> Point|&Erzeuge Vektor\\
|xy|		&|:: Point -> Point -> Point|	&|xy p1 p2|\\
		&				&| = vec (xpart p1, ypart p2)|\\
|med|		&|:: Numeric -> Point -> Point|	&\\
		&| -> Point|			&\\
|(+), (-), (*), (/)|&|:: Point -> Point -> Point|&Komponentenweise\\
|(.*)|		&|:: Numeric -> Point -> Point|	&Multiplikation mit Skalar\\
\multicolumn{2}{||l||}{|negate, abs, signum,|}	&\\
|recip, exp, log, sqrt,|&|:: Point -> Point|	&\\
|fromInteger|	&|:: Integer -> Point|&$i\leadsto(i, i)$\\
|fromInt|	&|:: Int -> Point|		&\\
|fromRational|	&|:: Rational -> Point|		&\\
\hline
\end{tabular}
}


\subsection{Plazierungen}

Mit den Primitiven |text| und |dot| allein kann man kein komplexes
Bild aufbauen. Deshalb gibt es vielf"altige M"oglichenkeiten aus mehreren
Bildern ein neues zu konstruieren und auf die relative Plazierung
der Einzelbilder Einflu"s zu nehmen.

\Tabelle{Plazierungen}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Befehl}&positioniert Bilder\\
\hline\hline
|(||-||)|	&|:: (IsPicture a, IsPicture b) => a|	&|p1 ||-|| p2|\\
		&| -> b -> Picture|			&|  = column [toPicture p1, toPicture p2]|\\
|(||=||)|	&|:: (IsPicture a, IsPicture b) => a|	&|p1 ||=|| p2|\\
		&| -> b -> Picture|			&| = column' 8 [toPicture p1, toPicture p2]|\\
|(||||||)|	&|:: (IsPicture a, IsPicture b) => a|	&|p1 |||||| p2|\\
		&| -> b -> Picture|			&| = row  [toPicture p1, toPicture p2]|\\
|(||||||||)|	&|:: (IsPicture a, IsPicture b) => a|	&|p1 |||||||| p2|\\
		&| -> b -> Picture|			&| = row' 8 [toPicture p1, toPicture p2]|\\
|row|		&|:: IsPicture a => [a] -> Picture|	&|row = row' 0|\\
|column|	&|:: IsPicture a => [a] -> Picture|	&|column = column' 0|\\
|rowSepBy|		&|:: IsPicture a => Numeric -> [a]|	&|row' hSep ps|\\
		&| -> Picture|		&| = overlay [ ref (i <+ E) + vec(hSep,0)|\\
		&			&|                .= ref ((i+1) <+ W)|\\
		&			&|               || i <- [0..length ps - 2 ]] ps|\\
|columnSepBy|	&|:: IsPicture a => Numeric -> [a]|	&|column' vSep ps|\\
		&| -> Picture|		&| = overlay [ ref (i <+ S) - vec(0, vSep)|\\
		&			&|                .= ref ((i+1) <+ N)|\\
		&			&|               || i <- [0..length ps - 2 ]] ps|\\
|label|		&|:: Dir -> Picture -> Picture|	&\\
		&| -> Picture|			&Plaziere Label neben Bild\\
|overlay|	&|:: IsPicture a => [Equation] -> [a]|	&|overlay eqs ps =|\\
		&| -> Picture|				&| overlay' eqs Nothing ps|\\
|overlay'|	&|:: IsPicture a => [Equation]|	&\\
		&| -> Maybe Index -> [a] -> Picture|	&Gleichungen geben Beziehungen\\
		&				&zwischen Bildern an, Bilder\\
		&				&der Liste haben Namen "0","1"\ldots.\\
		&				&Index gibt an, da"s die neue\\
		&				&Bounding-box die des n-ten Bildes\\
		&				&ist. Nothing hei"st minimale\\
		&				&umschlie"sende Box.\\
\hline
\end{tabular}
}


\Beispiel{Automat (a)}{
\beside{0.9}{

> automat1 = constraint (ref ("B" <+ N) + vec(0,30)) S 
>		((toPicture $ circle (math "C\\atop b^*"))		# setName "C")
>	 	(rowSepBy 16 [(toPicture $ oval "Start") 		# setName "start",
>		(toPicture $ circle (math "{B\\atop (a||b)^*a}"))	# setName "B",
>		(toPicture $ circle (math "{D\\atop (a||b)^*ab}"))	# setName "D",
>		(toPicture $ oval "Stop")	 			# setName "stop"
>		])
>	where
>	constraint p d l p'
>		= overlay' [ p .= ref ((0::Int) <+ d) ] (Just 1) [l, p']

> automat3 = matrixSepBy 30 20 
>	[[empty, toPicture (circle (math "C\\atop b^*")) # setName "C"],
>	 [toPicture (oval "Start") # setName "start",
>	  toPicture (circle (math "{B\\atop (a||b)^*a}"))	# setName "B",
>	  toPicture (circle (math "{D\\atop (a||b)^*ab}"))	# setName "D",
>	  toPicture (oval "Stop")	 			# setName "stop"
>		]]


\rule{\linewidth}{0.5pt}
\vspace{10pt}

%if grafiken
\perform{ generate "automat" 1 automat1 }%
%else
\epsfig{file=automat.1}%
%endif

}
}

\subsubsection{Overlays}

\Tabelle{Gleichungen}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Ausdruck}&Bedeutung\\
\hline\hline
|.=|			&|:: Numeric -> Numeric -> Equation|&Gleichheit von Zahlen\\
|many|			&|:: [Numeric] -> Equation|&Gleichheit mehrerer Zahlen\\
|.=|			&|:: Point -> Point -> Equation|&Gleichheit von Punkten\\
|many|			&|:: [Point] -> Equation|&Gleichheit mehrerer Punkte\\
|ifBoolean b t e|	&|:: Boolean -> [Equation] -> [Equation]|&\\
&| -> Equation|&Bedingte Gleichheit\\
\hline
\end{tabular}
}

\Tabelle{Boolsche Werte}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Ausdruck}&Bedeutung\\
\hline\hline
|(.==)|		&|:: Numeric -> Numeric -> Boolean|&\\
|(./=)|		&|:: Numeric -> Numeric -> Boolean|&\\
|(.<)|		&|:: Numeric -> Numeric -> Boolean|&\\
|(.<=)|		&|:: Numeric -> Numeric -> Boolean|&\\
|(.==)|		&|:: Point -> Point -> Boolean|&\\
|(./=)|		&|:: Point -> Point -> Boolean|&\\
|(.<)|		&|:: Point -> Point -> Boolean|&\\
|(.<=)|		&|:: Point -> Point -> Boolean|&\\
|boolean|	&|:: Bool -> Boolean|&\\
|(+), (-), (*)|	&|:: Boolean -> Boolean -> Boolean|&$a\lor b, a\land\neg b, a\land b$\\
|negate, abs, signum|&|:: Boolean -> Boolean|	&$\neg a, \text{True}, \text{id}$\\
|fromInteger|	&|:: Integer -> Boolean|	&$i>0$\\
|fromInt|	&|:: Int -> Boolean|		&$i>0$\\
\hline
\end{tabular}
}



\Beispiel{Kreis mit overlay}{
\beside{0.6}{

> kreis 	:: (IsPicture a) => Numeric -> [a] -> Picture
> kreis r ps	= overlay
>		[ ref (j <* C) - r .* dir (d*fromInt j)
>		.= ref (j+1 <* C)
>			- r .* dir (d*(fromInt j+1)) 
>		|j <- [0..l-2]]
>		ps
>		where 
>		l	= length ps
>		d	= 360 / fromInt l
> kreis7 	:: Picture
> kreis7 	= kreis 40
>		[ draw 	[ line (ref C)
>			   (ref (mod (i+j) 7 <* C))
>			   # setArrowHead (default' 
>					# setArrowHeadStyle ahLine)
>			| j <-[1..6::Int]]
>			((toPicture) i # setName i)
>		| i <- [0..6::Int]]

}
\VRule{200pt}
%if grafiken
\beside{0.22}{
\perform{ generate "kreis" 1 kreis7 }%
}
%else
\beside{0.22}{
\epsfig{file=kreis.1}%
}
%endif
}



\Beispiel{Schnittpunkt mit overlay}{
\beside{0.7}{

> drei = overlay [	
>	ref (1 <* C)	.= ref (0 <* C) + vec(40,0),
>	ref (2 <* C)	.= ref (0 <* C) + whatever * dir (-45),
>	ref (1 <* C)	.= ref (2 <* C) + whatever * dir 50,
>	ref (2 <* C)	.= ref (3 <* C) + ref (0::Int),
>	ref (3 <* C)	.= ref (0 <* C) + ref (0::Int) ]
>	[text "0", text "1", text "2", toPicture bullet]

}
\VRule{90pt}
%if grafiken
\beside{0.15}{
\perform{ generate "drei" 1 drei }%
}
%else
\beside{0.15}{\epsfig{file=drei.1}}
%endif
}

\Beispiel{Spirale}{
\beside{0.5}{

> spirale 
>  = overlay [	ref (0 <* C) .= ref (1 <* C),
>		ref (0 <* C) .= vec (0,0)]
>	[draw [foldl (...) (toPath (vec (0,0))) punkte] 
>		(circle empty
>		# setDX (12*r)),
>	zz]
>  where
>  r	    = 2
>  zz	    = kreis (12*r)
>		(take 10 (cycle [bullet]))
>  punkte   = [(i*r*pi) .* dir (i*(180/12))
>	      | i <- [0..12]]

}
\VRule{140pt}
%if grafiken
\beside{0.3}{
\perform{ generate "spiral" 1 spirale }%
}
%else
\beside{0.3}{
\epsfig{file=spiral.1}%
}
%endif
}
  	
\Beispiel{Klammern mit overlay}{
\beside{0.95}{

> brack = setTrueBoundingBox brack1 |||| setTrueBoundingBox brack2
> 	where
>	brack1	= [bracket x (vec(0,   x*5), vec(x*5, x*5)) | x <- ws]
>		++[bracket x (vec(x*5, x*5), vec(x*5, 0)) | x <- ws]
>		where
>		ws = [1, 3] ++ [5, 10 .. 20]
>	brack2	= [bracket x (5 .* dir x, 60 .* dir x) | x <- ws]
>		where
>		ws = [0, 45 .. 360-45]

> bracket :: IsPicture a => a -> (Point, Point) -> Path
> bracket l (pl, pr)
>	= define [
>	ref "start"	.= pl,
>	ref "end"	.= pr,
>	var "ang"	.= angle (ref "start"-ref "end"),
>	var "d"		.= cond (dist (ref "start") (ref "end") .< 20)
>				(dist (ref "start") (ref "end")/4)
>				5,
>	ref "vecl"	.= var "d" .* dir (var "ang"-135),
>	ref "vecr"	.= var "d" .* dir (var "ang"-45),
>	ref "start2"	.= ref "start" + ref "vecl",
>	ref "end2"	.= ref "end" + ref "vecr",
>	ref "mid"	.= med 0.5 (ref "start") (ref "end")
>				+ (1.41 * var "d")
>				.* dir (var "ang"-90),
>	ref "midl"	.= ref "mid" - ref "vecl",
>	ref "midr"	.= ref "mid" - ref "vecr" ]
>		  (pl .... ref "start2" .--. ref "midl" .... ref "mid"
>	       .&. ref "mid" .... ref "midr" .--. ref "end2" .... pr
>		   # setPen (penCircle (0.001, var "d"/5) (var "ang"))
>		   # setLabel 0.5 C lab)
>	where 
>	lab	= overlay'
>			[var "ang"	.= angle (pl-pr),
>			ref (0 <* C) .= cond (var "ang" .< (-175.5) 
>						 + 175.5 .< var "ang")
>							(ref (1 <* S))
>	 		(cond (var "ang" .< (-112.5))	(ref (1 <* SE))
>			  (cond (var "ang" .< (-67.5))	(ref (1 <* E))
>			   (cond (var "ang" .< (-22.5))	(ref (1 <* NE))
>			    (cond (var "ang" .< 22.5)	(ref (1 <* N))
>			     (cond (var "ang" .< 67.5)	(ref (1 <* NW))
>			      (cond (var "ang" .< 112.5)(ref (1 <* W))
>				 			(ref (1 <* SW))
>			 ))))))]  (Just 0)
>		[empty, toPicture l]


\HRule

%if grafiken
\perform{ generate "bracket" 1 brack }
%else
\epsfig{file=bracket.1}
%endif
}}

\subsection{Transformationen}

\Tabelle{Vordefinierte Transformationen}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Befehl}&erzeugt\\
\hline\hline
|scale|		&|:: Angle -> Picture -> Picture|&|scale n p = transform (scaled n) p|\\
|rotate|	&|:: Angle -> Picture -> Picture|&|rotate a p|\\
		&				&| = transform (rotated a) p|\\
|transform|	&|:: Transformation -> Picture|	&\\
		&| -> Picture|			&Wende Transformation auf Bild an.\\
\hline
|rotated|	&|:: Angle -> Transformation|		&Erzeugt RotationsTransformation\\
|reflectedX|	&|:: Transformation|			&|reflectedX = affine ( 1, 0, 0, -1)|\\
|reflectedY|	&|:: Transformation|			&|reflectedY = affine (-1, 0, 0, 1)|\\
|scaled|	&|:: Double -> Transformation|		&|scaled a   = affine ( a, 0, 0, a)|\\
|scaledX|	&|:: Double -> Transformation|		&|scaledX a  = affine ( a, 0, 0, 1)|\\
|scaledY|	&|:: Double -> Transformation|		&|scaledY a  = affine (1, 0, 0, a)|\\
|affine|	&|:: (Double,Double,Double,Double)|&\\
		&| -> Transformation|			&Transformationsmatrix\\
|(&)|		&|:: Transformation|&\\
		&| -> Transformation|&\\
		&| -> Transformation|			&Sequenz zweier Transformationen\\
\hline
\end{tabular}
}


\subsection{Pfade}

\Tabelle{Pfade}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Ausdruck}&Bedeutung\\
\hline\hline
|(&)|		&|:: Path -> Path -> Path|	&Konkateniere Pfade\\
|(...)|		&|:: Path -> Path -> Path|	&Verbinde Pfade mit B\'ezier\\
|(.-.)|		&|:: Path -> Path -> Path|	&Verbinde Pfade mit Linie\\
|(....)|	&|:: Path -> Path -> Path|	&Verbinde Pfade mit B\'ezier stetig\\
|(.--.)|	&|:: Path -> Path -> Path|	&Verbinde Pfade mit Linie stetig\\
|cycle'|	&|:: Path|			&Erzeuge Zyklus\\
|line|		&|:: Point -> Point -> Path|	&|line p1 p2|\\
		&				&| = z' p1 .-. z' p2|\\
|curve|		&|:: Point -> Point -> Path|	&|curve p1 p2|\\
		&				&| = z' p1 ... z' p2|\\
\hline
|pathLength|	&|:: Num a => Path -> a|&\\
|forEachPath|	&\multicolumn{2}{l||}{|:: (PathElemDescr -> PathElemDescr)|}\\
		&| -> Path -> Path|		&\\
|pathSetEnd|	&\multicolumn{2}{l||}{|:: (PathElemDescr -> PathElemDescr)|}\\
		&| -> Path -> Path|		&\\
|pathGetEnd|	&|:: (PathElemDescr -> a)|	&\\
		&| -> Path -> a|		&\\
|pathGetStart|	&|:: (PathElemDescr -> a)|	&\\
		&| -> Path -> a|		&\\
|pathSetStart|	&\multicolumn{2}{l||}{|:: (PathElemDescr -> PathElemDescr)|}\\
		&| -> Path -> Path|		&\\
\hline
|cutPic|	&|:: Name -> CutPic|		&Name eines Bildes an dem eine\\
		&				&Linie abgeschnitten wird.\\
|hier|		&|:: Name -> CutPic -> CutPic|	&Hierarchischer Name.\\
|setStartCut|	&|:: CutPic -> Path -> Path|	&Schneide Pfadsegment hinter bbox\\
|removeStartCut|&|:: Path -> Path|		&Schneide Pfadsegmentanfang nicht\\
|setEndCut|	&|:: CutPic -> Path -> Path|	&Schneide Pfadsegment vor bbox\\
|removeEndCut|	&|:: Path -> Path|		&Schneide Pfadsegmentende nicht\\
\hline
|draw|		&|:: [Path] -> Picture|		&Zeichnet Pfade in Bild\\
		&| -> Picture|&\\
\hline
\end{tabular}
}





\Beispiel{Automat (b)}{
\beside{0.9}{

> automat2 = draw [
>		loopN "C"		# setLabel 0.5 S (math "b"),
>		loopSW "B"		# setLabel 0.5 N (math "a"),
>		to "start" "B" "a" S,
>		to "C" 	   "B" "a" W,
>		to "B" "D" "b" S,
>		to "D" "stop" "b" S,
>		to "start" "C" "b" SE	# setStartAngle 55,
>		to "stop" "C" "b" SW	# setStartAngle 135,
>		to "stop" "B" "a" N	# setStartAngle (-125),
>		to "D"    "B" "a" N	# setStartAngle (-145)
>		]
>	    	automat1
>	where
>	to a b l d	= curve (ref (a <+ C)) (ref (b <+ C))
>			# setArrowHead default'
>			# setLabel 0.5 d (math l)
>	loopN s		= ref (s <+ NE)
>			... ref (s <+ N) + vec(0, 0.5*width s)
>			... ref (s <+ NW)
>			# setArrowHead default'
>	loopSW s	= ref (s <+ SW)
>			... ref (s <+ S) + vec(-0.353*width s, 
>						     -0.353*width s)
>			... ref (s <+ S)
>			# setArrowHead default'

\rule{\linewidth}{0.5pt}
\vspace{10pt}

%if grafiken
\perform{ generate "automat" 2 automat2 }%
%else
\epsfig{file=automat.2}%
%endif

}
}

\subsubsection{Pfeile}

\Beispiel{Pfeilvarianten}{
\beside{0.55}{

> pfeil	= cdraws [f (line 	(vec (0,-fromInt y*16))
>				(vec (40,-fromInt y*16))) 
>		 | (y, f) <- zip [0..] fs ]
>     |=| (box (cdraw (curve	(vec (0,0))
>				(vec (40,0))
>		       # setEndAngle 60
>		       # setStartAngle 60
>		       # setArrowHead (arrowHeadSize 10 40) 
>		       # setStartArrowHead (arrowHeadSize 10 40 # ahToLine)
>			))
>	   # setBGColor white
>	   # setShadow (5,-5))
>	where
>	doubleAr ar 	= setArrowHead ar 
>			. setStartArrowHead ar
>	ahToLine	= setArrowHeadStyle AHLine
>	fs 		= [	id, 
>				doubleAr default',
>				doubleAr (arrowHeadSize 10 20),
>				doubleAr (arrowHeadSize 5 250),
>				doubleAr (default' # ahToLine),
>				doubleAr (arrowHeadSize 10 20  # ahToLine),
>				doubleAr (arrowHeadSize 5 180  # ahToLine),
>				doubleAr (arrowHeadSize 5 250  # ahToLine)
>			]

}
\VRule{250pt}
\beside{0.15}{
%if grafiken
\perform{ generate "arrow" 1 pfeil }
%else
\epsfig{file=arrow.1}
%endif
}}


\Tabelle{Pfeile}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Ausdruck}&Bedeutung\\
\hline\hline
|arrowHead|		&|:: PathArrowHead|&Normaler Pfeilkopf\\
|arrowHeadBig|	&|:: PathArrowHead|&Dickerer Pfeilkopf\\
|arrowHeadSize|	&|:: Double -> Double -> PathArrowHead|&Pfeilkopf mit L"ange, Winkel\\
|oarrowHead|	&|:: PathArrowHead|&Normaler Pfeilkopf Umri"s\\
|oarrowHeadBig|	&|:: PathArrowHead|&Dickerer Pfeilkopf Umri"s\\
|oarrowHeadSize|&|:: Double -> Double -> PathArrowHead|&Pfeilkopf mit L"ange, Winkel Umri"s\\
\hline
\end{tabular}
}

\subsection{Fl"achen}

\Tabelle{Fl"achen}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Ausdruck}&Bedeutung\\
\hline\hline
|area|		&|:: [Point] -> Area|	&Konstruiert Fl"achenobjekt\\
&&(gerade Kanten)\\
|toArea|	&|:: Path -> Area|	&Wandelt Pfad in Fl"achenobjekt\\
|setColor|	&|:: Color -> Area -> Area|&Setze Farbe\\
|getColor|	&|:: Area -> Color|	&Ermittle Farbe\\
|setPen|	&|:: Pen -> Area -> Area|&Setze Stift\\
|getPen|	&|:: Area -> Pen|	&Ermittle Stift\\
|setBack|	&|:: Area -> Area|	&Fl"ache erscheint unter Grafik\\
|setFront|	&|:: Area -> Area|	&Fl"ache "uberdeckt Grafik\\
\hline
|fill|		&|:: [Area] -> Picture -> Picture|&\\
\hline
\end{tabular}
}


\subsection{Clipping}

\Tabelle{Clipping}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Ausdruck}&Bedeutung\\
\hline\hline
|clip|&|:: Path -> Picture -> Picture|&Schneide Bild au"serhalb des Pfades ab\\
\hline
\end{tabular}
}


\Beispiel{Clipping}{
\beside{0.6}{

> clipping = column [	a, vspace (-5), math "\\oplus", vspace (-5),
>			b, math "\\downarrow", on [a, b]]
>	where
>	on ps = overlay [ ref (i <* C) .= ref ((i+1) <* C) 
>			| i <- [0..length ps-2]] ps
>	t = box "clip" 
>	a = clip (ref NW .-. ref NE
>		.-. ref SW .-. cycle') 
>		(t # setBGColor 0.8)
>	b = clip (ref SW .-. ref NE
>		.-. ref SE .-. cycle') 
>		(t # setColor white
>		   # setBGColor black)

}
\VRule{70pt}
%if grafiken
\beside{0.15}{
\perform{ generate "clip" 1 clipping }%
}
%else
\beside{0.15}{
\epsfig{file=clip.1}%
}
%endif
}

\Beispiel{Pac Man}{

\beside{0.7}{

> pac = clip 	(   ref SE + vec(0,15)
>		.-. ref C
>		.-. ref NE - vec(0,15)
>		... ref W
>		... cycle')
>		(matrixSepBy 0 0 (take 10 (
>			repeat (take 5 (
>				repeat "pac")))))

}
\VRule{60pt}%
\beside{0.25}{
%if grafiken
\perform{ generate "pacman" 1 pac }
%else
\epsfig{file=pacman.1}
%endif
}}


\subsection{Strichmuster}

\Tabelle{Strichmuster}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Ausdruck}&Bedeutung\\
\hline\hline
|dashed|	&|:: Pattern|&Gestrichelt |[3, 3]|\\
|dotted|	&|:: Pattern|&Gepunktet |[-1, 2.5, 0, 2.5]|\\
|dashPattern|	&|:: [Double] -> Pattern|&Liste mit L"ange f"ur aus, an, aus..\\
&&wenn erste Zahl = -1, dann an, aus, an..\\
\hline
\end{tabular}
}

\subsection{Stifte}

\Tabelle{Stifte}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Ausdruck}&Bedeutung\\
\hline\hline
|penCircle|	&|:: Double -> Pen|&Runder Stift mit Radius\\
|penCalli|	&|:: (Double,Double) -> Double -> Pen|&Ovaler Stift gedreht\\
|(+), (-), (*), (/)|&|:: Pen -> Pen -> Pen|&Wirkt auf runde Stifte\\
\multicolumn{2}{||l||}{|negate, abs, signum,|}&\\
|recip, exp, log, sqrt,|&|:: Pen -> Pen|&Wirkt auf runde Stifte\\
|fromInteger|	&|:: Integer -> Pen|	&Erzeugt runden Stift\\
|fromInt|	&|:: Int -> Pen|	&Erzeugt runden Stift\\
|fromRational|	&|:: Rational -> Pen|	&Erzeugt runden Stift\\
\hline
\end{tabular}
}


\section{Anwendungen}

\subsection{B"aume}


\Beispiel{Alignment}{
\begin{tabular}{c}
\beside{0.6}{

> tree1 = node dot [	edge (node dot [enode dot []]
>			      # setAlign AlignRightSon),
>			edge (node dot [enode dot []] 
>			      # setAlign AlignLeftSon)
>			]


}
\VRule{60pt}
%if grafiken
\beside{0.20}{
\perform{ generate "tree" 1 tree1 }%
}
%else
\beside{0.20}{
\epsfig{file=tree.1}%
}
%endif
\\


\beside{0.6}{

> tree2 = node2 [ edge2	(node2
>			[ edge2s (node2 
>				[upToRoot
>				 # setPattern dashed
>				 # setEndAngle 0
>				 # setStartAngle 130]),
>			  edge2s (node2 []),
>			  edge2s (node2 [])] 
>			 # setAlign alignRight),
>		  edge2 (node2 	
>			[ edge2 (node2 []),
>			  edge2 (node2 [upToRoot])]
>			 # setAlign alignLeft)
>			]
>	where
>	node2	= node dot
>	edge2	= edge' (line (ref (This <+ C)) (ref (Parent <+ C)))
>	edge2s	= edge' (stair (ref (This <+ C)) (ref (Parent <+ C)))
>	upToRoot= cross' (curve (ref (This <+ C)) (ref (Up 1 <+ C))
>			 # setStartAngle (90)
>			 # setArrowHead default')

}
\VRule{80pt}
%if grafiken
\beside{0.2}{
\perform{ generate "tree" 2 tree2 }%
}
%else
\beside{0.2}{
\epsfig{file=tree.2}%
}
%endif
\\


\beside{0.6}{

> tree3 = tree2
>	# setDistH 30

}
\VRule{40pt}
%if grafiken
\beside{0.20}{
\perform{ generate "tree" 3 tree3 }%
}
%else
\beside{0.20}{
\epsfig{file=tree.3}%
}
%endif
\\

\beside{0.6}{

> tree4 = forEachNode
>		(setDistH 10)
>		tree2

}
\VRule{40pt}
%if grafiken
\beside{0.20}{
\perform{generate "tree" 4 tree4}%
}
%else
\beside{0.20}{
\epsfig{file=tree.4}%
}
%endif
\end{tabular}
}


\Tabelle{B"aume}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Ausdruck}&Bedeutung\\
\hline\hline
|edge| 		&|:: Tree -> Edge|&|edge t|\\
		&		&| = edge' (line (ref This)|\\
		&               &|               (ref Parent)) t|\\
|edge'|		&|:: Path -> Tree -> Edge|&spezielle Kante\\
|cross|		&|:: Point -> Edge|&|cross p|\\
		&		&| = cross' (line (ref This) p)|\\
|cross'|	&|:: Path -> Edge|&Querkante\\
|enode| 	&|:: Picture -> [Edge]|	&|enode p ts|\\
		&| -> Edge|		&| = edge (node p ts)|\\
|node| 		&|:: Picture -> [Edge] -> Tree|&normaler Knoten\\
|toPicture|	&|:: Tree -> Picture|&Konvertierung\\
\hline
|stair|		&|:: Point -> Point -> Path|	&|stair p1 p2|\\
		&				&|  = z' p1|\\
		&				&|.-. z' (p1 + vec (0, 0.5*distY p2 p1))|\\
		&				&|.-. z' (p2 - vec (0, 0.5*distY p2 p1))|\\
		&				&|.-. z' p2|\\
		&				&Treppenstufe, n"utzlich f"ur B"aume\\
\hline
\end{tabular}
}



\Tabelle{Plazierung der S"ohne}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Ausdruck}&Bedeutung\\
\hline\hline
|DefaultAlign|		&|:: AlignSons|	&S"ohne m"oglichst dicht\\
|AlignLeft|		&|:: AlignSons|	&Wenn ein Sohn, dann Zweig nach links,\\
			&&sonst |DefaultAlign|\\
|AlignRight|		&|:: AlignSons|	&dito rechts\\
|AlignLeftSon|		&|:: AlignSons|	&Vater "uber linkem Sohn\\
|AlignRightSon|		&|:: AlignSons|	&dito rechts\\
|AlignOverN|		&|:: Int -> AlignSons|	&Vater "uber $n$-tem Sohn\\
			&			&($0=$ links)\\
|AlignAngles|		&|:: [Double] -> AlignSons|&Liste von Winkeln. Wenn mehr S"ohne\\
			&			&als Winkel, dann packe Rest dicht\\
|AlignConst|		&|:: Double -> AlignSons|&Konstanter Abstand zwischen S"ohnen;\\
			&			&kann zu "Uberlappung f"uhren\\
|AlignFunction|		&|:: (NodeDescr -> [Extent] -> Int|&\\
			&| -> [Numeric]) -> AlignSons|&\\
\hline
\end{tabular}
}


\Tabelle{Spezielle Attributierung f"ur B"aume}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Ausdruck}&Bedeutung\\
\hline\hline
|setDistH|	&|:: Separation -> Tree -> Tree|&Horiz. Abstand zwischen S"ohnen\\
|getDistH|	&|:: Tree -> Separation|&\\
|setDistV|	&|:: Separation -> Tree -> Tree|&Vert. Abstand zum Vater\\
|getDistV|	&|:: Tree -> Separation|&\\
|setAlign|	&|:: AlignSons -> Tree -> Tree|&Plazierung der S"ohne\\
|getAlign|	&|:: Tree -> AlignSons|&\\
\hline
\end{tabular}
}


\Tabelle{Knotenabst"ande}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Ausdruck}&Bedeutung\\
\hline\hline
|sepBorder|	&|:: Numeric -> Separation|&Abstand zwischen Bounding Boxen\\
|sepCenter|	&|:: Numeric -> Separation|&Abstand zwischen Zentren\\
|(+), (-), (*)|	&|:: Separation -> Separation|&\\
		&| -> Separation|&\\
|negate, abs, signum|&|:: Separation -> Separation|&\\
|fromInteger|	&|:: Integer -> Separation|	&$i \leadsto $|sepBorder |$i$\\
|fromInt|	&|:: Int -> Separation|		&$i \leadsto $|sepBorder |$i$\\
\hline
\end{tabular}
}


\Tabelle{Platzhalter f"ur Bilder bestimmter Knoten}{
\begin{tabular}{||l||l||}
\hline
Name&Bezeichnet\\
\hline\hline
|Parent|		&Aktuellen Vaterknoten\\
|This|		&Aktuellen Knoten\\
|Root|		&Wurzel\\
|Up 1, Up 2, ..|	&Knoten auf dem Weg zur Wurzel\\
|Son 0, Son 1, ..|	&S"ohne von links nach rechts\\
\hline
\multicolumn{2}{||l||}{
\parbox{0.8\linewidth}{|line (ref This) (ref Parent)| bezeichnet eine
Kante vom aktuellen Knoten zum Vaterknoten.
|line (ref This) (ref Root)| eine Kante zur Wurzel.
Kanten d"urfen beliebige Pfade sein.}}\\
\hline
\end{tabular}
}


\Tabelle{Hilfsfunktionen}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Ausdruck}&Bedeutung\\
\hline\hline
|forEachNode|		&| :: (Tree -> Tree) -> Tree -> Tree|	&Wende Fkt. auf jeden\\
			&					&Knoten an.\\
|forEachLevelNode|	&| :: (Tree -> Tree) -> Int -> Tree -> Tree|&Wende Fkt. auf jeden\\
			&					&Knoten einer bestimmten\\
			&					&Tiefe an ($0=$ Wurzel)\\
|forEachPic|		&| :: (Picture -> Picture) -> Tree -> Tree|&Wende Fkt. auf alle\\
			&					&Bilder an.\\
|forEachEdge|		&| :: (Path -> Path) -> Tree -> Tree|	&Wende Fkt. auf alle\\
			&					&Kanten an.\\
\hline
\end{tabular}
}

\Abbildung{Gro"ser Baum}{
%if grafiken
\perform{ generate "complex" 1 bsp10 }
%else
\epsfig{file=complex.1}
%endif
}

\subsection{Turtlegrafik}


\Beispiel{C-Kurve zehnter Ordnung}{
\beside{0.70}{

> turtle1	= box (cKurve 10 2)
>			# setBGColor white
>			# setDX 10
>			# setDY 10
>			# setShadow (2,-2)
> cKurve 0 l 	= forward l
> cKurve g l 	= toleft  & cKurve (g-1) l 
>		& toright & cKurve (g-1) l

}%
\VRule{150pt}%
%if grafiken
\beside{0.29}{
\perform{ generate "turtle" 1 turtle1 }%
}
%else
\beside{0.29}{
\epsfig{file=turtle.1}%
}
%endif
}


\Beispiel{Farben und Stifte in Turtlegrafiken}{
\beside{0.55}{

> turtle2 = ( haus rot 20 
>		& home & pu & fw 40 & pd
>		& haus (setColor blue.(haus dach)) 15 
>	) # setPen (penCircle (1,4) 45)
>	  
>	where
>	haus d l= (fw l # setPen 5) & toleft & fw l & toleft 
>		& fw l & turn 180
>		& d l
>		& turn (-45) & fw (l*sqrt(2)) 
>		& turn (-135) & fw l 
>		& turn (-135) & fw (l*sqrt(2)) 
>		& turn (-45)
>	rot l	= dach l 
>		# setColor (graduateMed red 0.9 10)
>	dach l	= turn 45 & fw (0.5*l*sqrt(2))
>		& toright & fw (0.5*l*sqrt(2)) 
>		& turn (-45)
>	fw	= forward
>	pu	= penUp
>	pd	= penDown

}%
\VRule{250pt}%
%if grafiken
\beside{0.2}{
\perform{ generate "turtle" 2 turtle2 }%
}
%else
\beside{0.2}{
\epsfig{file=turtle.2}%
}
%endif
}

\Tabelle{Befehle zur Turtlegrafik}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Befehl}&erzeugt\\
\hline\hline
|turtle, toPicture|&|:: Turtle -> Picture|		&Bild aus Turtlepfad\\
|home|		&|:: Turtle|			&Sprung zu $(0,0)$ Orientierung nach rechts\\
|relax|		&|:: Turtle|			&neutrales Element (tut nichts)\\
|left|		&|:: Turtle|			&Drehung 90 Grad nach links\\
|right|		&|:: Turtle|			&Drehung 90 Grad nach rechts\\
|turn|		&|:: Orientation -> Turtle|	&Drehung (positiv = links)\\
|turnl|		&|:: Orientation -> Turtle|	&Drehung nach links\\
|turnr|		&|:: Orientation -> Turtle|	&Drehung nach rechts\\
|forward|	&|:: Double -> Turtle|		&Schritt in die aktuelle Richtung\\
|backwards|	&|:: Double -> Turtle|		&R"uckschritt in die akt. Richtung\\
|penUp|		&|:: Turtle|			&Hebe Zeichenstift\\
|penDown|	&|:: Turtle|			&Senke Zeichenstift\\
|(&)|		&|:: Turtle -> Turtle -> Turtle|&Konkatenation zweier Turtlepfade\\
|plot|		&|:: [Turtle] -> Turtle|	&Konkatenation mehrerer Turtlepfade\\

\hline
\end{tabular}
}


\subsection{Canvas}

\Tabelle{Befehle zur Canvasgrafik}{
\begin{tabular}{||ll||l||}
\hline
\multicolumn{2}{||l||}{Befehl}&erzeugt\\
\hline\hline
|relax|	&|:: Paint|			&Zeichne nichts\\
|(&)|	&|:: Paint -> Paint -> Paint|	&Zeichne nacheinander\\
|toPicture|&|:: Paint -> Picture|	&\\
|cdrop|	&|:: (Numeric,Numeric) -> Picture|&Plaziert ein Bild an\\
	&| -> Paint|			&Position\\
|cdraw|	&|:: Path -> Paint|		&|cdraw p = cdraws [p]|\\
|cdraws|&|:: [Path] -> Paint|		&Zeichnet Pfade\\
|cfill|	&|:: Area -> Paint|		&|cfill a = cfills [a]|\\
|cfills|&|:: [Area] -> Paint|		&F"ullt Fl"achen\\
|cclip|	&|:: Path -> Paint|		&Schneidet inneres des Pfades aus\\
\hline
\end{tabular}
}

\Beispiel{Farbkreis}{
\beside{0.6}{

> colorcirc	= transform (affine (0.5, 0, 0.1, 0.425, 0, 0))
>		(color 60 
>		(bw 50 
>		(color 41.7 
>		(bw 34.7 
>		(color 28.9
>		(bw 24.1
>		(color 20.1
>		(bw 16.7
>		(color 13.95
>		(bw 11.63 empty))))))))))
>	where
>	color r p= fill (areas r
>			(\i->hsv2rgb (i,1,1))) p
>	bw r p	= fill (areas r
>			(\i->grey (abs(i-180)/180))) p
>	areas r c= [ toArea [	r.*dir (Numeric i),
>				r.*dir (2+Numeric i),
>		    		(1.15*r).*dir (2+Numeric i),
>				(1.15*r).*dir (Numeric i)]
>			# setPen 0.01 
>			# setColor (c i)
>			# setFront
>		           | i<-[0, 4..356]]

}
\VRule{140pt}
%if grafiken
\beside{0.15}{
\perform{ generate "colorcirc" 1 colorcirc }%
}
%else
\beside{0.15}{
\epsfig{file=colorcirc.1}%
}
%endif
}




\Beispiel{Wahl}{
\beside{0.8}{

> barchart = chart [ (35,0.5), (40,0.3+red), (6,green), (3,red-0.3), (6,yellow) ]

> chart bs 	= cfills floor
>		& cdraws (grid 10) 
>		& cfills (bars 0 bs)
>		& labels 0 bs
>	where
>	hSize		= hSep * fromInt (length bs)
>	hSep		= 35
>	width		= 20
>	floor		= [ toArea [	vec (-5, 3), vec(-18, -10),
>					vec (hSize-13, -10), vec(hSize, 3)] 
>				# setColor 0.7 
>				# setPen 1]
>	grid (-1)	= []
>	grid n		= line (vec(-5, n*5+3)) (vec(hSize, n*5+3)):grid (n-1)
>	bars _ [] 	= []
>	bars n (bc:bs)= bar n bc : top  n bc : side n bc : bars (n+1) bs
>	bar n (b,c) = toArea [vec(n*hSep,0),vec(n*hSep+width,0),
>			vec(n*hSep+width,b),vec(n*hSep,b)]
>			# setPen 1
>			# setFront
>			# setColor (c*graduateMed 0.8 0.3 45)
>	top n (b,c) = toArea [vec(n*hSep+width,b),vec(n*hSep,b),
>			vec(n*hSep+3,b+3),vec(n*hSep+width+3,b+3)]
>			# setPen 1
>			# setFront
>			# setColor (c*graduateMed 0.3 0.8 (-45))
>	side n (b,c) = toArea [vec(n*hSep+width,0),vec(n*hSep+width,b),
>			vec(n*hSep+width+3,b+3),vec(n*hSep+width+3,3)]
>			# setPen 1
>			# setFront
>			# setColor (c*graduateMed 0.6 0.2 0)
>	labels _ []	= relax
>	labels n ((b,_):bs)
>			= cdrop (n*hSep+width/2+3,b+10) 
>				(tex (num2String b) 
>				 # setBGColor white)
>			& labels (n+1) bs
>			where
>			num2String (Numeric n)	= "\\tiny "++(show n)++"\\%"
>			num2String _		= ""

\rule{\linewidth}{0.5pt}\\[1em]
%if grafiken
\perform{ generate "barchart" 1 barchart }
%else
\epsfig{file=barchart.1}
%endif
}}


\begin{hide}


> roundNode s ns = enode (n s) ns
> n a		 = circle ("\\tiny "++a)
>		   # setDX 0
>		   # setDY 0


> e=node (n "A")
>	[roundNode "B" [e21,e22], roundNode "F" [e23,e24],
>	roundNode "G" [e25,e26]]
> e21=roundNode "C" [roundNode "E" [roundNode "F" [roundNode "D" []],
>	roundNode "G" [e52], roundNode "L" [e53,e54]]]
> e22=roundNode "D" [roundNode "F" [roundNode "E" [roundNode "C" [],e56,e57]]]
> e23=roundNode "D" [roundNode "B" [roundNode "C" [roundNode "E" [e56,e57]]]]
> e24=roundNode "E" [roundNode "C" [roundNode "B" [roundNode "D" []]],e56,e57]
> e25=roundNode "E" [e37, e38, roundNode "L" [e412,e413]]
> e26=roundNode "H" [roundNode "I" [roundNode "K" [roundNode "J" [e617, e618]]]]
> e52=roundNode "H" [roundNode "I" [roundNode "K" [roundNode "J" 
>	[roundNode "L" [roundNode "M" []],roundNode "M" [roundNode "L" []]]]]]
> e53=roundNode "J" [roundNode "K" [roundNode "I" [roundNode "H" 
>	[roundNode "G" []]]],roundNode "M" []]
> e54=roundNode "M" [roundNode "J" [roundNode "K" [roundNode "I" 
>	[roundNode "H" [roundNode "G" []]]]]]
> e56=roundNode "G" [e52]
> e57=roundNode "L" [e53,e54]
> e37=roundNode "C" [roundNode "B" [roundNode "D" [roundNode "F" []]]]
> e38=roundNode "F" [roundNode "D" [roundNode "B" [roundNode "C" []]]]
> e412=roundNode "J" [roundNode "K" [roundNode "I" [roundNode "H" []]],
>	roundNode "M" []]
> e413=roundNode "M" [roundNode "J" [roundNode "K" [roundNode "I" 
>	[roundNode "H" []]]]]
> e617=roundNode "L" [roundNode "E" [e37,e38],roundNode "M" []]
> e618=roundNode "M" [roundNode "L" [roundNode "E" [e37,e38]]]


> bsp10 = scale 0.5 (forEachNode (setDistH (DistCenter (0.5*cm))) e)


> bound = (tex "Dies ist ein Text" 
>			# setColor 0.7 
>		)
>		# label C  (tex "C")  
>		# label N  (tex "N")  
>		# label NE (tex "NE") 
>		# label E  (tex "E")  
>		# label SE (tex "SE") 
>		# label S  (tex "S")  
>		# label SW (tex "SW") 
>		# label W  (tex "W")  
>		# label NW (tex "NW") 
>		# at C  dot
>		# at N  dot
>		# at NE dot
>		# at E  dot
>		# at SE dot
>		# at S  dot
>		# at SW dot
>		# at W  dot
>		# at NW dot
> textBounding1 = box bound 
>		# setDX 0 
>		# setDY 0



> textBox a	= draw 	[line' N "dy" W, line' S "dy" W,
> 			line' W "dx" S, line' E "dx" S] (freeBox a)
>		where
>		line' d l dl = line (ref d) (ref ("text" <+ d))
>					# setLabel 0.5 dl (tex l) 
>					# setArrowHead default'
>					# setStartArrowHead default'

> triBox a	= draw [line' SW "dx" S, line' SE "dx" S] (freeBox a)
>		where
>		line' d l dl = line (ref d) (xy (ref ("text" <+ d)) (ref d))
>					# setLabel 0.5 dl (tex l) 
>					# setArrowHead default'
>					# setStartArrowHead default'

> circBox a 	= draw [line' W "dx" S, line' E "dx" S] (freeBox a)
>		where
>		line' d l dl = line (ref d) (ref ("text" <+ d)) 
>					# setLabel 0.5 dl (tex l) 
>					# setArrowHead default'
>					# setStartArrowHead default'

> freeBox a	= toPicture (a (tex "Dies ist ein Text"
>					# setColor 0.8
>					# setName "text")
>					# setDX 20
>					# setDY 20)
>						# label C  (tex "C")
>						# label N  (tex "N")
>						# label NE (tex "NE")
>						# label E  (tex "E")
>						# label SE (tex "SE")
>						# label S  (tex "S")
>						# label SW (tex "SW")
>						# label W  (tex "W")
>						# label NW (tex "NW")
>						# at C  dot
>						# at N  dot
>						# at NE dot
>						# at E  dot
>						# at SE dot
>						# at S  dot
>						# at SW dot
>						# at W  dot
>						# at NW dot
>						# setColor 0.6
>		where
>		line' d l dl = line (ref d) (ref ("text" <+ d))
>					# setLabel 0.5 dl (tex l) 
>					# setArrowHead default'
>					# setStartArrowHead default'



> bsp9 = Draw [line (ref ((0::Int) <+ SE)) (ref ((1::Int) <+ C))]
>		((row [tex "1", tex "2", tex "3", tex "4"]
>		# setName "a") |-| tex "1")

> brack0 = draw [bracket "1" (xy (ref ("tre" <+ W)) (ref ("tre" <+ 1 <* C)),
>			xy (ref ("tre" <+ W)) (ref ("tre" <+ 0 <* C))),
>		 bracket "1" (	xy (ref ("tre" <+ W)) (ref ("tre" <+ 2 <* C)),
>			xy (ref ("tre" <+ W)) (ref ("tre" <+ 1 <* C)))]
>	(toPicture (forEachLevelNode 1 (setDistV 30) tree2) # setName "tre") 


\end{hide}

\Beispiel{Chip}{
\beside{0.9}{

> chip	= scale 0.72 (	boxRek (boxRek box1' (setPattern dashed)) (setPattern dotted))
>	where
>	circ	= toPicture (circle empty # setDX 4)
>	tree	= toPicture (node circ [enode circ [enode circ [],enode circ []],
>			enode circ [enode circ [], enode circ []]])
>			# setName (0::Int)
>	pin n p	= p .-. vec (xpart p, ypart (ref N))
>		# setLabel 1 S (toPicture (dot # setDX 2) # setName n)
>	box1	= box (overlay [ref (0 <* N) 
>				.= ref (1 <* N) - vec(0.5*width (0::Int), 0)]
>				[tree, circ # setName "circ"])
>		# setDX 10
>		# setDY 10
>	box1'	= draw [	ref ("circ" <+ SW) .-. ref ("circ" <+ C) + vec(-8,-4),
>				ref ("circ" <+ SE) .-. ref ("circ" <+ C) + vec( 8,-4),
>				pin "a" (ref (0 <* 0 <* C)),
>				pin "b" (ref ("circ" <+ C) + vec(-8,-4)),
>				pin "d" (ref ("circ" <+ C) + vec( 8,-4)),
>				pin "c" (ref ("circ" <+ C))]
>		      box1
>	boxRek b f
>		= draw 	[	pin "d" (ref (1 <* "d" <+ C)),
>				pin "a" (ref (0 <* "c" <+ C)),
>				pin "b" (ref (1 <* "b" <+ C)),
>				pin "c" (ref (1 <* "c" <+ C))]
>		(box (setTrueBoundingBox (
>			draw [	curve (ref (0 <* "a" <+ C)) (ref (0 <* "b" <+ C)) 
>				# setStartAngle 25,
>				curve (ref (0 <* "d" <+ C)) (ref (1 <* "a" <+ C)) 
>				# setStartAngle 25]
>			    ((b # setName (0::Int)) |||| (b # setName (1::Int)))))
>		# setDX 10
>		# setDY 10
>		# f)


\rule{\linewidth}{0.5pt}\\[1em]
%if grafiken
\perform{ generate "chip" 1 chip }%
%else
\epsfig{file=chip.1}%
%endif
}}

\Beispiel{Komplex analytisch}{
\beside{0.9}{

> kompl = matrix [[grid, pow2], [pow3, func]]
>	where
>	grid	 	= scale 7    (plane zId)
>	pow2 		= scale 0.4  (plane zPow2)
>	pow3 		= scale 0.05 (plane zPow3)
>	func 		= scale 0.05 (plane zFunc)
>	z a		= PathPoint (vec a)
>	zId   x y	= z (x, y)
>	zPow2 x y	= z (2*x*y, x*x-y*y)
>	zPow3 x y	= z (x*(x*x-3*y*y), y*(3*x*x-y*y))
>	zFunc x y	= z (2*x*x*y+x*x*x-y*y*x, 2*x*y*y+x*x*y-y*y*y)
>	plane f 	= toPicture (
>				cdraws (map toPath (horiz f))
>			      & cdraws (map toPath (vert f)))
>			where
>			horiz f	= [[f (fromInt x) (fromInt y) 
>				  | x <- [-9..9]] | y <- [-9..9]]
>			vert f	= [[f (fromInt x) (fromInt y) 
>				  | y <- [-9..9]] | x <- [-9..9]]
>	toPath ps 	= (foldl1 (...) ps) # setPen 0.001


\rule{\linewidth}{0.5pt}\\[1em]
%if grafiken
\perform{ generate "kompl" 1 kompl }%
%else
\epsfig{file=kompl.1}%
%endif
}}


\section{Matrix}

\Beispiel{Ausrichtung von Feldern in Tabellen}{
\beside{0.9}{

> matrBsp = matrixAlign
>		 [  [	cell' C (math "\\setminus"),
>			cell' W "linksb\\\"undig",
>			cell' W "mittig",
>			cell' W "rechtsb\\\"undig"],
>		      [	cell' W ("vertikal" |-| "oben"),
>			cell' NW "NW",
>			cell' N "N",
>			cell' NE "NE"],
>		      [	cell' W ("vertikal" |-| "mittig"),
>			cell' W "W",
>			cell' C "C",
>			cell' E "E"],
>		      [	cell' W ("vertikal" |-| "unten"), 
>			cell' SW "SW",
>			cell' S "S",
>			cell' SE "SE"] ]

\rule{\linewidth}{0.5pt}\\[1em]
%if grafiken
\perform{ generate "matrix" 1 matrBsp }%
%else
\epsfig{file=matrix.1}%
%endif
}}

\section{Parameter}

\Tabelle{Parameter in der Datei .FuncMP}{
\begin{tabular}{||l||l||}
\hline
Parameter	&gibt an\\
\hline\hline
|prolog|	&Dieser Text erscheint am Anfang der Metapostdateien.\\
|epilog|	&Dieser Text erscheint am Ende der Metapostdateien.\\
|funcmp_rts|	&Parameter f"ur das Binary |funcmp_bin|.\\
|funcmp_bin|	&Name des Binaries.\\
|mp_bin|	&Name von \MP.\\
\hline
\end{tabular}
}


\Beispiel{Beispieldatei}{
\beside{0.9}{
\begin{verbatim}
prolog          = "verbatimtex\n\
                   \\\documentclass[10pt,oneside,a4paper,fleqn,leqno]{report}\n\
                   \\\usepackage{mflogo}\n\
                   \\\usepackage[german,english]{babel}\n\
                   \\\begin{document}\n\
                   \etex\n\n\
                   \input boxes\n\
                   \input FuncMP"
epilog          = "\n\n\\end"

defaultDX       = 3
defaultDY       = 3
textDX          = 2
textDY          = 2

funcmp_rts      = "+RTS -H24m -K1M -RTS"
funcmp_bin      = "./FuncMP"

mp_bin          = "virmp "
\end{verbatim}
}}

\end{document}



> main = do
>	generate "bound" 1 textBounding1
>	generate "primitives" 1 primitives
>	generate "text" 1 bsp0
>	generate "text" 3 bild3
>	generate "colors" 1 colors
>	generate "text" 4 from0
>	generate "punkte" 1 punkte
>	generate "automat" 1 automat1
>	generate "kreis" 1 kreis7
>	generate "drei" 1 drei
>	generate "spiral" 1 spirale
>	generate "bracket" 1 brack
>	generate "automat" 2 automat2
>	generate "arrow" 1 pfeil
>	generate "clip" 1 clipping
>	generate "pacman" 1 pac
>	generate "tree" 1 tree1
>	generate "tree" 2 tree2
>	generate "tree" 3 tree3
>	generate "tree" 4 tree4
>	generate "complex" 1 bsp10
>	generate "turtle" 1 turtle1
>	generate "turtle" 2 turtle2
>	generate "colorcirc" 1 colorcirc
>	generate "barchart" 1 barchart
>	generate "chip" 1 chip
>	generate "kompl" 1 kompl
>	generate "matrix" 1 matrBsp
