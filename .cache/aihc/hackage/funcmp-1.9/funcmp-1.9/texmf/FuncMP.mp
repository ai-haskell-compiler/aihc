%% 
%% FMP Low-Level MetaPost Code
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

vardef bitline(expr x, y, d)(text s) =
 save i,ha,hb,h;
 string line;
 line:="";
 for i=0 upto length s div 2-1:
   ha:= hex(substring(i*2,i*2+1) of s);
   hb:= hex(substring(i*2+1,i*2+2) of s);
   h := ha*16+hb;
   line := line & char h;
 endfor;
 if d=24:
   draw (line infont "fmp24") shifted (x, y) scaled 1;
 else: 
   if d=8:
     draw ((line&char 255) infont "fmp8") shifted (x, y) scaled 1;
   else:
     draw (line infont "fmp1") shifted (x, y) scaled 1;
   fi;
 fi;
enddef;

% ---------------------------------

vardef shapeit@# =
  beginbox_("shapepath_","sizebox_",@#,"");
  generic_declare(string) _n.p;
  generic_declare(pair) _n.n, _n.ne, _n.e, _n.se, _n.s, _n.sw, _n.w, _n.nw, _n.c;
enddef;

def shapepath_(suffix $) =
  scantokens $.p
enddef;

% ---------------------------------

vardef cloneit@#(suffix $) =
  _n_		:= str @#;
  generic_declare(pair)		_n.off, _n.c;
  generic_declare(string)	pproc_._n, sproc_._n, _n.p;
  generic_declare(picture)	pic_._n;
  pproc_@#	:= pproc_$;
  sproc_@#	:= sproc_$;
  pic_@#	:= nullpicture;
  @#p		= $p;
  generic_declare(pair) _n.sw, _n.s, _n.se, _n.e, _n.ne, _n.n, _n.nw, _n.w;
  generic_declare(numeric) _n.dx, _n.dy;

  @#c		= $c;
  @#off		= $off;
  @#n		= $n;
  @#ne		= $ne;
  @#e		= $e;
  @#se		= $se;
  @#s		= $s;
  @#sw		= $sw;
  @#w		= $w;
  @#nw		= $nw;
  @#c		= $c;
  @#dx		= $dx;
  @#dy		= $dy;
enddef;

% ---------------------------------

vardef trans(expr a,b,c,d,e,f) =
save localtrans;
transform localtrans;

xpart  localtrans = e;
ypart  localtrans = f;
xxpart localtrans = a;
xypart localtrans = b;
yxpart localtrans = c;
yypart localtrans = d;

localtrans
enddef;

% ---------------------------------

def graduate(expr a, b, border, n, alpha) =
begingroup
  picture pict,pictt;
  numeric h;
 
  pict:=nullpicture;
  pictt:=nullpicture;
 
  addto pictt doublepath border rotated -alpha;
  h:=ypart (ulcorner pictt-llcorner pictt);
  for i=0 upto n:
  addto pict doublepath llcorner pictt + (0,i/n*h) -- lrcorner pictt + (0,i/n*h)
    withcolor i/n*a+(n-i)/n*b withpen pencircle scaled (h/n+1);
  endfor
  pict:=pict rotated alpha;
  clip pict to border;
  draw pict;
endgroup
enddef;

def graduatePathNoCycle(expr a, b, p, pat, pen, n) =
if picture pat:
  begingroup
    picture pict,pictt;
    path circ;
    numeric h;

    pict:=nullpicture;
    pictt:=nullpicture;
    for i=0 upto n-1:
      pictt:=nullpicture;
      if numeric pen: addto pictt doublepath p dashed pat;
      else:       addto pictt doublepath p dashed pat withpen pen;
      fi
      circ := point i/n*length p of p .. point (i+1)/n*length p of p .. cycle;
      clip pictt to circ;
      addto pict also pictt withcolor (i/n)*b+(1-i/n)*a;
    endfor;
    draw pict;
  endgroup
else:
  begingroup
    for i=0 upto (n-1):
      draw subpath (i/n*length p,(i+1)/n*length p) of p withcolor (i/n)*b+(1-i/n)*a withpen pen;
    endfor
  endgroup
fi
enddef;

def graduatePath(expr a, b, p, pat, pen, n, alpha) =
if cycle p:
  begingroup
    phi := alpha / 360;
    graduatePathNoCycle(a, b, subpath (phi*length p,(phi+0.5)*length p) of p, pat, pen, n/2);
    graduatePathNoCycle(b, a, subpath ((phi+0.5)*length p,(1+phi)*length p) of p, pat, pen, n/2);
  endgroup
else: 
  graduatePathNoCycle(a, b, p, pat, pen, n);
fi
enddef;

def graduatePic(expr a, b, pic, n, alpha) =
  picture pict,pictt;
  path p;
  numeric h;

  pict:=nullpicture;
  pictt:=pic rotated -alpha;
  h:=ypart (ulcorner pictt-llcorner pictt);
  p:=llcorner pictt -- lrcorner pictt --
     lrcorner pictt + (0,1/n*h) -- llcorner pictt + (0,1/n*h) -- cycle;

  for i=0 upto n:
   pictt:=pic rotated -alpha;
   clip pictt to p shifted (0,i/n*h);
   draw pictt rotated alpha withcolor i/n*a+(n-i)/n*b;
  endfor;
enddef;

vardef varrowheadFull (expr p,l,a) =
  save q,e; path q,r; pair e;
  e = point length p of p;
  q = gobble(p shifted -e cutafter makepath(pencircle scaled 2l))
    cuttings;
  if a < 180:
    (q rotated .5a & reverse q rotated -.5a -- cycle) shifted e
  else:
    r = (q rotated .5a & reverse q rotated -.5a -- cycle);
    (r shifted -(point 2.5 of r) shifted e)
  fi
enddef;

vardef varrowhead (expr p,l,a) =
  save q,e; path q,r; pair e;
  e = point length p of p;
  q = gobble(p shifted -e cutafter makepath(pencircle scaled 2l))
    cuttings;
  (q rotated .5a & reverse q rotated -.5a) shifted e
enddef;
