input boxes;

u := 0.8cm;

%vardef cuta(suffix a, b) expr p =
%  drawarrow p cutbefore bpath.a cutafter bpath.b;
%  point .5*length p of p
%enddef;

vardef cuta(suffix a, b)=
  path p;
  p := a.c..b.c;
  drawarrow p cutbefore bpath.a cutafter bpath.b;
  point .5*length p of p
enddef;

def arrow(suffix a, b)=
  path p;
  p := a.c..b.c;
  drawarrow p cutbefore bpath.a cutafter bpath.b;
enddef;

def put(suffix node, left, right)= 
  begingroup;
    node.c - left.c = whatever*dir(60);
    (xpart node.c) - (xpart left.c) = 2u;
    right.c - node.c = whatever*dir(-60);
    (xpart right.c) - (xpart node.c) = 2u;
  endgroup;
enddef;

beginfig(1);
verbatimtex \def\stk#1#2{$\displaystyle{\matrix{#1\cr#2\cr}}$} etex

circleit.aa(btex \stk{amount:11}{kind:5} etex); a.dx = a.dy;
circleit.bb(btex \stk{amount:11}{kind:4} etex);
circleit.cc(btex $amount:-39$ etex);
circleit.dd(btex \stk{amount:11}{kind:3} etex);
circleit.ee(btex $amount:-14$ etex);
circleit.ff(btex \stk{amount:11}{kind:2} etex);
circleit.gg(btex \stk{amount:1}{kind:3} etex);
circleit.hh(btex \stk{amount:11}{kind:1} etex);
circleit.ii(btex \stk{amount:6}{kind:2} etex);


put(aa, bb, cc);
put(bb, dd, ee);
put(dd, ff, gg);
put(ff, hh, ii);
drawboxed(aa, bb, dd, ff, gg, hh, ii);
drawoptions(withcolor red);
drawboxed(cc, ee);
drawoptions();

label.urt(btex $11-50$ etex, cuta(aa,cc));
arrow(aa, bb);
label.urt(btex $11-25$ etex, cuta(bb,ee));
arrow(bb,dd);
label.urt(btex $11-10$ etex, cuta(dd,gg));
arrow(dd, ff);
label.urt(btex $11-5$ etex, cuta(ff, ii));
arrow(ff, hh);
label.bot(btex $\vdots$ etex, hh.s);
label.bot(btex $\vdots$ etex, ii.s);
label.bot(btex $\vdots$ etex, gg.s);
endfig;
end;
