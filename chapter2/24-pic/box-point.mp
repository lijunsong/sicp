input boxes;
u := 1cm;
defaultscale := 13pt/fontsize(defaultfont);

def joinpair(suffix ba, bb) = 
    ba.ne = bb.nw; ba.se = bb.sw;
enddef;

def makepair suffix $ =
    forsuffixes $$=$car, $cdr: 
        boxit$$(btex $\bullet$ etex scaled 2);
    endfor;
    joinpair($car, $cdr);
enddef;

def drawpair suffix $ =
    forsuffixes $$=$car, $cdr:
        drawboxed($$);
    endfor;
enddef;

beginfig(1);
interim defaultdy:=0.5u;
interim defaultdx:=0.5u;

makepair p1;
makepair p2;
makepair p4;
makepair p5;
makepair p6;
makepair p7;

boxit.n1("1");
boxit.n2("2");
boxit.n3("3");
boxit.n4("4");

% p1 and p2
p1cdr.e - p2car.w = (-2u, 0);
p1car.c - n1.c = (0, 2u);

% p2 and p4
p2car.c - p4car.c = (0, 2u);
p4car.c - n2.c = (0, 2u);

% p4 and p5
p4cdr.e - p5car.w = (-2u, 0);

% p5 and p6
p5car.c - p6car.c = (0, 2u);

% p6 and p7, n3
p6car.c - n3.c = (0, 2u);
p6cdr.e - p7car.w = (-2u, 0);

% p7
p7car.c - n4.c = (0, 2u);

drawpair p1;
drawpair p2;
drawpair p4;
drawpair p5;
drawpair p6;
drawpair p7;

drawboxed(n1, n2, n3, n4);
drawarrow p1car.c--n1.n;
drawarrow p1cdr.c--p2car.w;
drawarrow p2car.c--p4car.n;
drawarrow p4car.c--n2.n;
drawarrow p4cdr.c--p5car.w;
drawarrow p5car.c--p6car.n;
drawarrow p6car.c--n3.n;
drawarrow p6cdr.c--p7car.w;
drawarrow p7car.c--n4.n;

% draw nil
draw p2cdr.sw--p2cdr.ne;
draw p5cdr.sw--p5cdr.ne;
draw p7cdr.sw--p7cdr.ne;

endfig;
end;
