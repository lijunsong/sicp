beginfig(1);

u := 1cm;

pair p[];
p1 := (-3u, -3u);
p1.5 := (3u, 3u);
p2 := (0, -5u);
p2.5 := (5u, -5u);
p3 := (5u, -7u);
p4 := (7u, -7u);

draw p1 withpen pencircle scaled 3;
draw p1.5 withpen pencircle scaled 3;
draw p2 withpen pencircle scaled 3;
draw p2.5 withpen pencircle scaled 3;
draw p3 withpen pencircle scaled 3;
draw p4 withpen pencircle scaled 3;

draw origin--p1; label.bot("1", p1);
draw origin--p1.5;
draw p1.5--p2; label.bot("2", p2);
draw p1.5--p2.5;
draw p2.5--p3; label.bot("3", p3);
draw p2.5--p4; label.bot("4", p4);

endfig;
end;
