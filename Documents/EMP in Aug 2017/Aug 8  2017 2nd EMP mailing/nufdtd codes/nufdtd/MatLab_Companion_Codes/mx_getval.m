% mx_getval(mx,x,y)
% Gets Cartesian coordinate value from any output file matrix.
% mx: 2D matrix variable name
% x: x-direction coordinate
% y: y-direction coordinate
% Example function calls:
%  If mx is 2D, then use:
%   val = mx_getval(mx,3,42);
%  This gets the value from 2D matrix mx at Cartesian coordinate (3,42).
%  If mx is 3D, then use:
%   val = mx_getval(mx(:,:,7),3,42);
%  This gets the value from 2D matrix mx(:,:,1) at Cartesian coordinate (3,42).

function val = mx_getval(mx,x,y)
% cd C:\aardvark\nufdtd3d\monopole

mxtemp = fliplr(rot90(fliplr(mx)));
val = mxtemp(x,y);

