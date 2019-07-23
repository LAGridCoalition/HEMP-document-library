% fc_jpg(fc,otsn,fn)
% Creates a jpeg from a figure at a specified output time 
%  step number of a specified field component matrix.
% Returns the figure's handle.
% Companion file to fc_read.
% fc: field component matrix
% otsn: output time step number
% fn: jpeg file name (without extension)
% Example function call:
%  fig = fc_jpg(fc,10,'field');
% This makes jpeg file 'field.jpg' from pre-existing 
%  field component matrix fc at output time step 10.
  
function fig = print_jpg(fc,otsn,fn)
% cd C:\aardvark\matlab

fig = fc_animate(fc,otsn,otsn);
print(gcf,'-djpeg',fn)

