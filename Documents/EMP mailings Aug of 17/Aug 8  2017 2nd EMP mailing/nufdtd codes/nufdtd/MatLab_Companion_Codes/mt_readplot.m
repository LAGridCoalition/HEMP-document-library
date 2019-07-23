% mt_readplot(mdir,mloc)
% Reads and plots material output files.
% mdir,mloc = slice direction, location
%  mdir: valid values {'x','y','z'}
%   'x' = slice in y-z direction
%   'y' = slice in x-z direction
%   'z' = slice in x-y direction
%  mloc: valid values [1,n@]
%   where n@ = # points in slice direction
% Example function call:
%  mt = mt_readplot('y',25);
% This reads & plots the material distribution at the slice y=25

function mt = mt_readplot(mdir,mloc)
% cd C:\aardvark\nufdtd3d\monopole

% Create file name depending on how many digits mloc has
if mloc<1  % mloc too small
    disp('Error: mloc < 1')
    return
elseif mloc<10  % mloc a 1-digit number
    fname = [mdir,'00',num2str(mloc),'.dat'];
elseif mloc<100  % mloc a 2-digit number
    fname = [mdir,'0',num2str(mloc),'.dat'];
elseif mloc<1000  % mloc a 3-digit number
    fname = [mdir,num2str(mloc),'.dat'];
else  % mloc too large
    disp('Error: mloc >= 1000')
    return
end    

mt = eval([' importdata(''mt_',fname,''');']);

imagesc(flipud(mt))  % plot
axis xy  % display cartesian coordinates
colorbar
title(['id\_',fname,' : material distribution at ',mdir,'=',num2str(mloc)],'Fontsize',14)
xlabel('x/y-direction','Fontsize',14)
ylabel('y/z-direction','Fontsize',14)

