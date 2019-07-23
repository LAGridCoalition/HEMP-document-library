% fc_read(mfld,mdir,mloc,nots)
% Reads a sequence of field component output files.
% Returns 3D matrix containing the field component data.
%  This matrix can be animated using fc_animate or fc_animate2.
% This program is called by fc_img_mv.
%  First two dimensions: Spatial dimensions of data.
%  Third dimension: Output time step number.
% mfld,mdir,mloc = slice field, direction, location
%  mfld: valid values {'ex','ey','ez','hx','hy','hz'}
%   values correspond to field component being read
%  mdir: valid values {'x','y','z'}
%   'x' = slice in x-direction, i.e. along y-z plane
%   'y' = slice in y-direction, i.e. along x-z plane
%   'z' = slice in z-direction, i.e. along x-y plane
%  mloc: valid values [1,n@]
%   where n@ = # points in slice direction
% nots: Number of Output Time Steps (number of files) to read in
% Example function call:
%  fc = fc_read('ex','z',31,100);
% This reads Ex at the slice y=25 for output time steps 1 through 100

function fc = fc_read(mfld,mdir,mloc,nots)
% cd C:\aardvark\nufdtd3d_mur\thesisResults\spiral\spiral9dielec

% Create field component file name depending on how many digits mloc has
if mloc<1  % mloc too small
    disp('Error: mloc < 1')
    return
elseif mloc<10  % mloc a 1-digit number
    fname = [mfld,'_',mdir,'00',num2str(mloc)];
elseif mloc<100  % mloc a 2-digit number
    fname = [mfld,'_',mdir,'0',num2str(mloc)];
elseif mloc<1000  % mloc a 3-digit number
    fname = [mfld,'_',mdir,num2str(mloc)];
else  % mloc too large
    disp('Error: mloc >= 1000')
    return
end

% nots error catches
if nots<1  % nots too small
    disp('Error: nots < 1')
    return
elseif nots>=1000  % nots too large
    disp('Error: nots >= 1000')
    return
end

for n=1:nots  % for every output time step
    if n<10  % n a 1-digit number
        fc(:,:,n) = eval(['importdata(''',fname,'_t00',num2str(n),'.dat'');']);
    elseif n<100
        fc(:,:,n) = eval(['importdata(''',fname,'_t0',num2str(n),'.dat'');']);
    elseif n<1000
        fc(:,:,n) = eval(['importdata(''',fname,'_t',num2str(n),'.dat'');']);
    end
end
