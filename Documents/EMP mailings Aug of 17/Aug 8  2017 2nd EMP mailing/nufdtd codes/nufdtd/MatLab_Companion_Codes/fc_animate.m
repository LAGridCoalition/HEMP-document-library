% fc_animate(mname,nstart,nend)
% Creates an animation of field output files
%  from an existing MatLab matrix.
% Returns a matrix containing the animation.
% Companion file to fc_read.
% mname: matrix to plot
% nstart, nend: starting, ending output time step numbers
%  For example, use (5,34) for plotting from the fifth to
%   the 34th output time step number.
% To view the movie, try the MatLab command "movie".
% To print video files, try MatLab the command "movie2avi".
% Example function call:
%  mv = fc_animate(fc,1,100);
% This animates pre-existing field component matrix fc
%  for output time steps 1 through 100
% Example video file print:
%  movie2avi(mv,'movie.avi');
% This turns the pre-existing movie matrix mv
%  into the avi file 'movie.avi'
  
function mv = fc_animate(mname,nstart,nend)
% cd C:\aardvark\nufdtd3d_mur\thesisResults\spiral\spiral5fs

% (low, low-mid, middle, high-mid, high) = (black, blue, orange, yellow, white)
CMRmap  =  [0.00 0.00 0.00;  0.15 0.15 0.50;  0.30 0.15 0.75;  0.60 0.20 0.50;
  1.00 0.25 0.15;  0.90 0.50 0.00;  0.90 0.75 0.10;  0.90 0.90 0.50;  1.00 1.00 1.00 ];

% (low, low-mid, middle, high-mid, high) = (light blue, blue, black, brown, light brown/yellow)
CMRmap2 = [.75 1 1;.6 .9 1;.45 .75 .9;.45 .5 .75;.35 .25 .55;.2 .05 .3;.1 0 .2; 0 0 0;
  .07 .05 0;.15 .1 0;.2 .2 .1;.55 .4 0;.75 .7 .1;.9 .85 .2;1 1 .7];

mnamesize = size(mname);
nots = mnamesize(3);  % Number of Output Time Steps

% Error catches
if nstart > nend
    disp('Error: nstart > nend')
    return
elseif nstart < 1
    disp('Error: nstart < 1')
    return
elseif nend > nots
    disp(['Error: nend too large.  Need nend <= ',num2str(nots)])
    return
end

for n=nstart:nend  % For each desired output time step
    imagesc(flipud(mname(:,:,n)))  % Plot.  Coordinate flip needed for conversion to Cartesian coordinates.
    axis xy  % Display cartesian coordinates
    colormap(CMRmap)
    title(['Output Time Step ',num2str(n)],'Fontsize',14)
    xlabel('x/y-direction','Fontsize',14)
    ylabel('y/z-direction','Fontsize',14)
    grid off
    caxis([-.01,.01])  % Dependent axis range
    colorbar
    axis image
    mv(n)=getframe;  % For making a movie
    pause(.02)
end

