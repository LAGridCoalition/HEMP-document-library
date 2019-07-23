% fc_img_mv(nf,nts,im_ots,cvals)
% Reads sequences of field component output files into matrices.
%  Program fc_read is used for this.
% Creates .avi movie files and .jpg image files from these matrices.
%  Program fc_animate2 is used for this.
% The field component slices to be read in should be in a file in
%  this directory called "fc_slices.txt".  Slice information should
%  be in the same format as used in nufdtd3d input:
%   (field,direction,location)
%     field: Field to output.
%      1 - Hx; 2- Hy; 3 - Hz; 4 - Ex; 5 - Ey; 6 - Ez
%     direction: Direction of slice.
%      1 - x; 2 - y; 3 - z
%     location: Location (coordinate) of slice.
%  For example, 
%    4 2   25
%   corresponds to Ex at y=25.
% nf: Number of files per field component sequence
% nts: Number of time steps in simulation
% im_ots: Output time steps for printing images
%  This must be a 1D array.
% cvals: caxis (image cutoff) values for all movies and images.
%  This must be a 1D array.
%  Images & movies will be truncated at +/- cval
%   using the command caxis([-cval,cval])

function fc_img_mv(nf,nts,im_ots,cvals)
% cd C:\aardvark\nufdtd3d_mur\thesisResults\spiral\spiral8

fcs = importdata('fc_slices.txt');

for nfc=1:size(fcs,1)  % for each field component slice
%for nfc=1:size(fcs,1)  % for each field component slice
    switch fcs(nfc,1)  % Field component
        case 1  % Hx
            fc='hx';
        case 2  % Hy
            fc='hy';
        case 3  % Hz
            fc='hz';
        case 4  % Ex
            fc='ex';
        case 5  % Ey
            fc='ey';
        case 6  % Ez
            fc='ez';
    end
    switch fcs(nfc,2)  % Slice direction
        case 1  % x
            sd='x';
        case 2  % y
            sd='y';
        case 3  % z
            sd='z';
    end
    sc_int = fcs(nfc,3);  % Slice coordinate (integer)
    if sc_int < 10  % Output time step
        sc_str = ['00',num2str(sc_int)];  % Output time step string
    elseif sc_int < 100
        sc_str = ['0',num2str(sc_int)];
    elseif sc_int < 1000  % image output time step
        sc_str = num2str(sc_int);
    end
    
    fcseq = fc_read(fc,sd,sc_int,nf);  % Read in field component sequence
        
    for nc=1:size(cvals,2)  % For each cvals (plot precision) value
        czs = '';
        for cz=1:ceil(log10(cvals(nc)))  % For each cvals zero in mvfn
            czs = [czs,'0'];  % Add a zero
        end

        mv = fc_animate2(fcseq,1,nf,cvals(nc));  % Plot field component sequence
        
        % cvals(nc) = coef * 10^expval
        expval = floor(log10(cvals(nc)));  % exponent value
        coef = cvals(nc)/10^expval;  % coefficient
        if expval<0  % exponent value is negative, i.e. |cvals(nc)| < 1
            expsign = 'n';
        else  % exponent value is non-negative, i.e. |cvals(nc)| >= 1
            expsign = 'p';
        end
        c_str = [num2str(coef),'e',expsign,num2str(abs(expval))];
            
        mvfn = ['mv_',fc,'_',sd,sc_str,'_c',c_str,'_t',nts,'.avi'];  % movie file name
        movie2avi(mv,mvfn);  % Create movie file

        for nim=1:size(im_ots,2)  % For each im_ots (image output time step) value
            fig = fc_animate2(fcseq,im_ots(nim),im_ots(nim),c(nc));
            if im_ots(nim) < 10  % Output time step
                ots_str = ['00',num2str(im_ots(nim))];  % Output time step string
            elseif im_ots(nim) < 100
                ots_str = ['0',num2str(im_ots(nim))];
            elseif im_ots(nim) < 1000  % image output time step
                ots_str = num2str(im_ots(nim));
            end
            imfn = ['im_',fc,'_',sd,sc_str,'_c',c_str,'_t',ots_str,'.jpg'];  % movie file name
            print(gcf,'-djpeg',imfn);  % Create image file
        end
    end
    
end

return

