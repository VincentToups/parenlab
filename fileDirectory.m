function [theDir] = fileDirectory(wholeFile)
%Return the directory part of a file.
ii = find(equalequal(wholeFile, '/'));
if isempty(ii)
  theDir = wholeFile;
else
  theDir = wholeFile(((1):(minus(ii(end), 1))));
end
;

end