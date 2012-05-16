function [name] = fileName(wholeFile)
%Return the name part of a file.
ii = find(equalequal(wholeFile, '/'));
if isempty(ii)
  name = wholeFile;
else
  name = wholeFile(((plus(1, ii(end))):(end)));
end
;

end