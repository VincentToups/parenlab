function [files] = directoryContents(theDir, postFilter)
%Return DIRECTORIES in THE-DIR, as strings, excluding
%directories and anything which fails post-filter, which is by
%default always true.
if ~(strcmp(theDir(end), '/'))
  theDir = [ theDir '/' ];
else
  [];
end
;
if ~(exist('postFilter'))
  postFilter = @lambdaminus34207;
else
  [];
end
;
initialFiles = dir(theDir);
files = cellArray();
for s = flatAcross(initialFiles)
  name = [ theDir s.name ];
  files{plus(end, 1)} = name;
end
;
function [retvalminus34208] = lambdaminus34207(x)
%
retvalminus34208 = 1;

end

end