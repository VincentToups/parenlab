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
  postFilter = @lambdaminus60145;
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
function [retvalminus60146] = lambdaminus60145(x)
%
retvalminus60146 = 1;

end

end