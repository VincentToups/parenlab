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
  postFilter = @lambdaminus111159;
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
function [retvalminus111160] = lambdaminus111159(x)
%
retvalminus111160 = 1;

end

end