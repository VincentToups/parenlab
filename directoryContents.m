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
  postFilter = @lambdaminus170738;
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
function [retvalminus170739] = lambdaminus170738(x)
%
retvalminus170739 = 1;

end

end