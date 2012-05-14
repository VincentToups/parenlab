function [files] = directoryFiles(theDir, postFilter)
%Return FILES in THE-DIR, as strings, excluding
%directories and anything which fails post-filter, which
%is by default always true.
if ~(strcmp(theDir(end), '/'))
  theDir = [ theDir '/' ];
else
  [];
end
;
if ~(exist('postFilter'))
  postFilter = @lambdaminus147394;
else
  [];
end
;
initialFiles = dir(theDir);
files = cellArray();
for s = flatAcross(initialFiles)
  name = [ theDir s.name ];
  if andFunction(@()~(s.isdir), @()funcall(postFilter, name))
    files{plus(end, 1)} = name;
  else
    [];
  end
  ;
end
;
function [retvalminus147395] = lambdaminus147394(x)
%
retvalminus147395 = 1;

end

end