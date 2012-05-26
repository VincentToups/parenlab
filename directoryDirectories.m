function [files] = directoryDirectories(theDir, postFilter)
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
  postFilter = @lambdaminus60143;
else
  [];
end
;
initialFiles = dir(theDir);
files = cellArray();
for s = flatAcross(initialFiles)
  name = [ theDir s.name ];
  if andFunction(@()s.isdir, @()funcall(postFilter, name), @()~(strcmp('..', s.name)), @()~(strcmp('.', s.name)))
    files{plus(end, 1)} = name;
  else
    [];
  end
  ;
end
;
function [retvalminus60144] = lambdaminus60143(x)
%
retvalminus60144 = 1;

end

end