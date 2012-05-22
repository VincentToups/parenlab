function [files] = treeFiles(root, postFilter)
%Return the file-contents of the directory 
%ROOT. subject to the filter POST-FILTER.
if ~(strcmp(root(end), '/'))
  theDir = [ root '/' ];
else
  [];
end
;
if ~(exist('postFilter'))
  postFilter = @lambdaminus111161;
else
  [];
end
;
files = directoryFiles(root, postFilter);
subDirectories = directoryDirectories(root, postFilter);
forLoopValueminus111163 = flatAcross(subDirectories);
for i = ((1):(length(forLoopValueminus111163)))
  subRoot = forLoopValueminus111163(i);
  subRoot = subRoot{1};
  files = [ files treeFiles(subRoot, postFilter) ];
end
clear 'forLoopValueminus111163';
;
function [retvalminus111162] = lambdaminus111161(x)
%
retvalminus111162 = 1;

end

end