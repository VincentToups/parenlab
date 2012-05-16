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
  postFilter = @lambdaminus170740;
else
  [];
end
;
files = directoryFiles(root, postFilter);
subDirectories = directoryDirectories(root, postFilter);
forLoopValueminus170742 = flatAcross(subDirectories);
for i = ((1):(length(forLoopValueminus170742)))
  subRoot = forLoopValueminus170742(i);
  subRoot = subRoot{1};
  files = [ files treeFiles(subRoot, postFilter) ];
end
clear 'forLoopValueminus170742';
;
function [retvalminus170741] = lambdaminus170740(x)
%
retvalminus170741 = 1;

end

end