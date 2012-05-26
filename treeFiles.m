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
  postFilter = @lambdaminus60147;
else
  [];
end
;
files = directoryFiles(root, postFilter);
subDirectories = directoryDirectories(root, postFilter);
forLoopValueminus60149 = flatAcross(subDirectories);
for i = ((1):(length(forLoopValueminus60149)))
  subRoot = forLoopValueminus60149(i);
  subRoot = subRoot{1};
  files = [ files treeFiles(subRoot, postFilter) ];
end
clear 'forLoopValueminus60149';
;
function [retvalminus60148] = lambdaminus60147(x)
%
retvalminus60148 = 1;

end

end