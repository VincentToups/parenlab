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
  postFilter = @lambdaminus34209;
else
  [];
end
;
files = directoryFiles(root, postFilter);
subDirectories = directoryDirectories(root, postFilter);
forLoopValueminus34211 = flatAcross(subDirectories);
for i = ((1):(length(forLoopValueminus34211)))
  subRoot = forLoopValueminus34211(i);
  subRoot = subRoot{1};
  files = [ files treeFiles(subRoot, postFilter) ];
end
clear 'forLoopValueminus34211';
;
function [retvalminus34210] = lambdaminus34209(x)
%
retvalminus34210 = 1;

end

end