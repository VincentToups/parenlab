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
  postFilter = @lambdaminus77708;
else
  [];
end
;
files = directoryFiles(root, postFilter);
subDirectories = directoryDirectories(root, postFilter);
forLoopValueminus77710 = flatAcross(subDirectories);
for i = ((1):(length(forLoopValueminus77710)))
  subRoot = forLoopValueminus77710(i);
  subRoot = subRoot{1};
  files = [ files treeFiles(subRoot, postFilter) ];
end
clear 'forLoopValueminus77710';
;
function [retvalminus77709] = lambdaminus77708(x)
%
retvalminus77709 = 1;

end

end