function [ignore] = deletemtimes(file, varargin)
%Just like DELETE* except deletes multiple files and returns a value.
ignore = 0;
delete(file);
forLoopValueminus111170 = flatAcross(varargin);
for i = ((1):(length(forLoopValueminus111170)))
  v = forLoopValueminus111170(i);
  v = v{1};
  delete(v);
end
clear 'forLoopValueminus111170';
;

end