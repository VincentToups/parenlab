function [ignore] = deletemtimes(file, varargin)
%Just like DELETE* except deletes multiple files and returns a value.
ignore = 0;
delete(file);
forLoopValueminus60156 = flatAcross(varargin);
for i = ((1):(length(forLoopValueminus60156)))
  v = forLoopValueminus60156(i);
  v = v{1};
  delete(v);
end
clear 'forLoopValueminus60156';
;

end