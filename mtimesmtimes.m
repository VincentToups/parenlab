function [r] = mtimesmtimes(varargin)
%What?
'What?';
r = 1;
for x = varargin
  r = mtimes(r, x{1});
end
;
