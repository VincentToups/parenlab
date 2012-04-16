function [r] = plusplus(varargin)
%What?
'What?';
r = 0;
for x = varargin
  r = plus(r, x{1});
end
;
