function [r] = mtimesmtimes(varargin)
%Multiply as many numbers as you'd like.
'Multiply as many numbers as you''d like.';
r = 1;
for x = varargin
  r = mtimes(r, x{1});
end
;
end
