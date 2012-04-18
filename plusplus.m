function [r] = plusplus(varargin)
%Add as many numbers as you'd like.
'Add as many numbers as you''d like.';
r = 0;
for x = varargin
  r = plus(r, x{1});
end
;
end
