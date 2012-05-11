function [g] = divideand(varargin)
%Return a function G which is the logical and of the results of passing args to all of the functions in varargin.
functions = varargin;
;
function [b] = divideandInner(varargin)
%
b = funcall(first(functions, varargin));
for fun = flatAcross(functions{((2):(end))})
  fun = fun{1};
  b = andFunction(@()b, @()funcall(fun, varargin));
  if ~(b)
    return;
  else
    [];
  end
  ;
end
;

end

end