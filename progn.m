function val=progn(varargin)
% Simulate a progn.
%

for vi=1:(length(varargin)-1)
  item=varargin{vi};
  if ischar(item)
    evalin('caller',item);
  else
    item();
  end
end
val = evalin ('caller',item);
