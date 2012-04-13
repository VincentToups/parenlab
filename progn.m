function val=progn(varargin)
% Simulate a progn.
%

for vi=1:(length(varargin)-1)
  item=varargin{vi};
  evalin('caller',item);
end
val = evalin ('caller',item);
