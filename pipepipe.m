function r=pipepipe(o,varargin)
% vectorized or operation.
r = o;
for vi=1:length(varargin)
  item=varargin{vi};
  r = r | item;
end
