function r = divide(n,varargin)
r = n;
for vi=1:length(varargin)
  item=varargin{vi};
  r = r/item;
end
