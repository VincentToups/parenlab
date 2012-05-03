function r=tildaequal(a,b,varargin)
r = a ~= b;
for vi=1:length(varargin)
  item=varargin{vi};
  r = r ~= item;
end
