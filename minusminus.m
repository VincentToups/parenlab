function r=minusminus(v,varargin)
r = v;
for vi=1:length(varargin)
  item=varargin{vi};
  r = r - item;
end

