function r=dotdivide(r,varargin)
%

for vi=1:length(varargin)
  item=varargin{vi};
  r = r ./ item;
end

