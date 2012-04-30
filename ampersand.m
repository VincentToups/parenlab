function r = ampersand(varargin)
r = varargin{1};
for vi=1:length(varargin)
  item=varargin{vi};
  r = r & item;
end
