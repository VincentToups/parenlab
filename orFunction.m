function r = orFunction(varargin)
r = 0;
for vi=1:length(varargin)
  item=varargin{vi};
  pr = item();
  if pr
    r = pr;
    return;
  end
end
