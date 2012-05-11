function [r] = minusgreaterThan(struct, varargin)
%
r = struct;
for i = ((1):(length(varargin)))
  key = varargin{i};
  r = structAccess(r, key);
end
;

end