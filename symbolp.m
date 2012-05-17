function [b] = symbolp(o)
%
andFunction(@()isstruct(o), @()hasfield(o, 'type'), @()strcmp(o.type, 'symbol'));

end