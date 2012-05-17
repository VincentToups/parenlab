function [b] = keywordp(o)
%
andFunction(@()isstruct(o), @()hasfield(o, 'type'), @()strcmp(o.type, 'keyword'));

end