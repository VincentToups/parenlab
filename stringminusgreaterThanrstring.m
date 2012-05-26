function [s] = stringminusgreaterThanrstring(ms)
%
s = '"';
for c = ms
  if strcmp(c, '"')
    s = [ s '\"' ];
  else
    s = [ s c ];
  end
  ;
end
;
;
s = [ s '"' ];

end