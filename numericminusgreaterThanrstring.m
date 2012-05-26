function [s] = numericminusgreaterThanrstring(o)
%
if equalequal(1, length(o))
  s = sprintf('%d', o);
elseif equalequal(2, length(size(o)))
  s = '(matrix';
  for i = ((1):(size(o, 1)))
    for j = ((1):(size(o, 2)))
      s = [ s ' ' numericminusgreaterThanrstring(o(i, j)) ];
    end
    ;
    if ~(equalequal(i, size(o, 1)))
      s = [ s ' :' ];
    else
      [];
    end
    ;
  end
  ;
  ;
  s = [ s ')' ];
elseif 'otherwise'
  s = [ '(reshape ' numericminusgreaterThanrstring(flatDown(o)) ' ' numericminusgreaterThanrstring(size(o)) ')' ];
end
;

end