function [s] = cellminusgreaterThanrstring(c)
%
if equalequal(1, length(c))
  s = sprintf('(cell-array %s)', objectminusgreaterThanplString(first(c)));
elseif equalequal(2, length(size(c)))
  s = '(matrix{}';
  for i = ((1):(size(c, 1)))
    for j = ((1):(size(c, 2)))
      s = [ s ' ' objectminusgreaterThanplString(c{i, j}) ];
    end
    ;
    if ~(equalequal(i, size(c, 1)))
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
  s = [ '(reshape ' cellminusgreaterThanrstring(flatDown(c)) ' ' numericminusgreaterThanrstring(size(c)) ')' ];
end
;

end