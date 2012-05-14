function [r] = equal(a, b)
%Generalized, structural equality.
if strcmp(class(a), class(b))
  if isclass('cell', a)
    r = cellEqual(a, b);
  elseif isclass('struct', a)
    r = structEqual(a, b);
  elseif isclass('char', a)
    r = strcmp(a, b);
  elseif andFunction(@()isnumeric(a), @()isnumeric(b))
    r = all(equalequal(a, b));
  end
  ;
elseif andFunction(@()isnumeric(a), @()isnumeric(b))
  r = all(equalequal(a, b));
elseif 'otherwise'
  r = 0;
end
;

end