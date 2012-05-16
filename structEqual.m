function [r] = structEqual(s1, s2)
%
fields1 = sort(fieldnames(s1));
fields2 = sort(fieldnames(s2));
if andFunction(@()all(equalequal(size(s1), size(s2))), @()equalequal(length(fieldnames(s1)), length(fieldnames(s2))), @()all(strcmp(fields1, fields2)))
  if equalequal(countOf(s1), 1)
    forLoopValueminus170744 = flatAcross(fields1);
    for i = ((1):(length(forLoopValueminus170744)))
      f = forLoopValueminus170744(i);
      f = f{1};
      if ~(equal(s1.(f), s2.(f)))
        r = 0;
        return;
      else
        [];
      end
      ;
    end
    clear 'forLoopValueminus170744';
    ;
    r = 1;
  elseif 'otherwise'
    forLoopValueminus170745 = s1;
    for i = ((1):(length(forLoopValueminus170745)))
      ss1 = forLoopValueminus170745(i);
      ss2 = s2(i);
      if ~(equal(ss1, ss2))
        r = 0;
        return;
      else
        [];
      end
      ;
    end
    clear 'forLoopValueminus170745';
    ;
    r = 1;
  end
  ;
elseif 'otherwise'
  r = 0;
end
;

end