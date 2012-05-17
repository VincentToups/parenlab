function [r] = cellEqual(c1, c2)
%
if ~(all(equalequal(size(c2), size(c1))))
  r = 0;
  return;
else
  [];
end
;
c1 = flatAcross(c1);
c2 = flatAcross(c2);
forLoopValueminus77711 = flatAcross(c1);
for i = ((1):(length(forLoopValueminus77711)))
  c1El = forLoopValueminus77711(i);
  c1El = c1El{1};
  c2El = c2{i};
  if ~(equal(c1El, c2El))
    r = 0;
    return;
  else
    [];
  end
  ;
end
clear 'forLoopValueminus77711';
;
r = 1;

end