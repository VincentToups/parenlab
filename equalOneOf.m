function [b] = equalOneOf(value, values)
%
b = 0;
forLoopValueminus60153 = flatAcross(values);
for i = ((1):(length(forLoopValueminus60153)))
  valuemtimes = forLoopValueminus60153(i);
  valuemtimes = valuemtimes{1};
  if equal(value, valuemtimes)
    b = 1;
    return;
  else
    [];
  end
  ;
end
clear 'forLoopValueminus60153';
;

end