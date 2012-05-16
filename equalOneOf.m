function [b] = equalOneOf(value, values)
%
b = 0;
forLoopValueminus170746 = flatAcross(values);
for i = ((1):(length(forLoopValueminus170746)))
  valuemtimes = forLoopValueminus170746(i);
  valuemtimes = valuemtimes{1};
  if equal(value, valuemtimes)
    b = 1;
    return;
  else
    [];
  end
  ;
end
clear 'forLoopValueminus170746';
;

end