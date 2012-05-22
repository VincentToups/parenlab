function [b] = equalOneOf(value, values)
%
b = 0;
forLoopValueminus111167 = flatAcross(values);
for i = ((1):(length(forLoopValueminus111167)))
  valuemtimes = forLoopValueminus111167(i);
  valuemtimes = valuemtimes{1};
  if equal(value, valuemtimes)
    b = 1;
    return;
  else
    [];
  end
  ;
end
clear 'forLoopValueminus111167';
;

end