function [b] = equalOneOf(value, values)
%
b = 0;
forLoopValueminus34215 = flatAcross(values);
for i = ((1):(length(forLoopValueminus34215)))
  valuemtimes = forLoopValueminus34215(i);
  valuemtimes = valuemtimes{1};
  if equal(value, valuemtimes)
    b = 1;
    return;
  else
    [];
  end
  ;
end
clear 'forLoopValueminus34215';
;

end