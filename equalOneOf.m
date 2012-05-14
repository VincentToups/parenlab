function [b] = equalOneOf(value, values)
%
b = 0;
forLoopValueminus147399 = flatAcross(values);
for i = ((1):(length(forLoopValueminus147399)))
  valuemtimes = forLoopValueminus147399(i);
  valuemtimes = valuemtimes{1};
  if equal(value, valuemtimes)
    b = 1;
    return;
  else
    [];
  end
  ;
end
clear 'forLoopValueminus147399';
;

end