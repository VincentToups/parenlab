function [b] = equalOneOf(value, values)
%
b = 0;
forLoopValueminus77714 = flatAcross(values);
for i = ((1):(length(forLoopValueminus77714)))
  valuemtimes = forLoopValueminus77714(i);
  valuemtimes = valuemtimes{1};
  if equal(value, valuemtimes)
    b = 1;
    return;
  else
    [];
  end
  ;
end
clear 'forLoopValueminus77714';
;

end