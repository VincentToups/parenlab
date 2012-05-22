function [result] = equaldigit(input)
%Parser one character from the set 0-9.
result = funcall(pbind(@equalitem, @lambdaminus108380), input);
function [retvalminus108381] = lambdaminus108380(i)
%
retvalminus108381 = fif(~(isempty(strfind('0123456789', i))), @()preturn(i), @()@equalnil);

end

end