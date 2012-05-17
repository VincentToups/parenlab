function [result] = equaldigit(input)
%Parser one character from the set 0-9.
result = funcall(pbind(@equalitem, @lambdaminus82863), input);
function [retvalminus82864] = lambdaminus82863(i)
%
retvalminus82864 = fif(~(isempty(strfind('0123456789', i))), @()preturn(i), @()@equalnil);

end

end