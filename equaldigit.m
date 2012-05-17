function [result] = equaldigit(input)
%Parser one character from the set 0-9.
result = funcall(pbind(@equalitem, @lambdaminus77724), input);
function [retvalminus77725] = lambdaminus77724(i)
%
retvalminus77725 = fif(~(isempty(strfind('0123456789', i))), @()preturn(i), @()@equalnil);

end

end