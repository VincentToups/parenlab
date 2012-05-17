function [oparser] = equalgreaterThanstring(s)
%Return a parser which matches the string S only.
oparser = pbind(equalgreaterThanitems(length(s)), @lambdaminus77722);
function [retvalminus77723] = lambdaminus77722(subString)
%
retvalminus77723 = fif(strcmp(subString, s), @()preturn(s), @()@equalnil);

end

end