function [oparser] = equalgreaterThanstring(s)
%Return a parser which matches the string S only.
oparser = pbind(equalgreaterThanitems(length(s)), @lambdaminus82861);
function [retvalminus82862] = lambdaminus82861(subString)
%
retvalminus82862 = fif(strcmp(subString, s), @()preturn(s), @()@equalnil);

end

end